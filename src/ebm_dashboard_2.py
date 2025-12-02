# app.py
import os
import glob
import io
import base64
from typing import Tuple, List, Dict, Any

import joblib
import pandas as pd
import numpy as np
import plotly.graph_objs as go
import streamlit as st


# ---------- Helpers to load explanation objects ----------
def find_joblib_file(prefixes: List[str], folder: str = "models") -> str:
    """
    Find the first file in folder whose filename lower contains any of the prefixes (lower).
    Returns path or empty string if not found.
    """
    if not os.path.isdir(folder):
        return ""
    files = glob.glob(os.path.join(folder, "*.joblib")) + glob.glob(os.path.join(folder, "*.pkl"))
    for p in prefixes:
        for f in files:
            if p.lower() in os.path.basename(f).lower():
                return f
    # fallback: if only one joblib file exists use it
    if len(files) == 1:
        return files[0]
    return ""

def load_joblib_safely(path: str):
    try:
        return joblib.load(path)
    except Exception as e:
        st.error(f"Failed to load {path}: {e}")
        return None

# ---------- Helper functions to extract data from InterpretML explanation objects ----------
def extract_global_importances(expl_global: Any) -> pd.DataFrame:
    """
    Try multiple common InterpretML shapes to produce a DataFrame with columns:
    ['feature', 'importance', 'direction' (optional), 'raw'].
    If expl_global.data is already a DataFrame we'll try to coerce it.
    """
    if expl_global is None:
        return pd.DataFrame(columns=["feature", "importance"])

    data = getattr(expl_global, "data", None)
    # If it's a DataFrame already
    if isinstance(data, pd.DataFrame):
        df = data.copy()
        # try to identify importance column
        if "importance" not in df.columns:
            # try common column names
            for cand in ["scores", "score", "importance", "importances", "global_importance"]:
                if cand in df.columns:
                    df = df.rename(columns={cand: "importance"})
                    break
        # if feature names are index
        if df.index.names and df.index.shape[0] == df.shape[0]:
            df = df.reset_index()
            if "index" in df.columns:
                df = df.rename(columns={"index": "feature"})
        if "feature" not in df.columns and df.shape[1] >= 1:
            df = df.rename(columns={df.columns[0]: "feature"})
        if "importance" not in df.columns and df.shape[1] >= 2:
            df = df.rename(columns={df.columns[1]: "importance"})
        return df[["feature", "importance"]].dropna()
    # If data is dict (common in InterpretML: data -> {'scores':..., 'names':...} )
    if isinstance(data, dict):
        # Try common keys
        names = None
        importances = None
        for k in ["names", "feature_names", "features", "columns"]:
            if k in data:
                names = data[k]
                break
        for k in ["scores", "importance", "importances", "global_importance"]:
            if k in data:
                importances = data[k]
                break
        # Another structure: data = {'scores': {'names':[], 'scores':[]}}
        if importances is None and "scores" in data and isinstance(data["scores"], dict):
            s = data["scores"]
            if "names" in s and "scores" in s:
                names = s["names"]
                importances = s["scores"]
        if names is None and importances is None:
            # Try top-level attributes
            if hasattr(expl_global, "feature_importances_"):
                importances = getattr(expl_global, "feature_importances_")
            if hasattr(expl_global, "feature_names"):
                names = getattr(expl_global, "feature_names")
        if importances is None:
            # inspect entire dict for any list-like of length >1
            for v in data.values():
                if isinstance(v, (list, np.ndarray)) and len(v) > 0:
                    if importances is None:
                        importances = v
                    elif names is None and all(isinstance(x, str) for x in v):
                        names = v
        if names is None and importances is not None:
            # fallback: make generic names
            names = [f"f{i}" for i in range(len(importances))]
        if importances is None:
            return pd.DataFrame(columns=["feature", "importance"])
        # normalize to arrays
        names = list(names)
        importances = list(importances)
        df = pd.DataFrame({"feature": names, "importance": importances})
        return df
    # Last resort: maybe expl_global has .feature_importance or .global_importance attributes
    if hasattr(expl_global, "feature_importance"):
        fi = getattr(expl_global, "feature_importance")
        if isinstance(fi, dict):
            df = pd.DataFrame(list(fi.items()), columns=["feature", "importance"])
            return df
    # nothing found -> empty
    return pd.DataFrame(columns=["feature", "importance"])

def extract_global_dependence(expl_global: Any, feature: str) -> Tuple[np.ndarray, np.ndarray]:
    """
    Attempt to extract per-feature dependence arrays: (x_values, y_values)
    InterpretML sometimes stores partial dependence info in expl_global.data or expl_global.local_importance.
    We'll try to heuristically find arrays for the chosen feature.
    If not available, return empty arrays.
    """
    data = getattr(expl_global, "data", None)
    if data is None:
        return np.array([]), np.array([])

    # If data is DataFrame and has a column like '{feature}_values' or 'values' and 'mean'
    if isinstance(data, pd.DataFrame):
        # try to find columns that seem to correspond
        # e.g. columns: ['feature', 'values', 'mean']
        rows = data[data.iloc[:, 0] == feature]
        if not rows.empty:
            # If there is a 'values' field that stores list-like
            for col in rows.columns:
                val = rows.iloc[0][col]
                if isinstance(val, (list, np.ndarray)):
                    y = np.array(val)
                    x = np.arange(len(y))
                    return x, y
    if isinstance(data, dict):
        # Some structures: data['dependence'][feature] = {'x':..., 'y':...}
        for key in ["dependence", "feature_dependence", "partial_dependence"]:
            if key in data and feature in data[key]:
                fd = data[key][feature]
                if isinstance(fd, dict):
                    x = np.array(fd.get("x", fd.get("values", [])))
                    y = np.array(fd.get("y", fd.get("effect", [])))
                    return x, y
        # Another common structure: data['scores'] could be 2D (feature x bins)
        if "scores" in data and isinstance(data["scores"], dict):
            s = data["scores"]
            if feature in s:
                arr = np.array(s[feature])
                x = np.arange(arr.shape[0])
                y = arr
                return x, y
    return np.array([]), np.array([])

def extract_local_for_instance(expl_local: Any, instance_index: int) -> Dict[str, Any]:
    """
    Extract feature contributions and feature values for a local explanation.
    Expecting something like expl_local.data with keys per-instance.
    Return dict with:
      - 'contributions': pd.Series(feature -> contribution)
      - 'feature_values': pd.Series(feature -> value)
      - 'true': true outcome (if available)
      - 'predicted': predicted outcome (if available)
    """
    if expl_local is None:
        return {}
    data = getattr(expl_local, "data", None)

    # Many shapes -> try to detect:
    # - data could be a dict with keys 'local_importance', 'names', 'scores', 'predicted_values', 'true_values'
    if isinstance(data, dict):
        # Try multiple common representations
        names = None
        loc_scores = None
        preds = None
        trues = None
        for key in ["names", "feature_names", "features"]:
            if key in data:
                names = data[key]
                break
        # local importances often under 'local_importance' or 'scores' (2D array)
        for key in ["local_importance", "local_importances", "scores", "contributions", "local"]:
            if key in data:
                loc_scores = data[key]
                break
        for key in ["predicted_values", "predictions", "predicted", "y_pred"]:
            if key in data:
                preds = data[key]
                break
        for key in ["true_values", "y_true", "labels", "y"]:
            if key in data:
                trues = data[key]
                break
        # If loc_scores is 2D array-like (n_instances x n_features)
        if loc_scores is not None and isinstance(loc_scores, (list, np.ndarray)):
            arr = np.array(loc_scores)
            if arr.ndim == 2:
                try:
                    row = arr[instance_index]
                except Exception:
                    # clamp
                    row = arr[0]
                # names fallback
                if names is None:
                    # try data['feature_values'] keys
                    if "feature_values" in data and isinstance(data["feature_values"], dict):
                        names = list(data["feature_values"].keys())
                if names is None:
                    names = [f"f{i}" for i in range(len(row))]
                contributions = pd.Series(row, index=names)
                # feature_values:
                feature_values = None
                if "feature_values" in data:
                    fv = data["feature_values"]
                    # if dict of lists
                    if isinstance(fv, dict):
                        try:
                            fv_series = {k: (v[instance_index] if isinstance(v, (list, np.ndarray)) else v)
                                         for k, v in fv.items()}
                            feature_values = pd.Series(fv_series)
                        except Exception:
                            feature_values = pd.Series({k: (v[0] if isinstance(v,(list, np.ndarray)) else v) for k,v in fv.items()})
                # predictions/trues
                pred_val = None
                true_val = None
                if preds is not None:
                    pred_val = preds[instance_index] if isinstance(preds, (list, np.ndarray)) and len(preds) > instance_index else preds[0] if isinstance(preds, (list, np.ndarray)) else preds
                if trues is not None:
                    true_val = trues[instance_index] if isinstance(trues, (list, np.ndarray)) and len(trues) > instance_index else trues[0] if isinstance(trues, (list, np.ndarray)) else trues
                return {
                    "contributions": contributions.sort_values(ascending=False),
                    "feature_values": feature_values,
                    "predicted": pred_val,
                    "true": true_val
                }
        # Other shape: data could be list of local explanations, each a dict { 'names':[], 'scores':[], 'feature_values':[] }
        if isinstance(data, list) and len(data) > 0:
            # try to pick the instance_index entry
            entry = data[min(instance_index, len(data)-1)]
            if isinstance(entry, dict):
                names = entry.get("names") or entry.get("feature_names") or names
                scores = entry.get("scores") or entry.get("local_importance") or entry.get("contributions")
                fv = entry.get("feature_values") or entry.get("values")
                if scores is not None and names is not None:
                    s = pd.Series(scores, index=names)
                    return {
                        "contributions": s.sort_values(ascending=False),
                        "feature_values": pd.Series(fv) if fv is not None else None,
                        "predicted": entry.get("predicted", None),
                        "true": entry.get("true", None)
                    }
    # If expl_local has attributes like .local_importance_, .predictions_, etc:
    if hasattr(expl_local, "local_importance_"):
        arr = np.array(getattr(expl_local, "local_importance_"))
        if arr.ndim == 2:
            row = arr[instance_index] if instance_index < arr.shape[0] else arr[0]
            names = getattr(expl_local, "feature_names", [f"f{i}" for i in range(len(row))])
            contributions = pd.Series(row, index=names).sort_values(ascending=False)
            fv = getattr(expl_local, "feature_values_", None)
            if fv is not None:
                fv_series = pd.Series(fv[instance_index], index=names)
            else:
                fv_series = None
            pred_val = getattr(expl_local, "predicted_", None)
            true_val = getattr(expl_local, "true_", None)
            return {"contributions": contributions, "feature_values": fv_series, "predicted": pred_val, "true": true_val}

    # fallback: inspect attributes
    if hasattr(expl_local, "dataframe") and isinstance(expl_local.dataframe, pd.DataFrame):
        df = expl_local.dataframe
        if instance_index < len(df):
            row = df.iloc[instance_index]
            return {"contributions": pd.Series(row.to_dict()).sort_values(ascending=False),
                    "feature_values": row.to_dict(),
                    "predicted": None, "true": None}

    return {}

# ---------- Plot helpers ----------
def make_horizontal_importance_bar(df: pd.DataFrame, title: str = "Feature importances") -> go.Figure:
    df = df.copy()
    df = df.dropna(subset=["feature", "importance"])
    # sort descending importance
    df["importance"] = pd.to_numeric(df["importance"], errors="coerce").fillna(0)
    df = df.sort_values("importance", ascending=True)  # ascending True for horizontal bar with top feature on top
    fig = go.Figure(go.Bar(
        x=df["importance"],
        y=df["feature"].astype(str),
        orientation="h",
        hovertemplate="%{y}<br>Score: %{x}<extra></extra>",
        labels = {
            "importance": "Mean Absolute Score (Weighted)"
        }
    ))
    fig.update_layout(
        title=title,
        margin=dict(l=200, r=20, t=50, b=50),
        paper_bgcolor="white",
        plot_bgcolor="white",
        xaxis=dict(automargin=True),
        height=600
    )
    return fig

def make_feature_dependence_fig(x, y, feature_name: str) -> go.Figure:
    if x.size == 0 or y.size == 0:
        fig = go.Figure()
        fig.add_annotation(text="No dependence data available for this feature",
                           xref="paper", yref="paper", x=0.5, y=0.5, showarrow=False)
        fig.update_layout(paper_bgcolor="white", plot_bgcolor="white")
        return fig
    fig = go.Figure()
    # scatter/line
    fig.add_trace(go.Scatter(x=x, y=y, mode="lines+markers", name=feature_name, hovertemplate="x: %{x}<br>y: %{y}<extra></extra>"))
    fig.update_layout(title=f"{feature_name} dependence", xaxis_title="feature values / bins", yaxis_title="effect", paper_bgcolor="white", plot_bgcolor="white", margin=dict(l=60, r=20, t=40, b=50))
    fig.update_xaxes(automargin=True)
    fig.update_yaxes(automargin=True)
    return fig

def make_local_contributions_bar(contrib_series: pd.Series, title: str = "Local contributions") -> go.Figure:
    s = contrib_series.copy()
    s = s.fillna(0)
    # Sort descending by absolute contribution magnitude for display
    s = s.reindex(s.abs().sort_values(ascending=False).index)
    colors = ["green" if v >= 0 else "red" for v in s.values]
    fig = go.Figure(go.Bar(
        x=s.values,
        y=[str(i) for i in s.index],
        orientation="h",
        marker=dict(color=colors),
        hovertemplate="%{y}<br>contribution: %{x}<extra></extra>"
    ))
    fig.update_layout(title=title, margin=dict(l=200, r=20, t=50, b=50), paper_bgcolor="white", plot_bgcolor="white")
    fig.update_xaxes(automargin=True)
    return fig

def make_feature_values_table(feature_values: pd.Series) -> go.Figure:
    # Build a simple table using plotly
    if feature_values is None or len(feature_values) == 0:
        fig = go.Figure()
        fig.add_annotation(text="No feature values available", xref="paper", yref="paper", x=0.5, y=0.5, showarrow=False)
        fig.update_layout(paper_bgcolor="white", plot_bgcolor="white")
        return fig
    names = list(map(str, feature_values.index))
    values = [str(x) for x in feature_values.values]
    fig = go.Figure(data=[go.Table(header=dict(values=["feature", "value"]), cells=dict(values=[names, values]))])
    fig.update_layout(paper_bgcolor="white", plot_bgcolor="white", margin=dict(l=20, r=20, t=20, b=20))
    return fig

# ---------- Export helpers ----------
def fig_to_bytes_png(fig: go.Figure) -> bytes:
    """
    Convert Plotly fig to PNG bytes. Requires kaleido installed.
    """
    return fig.to_image(format="png", scale=2)

def fig_to_html_bytes(fig: go.Figure) -> bytes:
    return fig.to_html(full_html=True, include_plotlyjs="cdn").encode("utf-8")

# ---------- App UI ----------
st.set_page_config(page_title="InterpretML-like Dashboard", layout="wide")
st.markdown("<style>body{background-color: white;}</style>", unsafe_allow_html=True)

st.title("InterpretML-style interactive dashboard")

# Load explain_global and explain_local from models folder
st.sidebar.header("Files & loading")
models_folder = st.sidebar.text_input("Models folder", value="models")
# detection
global_path = find_joblib_file(["explain_global", "global_expl", "global"], folder=models_folder)
local_path = find_joblib_file(["explain_local", "local_expl", "local"], folder=models_folder)

st.sidebar.write("Detected files:")
st.sidebar.write(f"Global: `{os.path.basename(global_path)}`" if global_path else "Global: not found")
st.sidebar.write(f"Local: `{os.path.basename(local_path)}`" if local_path else "Local: not found")

if st.sidebar.button("(Re)load files"):
    st.experimental_rerun()

expl_global = load_joblib_safely(global_path) if global_path else None
expl_local = load_joblib_safely(local_path) if local_path else None

# Extract global importances DataFrame
global_df = extract_global_importances(expl_global)
if global_df.empty:
    st.sidebar.warning("Could not detect global importances automatically. Ensure explain_global object has .data or is a DataFrame/dict with importances.")

# Consolidate feature list for selectors (from global importances and from local if possible)
features = list(global_df["feature"].astype(str)) if not global_df.empty else []
# try to get features from expl_local (if available)
if expl_local is not None:
    data = getattr(expl_local, "data", None)
    if isinstance(data, dict):
        # maybe data['names']
        for k in ["names", "feature_names", "features"]:
            if k in data and isinstance(data[k], (list, tuple)):
                for f in data[k]:
                    if f not in features:
                        features.append(f)
    elif hasattr(expl_local, "feature_names"):
        for f in getattr(expl_local, "feature_names"):
            if f not in features:
                features.append(f)

features = [str(f) for f in features]

tabs = st.tabs(["Global", "Local", "Export"])

# ---------- GLOBAL TAB ----------
with tabs[0]:
    st.header("Global explanations")
    col1, col2 = st.columns([1, 3])
    with col1:
        selected = st.selectbox("Select feature (choose 'All' for global importances)", options=["All"] + features, index=0)
        st.markdown("**Notes:**\n- 'All' shows a horizontal bar with overall importance.\n- Selecting a feature shows the feature dependence and (if available) feature-specific importance.")
    with col2:
        if selected == "All":
            # show global importances as horizontal bar
            if global_df.empty:
                st.warning("Global importances not available.")
            else:
                fig_imp = make_horizontal_importance_bar(global_df, title="Overall feature importances")
                st.plotly_chart(fig_imp, use_container_width=True)
        else:
            # show two graphs typically used in explain_global and align their x-axis.
            # Graph A: dependence (x: feature values, y: effect)
            x1, y1 = extract_global_dependence(expl_global, selected)
            fig_dep = make_feature_dependence_fig(x1, y1, selected)

            # Graph B: for the selected feature, a focused importance or distribution summary
            # We'll show a small bar for the feature's global importance (if present) and a distribution of feature values if we can find them.
            # For distribution try to extract from expl_global.data or expl_local.feature_values
            # Approach: if expl_local contains 'feature_values' dict of lists, extract that column.
            dist_x = np.array([])
            if expl_local is not None:
                ld = getattr(expl_local, "data", None)
                if isinstance(ld, dict) and "feature_values" in ld and isinstance(ld["feature_values"], dict) and selected in ld["feature_values"]:
                    dist_x = np.array(ld["feature_values"][selected])
            # otherwise try expl_global.data for bins in dependence result x1
            if dist_x.size == 0 and x1.size > 0:
                dist_x = x1

            # Build small importance bar for the selected feature
            imp_val = None
            if not global_df.empty:
                row = global_df[global_df["feature"].astype(str) == selected]
                if not row.empty:
                    imp_val = float(row.iloc[0]["importance"])

            # Create two subplots stacked (top: dependence, bottom: distribution or importance)
            # We'll display them separately but align x-axes: use combined x-range computed from both x1 and dist_x
            fig_bottom = go.Figure()
            if dist_x.size > 0:
                # show histogram-like distribution
                fig_bottom.add_trace(go.Histogram(x=dist_x, marker_line_width=0))
                fig_bottom.update_layout(title=f"{selected} distribution (approx.)", xaxis_title="value", yaxis_title="count", paper_bgcolor="white", plot_bgcolor="white", margin=dict(l=60, r=20, t=40, b=50))
            elif imp_val is not None:
                # show a single-value bar
                fig_bottom.add_trace(go.Bar(x=[imp_val], y=[selected], orientation="h", hovertemplate="Importance: %{x}<extra></extra>"))
                fig_bottom.update_layout(title=f"{selected} global importance", margin=dict(l=200, r=20, t=40, b=20), paper_bgcolor="white", plot_bgcolor="white")
            else:
                fig_bottom.add_annotation(text="No distribution or importance available for this feature", xref="paper", yref="paper", x=0.5, y=0.5, showarrow=False)
                fig_bottom.update_layout(paper_bgcolor="white", plot_bgcolor="white")

            # Align x-axis ranges: compute min/max from both figs if numeric
            def get_x_range_from_array(arr):
                if arr is None or len(arr) == 0:
                    return None
                try:
                    arr = np.array(arr, dtype=float)
                    return float(np.nanmin(arr)), float(np.nanmax(arr))
                except Exception:
                    return None

            r1 = get_x_range_from_array(x1)
            r2 = None
            try:
                if dist_x.size > 0:
                    r2 = get_x_range_from_array(dist_x)
            except Exception:
                r2 = None

            combined_min, combined_max = None, None
            for r in (r1, r2):
                if r:
                    if combined_min is None or r[0] < combined_min:
                        combined_min = r[0]
                    if combined_max is None or r[1] > combined_max:
                        combined_max = r[1]

            # If both numeric ranges exist, set them to xaxis for both (ensures alignment)
            if combined_min is not None and combined_max is not None and combined_min != combined_max:
                fig_dep.update_xaxes(range=[combined_min, combined_max])
                fig_bottom.update_xaxes(range=[combined_min, combined_max])

            # Show the two figures stacked one after the other
            st.plotly_chart(fig_dep, use_container_width=True)
            st.plotly_chart(fig_bottom, use_container_width=True)

# ---------- LOCAL TAB ----------
with tabs[1]:
    st.header("Local explanations (per-instance)")
    # Determine number of instances we can show (from expl_local.data)
    instance_count = 0
    sample_indices = []
    # try to infer number of instances
    if expl_local is not None:
        data = getattr(expl_local, "data", None)
        # many possible shapes
        if isinstance(data, dict):
            # if scores exist as 2D array
            for key in ["local_importance", "local_importances", "scores", "contributions"]:
                if key in data and isinstance(data[key], (list, np.ndarray)):
                    arr = np.array(data[key])
                    if arr.ndim == 2:
                        instance_count = arr.shape[0]
                        break
            # if feature_values present as dict of lists:
            if instance_count == 0 and "feature_values" in data and isinstance(data["feature_values"], dict):
                # pick length of any list
                for v in data["feature_values"].values():
                    if isinstance(v, (list, np.ndarray)):
                        instance_count = len(v)
                        break
            # if list of local entries
            if instance_count == 0 and isinstance(data.get("local", None), list):
                instance_count = len(data["local"])
        elif isinstance(data, list):
            instance_count = len(data)
        elif hasattr(expl_local, "local_importance_"):
            arr = np.array(getattr(expl_local, "local_importance_"))
            if arr.ndim == 2:
                instance_count = arr.shape[0]
    if instance_count == 0:
        st.warning("Could not detect number of local explanations automatically. The local tab may not be populated. Please ensure your explain_local object contains 'local_importance' or similar fields.")
    # build index chooser
    index_input = st.number_input("Instance index (0-based)", min_value=0, max_value=max(0, instance_count-1), value=0, step=1)
    # allow selection by dropdown of predicted/true etc if available
    with st.expander("Preview of available local explanation metadata"):
        st.write("Detected instance_count:", instance_count)
        st.write("expl_local keys/attributes:")
        if expl_local is None:
            st.write("No local explain object loaded.")
        else:
            st.write([k for k in dir(expl_local) if not k.startswith("_")][:80])
            st.write("data preview (truncated):")
            st.write(str(getattr(expl_local, "data", "None"))[:1000])

    # Extract local explanation for selected instance
    local_info = extract_local_for_instance(expl_local, int(index_input))
    if not local_info:
        st.info("Local explanation not available for the selected instance.")
    else:
        contribs: pd.Series = local_info.get("contributions")
        fvalues: pd.Series = local_info.get("feature_values")
        pred = local_info.get("predicted")
        true = local_info.get("true")

        st.subheader("Prediction and label")
        st.write(f"**Predicted:** {pred}")
        st.write(f"**True / Actual:** {true}")

        # Show contributions bar
        if contribs is not None and len(contribs) > 0:
            fig_local = make_local_contributions_bar(contribs, title=f"Local feature contributions (instance {int(index_input)})")
            st.plotly_chart(fig_local, use_container_width=True)
        else:
            st.warning("Local contributions not found or empty.")

        # Show feature values table
        if fvalues is not None:
            fig_vals = make_feature_values_table(fvalues)
            st.plotly_chart(fig_vals, use_container_width=True)
        else:
            st.info("Feature values not available for this instance.")

# ---------- EXPORT TAB ----------
with tabs[2]:
    st.header("Export plots")
    st.markdown("Select which plot to export and the format. PNG export requires `kaleido` package (recommended). Otherwise HTML export is available.")
    # For convenience, prepare some figures we might export:
    export_figs = {}
    if not global_df.empty:
        export_figs["overall_importances"] = make_horizontal_importance_bar(global_df, title="Overall feature importances")
    # If a feature currently selected in global tab exists, create its two plots for export
    # We'll attempt to reuse `selected` from above when in the session; but if not available, pick first feature
    try:
        current_feature = selected if 'selected' in locals() else (features[0] if features else None)
    except Exception:
        current_feature = features[0] if features else None
    if current_feature:
        x1, y1 = extract_global_dependence(expl_global, current_feature)
        export_figs[f"{current_feature}_dependence"] = make_feature_dependence_fig(x1, y1, current_feature)
    # Local instance fig
    if 'contribs' in locals() and contribs is not None:
        export_figs[f"instance_{int(index_input)}_contributions"] = make_local_contributions_bar(contribs, title=f"Instance {int(index_input)} contributions")

    sel_export = st.selectbox("Choose figure to export", options=list(export_figs.keys()) if export_figs else ["none"])
    fmt = st.radio("Format", options=["png", "html"])
    if sel_export == "none":
        st.info("No figures available to export.")
    else:
        fig_to_export = export_figs[sel_export]
        if fmt == "png":
            try:
                img_bytes = fig_to_bytes_png(fig_to_export)
                b64 = base64.b64encode(img_bytes).decode()
                href = f'<a href="data:file/png;base64,{b64}" download="{sel_export}.png">Download PNG</a>'
                st.markdown(href, unsafe_allow_html=True)
            except Exception as e:
                st.error(f"PNG export failed (kaleido may be missing). Error: {e}. Try HTML export instead.")
        else:
            html_bytes = fig_to_html_bytes(fig_to_export)
            b64 = base64.b64encode(html_bytes).decode()
            href = f'<a href="data:text/html;base64,{b64}" download="{sel_export}.html">Download HTML</a>'
            st.markdown(href, unsafe_allow_html=True)

st.markdown("---")
st.caption("This dashboard attempts to reproduce the key interactive plots created by InterpretML's explain_global and explain_local output. It is resilient to a few common variations in the shape of the saved joblib objects. If your saved objects have a different structure, adapt the helper extraction functions near the top of the file.")
