"""
EBM Streamlit app (visualize-only) with dropdown for Local instances
- Local tab uses a dropdown selectbox for instance selection (not a slider).
- Plot title font reduced relative to earlier version.
- Uses ebm.explain_global().visualize(i) and ebm.explain_local(...).visualize(i).
- No use of streamlit.uploaded_file_manager.
"""

import io
import zipfile
from typing import Any, Optional, Tuple, List

import joblib
import numpy as np
import pandas as pd
import streamlit as st
import plotly.graph_objs as go

title = "Climate Shocks & Sexual Violence Dashboard"
st.set_page_config(page_title=title, layout="wide")

# ---------------- FONT SIZES (reduced title size) ----------------
PLOT_TITLE_FONT = dict(size=20, family="Arial, sans-serif")   # reduced title size
AXIS_TITLE_FONT = dict(size=18, family="Arial, sans-serif")
TICK_FONT = dict(size=14, family="Arial, sans-serif")

# ---------------- SAFE LOADERS ----------------
def load_model_from_upload(uploaded: Optional[Any]) -> Optional[Any]:
    if uploaded is None:
        return None
    try:
        data = uploaded.read()
        model = joblib.load(io.BytesIO(data))
        return model
    except Exception as e:
        st.sidebar.error(f"Failed to load model: {e}")
        return None

def load_csv_from_upload(uploaded: Optional[Any]) -> Optional[pd.DataFrame]:
    if uploaded is None:
        return None
    try:
        return pd.read_csv(uploaded)
    except Exception as e:
        st.sidebar.error(f"Failed to read CSV: {e}")
        return None

# ---------------- VISUALIZE HELPERS ----------------

def apply_white_plot_style(fig: go.Figure) -> go.Figure:
    fig.update_layout(
        paper_bgcolor="white",
        plot_bgcolor="white",
        title_font=PLOT_TITLE_FONT,
        font_color="black",
        margin=dict(l=240, r=40, t=60, b=40)
    )
    fig.update_xaxes(
        title_font=AXIS_TITLE_FONT,
        tickfont=TICK_FONT,
        color="black",
        showline=True,
        linewidth=1.5,
        linecolor="black",
        mirror=True,
        gridcolor="lightgray",
    )
    fig.update_yaxes(
        title_font=AXIS_TITLE_FONT,
        tickfont=TICK_FONT,
        color="black",
        showline=True,
        linewidth=1.5,
        linecolor="black",
        mirror=True,
        gridcolor="lightgray",
    )
    return fig


def safe_visualize_global(ebm: Any, idx: int) -> Optional[go.Figure]:
    try:
        global_expl = st.session_state.get("_ebm_global")
        if global_expl is None:
            global_expl = ebm.explain_global()
            st.session_state["_ebm_global"] = global_expl

        fig = global_expl.visualize(idx)
        fig = apply_white_plot_style(fig)
        return fig
    except Exception as e:
        st.error(f"Global visualize({idx}) failed: {e}")
        return None

def safe_visualize_local(ebm: Any, X: pd.DataFrame, y: Optional[pd.Series], idx: int) -> Optional[go.Figure]:
    try:
        key = "_ebm_local_with_y" if y is not None else "_ebm_local"
        local_expl = st.session_state.get(key)
        if local_expl is None:
            local_expl = ebm.explain_local(X, y) if y is not None else ebm.explain_local(X)
            st.session_state[key] = local_expl

        fig = local_expl.visualize(idx)
        fig = apply_white_plot_style(fig)
        return fig
    except Exception as e:
        st.error(f"Local visualize({idx}) failed: {e}")
        return None

# ---------------- TERM IMPORTANCES ----------------
def compute_importances_from_model(ebm: Any) -> Optional[List[Tuple[str, float]]]:
    try:
        arr = np.asarray(ebm.term_importances(importance_type="avg_weight"))
        if hasattr(ebm, "term_names_"):
            names = list(getattr(ebm, "term_names_"))
        else:
            names = [f"Feature {i}" for i in range(len(arr))]
        return list(zip(names, arr.astype(float).tolist()))
    except Exception:
        return None

def build_importance_fig(imps: List[Tuple[str, float]], k: int = 15) -> go.Figure:
    top = sorted(imps, key=lambda x: x[1], reverse=True)[:k]
    names = [n for n,_ in top][::-1]
    vals  = [v for _,v in top][::-1]
    fig = go.Figure(
        go.Bar(
            x=vals, 
            y=names, 
            orientation="h",
            hovertemplate="%{y}<br>Score: %{x:.4f}<extra></extra>",
        )
    )
    fig.update_layout(
        title="Top Feature Importances", 
        title_font=PLOT_TITLE_FONT,
        xaxis_title = "Mean Absolute Score (Weighted)",
        paper_bgcolor="white",
        plot_bgcolor="white",
        font_color="black",
        height=560, 
        margin=dict(l=240, r=40, t=60, b=40),
    )
    fig.update_xaxes(
        title="importance", 
        title_font=AXIS_TITLE_FONT, 
        tickfont=TICK_FONT,
        showline=True,
        linewidth=1.5,
        linecolor="black",
        mirror=True,
        gridcolor="lightgray",
    )
    fig.update_yaxes(
        tickfont=TICK_FONT, 
        automargin=True,
        showline=True,
        linewidth=1.5,
        linecolor="black",
        mirror=True,
        gridcolor="lightgray",
    )
    return fig

# ---------------- SIDEBAR ----------------
st.sidebar.header("Upload model & data")
uploaded_model = st.sidebar.file_uploader("EBM model (.pkl or .joblib)", type=["pkl", "joblib"])
uploaded_csv = st.sidebar.file_uploader("CSV for local explanations (optional)", type=["csv"])
y_colname = st.sidebar.text_input("Label column name in CSV (optional)", "")

ebm = load_model_from_upload(uploaded_model)
if ebm is None:
    st.warning("Please upload a trained EBM model (.pkl) in the sidebar to use the app.")
    st.stop()

# Load CSV dataset for local explanations if provided
X_df: Optional[pd.DataFrame] = None
y_series: Optional[pd.Series] = None
if uploaded_csv is not None:
    df = load_csv_from_upload(uploaded_csv)
    if df is not None:
        if y_colname and y_colname in df.columns:
            y_series = df[y_colname].copy()
            X_df = df.drop(columns=[y_colname])
        else:
            X_df = df

# ---------------- MAIN UI ----------------
st.title(title)
st.markdown("<h4 style='color:#555;'>Variable Explanation</h4>", unsafe_allow_html=True)
tabs = st.tabs(["Global", "Local", "Export"])

# ---------------- GLOBAL TAB ----------------
with tabs[0]:
    st.subheader("Global explanations")

    # Try obtaining number of terms
    n_terms = 0
    try:
        gexpl = ebm.explain_global()
        # many versions have 'selector' or can be iterated; try len of selector
        n_terms = len(getattr(gexpl, "selector", [])) or 0
    except Exception:
        # fallback to term_importances
        imps_tmp = compute_importances_from_model(ebm)
        n_terms = len(imps_tmp) if imps_tmp is not None else 0

    # term names
    if hasattr(ebm, "term_names_"):
        term_names = list(getattr(ebm, "term_names_"))
    else:
        term_names = [f"Feature {i}" for i in range(n_terms)]

    selection = st.selectbox("Select feature (or All)", ["All"] + term_names, index=0)

    if selection == "All":
        imps = compute_importances_from_model(ebm)
        if imps is None:
            st.error("Could not compute importances from model.")
        else:
            fig = build_importance_fig(imps, k=15)
            col_plot, col_buttons = st.columns([8, 1], gap="small")
            with col_plot:
                st.plotly_chart(fig, width="stretch")
            html = fig.to_html(full_html=True, include_plotlyjs="cdn")
            try:
                png = fig.to_image(format="png", scale=2)
            except Exception:
                png = b""
            with col_buttons:
                st.download_button("Download HTML", data=html, file_name="importances_top15.html", mime="text/html")
                if png:
                    st.download_button("Download PNG", data=png, file_name="importances_top15.png", mime="image/png")
                else:
                    st.warning("PNG export requires 'kaleido'")
    else:
        idx = term_names.index(selection)
        fig = safe_visualize_global(ebm, idx)
        if fig is not None:
            col_plot, col_buttons = st.columns([8, 1], gap="small")
            with col_plot:
                st.plotly_chart(fig, width="stretch")
            html = fig.to_html(full_html=True, include_plotlyjs="cdn")
            try:
                png = fig.to_image(format="png", scale=2)
            except Exception:
                png = b""
            with col_buttons:
                st.download_button("Download HTML", data=html, file_name=f"global_{selection}.html", mime="text/html")
                if png:
                    st.download_button("Download PNG", data=png, file_name=f"global_{selection}.png", mime="image/png")
                else:
                    st.warning("PNG export requires 'kaleido'")

# ---------------- LOCAL TAB (dropdown instead of slider) ----------------
with tabs[1]:
    st.subheader("Local explanations")
    if X_df is None:
        st.info("Upload a CSV in the sidebar to enable Local explanations.")
    else:
        n_instances = X_df.shape[0]
        # Build selectbox options showing instance index and id if available
        # We'll try to get an 'id' from the local explanation if available; otherwise use index.
        # Use a short cache attempt to get local explanation once to extract instance ids if present.
        try:
            local_expl = st.session_state.get("_ebm_local_preview")
            if local_expl is None:
                # call once without storing heavy object permanently (we still cache safe_visualize_local results separately)
                local_expl = ebm.explain_local(X_df, y_series) if y_series is not None else ebm.explain_local(X_df)
                st.session_state["_ebm_local_preview"] = local_expl
        except Exception:
            local_expl = None

        # Prepare dropdown labels: prefer an id field if local_expl provides metadata
        options = []
        for i in range(n_instances):
            label = f"Instance {i}"
            # If local_expl exists, try to read its internal data for a human id or label
            if local_expl is not None:
                try:
                    # many versions: local_expl.data(i) returns the instance dict (or local_expl._data[i])
                    inst_data = None
                    try:
                        inst_data = local_expl.data(i)
                    except Exception:
                        # maybe stored in _data
                        inst_data = local_expl._data[i] if hasattr(local_expl, "_data") and i < len(local_expl._data) else None
                    if isinstance(inst_data, dict):
                        inst_id = inst_data.get("id", None) or inst_data.get("index", None)
                        if inst_id is not None:
                            label = f"Instance {i} (id={inst_id})"
                except Exception:
                    pass
            options.append((i, label))

        # Show selectbox using labels but return the index
        selected_pair = st.selectbox("Choose instance", options, format_func=lambda x: x[1], index=0)
        inst_idx = selected_pair[0] if isinstance(selected_pair, tuple) else int(selected_pair)

        # Display True and Predicted values
        try:
            pred_val = float(ebm.predict(X_df.iloc[[inst_idx]])[0])
        except Exception:
            pred_val = None
        true_val = None
        if y_series is not None and inst_idx < len(y_series):
            true_val = y_series.iloc[inst_idx]

        col_info, col_plotcol = st.columns([2, 9])
        # with col_info:
        #     st.markdown("**Predicted**")
        #     st.write(pred_val if pred_val is not None else "N/A")
        #     st.markdown("**True**")
        #     st.write(true_val if true_val is not None else "N/A")

        fig_local = safe_visualize_local(ebm, X_df, y_series, inst_idx)
        if fig_local is not None:
            col_plot, col_buttons = st.columns([8, 1], gap="small")
            with col_plot:
                st.plotly_chart(fig_local, width="stretch")
            html = fig_local.to_html(full_html=True, include_plotlyjs="cdn")
            try:
                png = fig_local.to_image(format="png", scale=2)
            except Exception:
                png = b""
            with col_buttons:
                st.download_button("Download HTML", data=html, file_name=f"local_{inst_idx}.html", mime="text/html")
                if png:
                    st.download_button("Download PNG", data=png, file_name=f"local_{inst_idx}.png", mime="image/png")
                else:
                    st.warning("PNG export requires 'kaleido'")

# ---------------- EXPORT TAB ----------------
with tabs[2]:
    st.subheader("Export all HTMLs")
    if st.button("Build ZIP of all global & local HTMLs"):
        buffer = io.BytesIO()
        with zipfile.ZipFile(buffer, "w", compression=zipfile.ZIP_DEFLATED) as zf:
            # global
            try:
                gexpl = ebm.explain_global()
                n = len(getattr(gexpl, "selector", [])) or 0
                for ii in range(n):
                    fig = safe_visualize_global(ebm, ii)
                    if fig is not None:
                        zf.writestr(f"global_{ii}.html", fig.to_html(full_html=True, include_plotlyjs="cdn"))
            except Exception:
                pass
            # local
            if X_df is not None:
                for jj in range(X_df.shape[0]):
                    fig = safe_visualize_local(ebm, X_df, y_series, jj)
                    if fig is not None:
                        zf.writestr(f"local_{jj}.html", fig.to_html(full_html=True, include_plotlyjs="cdn"))
            # importances
            imps = compute_importances_from_model(ebm)
            if imps:
                imp_fig = build_importance_fig(imps)
                zf.writestr("importances_top15.html", imp_fig.to_html(full_html=True, include_plotlyjs="cdn"))
        buffer.seek(0)
        st.download_button("Download ZIP", data=buffer, file_name="ebm_visualize_htmls.zip", mime="application/zip")
