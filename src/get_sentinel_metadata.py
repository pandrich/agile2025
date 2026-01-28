"""
export_s1_metadata_and_results.py

Plain-Python Earth Engine script (no Jupyter) to:
- Export Sentinel-1 metadata per-month in small batches to Google Drive (or fallback to local CSV).
- Then read downloaded metadata and submit per-image per-district exports to Drive.
- Uses resume-safe JSONL logs to skip already-submitted batches/images on reruns.

Edit PARAMETERS below before running.
"""

from pathlib import Path
import ee
import geemap
import geopandas as gpd
import pandas as pd
import numpy as np
import time
import json
import os
from pathlib import Path
import sys
import logging

# ------------------ PARAMETERS: edit these ------------------
S1_COLL = "COPERNICUS/S1_GRD"
START_DATE = "2020-01-01"
END_DATE = "2025-08-31"

# Local paths: dissolved shapefile (single polygon) and admin2 shapefile
RAW_DATA_DIR = Path("./data/raw")
PROC_DATA_DIR = Path("./data/processed/climate/flood_files")
DISTRICT_SHP = RAW_DATA_DIR / "geographies/uganda_herbert/uganda_districts.shp"

# Drive folders for exports
DRIVE_META_DIR = Path('/Users/paoich/Library/CloudStorage/GoogleDrive-andrichpaolo@gmail.com/My Drive/data/flood_files/meta')
DRIVE_RESULTS_DIR = Path('/Users/paoich/Library/CloudStorage/GoogleDrive-andrichpaolo@gmail.com/My Drive/data/flood_files')

# Local folders for fallback and metadata
LOCAL_METADATA_DIR = PROC_DATA_DIR / "metadata"

# Resume logs
META_LOG = PROC_DATA_DIR / "meta_batch_log.jsonl"      # records submitted metadata batches
IMAGE_LOG = PROC_DATA_DIR / "image_export_log.jsonl"   # records submitted per-image exports

# Batch control
BATCH_SIZE = 50              # number of images per metadata batch export; reduce to 5-20 if issues
SLEEP_BETWEEN_BATCHES = 1.0  # seconds
SLEEP_BETWEEN_TASKS = 0.5    # seconds

# Flood detection parameters for per-image exports
BASELINE_START = "2015-01-01"
BASELINE_END = "2019-12-31"
ANOMALY_THRESHOLD = -3.0
PERM_WATER_OCCURRENCE_PCT = 50
SCALE = 10

# ------------------------------------------------------------

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s: %(message)s")


def init_ee():
    """Authenticate + init EE (interactive the first time)."""
    try:
        ee.Initialize(project = "divine-catalyst-330916")
        logging.info("Earth Engine initialized.")
    except Exception:
        logging.info("Authenticating Earth Engine; follow the printed URL in your browser.")
        ee.Authenticate()
        ee.Initialize(project = "divine-catalyst-330916")
        logging.info("Earth Engine initialized after auth.")


def load_geometries():
    """Load dissolved geometry and districts, convert to EE objects (no printing)."""
    # Dissolved geometry
    if not Path(DISTRICT_SHP).exists():
        logging.error("DISSOLVED_SHAPE not found: %s", DISTRICT_SHP)
        sys.exit(1)
    dissolve_gdf = gpd.read_file(DISTRICT_SHP).to_crs(epsg=4326)
    if len(dissolve_gdf) > 1:
        dissolve_gdf = dissolve_gdf.dissolve().reset_index(drop=True)

    # Convert dissolved to ee FeatureCollection -> geometry
    dissolved_fc = geemap.geopandas_to_ee(dissolve_gdf)
    uganda_geom = dissolved_fc.geometry()

    # Districts
    if not Path(DISTRICT_SHP).exists():
        logging.error("DISTRICT_SHP not found: %s", DISTRICT_SHP)
        sys.exit(1)
    districts_gdf = (
        gpd.read_file(
            RAW_DATA_DIR / 
            "geographies" /
            "uganda_herbert" /
            "uganda_districts.shp"
        )
        .rename(columns = str.lower)
        .assign(
            district = lambda x: np.where(
                x["district"] == "SSEMBABULE",
                "Sembabule",
                x["district"].str.title()
            ),
            area_m2 = lambda x: x["geometry"].area
        )
        .to_crs(4326)
        [[
            "district",
            "area_m2",
            "geometry"
        ]]      
    )

    # pick name
    districts_fc = geemap.geopandas_to_ee(districts_gdf)

    # Ensure properties exist server-side
    def ensure_props(f):
        return ee.Feature(f).set({"district": f.get("district"), "area_m2": f.get("area_m2")})

    districts_fc = districts_fc.map(ensure_props)

    logging.info("Loaded dissolved geometry and %d districts.", len(districts_gdf))
    return uganda_geom, districts_fc


def append_jsonl(record: dict, path: Path):
    """Append a JSON record to a newline-delimited log file."""
    with open(path, "a") as fh:
        fh.write(json.dumps(record) + "\n")
        fh.flush()
        os.fsync(fh.fileno())

def safe_number(val):
    return ee.Number(ee.Algorithms.If(val, val, 0))


def load_processed_prefixes(logpath: Path):
    seen = set()
    if logpath.exists():
        with open(logpath, "r") as fh:
            for line in fh:
                try:
                    rec = json.loads(line.strip())
                    if "file_prefix" in rec:
                        seen.add(rec["file_prefix"])
                except Exception:
                    continue
    return seen


# ---------- Metadata batch export (attempt Drive, fallback to local CSV) ----------
def export_monthly_metadata(uganda_geom):
    """
    Page S1 by month and small batches. For each batch attempt Drive export; if TypeError occurs
    (client serialization problem) fallback to small getInfo() and write a local CSV.
    """
    processed_prefixes = load_processed_prefixes(META_LOG)

    months = pd.date_range(START_DATE, END_DATE, freq="MS")
    for m in months:
        m_start = m.strftime("%Y-%m-%d")
        m_end = (m + pd.offsets.MonthEnd(1)).strftime("%Y-%m-%d")
        logging.info("Preparing month %s -> %s", m_start, m_end)

        coll = (
            ee.ImageCollection(S1_COLL)
            .filterDate(m_start, m_end)
            .filter(ee.Filter.eq("instrumentMode", "IW"))
            .filter(ee.Filter.listContains("transmitterReceiverPolarisation", "VV"))
            .filterBounds(uganda_geom)
        )

        offset = 0
        batch_idx = 0
        while True:
            # create ee.List of next batch server-side
            batch_list = coll.toList(BATCH_SIZE, offset)  # ee.List of image objects

            # server-side map to Features (do NOT use ImageCollection.map to produce features)
            def _make_feat(img):
                img = ee.Image(img)
                return ee.Feature(
                    None,
                    {
                        "image_id": ee.String(img.get("system:index")),
                        "time_start": ee.Number(img.get("system:time_start")),
                    },
                )

            batch_features_list = batch_list.map(_make_feat)  # ee.List of Features
            batch_fc = ee.FeatureCollection(batch_features_list)  # server-side FC

            # file_prefix = f"s1_meta_{m_start}_batch{batch_idx:03d}"

            # task = ee.batch.Export.table.toDrive({
            #     'collection': batch_fc,
            #     'description': f"export_{file_prefix}",
            #     'folder': DRIVE_META_DIR,
            #     'fileNamePrefix': file_prefix,
            #     'fileFormat': 'CSV'
            # })

            # try:
            #     task.start()
            #     logging.info("Submitted export task for %s", file_prefix)
            # except TypeError as e:
            #     logging.warning("Serialization error for %s: %s", file_prefix, e)
            #     # Fallback: save small batch directly to local CSV
            #     try:
            #         # WARNING: only do this fallback if BATCH_SIZE is *very* small
            #         info = batch_fc.getInfo()
            #         feats = info.get("features", [])
            #         rows = []
            #         for f in feats:
            #             rows.append(f["properties"])
            #         df = pd.DataFrame(rows)
            #         df.to_csv(LOCAL_METADATA_DIR / f"{file_prefix}.csv", index=False)
            #         logging.info("Fallback local CSV written for %s", file_prefix)
            #     except Exception as ee2:
            #         logging.error("Fallback failed for %s: %s", file_prefix, ee2)

            # attempt to check small size (safe for small batch)
            batch_size = batch_fc.size()
            try:
                batch_count = int(batch_fc.size().getInfo())
            except Exception:
                batch_count = None

            if batch_count == 0:
                logging.info("No more images in month %s at offset %d", m_start, offset)
                break

            file_prefix = f"s1_meta_{m_start}_batch{batch_idx:03d}"
            if file_prefix in processed_prefixes:
                logging.info("Skipping already-processed prefix %s", file_prefix)
            else:
                try:
                    info = batch_fc.getInfo()  # small batch -> OK
                    feats = info.get("features", [])
                    rows = []
                    for f in feats:
                        props = f.get("properties", {})
                        rows.append(
                            {
                                "image_id": props.get("image_id"),
                                "time_start": props.get("time_start"),
                                "orbit_pass": props.get("orbit_pass"),
                            }
                        )
                    df = pd.DataFrame(rows)
                    outpath = PROC_DATA_DIR / f"{file_prefix}.csv"
                    df.to_csv(outpath, index=False)
                    logging.info("Wrote fallback local CSV %s (rows=%d)", outpath, len(df))
                    append_jsonl({"file_prefix": file_prefix, "month": m_start, "batch_idx": batch_idx, "method": "local_csv"}, META_LOG)
                except Exception as e:
                    logging.exception("Fallback getInfo() failed for %s: %s", file_prefix, str(e))
                    append_jsonl({"file_prefix": file_prefix, "month": m_start, "batch_idx": batch_idx, "method": "fallback_failed", "error": str(e)}, META_LOG)

            # advance
            batch_idx += 1
            offset += BATCH_SIZE
            time.sleep(SLEEP_BETWEEN_BATCHES)

        logging.info("Month %s scheduled.", m_start)

    logging.info("All metadata scheduling done. Check Drive or local metadata directory.")


# ---------- Read metadata CSVs locally and submit per-image per-district exports ----------
def read_local_metadata_csvs(local_dir: Path):
    csvs = sorted(local_dir.glob("s1_meta_*.csv"))
    df_list = []
    for p in csvs:
        try:
            df = pd.read_csv(p, dtype={"image_id": str, "time_start": object})
            df_list.append(df)
        except Exception:
            logging.exception("Failed reading %s", p)
    if df_list:
        return pd.concat(df_list, ignore_index=True)
    else:
        return pd.DataFrame(columns=["image_id", "time_start", "orbit_pass"])


def submit_per_image_exports(districts_fc):
    """
    Read metadata CSVs from LOCAL_METADATA_DIR and submit per-image per-district exports
    as ee.batch.Export.table.toDrive() tasks. Uses IMAGE_LOG for resume.
    """
    # load local images list from LOCAL_METADATA_DIR (downloaded from Drive or fallback local CSVs)
    meta_df = read_local_metadata_csvs(LOCAL_METADATA_DIR)
    if meta_df.empty:
        logging.warning("No metadata CSVs found in %s. Ensure you downloaded Drive exports or that fallback CSVs exist.", LOCAL_METADATA_DIR)
        return

    # Build processed map
    processed = {}
    if IMAGE_LOG.exists():
        with open(IMAGE_LOG, "r") as fh:
            for line in fh:
                try:
                    r = json.loads(line.strip())
                    processed[r["image_id"]] = r
                except Exception:
                    continue

    # Process rows
    for _, row in meta_df.iterrows():
        image_id = str(row["image_id"])
        if image_id in processed and processed[image_id].get("status") == "submitted":
            logging.info("Skipping already-submitted image %s", image_id)
            continue

        try:
            # Build per-district FeatureCollection server-side for this image and export to Drive
            img = ee.Image(f"{S1_COLL}/{image_id}").select("VV")
            acq_date = ee.Date(img.get("system:time_start")).format("YYYY-MM-dd'T'HH:mm:ss'Z'")

            perm_water = ee.Image("JRC/GSW1_4/GlobalSurfaceWater").select("occurrence").gte(PERM_WATER_OCCURRENCE_PCT)

            def per_feature(feature):
                geom = feature.geometry()

                # Baseline for this district/month (as before)
                month_num = ee.Number(ee.Date(img.get('system:time_start')).get('month'))
                baseline_col = (
                    ee.ImageCollection(S1_COLL)
                    .filterDate(BASELINE_START, BASELINE_END)
                    .filter(ee.Filter.eq('instrumentMode', 'IW'))
                    .filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VV'))
                    .filterBounds(geom)
                    .filter(ee.Filter.calendarRange(month_num, month_num, 'month'))
                    .select('VV')
                )
                baseline_exists = baseline_col.size().gt(0)
                baseline = baseline_col.median().clip(geom)

                img_clip = img.clip(geom)
                anomaly = img_clip.subtract(baseline)
                flood_mask = anomaly.lte(ANOMALY_THRESHOLD).And(perm_water.Not())

                # reduceRegion returns possibly-null values: handle using ee.Algorithms.If(...)
                flooded_area_raw = ee.Image.pixelArea().updateMask(flood_mask).reduceRegion(
                    ee.Reducer.sum(), geom, SCALE, maxPixels=1e13).get('area')
                flooded_num = safe_number(flooded_area_raw)

                valid_area_raw = ee.Image.pixelArea().updateMask(img_clip.mask()).reduceRegion(
                    ee.Reducer.sum(), geom, SCALE, maxPixels=1e13).get('area')
                valid_num = safe_number(valid_area_raw)

                district_area = ee.Number(feature.get('area_m2'))

                # compute coverage_pct and flood_fraction robustly
                coverage_pct = ee.Algorithms.If(
                    district_area.gt(0),
                    valid_num.divide(district_area).multiply(100),
                    ee.Number(0)
                )
                flood_fraction = ee.Algorithms.If(
                    district_area.gt(0),
                    flooded_num.divide(district_area),
                    ee.Number(0)
                )

                # Set properties (ensure values are EE objects)
                return feature.set({
                    'image_id': image_id,                    # if image_id is a Python string, it's OK; otherwise ee.String(...)
                    'acq_time': acq_date,
                    'flooded_m2': flooded_num,
                    'district_area_m2': district_area,
                    'flood_fraction': flood_fraction,
                    'coverage_pct': coverage_pct,
                    'baseline_exists': baseline_exists
                })

            results_fc = districts_fc.map(per_feature)
            results_props_fc = ee.FeatureCollection(
                results_fc.map(lambda f: ee.Feature(None, ee.Feature(f).toDictionary()))
            )
            prefix = f"{image_id}_district_floods"
            try:
                task = ee.batch.Export.table.toDrive(
                    {
                        "collection": results_fc,
                        "description": f"perimage_{image_id}",
                        "folder": DRIVE_RESULTS_DIR,
                        "fileNamePrefix": prefix,
                        "fileFormat": "CSV",
                    }
                )
                task.start()
                logging.info("Started per-image export for %s", image_id)
                rec = {"image_id": image_id, "task_description": f"perimage_{image_id}", "status": "submitted"}
                append_jsonl(rec, IMAGE_LOG)
                processed[image_id] = rec
            except TypeError as te:
                # If serialization fails, fallback to small client-side getInfo for results_fc and write local CSV.
                logging.warning("TypeError on per-image task.start() for %s: %s", image_id, str(te))
                try:
                    info = results_props_fc.getInfo()
                    feats = info.get("features", [])
                    rows = []
                    for f in feats:
                        p = f.get("properties", {})
                        rows.append(
                            {
                                "image_id": p.get("image_id"),
                                "acq_time": p.get("acq_time"),
                                "district_name": p.get("district_name"),
                                "flooded_m2": p.get("flooded_m2"),
                                "district_area_m2": p.get("district_area_m2"),
                                "flood_fraction": p.get("flood_fraction"),
                                "coverage_pct": p.get("coverage_pct"),
                                "baseline_exists": p.get("baseline_exists"),
                            }
                        )
                    outpath = PROC_DATA_DIR / f"{prefix}.csv"
                    pd.DataFrame(rows).to_csv(outpath, index=False)
                    logging.info("Wrote local per-image CSV for %s (%s rows)", image_id, len(rows))
                    rec = {"image_id": image_id, "status": "local_csv_fallback"}
                    append_jsonl(rec, IMAGE_LOG)
                    processed[image_id] = rec
                except Exception as e:
                    logging.exception("getInfo for per-image %s failed: %s", image_id, str(e))
                    append_jsonl({"image_id": image_id, "status": "fallback_failed", "error": str(e)}, IMAGE_LOG)

        except Exception as e:
            logging.exception("Unexpected error preparing per-image export for %s: %s", image_id, str(e))
            append_jsonl({"image_id": image_id, "status": "prepare_failed", "error": str(e)}, IMAGE_LOG)

        time.sleep(SLEEP_BETWEEN_TASKS)


def main():
    init_ee()
    uganda_geom, districts_fc = load_geometries()
    # Stage 1: export metadata (Drive preferred; fallback writes local CSVs)
    # export_monthly_metadata(uganda_geom)

    # After Drive exports complete, download CSVs to LOCAL_METADATA_DIR (manual or automated).
    # Stage 2: read downloaded metadata CSVs and submit per-image per-district exports
    submit_per_image_exports(districts_fc)

    logging.info("Script finished.")


if __name__ == "__main__":
    main()
