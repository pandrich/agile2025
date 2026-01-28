#!/usr/bin/env python3
"""
download_s1_cdse_uganda_fixed_datetime.py

Fixed script for querying CDSE STAC for Sentinel-1 GRD (Uganda),
datetime 2020-01-01T00:00:00Z -> 2025-08-31T23:59:59Z,
filtering to sar:instrument_mode == "IW" and sar:polarizations contains "VV".

Requirements:
    pip install requests tqdm

If some assets require auth, set:
    export CDSE_TOKEN="your_token_here"
"""

from pathlib import Path
import os
import json
import time
import argparse
from urllib.parse import urlparse, urljoin
import requests
from tqdm import tqdm

# ---- Configuration defaults ----
RAW_DATA_DIR = Path(".data/raw/sentinel1")
RAW_DATA_DIR.mkdir(parents=True, exist_ok=True)
STAC_SEARCH_URL = "https://stac.dataspace.copernicus.eu/v1/search"
COLLECTION = "sentinel-1-grd"
# Uganda bbox (min_lon, min_lat, max_lon, max_lat)
UGANDA_BBOX = [29.0, -1.5, 35.2, 4.5]
DATETIME = "2020-01-01/2025-08-31"
CDSE_TOKEN = "eyJhbGciOiJSUzI1NiIsInR5cCIgOiAiSldUIiwia2lkIiA6ICJYVUh3VWZKaHVDVWo0X3k4ZF8xM0hxWXBYMFdwdDd2anhob2FPLUxzREZFIn0.eyJleHAiOjE3NjQ4NzQxMzMsImlhdCI6MTc2NDg3MjMzMywianRpIjoiMTMzMTQ4YmEtMmU2ZS00NWMyLTg2MGMtOWY5M2VmMDhkYjRlIiwiaXNzIjoiaHR0cHM6Ly9pZGVudGl0eS5kYXRhc3BhY2UuY29wZXJuaWN1cy5ldS9hdXRoL3JlYWxtcy9DRFNFIiwiYXVkIjpbIkNMT1VERkVSUk9fUFVCTElDIiwiYWNjb3VudCJdLCJzdWIiOiJhNzE4MDk5Yi1lMWM5LTRiNGUtYjYyMC0yZTI4YTBjZWI0ZDgiLCJ0eXAiOiJCZWFyZXIiLCJhenAiOiJjZHNlLXB1YmxpYyIsInNlc3Npb25fc3RhdGUiOiI3ZTkxZTcwNy0xZjQwLTRkNWQtODNkMy05YjVhOGZkMDM2OGIiLCJhbGxvd2VkLW9yaWdpbnMiOlsiaHR0cHM6Ly9sb2NhbGhvc3Q6NDIwMCIsIioiLCJodHRwczovL3dvcmtzcGFjZS5zdGFnaW5nLWNkc2UtZGF0YS1leHBsb3Jlci5hcHBzLnN0YWdpbmcuaW50cmEuY2xvdWRmZXJyby5jb20iXSwicmVhbG1fYWNjZXNzIjp7InJvbGVzIjpbImNvcGVybmljdXMtZ2VuZXJhbC1xdW90YSIsIm9mZmxpbmVfYWNjZXNzIiwidW1hX2F1dGhvcml6YXRpb24iLCJkZWZhdWx0LXJvbGVzLWNkYXMiLCJjb3Blcm5pY3VzLWdlbmVyYWwiXX0sInJlc291cmNlX2FjY2VzcyI6eyJhY2NvdW50Ijp7InJvbGVzIjpbIm1hbmFnZS1hY2NvdW50IiwibWFuYWdlLWFjY291bnQtbGlua3MiLCJ2aWV3LXByb2ZpbGUiXX19LCJzY29wZSI6IkFVRElFTkNFX1BVQkxJQyBvcGVuaWQgZW1haWwgcHJvZmlsZSBvbmRlbWFuZF9wcm9jZXNzaW5nIHVzZXItY29udGV4dCIsInNpZCI6IjdlOTFlNzA3LTFmNDAtNGQ1ZC04M2QzLTliNWE4ZmQwMzY4YiIsImdyb3VwX21lbWJlcnNoaXAiOlsiL2FjY2Vzc19ncm91cHMvdXNlcl90eXBvbG9neS9jb3Blcm5pY3VzX2dlbmVyYWwiLCIvb3JnYW5pemF0aW9ucy9kZWZhdWx0LWE3MTgwOTliLWUxYzktNGI0ZS1iNjIwLTJlMjhhMGNlYjRkOC9yZWd1bGFyX3VzZXIiXSwiZW1haWxfdmVyaWZpZWQiOnRydWUsIm5hbWUiOiJQYW9sbyBBbmRyaWNoIiwib3JnYW5pemF0aW9ucyI6WyJkZWZhdWx0LWE3MTgwOTliLWUxYzktNGI0ZS1iNjIwLTJlMjhhMGNlYjRkOCJdLCJ1c2VyX2NvbnRleHRfaWQiOiI4M2I4MGNiMS00NmNiLTRkNjUtYTZmMi1jMjI2NGNjY2RjNDgiLCJjb250ZXh0X3JvbGVzIjp7fSwiY29udGV4dF9ncm91cHMiOlsiL2FjY2Vzc19ncm91cHMvdXNlcl90eXBvbG9neS9jb3Blcm5pY3VzX2dlbmVyYWwvIiwiL29yZ2FuaXphdGlvbnMvZGVmYXVsdC1hNzE4MDk5Yi1lMWM5LTRiNGUtYjYyMC0yZTI4YTBjZWI0ZDgvcmVndWxhcl91c2VyLyJdLCJwcmVmZXJyZWRfdXNlcm5hbWUiOiJhbmRyaWNocGFvbG9AZ21haWwuY29tIiwiZ2l2ZW5fbmFtZSI6IlBhb2xvIiwiZmFtaWx5X25hbWUiOiJBbmRyaWNoIiwidXNlcl9jb250ZXh0IjoiZGVmYXVsdC1hNzE4MDk5Yi1lMWM5LTRiNGUtYjYyMC0yZTI4YTBjZWI0ZDgiLCJlbWFpbCI6ImFuZHJpY2hwYW9sb0BnbWFpbC5jb20ifQ.jRzQib6GF2MIQuAxSs6nGrrgEmNqBJSKWYBb42Dk5lp0tZDtPazHXTlSCoySJ3fhp_f5BmdelEpXXzCJzOvX-Dfs08GW784hFrF34Tz44OxC-E89t2SFKG9sQO2J8L0uu1d1zoW96E4CqXWJFtNyVnx0UhCQmQoUMluxp356I3dsk59Y8FvsBhagWCYhd8K-J1579cQ9gqvbaATzNWWafFYYQsohO3fJdCaLO0Wuj4SV18WLZqGfloneHPmNIcxe8xeG7wQvNqjk5x6UEg_yuToyfXJlTSTozXkp-IqPkuh50pX114I0Cz8EHo-G2H26oE0zJEIYoPA7rSUiji2ekA"

# Try these asset keys first (common names for GRD backscatter COGs)
PREFERRED_ASSETS = ["vv", "VH", "vh", "VV", "vh", "hh", "hv", "s1-radiometry", "s1-vv"]

# Per-page items to request (STAC limit param)
PAGE_LIMIT = 100

# HTTP settings
REQUEST_TIMEOUT = 90  # seconds
RETRY_BACKOFF = [1, 2, 5]  # seconds between retries


# ---- Helpers ----
def get_headers():
    headers = {"Accept": "application/json"}
    token = os.environ.get("CDSE_TOKEN")
    if token:
        headers["Authorization"] = f"Bearer {token}"
    return headers

def post_stac(payload):
    headers = get_headers()
    headers["Content-Type"] = "application/json"
    for attempt in range(len(RETRY_BACKOFF) + 1):
        try:
            r = requests.post(STAC_SEARCH_URL, headers=headers, json=payload, timeout=REQUEST_TIMEOUT)
            # helpful debugging if server returns error
            if r.status_code >= 400:
                print(f"POST {STAC_SEARCH_URL} returned {r.status_code}")
                print("Response text:\n", r.text)
                r.raise_for_status()
            return r.json()
        except requests.HTTPError:
            raise
        except Exception as e:
            if attempt < len(RETRY_BACKOFF):
                wait = RETRY_BACKOFF[attempt]
                print(f"POST network error: {e!r}. retrying in {wait}s...")
                time.sleep(wait)
            else:
                raise

def get_json(url):
    headers = get_headers()
    for attempt in range(len(RETRY_BACKOFF) + 1):
        try:
            r = requests.get(url, headers=headers, timeout=REQUEST_TIMEOUT)
            if r.status_code >= 400:
                print(f"GET {url} returned {r.status_code}")
                print("Response text:\n", r.text)
                r.raise_for_status()
            return r.json()
        except requests.HTTPError:
            raise
        except Exception as e:
            if attempt < len(RETRY_BACKOFF):
                wait = RETRY_BACKOFF[attempt]
                print(f"GET error: {e!r}. retrying in {wait}s...")
                time.sleep(wait)
            else:
                raise

def find_next(resp_json):
    for link in resp_json.get("links", []):
        if link.get("rel") == "next" and link.get("href"):
            return link["href"]
    return None

def sanitize(href):
    p = urlparse(href)
    name = os.path.basename(p.path)
    return name or "asset.bin"

def stream_download(href, dst):
    headers = get_headers()
    with requests.get(href, headers=headers, stream=True, timeout=REQUEST_TIMEOUT) as r:
        r.raise_for_status()
        total = int(r.headers.get("content-length", 0) or 0)
        tmp = dst + ".part"
        with open(tmp, "wb") as f, tqdm(total=total, unit="B", unit_scale=True, desc=os.path.basename(dst)) as pbar:
            for chunk in r.iter_content(chunk_size=8192):
                if chunk:
                    f.write(chunk)
                    pbar.update(len(chunk))
        os.replace(tmp, dst)

def choose_asset(assets):
    for k in PREFERRED_ASSETS:
        if k in assets and assets[k].get("href"):
            return assets[k]["href"], k
    # fallback: first .tif/.tiff/.zip asset
    for k, v in assets.items():
        href = v.get("href")
        if href and (href.lower().endswith(".tif") or href.lower().endswith(".tiff") or href.lower().endswith(".zip")):
            return href, k
    return None, None

# ---- Build payload (with CQL2-json filter) ----
def build_payload(limit=PAGE_LIMIT):
    payload = {
        "collections": [COLLECTION],
        "bbox": UGANDA_BBOX,
        "datetime": DATETIME,
        "limit": limit,
        "filter": {
            "op": "and",
            "args": [
                # instrument mode must equal "IW"
                {"op": "eq", "args": [{"property": "sar:instrument_mode"}, "IW"]},
                # 'in' with property first: sar:polarizations contains "VV"
                {"op": "in", "args": [{"property": "sar:polarizations"}, "VV"]}
            ]
        },
        "filter-lang": "cql2-json"
    }
    return payload

def iterate_items(payload):
    resp = post_stac(payload)
    for f in resp.get("features", []):
        yield f
    next_href = find_next(resp)
    while next_href:
        resp = get_json(next_href)
        for f in resp.get("features", []):
            yield f
        next_href = find_next(resp)

# ---- Main run ----
def run(outdir=str(RAW_DATA_DIR.resolve()), max_items=None, verbose=True):
    os.makedirs(outdir, exist_ok=True)
    payload = build_payload()
    if verbose:
        print("Search payload:")
        print(json.dumps(payload, indent=2))
        print("Starting search & download...")
    count = 0
    for item in iterate_items(payload):
        item_id = item.get("id", f"item_{count}")
        if verbose:
            print(f"\nItem #{count+1}: id={item_id}")
        item_dir = os.path.join(outdir, item_id)
        os.makedirs(item_dir, exist_ok=True)
        assets = item.get("assets", {})
        if not assets:
            if verbose:
                print("  No assets listed; skipping.")
            count += 1
            if max_items and count >= max_items:
                break
            continue
        href, key = choose_asset(assets)
        if not href:
            if verbose:
                print("  No suitable asset found; skipping.")
            count += 1
            if max_items and count >= max_items:
                break
            continue
        fname = sanitize(href)
        out_path = os.path.join(item_dir, fname)
        try:
            if verbose:
                print(f"  Downloading asset '{key}' -> {out_path}")
            stream_download(href, out_path)
            if verbose:
                print("  Download finished.")
        except Exception as e:
            print(f"  Download error: {e!r}")
        count += 1
        if max_items and count >= max_items:
            if verbose:
                print(f"Reached max_items={max_items}; stopping.")
            break
    if verbose:
        print(f"\nDone. Processed {count} items. Downloads under: {outdir}")

# ---- CLI ----
def parse_args():
    p = argparse.ArgumentParser(description="Download Sentinel-1 (VV / IW) from CDSE STAC for Uganda")
    p.add_argument("--outdir", default="downloads")
    p.add_argument("--max-items", type=int, default=None)
    p.add_argument("--limit", type=int, default=PAGE_LIMIT)
    p.add_argument("--quiet", action="store_true")
    return p.parse_args()

if __name__ == "__main__":
    args = parse_args()
    PAGE_LIMIT = args.limit if args.limit else PAGE_LIMIT
    run(outdir=args.outdir, max_items=args.max_items, verbose=not args.quiet)
