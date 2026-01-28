#!/usr/bin/env python3
"""
cdse_stac_diagnostics.py

1) List collections from CDSE STAC
2) Run a simple search (no filter) and print the first item's property keys (sample)
3) Try three small filter payloads (different 'in' / 'contains' variants) and print status + body

Requirements:
    pip install requests
Usage:
    python cdse_stac_diagnostics.py
If needed for access, set env var CDSE_TOKEN
"""

import os, json, requests
from urllib.parse import urljoin

BASE = "https://stac.dataspace.copernicus.eu"
COLLECTIONS_URL = urljoin(BASE, "/v1/collections")
SEARCH_URL = urljoin(BASE, "/v1/search")
HEADERS = {"Accept": "application/json", "Content-Type": "application/json"}
token = os.environ.get("CDSE_TOKEN")
if token:
    HEADERS["Authorization"] = f"Bearer {token}"

def list_collections():
    print("=== Collections (first 50) ===")
    r = requests.get(COLLECTIONS_URL, headers=HEADERS, timeout=30)
    print("Status:", r.status_code)
    try:
        j = r.json()
        cols = j.get("collections", [])
        for c in cols[:50]:
            print(" - id:", c.get("id"), "| title:", c.get("title"))
    except Exception:
        print("Response text:", r.text)

def sample_search_no_filter():
    print("\n=== Sample search (no filter) ===")
    payload = {"collections": ["sentinel-1-grd"], "bbox": [29.0, -1.5, 35.2, 4.5], "datetime": "2020-01-01/2025-08-31", "limit": 1}
    r = requests.post(SEARCH_URL, headers=HEADERS, json=payload, timeout=30)
    print("Status:", r.status_code)
    try:
        j = r.json()
    except Exception:
        print("Response text:", r.text)
        return
    feats = j.get("features", [])
    if not feats:
        print("No features returned in sample search.")
        return
    item = feats[0]
    print("Item id:", item.get("id"))
    props = item.get("properties", {})
    print("Properties keys (sample):")
    # print first 50 property keys and some example values
    for k in list(props.keys())[:50]:
        print(" -", k, ":", props.get(k))
    print("\nAssets keys:")
    for a in item.get("assets", {}).keys():
        print(" -", a)

def try_filter(payload, label):
    print(f"\n=== Trying filter variant: {label} ===")
    print("Payload (trimmed):", json.dumps(payload, indent=2)[:1000])
    r = requests.post(SEARCH_URL, headers=HEADERS, json=payload, timeout=30)
    print("Status:", r.status_code)
    try:
        print("Response JSON (trimmed):")
        j = r.json()
        print(json.dumps(j, indent=2)[:2000])
    except Exception:
        print("Response text:", r.text)

def run_tests():
    # Base payload
    base = {"collections": ["sentinel-1-grd"],
            "bbox": [29.0, -1.5, 35.2, 4.5],
            "datetime": "2020-01-01/2025-08-31",
            "limit": 1}

    # Variant A: 'in' with property FIRST (property, value) - property first
    filt_a = {
      "op": "and",
      "args": [
        {"op": "eq", "args": [{"property": "sar:instrument_mode"}, "IW"]},
        {"op": "in", "args": [{"property": "sar:polarizations"}, "VV"]}
      ]
    }
    p_a = dict(base); p_a.update({"filter": filt_a, "filter-lang": "cql2-json"})
    try_filter(p_a, "in_property_first")

    # Variant B: 'in' with value FIRST (value, property)
    filt_b = {
      "op": "and",
      "args": [
        {"op": "eq", "args": [{"property": "sar:instrument_mode"}, "IW"]},
        {"op": "in", "args": ["VV", {"property": "sar:polarizations"}]}
      ]
    }
    p_b = dict(base); p_b.update({"filter": filt_b, "filter-lang": "cql2-json"})
    try_filter(p_b, "in_value_first")

    # Variant C: 'contains' (property, value)
    filt_c = {
      "op": "and",
      "args": [
        {"op": "eq", "args": [{"property": "sar:instrument_mode"}, "IW"]},
        {"op": "contains", "args": [{"property": "sar:polarizations"}, "VV"]}
      ]
    }
    p_c = dict(base); p_c.update({"filter": filt_c, "filter-lang": "cql2-json"})
    try_filter(p_c, "contains_property_first")

if __name__ == "__main__":
    list_collections()
    sample_search_no_filter()
    run_tests()
