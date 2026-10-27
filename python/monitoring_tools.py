"""Shared tools for the Python steps of the monitoring pipeline."""
from __future__ import annotations

import re
import zipfile
from pathlib import Path

import pandas as pd
import yaml

ROOT = Path(__file__).resolve().parents[1]
SYNTHETIC_LABEL = "Synthetic data – not real findings"


def load_config(path: Path = ROOT / "config.yml") -> dict:
    with open(path, encoding="utf-8") as f:
        return yaml.safe_load(f)


def output_dir(cfg: dict) -> Path:
    return ROOT / cfg["outputs"]["dir"]


def from_sampling(cfg: dict) -> Path:
    return output_dir(cfg) / "from_sampling"


def write_csv(df: pd.DataFrame, path: Path) -> None:
    """CSV with Unix line endings on every OS, so outputs reproduce byte for byte.

    Files ending in .csv.gz are compressed with a fixed timestamp for the same reason.
    """
    path.parent.mkdir(parents=True, exist_ok=True)
    if path.suffix == ".gz":
        df.to_csv(path, index=False, lineterminator="\n",
                  compression={"method": "gzip", "mtime": 0})
    else:
        df.to_csv(path, index=False, lineterminator="\n")


def make_xlsx_reproducible(path: Path) -> None:
    """Fix the timestamps an .xlsx file carries, so the same content gives the same file."""
    with zipfile.ZipFile(path) as z:
        parts = [(info, z.read(info.filename)) for info in z.infolist()]
    with zipfile.ZipFile(path, "w") as z:
        for info, data in parts:
            if info.filename == "docProps/core.xml":
                data = re.sub(rb"(<dcterms:(?:created|modified)[^>]*>)[^<]*", rb"\g<1>2026-01-01T00:00:00Z", data)
            fixed = zipfile.ZipInfo(info.filename, date_time=(2026, 1, 1, 0, 0, 0))
            fixed.compress_type = zipfile.ZIP_DEFLATED
            fixed.external_attr = info.external_attr
            z.writestr(fixed, data)
