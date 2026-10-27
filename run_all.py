"""Run the whole monitoring pipeline, R and Python steps in order.

Usage:  python run_all.py            # every step
        python run_all.py 2          # from step 2 onwards

Rscript is found on PATH, or set the RSCRIPT environment variable to its full path.
Step 3 is a gate: if a validity check fails, the pipeline stops there.
"""
import glob
import os
import shutil
import subprocess
import sys
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parent
STEPS = [
    "00_import_sampling.py",
    "01_generate_submissions.py",
    "02_clean.R",
    "03_validity_checks.R",
    "04_partner_digests.py",
    "05_dashboard_data.R",
]


def find_rscript() -> str:
    found = os.environ.get("RSCRIPT") or shutil.which("Rscript")
    if not found and os.name == "nt":
        # standard per-user and machine-wide R installs on Windows
        candidates = glob.glob(os.path.expandvars(r"%LOCALAPPDATA%\Programs\R\R-*\bin\Rscript.exe")) + \
            glob.glob(r"C:\Program Files\R\R-*\bin\Rscript.exe")
        found = sorted(candidates)[-1] if candidates else None
    if not found:
        sys.exit("Rscript not found: put R on PATH or set RSCRIPT to its full path")
    return found


def main() -> int:
    start_at = int(sys.argv[1]) if len(sys.argv) > 1 else 0
    rscript = find_rscript()
    for step in STEPS:
        if int(step[:2]) < start_at:
            continue
        cmd = [rscript, f"scripts/{step}"] if step.endswith(".R") else [sys.executable, f"scripts/{step}"]
        print(f"\n=== {step}", flush=True)
        t0 = time.time()
        result = subprocess.run(cmd, cwd=ROOT)
        if result.returncode != 0:
            print(f"Stopped: {step} failed (exit {result.returncode})")
            return result.returncode
        print(f"--- {step} done in {time.time() - t0:.0f} s", flush=True)
    print("\nTo open the dashboard: Rscript -e \"shiny::runApp('dashboard_app')\"")
    return 0


if __name__ == "__main__":
    sys.exit(main())
