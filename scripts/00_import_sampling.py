"""Step 0: bring in the sample from msna-sampling.

Copies msna-sampling's outputs into data/synthetic/from_sampling/: from a sibling
checkout (../msna-sampling) if there is one, otherwise from the public GitHub repo.
"""
import shutil
import sys
import urllib.request
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "python"))
from monitoring_tools import ROOT, from_sampling, load_config  # noqa: E402

STATES = ["adamawa", "benue", "borno", "kaduna", "katsina", "kebbi", "nasarawa", "plateau",
          "sokoto", "yobe", "zamfara"]


def main() -> int:
    cfg = load_config()
    s = cfg["sampling"]
    dest = from_sampling(cfg)
    files = list(s["files"]) + [f"{s['households_dir']}/households_{st}.csv" for st in STATES]
    local = (ROOT / s["local_repo"] / "data" / "synthetic").resolve()
    source = f"local checkout {s['local_repo']}" if local.exists() else "GitHub"
    for name in files:
        target = dest / name
        target.parent.mkdir(parents=True, exist_ok=True)
        if local.exists():
            shutil.copyfile(local / name, target)
        else:
            with urllib.request.urlopen(f"{s['github_raw']}/data/synthetic/{name}", timeout=120) as r, \
                    open(target, "wb") as f:
                shutil.copyfileobj(r, f)
    print(f"Imported {len(files)} files from msna-sampling ({source}) into {dest.relative_to(ROOT)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
