import os
import json
import time
import pandas as pd
import importlib
import sys
import subprocess
from contextlib import contextmanager
from IPython import get_ipython
from IPython.display import display
from IPython.core.magic import register_cell_magic

# Usage
# import jupyter_utils
# ju = jupyter_utils.JupyterUtils()
#
# ju.max_display()
# ju.reset_display()
# with ju.full_display():
#     print(df)
# ju.save_interim(df, <name>)
# ju.load_interim(<name>)
# ju.vz(df)
#
# %%notify <(optional) message>
# %%timer
# %%execute_if <bool>


class JupyterUtils:
    def __init__(self, tmp_folder="/tmp/data"):
        self.tmp_folder = tmp_folder
        os.makedirs(self.tmp_folder, exist_ok=True)

    # ---------------------------
    # Display options
    # ---------------------------
    @staticmethod
    def reset_display():
        """Reset pandas display options to defaults."""
        pd.reset_option("display.max_rows")
        pd.reset_option("display.max_columns")
        pd.reset_option("display.max_colwidth")
        if "numpy" in sys.modules:
            import numpy

            numpy.set_printoptions(
                edgeitems=3,
                infstr="inf",
                linewidth=75,
                nanstr="nan",
                precision=8,
                suppress=False,
                threshold=1000,
                formatter=None,
            )
        if "torch" in sys.modules:
            import torch

            torch.set_printoptions(profile="default")

    @staticmethod
    def max_display():
        """Show all rows, columns, and full column width in pandas output."""
        pd.set_option("display.max_rows", None)
        pd.set_option("display.max_columns", None)
        pd.set_option("display.max_colwidth", None)
        if "numpy" in sys.modules:
            import numpy

            numpy.set_printoptions(threshold=numpy.inf)
        if "torch" in sys.modules:
            import torch

            torch.set_printoptions(threshold=float("inf"), linewidth=200)

    @contextmanager
    def full_display(self):
        """Temporarily show all rows/columns."""
        self.max_display()

        try:
            yield
        finally:
            self.reset_display()

    # ---------------------------
    # Data saving / loading
    # ---------------------------
    def load_interim(self, name="tmp"):
        """Load dataFrame or dictionary"""
        path = os.path.join(self.tmp_folder, f"{name}")
        if os.path.exists(f"{path}.parquet"):
            data = pd.read_parquet(f"{path}.parquet")
        elif os.path.exists(f"{path}.json"):
            with open(f"{path}.json") as f:
                data = json.load(f)
        else:
            print(f"path not found {path}")
            return None

        print(f"Loaded: {path}")
        return object

    def save_interim(self, obj, name="tmp"):
        """Save dataFrame as parquet or dictionary as JSON object"""
        from pathlib import Path

        # Remove files with same extension to load the correct one
        directory = Path(self.tmp_folder)

        for path in directory.glob(f"{name}.*"):
            if path.is_file():
                path.unlink()
        match obj:
            case dict():
                path = os.path.join(self.tmp_folder, f"{name}.json")
                with open(path, "w") as f:
                    json.dump(obj, f, default=str, indent=4)
                print(f"Dictionary saved: {path}")
            case pd.DataFrame():
                path = os.path.join(self.tmp_folder, f"{name}.parquet")
                obj.to_parquet(path, index=False)
                print(f"Dataframe saved: {path}")
            case _:
                print("Unknown type")

    # ---------------------------
    # Object visualization
    # ---------------------------
    def vz(self, obj, name="data"):
        """Visualize dicts or DataFrames with external viewer."""
        file_path = os.path.join(self.tmp_folder, name)
        self.save_interim(obj, name)
        match obj:
            case dict():
                os.system(
                    f"kitty_control --floating --geometry 'fullscreen toggle' \"jless {file_path}.json\""
                )
            case pd.DataFrame():
                os.system(
                    f"kitty_control --floating --geometry 'fullscreen toggle' \"vd {file_path}.parquet\""
                )
            case _:
                print("Unknown type")

    # ---------------------------
    # Quick summary / profiling
    # ---------------------------
    @staticmethod
    def summarize(df, include_stats=False):
        """Enhanced DataFrame summary."""
        print("Shape:", df.shape)
        print(f"Memory: {df.memory_usage(deep=True).sum() / 1024**2:.2f} MB")
        print("\nDtypes:\n", df.dtypes.value_counts())
        print("\nMissing values:")
        missing = df.isna().sum()
        if missing.sum() > 0:
            missing_pct = (missing / len(df) * 100).round(2)
            print(
                pd.DataFrame(
                    {"count": missing[missing > 0], "pct": missing_pct[missing > 0]}
                )
            )
        else:
            print("  No missing values")

        if include_stats:
            display("Numeric summary:", df.describe())

        display("Sample:", df.head(3))

    @staticmethod
    def freq(df, column, top=5, dropna=True):
        """Show value counts with cumulative percentage."""
        counts = df[column].value_counts(dropna=dropna)
        percentages = (counts / len(df) * 100).round(2)
        cumulative = percentages.cumsum().round(2)

        stats = pd.DataFrame(
            {"count": counts, "percentage": percentages, "cumulative": cumulative}
        )

        if top is not None:
            stats = stats.head(top)

        return stats

    @staticmethod
    def mem_usage(df):
        """Show memory usage of DataFrame columns."""
        usage = df.memory_usage(deep=True)
        usage_mb = usage / 1024**2
        return pd.DataFrame({"bytes": usage, "MB": usage_mb.round(2)}).sort_values(
            "MB", ascending=False
        )

    @staticmethod
    def diff_dfs(df1, df2, name1="df1", name2="df2"):
        """Compare two DataFrames and show differences."""
        print(f"Shape: {name1}={df1.shape}, {name2}={df2.shape}")
        print(f"\nColumns only in {name1}:", set(df1.columns) - set(df2.columns))
        print(f"Columns only in {name2}:", set(df2.columns) - set(df1.columns))
        print(f"\nCommon columns with different dtypes:")
        common = set(df1.columns) & set(df2.columns)
        for col in common:
            if df1[col].dtype != df2[col].dtype:
                print(f"  {col}: {df1[col].dtype} vs {df2[col].dtype}")

    @staticmethod
    def corr_with(df, target_col, top=10):
        """Show correlations with a target column."""
        numeric_df = df.select_dtypes(include="number")
        if target_col not in numeric_df.columns:
            print(f"Column '{target_col}' is not numeric")
            return None
        corrs = numeric_df.corr()[target_col].abs().sort_values(ascending=False)
        return corrs.head(top + 1)[1:]  # Exclude self-correlation

    # ---------------------------
    # Quick plotting
    # ---------------------------
    @staticmethod
    def quick_plot(df, cols=None):
        import matplotlib.pyplot as plt

        cols = cols or df.select_dtypes(include="number").columns
        df[cols].plot(figsize=(8, 5))
        plt.show()

    # ---------------------------
    # Reload module
    # ---------------------------
    @staticmethod
    def reload_module(mod):
        importlib.reload(mod)

    # ---------------------------
    # System / environment info
    # ---------------------------
    @staticmethod
    def sys_info():
        import platform

        print("Python:", sys.version)
        print("Platform:", platform.platform())
        print("Current working dir:", os.getcwd())


# ---------------------------
# Jupyter cell magics
# ---------------------------
@register_cell_magic
def notify(line, cell):
    """Send notification after cell execution, reporting failures."""
    result = get_ipython().run_cell(cell)
    msg = line or ("Cell failed" if not result.success else "Cell execution done")
    subprocess.run(["dunstify", "--timeout", "9999", msg])


@register_cell_magic
def execute_if(line, cell):
    """Execute cell if the arguments evaluate to True"""
    ip = get_ipython()
    user_ns = ip.user_ns  # gives you the notebook’s variable scope
    if eval(line, user_ns):
        ip.run_cell(cell)


@register_cell_magic
def timer(line, cell):
    """Measure execution time of a cell"""
    start = time.time()
    get_ipython().run_cell(cell)
    end = time.time()
    print(f"⏱️  Cell executed in {end - start:.3f} s")
