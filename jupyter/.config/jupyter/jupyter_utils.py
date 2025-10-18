import os
import json
import time
import pandas as pd
import sys
from IPython import get_ipython
from IPython.display import display
from IPython.core.magic import register_cell_magic


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

    from contextlib import contextmanager

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
    def save_interim(self, df, name="tmp"):
        """Save DataFrame as parquet."""
        path = os.path.join(self.tmp_folder, f"{name}.parquet")
        df.to_parquet(path, index=False)
        print(f"Saved: {path}")

    def load_interim(self, name="tmp"):
        """Load DataFrame from parquet."""
        path = os.path.join(self.tmp_folder, f"{name}.parquet")
        df = pd.read_parquet(path)
        print(f"Loaded: {path}")
        return df

    def dump_var(self, var, name="var"):
        """Save arbitrary variable as JSON."""
        path = os.path.join(self.tmp_folder, f"{name}.json")
        with open(path, "w") as f:
            json.dump(var, f, default=str, indent=4)
        print(f"Variable saved: {path}")

    # ---------------------------
    # Object visualization
    # ---------------------------
    def vz(self, obj, name="data"):
        """Visualize dicts or DataFrames with external viewer."""
        file_path = os.path.join(self.tmp_folder, name)
        match obj:
            case dict():
                self.dump_var(obj, name)
                os.system(
                    f"kitty_control --floating --geometry 'fullscreen toggle' \"jless {file_path}.json\""
                )
            case pd.DataFrame():
                self.save_interim(obj, name)
                os.system(
                    f"kitty_control --floating --geometry 'fullscreen toggle' \"vd {file_path}.parquet\""
                )
            case _:
                print("Unknown type")

    # ---------------------------
    # Quick summary / profiling
    # ---------------------------
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


# ---------------------------
# Jupyter cell magics
# ---------------------------
@register_cell_magic
def notify(line, cell):
    """Send notification after cell execution."""
    get_ipython().run_cell(cell)
    os.system("dunstify Cell execution done")


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
