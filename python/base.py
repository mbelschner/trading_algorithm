"""
Strategy-Interface fuer den Backtester.

Jede Strategie-Datei in diesem Ordner (ausser solchen mit fuehrendem '_')
muss folgendes exportieren:

    NAME          : str                       - Anzeigename
    PARAM_GRID    : dict[str, list]           - Parameter-Grid fuer Optimierung
    generate_signals(df, **params) -> pd.DataFrame
        - Erhaelt DataFrame mit Spalten Open/High/Low/Close/Volume
        - Muss eine Spalte 'Position' in {-1, 0, 1} zurueckgeben
          (long=1, short=-1, flat=0). Position gilt fuer die NAECHSTE Bar.
        - Darf weitere Spalten hinzufuegen (Indikatoren), aber 'Position' ist Pflicht.

Konvention: 'Position' entsteht aus Signalen, die NUR auf Daten bis inkl. der
aktuellen Bar zugreifen. Der Backtester multipliziert spaeter mit shift(1) der
Returns, damit kein Look-ahead-Bias entsteht.
"""
from itertools import product


def expand_grid(grid: dict):
    """Erzeugt alle Kombinationen aus einem PARAM_GRID."""
    if not grid:
        yield {}
        return
    keys = list(grid.keys())
    for values in product(*[grid[k] for k in keys]):
        yield dict(zip(keys, values))