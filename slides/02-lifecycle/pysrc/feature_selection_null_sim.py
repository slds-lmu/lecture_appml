"""Null-simulation demonstration of feature-selection leakage.

Used by slides03-data-leakage.tex (frames "Feature-selection leakage: ...").
The data has no real signal: y is a random binary target and all p features
are i.i.d. standard normal noise, independent of y. An honest CV procedure
should therefore report accuracy near 0.50.

The leaky pipeline filters features once on the full dataset and then runs
CV on the already-filtered data, which uses test-fold labels during
selection. The proper pipeline filters inside each CV fold.

Run:
    python feature_selection_null_sim.py

Expected (50 seeds, default parameters):
    Proper CV accuracy: ~0.49 +/- 0.06
    Leaky  CV accuracy: ~0.95 +/- 0.03
"""

import numpy as np
from sklearn.feature_selection import SelectKBest, f_classif
from sklearn.model_selection import StratifiedKFold, cross_val_score
from sklearn.pipeline import Pipeline
from sklearn.svm import LinearSVC


def simulate(seed, n=100, p=10_000, k=50):
    rng = np.random.default_rng(seed)

    # Pure-noise design matrix and a balanced random binary target.
    X = rng.normal(size=(n, p))
    y = np.r_[np.zeros(n // 2, dtype=int), np.ones(n - n // 2, dtype=int)]
    rng.shuffle(y)

    cv = StratifiedKFold(n_splits=5, shuffle=True, random_state=seed)
    # dual=False is the right choice when n > p (here p=k=50 after filtering,
    # n=100) and works on every sklearn version; max_iter is large because the
    # pure-noise data converges slowly.
    clf = LinearSVC(C=1, dual=False, max_iter=20_000, random_state=seed)

    # Proper: filtering happens inside the CV loop.
    proper_pipeline = Pipeline([
        ("filter", SelectKBest(score_func=f_classif, k=k)),
        ("clf", clf),
    ])
    proper_acc = cross_val_score(
        proper_pipeline, X, y, cv=cv, scoring="accuracy"
    ).mean()

    # Leaky: filtering happens once on the full dataset, then CV on the
    # already-filtered features.
    selector = SelectKBest(score_func=f_classif, k=k)
    X_selected = selector.fit_transform(X, y)
    leaky_acc = cross_val_score(
        clf, X_selected, y, cv=cv, scoring="accuracy"
    ).mean()

    return proper_acc, leaky_acc


if __name__ == "__main__":
    results = np.array([simulate(seed) for seed in range(50)])
    proper, leaky = results[:, 0], results[:, 1]

    print(f"Proper CV accuracy: {proper.mean():.3f} +/- {proper.std():.3f}")
    print(f"Leaky  CV accuracy: {leaky.mean():.3f} +/- {leaky.std():.3f}")
    print(f"Gap (leaky - proper): {leaky.mean() - proper.mean():.3f}")
    print(f"Proper range: [{proper.min():.3f}, {proper.max():.3f}]")
    print(f"Leaky  range: [{leaky.min():.3f}, {leaky.max():.3f}]")
