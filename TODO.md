# spatialSpells — Operational Gap Analysis

This document tracks gaps between spatialSpells and its consumer packages (spatialFolds, spatialThinning, spatialRF next). It is a decision-making reference, not a sprint backlog — priorities are for the user to set.

---

## 1. Absorb duplicates from sibling packages

These functions exist in spatialFolds and/or spatialThinning but duplicate (or closely overlap) what spatialSpells already provides. The work here is to (a) add missing variants to spatialSpells, (b) delete the sibling copies, and (c) replace them with `spatialSpells::` imports.

| spatialSpells function | spatialFolds | spatialThinning | Action |
|---|---|---|---|
| `cast_df_to_sf()` | duplicated | absent | Delete from spatialFolds; no change in spatialThinning |
| `cast_df_to_xy()` | duplicated (named `cast_df_to_xy`) | duplicated (named `cast_df_to_xy`) | Delete from both siblings; see naming note in §4 |
| `cast_sf_to_bbox()` | duplicated | absent | Delete from spatialFolds |
| `cast_xy_to_xyz()` | duplicated | absent | Delete from spatialFolds |
| `infra_guess_crs()` | duplicated (named `guess_crs`) | absent | Delete from spatialFolds |
| `utils_needs_spherical()` | duplicated | absent | Delete from spatialFolds |
| `validate_sf()` | duplicated (named `validate_arg_sf`) | absent | Delete from spatialFolds |
| `validate_arg_distance()` | present | duplicated | **Add to spatialSpells**, then delete from both siblings |
| `validate_arg_target()` | present | duplicated | **Add to spatialSpells**, then delete from both siblings |

`validate_arg_distance()` and `validate_arg_target()` are the only two functions in this group that spatialSpells does not yet have. Both siblings should be kept frozen until spatialSpells exports them.

---

## 2. Absorb utilities from spatialRF

### 2a. Distance matrix utilities → spatialSpells

These three functions in spatialRF are model-agnostic and belong in spatialSpells. They have no dependency on RF-specific objects.

| Function | Purpose |
|---|---|
| `weights_from_distance_matrix()` | Converts a distance matrix to spatial weights (1/d) |
| `double_center_distance_matrix()` | Double-centers a distance matrix (prerequisite for MEM) |
| `default_distance_thresholds()` | Generates four evenly-spaced distance thresholds from a distance matrix |

### 2b. Spatial eigenspace and autocorrelation → **spatialPredictors** (new package)

These spatialRF functions are higher-level spatial analysis tools. They depend on the utilities in §2a but are too domain-specific for a plumbing package. They belong in a new package named **`spatialPredictors`**, scoped as: *functions that generate spatial predictor variables from coordinates or distance matrices, usable in any modelling framework*.

The name is chosen for discoverability — it matches what a user searches for, and it names the package by its output rather than its method, leaving room for additional approaches beyond eigenvector maps.

| Function | Source | Purpose |
|---|---|---|
| `mem()` | spatialRF | Moran's Eigenvector Maps from a distance matrix |
| `mem_multithreshold()` | spatialRF | MEMs across multiple distance thresholds |
| `pca()` | spatialRF | PCA-based spatial predictors from a distance matrix |
| `pca_multithreshold()` | spatialRF | PCA spatial predictors across multiple thresholds |
| `moran()` | spatialRF | Moran's I test at a single distance threshold |
| `moran_multithreshold()` | spatialRF | Moran's I across multiple scales |
| *(new)* GAM spatial smooths | — | Wrappers around `mgcv::s(lat, lon, bs = "sos")` and similar thin-plate/SOS spline terms, returning predictor matrices in the same format as MEMs |

These are not spatialSpells work items but are recorded here so the overall refactor is visible in one place.

---

## 3. Migrations out of spatialRF (not spatialSpells work)

These spatialRF functions have clear homes in existing sibling packages. Listed here for cross-package coordination.

**→ spatialThinning**

| Function | Purpose |
|---|---|
| `thinning()` | Removes spatially proximate points by minimum distance |
| `thinning_til_n()` | Iterative thinning to reach a target sample size |

**→ spatialFolds**

| Function | Purpose |
|---|---|
| `make_spatial_fold()` | Creates a single spatial train/test fold |
| `make_spatial_folds()` | Creates multiple spatial folds (parallelized) |
| `rf_evaluate()` | Spatial cross-validation evaluation |
| `auc()` | Area under ROC curve (generic metric) |
| `root_mean_squared_error()` | RMSE / nRMSE (generic metric) |

---

## 4. Naming and consistency fixes

### `cast_df_to_xy` vs `cast_sf_to_xy`

Both spatialThinning and spatialFolds call their conversion function `cast_df_to_xy`, but the input is an **sf** object, not a plain data frame. spatialSpells already has `cast_df_to_xy()` with matching logic. The siblings should import `cast_df_to_xy()` from spatialSpells rather than carry their own copy — but the name is technically misleading. Consider adding a `cast_sf_to_xy()` alias or renaming the function in spatialSpells before the siblings are updated, so the public API is correct from the start.

### `infra_x_column_names` / `infra_y_column_names` datasets

spatialSpells ships these as package datasets (multilingual vectors of recognized column name variants). spatialFolds reimplements the same idea as a local `coordinates_names` list object; spatialThinning has no equivalent. Once the sibling packages depend on spatialSpells these datasets should be used everywhere and the spatialFolds local copy removed.
