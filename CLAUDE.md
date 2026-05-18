# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package purpose

`spatialSpells` is an infrastructure package that provides shared spatial utilities consumed by `spatialRF` (next version), `spatialFolds`, and `spatialThinning`. It is not a user-facing analysis package — its job is to eliminate duplicated boilerplate across those sibling packages.

## Development commands

```r
devtools::load_all()          # load all functions into session
devtools::document()          # regenerate NAMESPACE and man/ from roxygen2
devtools::check()             # full R CMD check
devtools::test()              # run all tests
testthat::test_file("tests/testthat/test-foo.R")  # run one test file
spelling::spell_check_package()  # check spelling (word list in inst/WORDLIST)
```

Rebuild package data after editing `dev_scripts/data_infra_xy_column_names.R`:
```r
source("dev_scripts/data_infra_xy_column_names.R")
```

## Architecture

### Function families

| Prefix | Role | Exported |
|--------|------|----------|
| `cast_*` | Type conversions (df ↔ sf, sf → bbox, xy → xyz) | Yes |
| `validate_*` | Input validation; return the (possibly coerced) object | Yes |
| `infra_*` | Shared plumbing: call stack, CRS guessing, column detection | Yes |
| `utils_*` | Pure utilities (spherical geometry detection) | Yes |
| `sf_helpers.R` | Internal helpers (`is_sf`, `sf_to_xy`, etc.) | No (`@noRd`) |

### Error message pattern — `infra_call_stack()`

All exported functions accept `...` and pull a `parent` argument from it:

```r
my_function <- function(x, ...) {
  dots <- list(...)
  call_stack <- infra_call_stack(parent = dots$parent, child = "spatialSpells::my_function()")
  stop("\n", call_stack, ": meaningful message.", call. = FALSE)
}
```

When `my_function` calls another spatialSpells function it passes `parent = call_stack` so error messages show a `└──` hierarchy. Functions that never receive a parent (top-level entry points) pass `parent = NULL`.

### Coordinate column name detection

`infra_x_column_names` and `infra_y_column_names` are package datasets (multilingual vectors of recognised column name variants). Functions match against these by lowercasing the dataframe's column names first. The source lists live in `dev_scripts/data_infra_xy_column_names.R`; regenerate with `usethis::use_data()` after editing.

### `@autoglobal` and `roxyglobals`

`DESCRIPTION` declares `roxyglobals` as the global-variable roclet. Tag every exported function with `@autoglobal` so `devtools::document()` keeps `R/globals.R` in sync instead of requiring manual `utils::globalVariables()` calls.

### CRS auto-detection

`infra_guess_crs()` detects EPSG:4326 (values within ±180/±90) and EPSG:3857 (Web Mercator scale). UTM and other projected CRS cannot be detected and return `NA_integer_` with a message. Callers that need a valid CRS must accept `NA` gracefully or document that `crs` is required.

### `validate_sf()` coercion contract

`validate_sf()` accepts either an `sf` object or a plain data frame. When given a data frame it calls `cast_df_to_sf(crs = NA)` internally and stamps the result with `attr(df, "validated") <- TRUE`. Downstream functions that call `validate_sf()` receive a guaranteed `sf` object and can skip re-validation.
