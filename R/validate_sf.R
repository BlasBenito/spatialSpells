#' Validate sf/data.frame Input
#'
#' @description
#' Validates sf or data.frame input. Consolidates validation logic used by
#' spatialSpells consumers that require a guaranteed sf object.
#'
#' @param df (required, sf or data.frame) Spatial data with point geometries.
#'   If data.frame, must have recognizable coordinate columns (x/lon/longitude
#'   and y/lat/latitude). Default: NULL
#' @param ... (optional) For internal arguments only.
#'
#' @return The validated sf object, with `attr(df, "validated")` set to `TRUE`.
#'
#' @details
#' Validation steps performed in order:
#' \enumerate{
#'   \item NULL check
#'   \item Zero rows check
#'   \item Convert to sf via cast_df_to_sf() with crs = NA
#' }
#'
#' @family arg_validation
#' @autoglobal
#' @export
validate_sf <- function(
  df = NULL,
  ...
) {
  dots <- list(...)

  call_stack <- infra_call_stack(
    parent = dots$parent,
    child = "spatialSpells::validate_sf()"
  )

  if (is.null(df)) {
    stop(
      "\n",
      call_stack,
      ": argument 'df' cannot be NULL.",
      call. = FALSE
    )
  }

  n_rows <- nrow(df)
  if (is.null(n_rows) || n_rows == 0) {
    stop(
      "\n",
      call_stack,
      ": argument 'df' has no rows.",
      call. = FALSE
    )
  }

  if (!inherits(x = df, what = "sf")) {
    df <- cast_df_to_sf(
      df = df,
      crs = NA,
      parent = call_stack
    )
  }

  # Check for identical coordinates (zero bbox area)
  bbox <- sf::st_bbox(df)
  if (
    bbox["xmin"] == bbox["xmax"] &&
      bbox["ymin"] == bbox["ymax"]
  ) {
    stop(
      "\n",
      call_stack,
      ": All points at same location",
      call. = FALSE
    )
  }

  attr(
    x = df,
    which = "validated"
  ) <- TRUE

  df
}
