#' Convert an sf data frame to an xy matrix
#' @description
#' Extracts coordinates from an sf data frame and returns them as a two-column
#' numeric matrix. POLYGON and MULTIPOLYGON geometries are first converted to
#' centroids before coordinate extraction.
#' @param df (required, sf) An sf data frame with spatial geometry
#' @param ... (optional) For internal arguments only.
#' @return Numeric matrix with columns "x" and "y". For POLYGON or MULTIPOLYGON
#'   input, the coordinates correspond to polygon centroids.
#' @family casting_functions
#' @autoglobal
#' @export
cast_df_to_xy <- function(
  df = NULL,
  ...
) {
  dots <- list(...)

  call_stack <- infra_call_stack(
    parent = dots$parent,
    child = "spatialSpells::cast_df_to_xy()"
  )

  if (is.null(df)) {
    stop(
      "\n", call_stack,
      ": argument 'df' cannot be NULL.",
      call. = FALSE
    )
  }

  if (!inherits(df, "sf")) {
    stop(
      "\n", call_stack,
      ": argument 'df' must be an sf object.",
      call. = FALSE
    )
  }

  df_geometry_type <- df |>
    sf::st_geometry_type(by_geometry = FALSE) |>
    as.character()

  if (df_geometry_type %in% c("LINESTRING", "MULTILINESTRING")) {
    stop(
      "\n", call_stack,
      ": LINESTRING geometries are not supported. ",
      "Convert to POINT geometries before calling this function.",
      call. = FALSE
    )
  }

  if (df_geometry_type %in% c("POINT", "MULTIPOINT")) {
    df_xy <- sf::st_coordinates(df)
  } else if (df_geometry_type %in% c("POLYGON", "MULTIPOLYGON")) {
    message("spatialSpells::cast_df_to_xy(): POLYGON geometry converted to centroids.")
    df_xy <- df |>
      sf::st_centroid() |>
      sf::st_coordinates()
  } else {
    stop(
      "\n", call_stack,
      ": Geometry type '", df_geometry_type, "' is not supported. ",
      "Supported types: POINT, MULTIPOINT, POLYGON, MULTIPOLYGON.",
      call. = FALSE
    )
  }

  if (nrow(df_xy) == 0) {
    stop(
      "\n", call_stack,
      ": Failed to extract coordinates from sf geometry.",
      call. = FALSE
    )
  }

  # Create xy matrix
  xy <- matrix(
    data = c(df_xy[, 1], df_xy[, 2]),
    ncol = 2,
    dimnames = list(NULL, c("x", "y"))
  )

  xy
}
