#' Convert a data frame with coordinates to an sf data frame
#' @description
#' Takes a data frame or tibble with x/y coordinate columns or a WKT geometry
#' column and converts it to an `sf` data frame. When x/y columns are detected,
#' the result has POINT geometry. When a WKT column is used, the geometry type
#' matches the WKT content (POINT, POLYGON, etc.).
#' @param df (required, data.frame) Dataframe with coordinate columns (see
#'   [infra_x_column_names] and [infra_y_column_names]) or a WKT geometry
#'   column. Default: NULL
#' @param crs (optional, integer or NULL) EPSG code. If NULL (default), the CRS
#'   is guessed automatically via [infra_guess_crs()] for x/y paths. For WKT
#'   paths, CRS cannot be auto-detected and the result will have no CRS unless
#'   `crs` is supplied. Pass an explicit integer (e.g. 4326, 3857) to override
#'   detection. Default: NULL
#' @param x_column (optional, character string) Name of the x / longitude
#'   column. Passed to [infra_get_xy_columns()] for detection. Default: NULL
#' @param y_column (optional, character string) Name of the y / latitude
#'   column. Passed to [infra_get_xy_columns()] for detection. Default: NULL
#' @param wkt_column (optional, character string) Name of the WKT geometry
#'   column. Passed to [infra_get_wkt_column()] for detection. Default: NULL
#' @param quiet (optional, logical) If TRUE, all messages are silenced.
#'   Default: FALSE
#' @param ... (optional) For internal arguments only.
#' @return sf data frame with POINT geometry (from x/y columns) or the geometry
#'   type encoded in the WKT column.
#' @details
#' Detection tries x/y columns first, then falls back to a WKT column. When
#' `crs = NULL` and coordinates are used, CRS detection is attempted using
#' coordinate ranges and column names. EPSG:4326 is returned when x ∈ \[-180,
#' 180\] and y ∈ \[-90, 90\]. EPSG:3857 is returned for Web Mercator scale
#' values (x > ±1,000,000). UTM and other projected CRS cannot be detected from
#' coordinate values alone — pass `crs` explicitly in those cases.
#'
#' Small local datasets in a projected CRS whose coordinate values happen to
#' fall within ±180/±90 will be misidentified as EPSG:4326; pass `crs`
#' explicitly to avoid this.
#' @seealso [infra_guess_crs()]
#' @family casting_functions
#' @autoglobal
#' @export
cast_df_to_sf <- function(
  df = NULL,
  crs = NULL,
  x_column = NULL,
  y_column = NULL,
  wkt_column = NULL,
  quiet = FALSE,
  ...
) {
  dots <- list(...)

  call_stack <- infra_call_stack(
    parent = dots$parent,
    child = "spatialSpells::cast_df_to_sf()"
  )

  # Check if already an sf object
  if (inherits(x = df, what = "sf")) {
    return(df)
  }

  # Validate df is a data.frame
  if (!is.data.frame(df)) {
    stop(
      "\n",
      call_stack,
      ": argument 'df' must be a data.frame.",
      call. = FALSE
    )
  }

  # Validate df has rows
  if (nrow(df) == 0) {
    stop(
      "\n",
      call_stack,
      ": argument 'df' has no rows.",
      call. = FALSE
    )
  }

  # Validate user-supplied crs (skip when NULL — will be auto-detected later)
  if (!is.null(crs)) {
    if (length(crs) != 1 || (!is.na(crs) && !is.numeric(crs))) {
      stop(
        "\n",
        call_stack,
        ": argument 'crs' must be NA or a single numeric EPSG code.",
        call. = FALSE
      )
    }
  }

  # X/Y path (primary)
  xy <- tryCatch(
    infra_get_xy_columns(
      columns  = colnames(df),
      x_column = x_column,
      y_column = y_column,
      quiet    = quiet,
      parent   = call_stack
    ),
    error = function(e) NULL
  )

  if (!is.null(xy)) {
    x_col <- xy["x_column"]
    y_col <- xy["y_column"]

    x_vals <- df[[x_col]]
    y_vals <- df[[y_col]]

    if (!is.numeric(x_vals)) {
      stop(
        "\n", call_stack,
        ": x-coordinate column '", x_col,
        "' must be numeric. Found type: ", class(x_vals)[1], ".",
        call. = FALSE
      )
    }
    if (!is.numeric(y_vals)) {
      stop(
        "\n", call_stack,
        ": y-coordinate column '", y_col,
        "' must be numeric. Found type: ", class(y_vals)[1], ".",
        call. = FALSE
      )
    }

    x_na_count <- sum(is.na(x_vals))
    y_na_count <- sum(is.na(y_vals))
    if (x_na_count > 0 || y_na_count > 0) {
      stop(
        "\n", call_stack,
        ": Coordinate columns contain NA values. x: ", x_na_count,
        " NA(s), y: ", y_na_count, " NA(s). ",
        "Please remove or impute missing coordinates.",
        call. = FALSE
      )
    }

    x_inf_count <- sum(is.infinite(x_vals))
    y_inf_count <- sum(is.infinite(y_vals))
    if (x_inf_count > 0 || y_inf_count > 0) {
      stop(
        "\n", call_stack,
        ": Coordinate columns contain infinite values. x: ", x_inf_count,
        " Inf(s), y: ", y_inf_count, " Inf(s). ",
        "Please remove or replace infinite coordinates.",
        call. = FALSE
      )
    }

    if (is.null(crs)) {
      crs <- infra_guess_crs(
        x      = x_vals,
        y      = y_vals,
        x_name = x_col,
        y_name = y_col
      )
    }

    return(sf::st_as_sf(
      x      = df,
      coords = c(x_col, y_col),
      remove = FALSE,
      crs    = crs
    ))
  }

  # WKT path (fallback)
  wkt_col <- tryCatch(
    infra_get_wkt_column(
      df         = df,
      wkt_column = wkt_column,
      quiet      = quiet,
      parent     = call_stack
    ),
    error = function(e) {
      if (grepl("cannot find a valid WKT column", conditionMessage(e), fixed = TRUE)) {
        return(NULL)
      }
      stop(e)
    }
  )

  if (!is.null(wkt_col)) {
    parsed <- tryCatch(sf::st_as_sfc(df[[wkt_col]]), error = function(e) NULL)

    if (is.null(parsed)) {
      stop(
        "\n", call_stack,
        ": column '", wkt_col,
        "' looks like WKT but could not be parsed by sf::st_as_sfc().",
        call. = FALSE
      )
    }

    n_na_geom <- sum(is.na(parsed))
    if (n_na_geom > 0) {
      stop(
        "\n", call_stack,
        ": WKT column '", wkt_col, "' has ", n_na_geom,
        " unparseable row(s).",
        call. = FALSE
      )
    }

    if (!quiet) {
      n_invalid <- sum(!sf::st_is_valid(parsed), na.rm = TRUE)
      if (n_invalid > 0) {
        message(
          "\n", call_stack,
          ": WKT column '", wkt_col, "' has ", n_invalid,
          " geometrically invalid row(s). Consider sf::st_make_valid()."
        )
      }
    }

    if (is.null(crs)) {
      if (!quiet) {
        message(
          "\n", call_stack,
          ": CRS cannot be auto-detected from WKT strings. ",
          "The result will have no CRS. Pass `crs` explicitly if needed."
        )
      }
      crs <- NA_integer_
    }

    return(sf::st_as_sf(
      x      = df,
      wkt    = wkt_col,
      remove = FALSE,
      crs    = crs
    ))
  }

  stop(
    "\n", call_stack,
    ": argument 'df' has no recognisable x/y coordinate columns ",
    "(see 'spatialSpells::infra_x_column_names') and no WKT column ",
    "(names or content matching POINT/POLYGON/etc.).",
    call. = FALSE
  )
}
