#' Validate a data frame for conversion to sf
#'
#' @description
#' Validates that a data.frame (or tibble/data.table) can be safely converted
#' to an sf object. Checks for valid x/y coordinate columns first; if
#' none are found, falls back to WKT column detection. Returns the validated
#' df stamped with `attr(df, "validated") <- TRUE`.
#'
#' @param df (required, data.frame) Dataframe to validate. Default: NULL
#' @param x_column (optional, character string) Name of the x / longitude
#'   column. Default: NULL
#' @param y_column (optional, character string) Name of the y / latitude
#'   column. Default: NULL
#' @param wkt_column (optional, character string) Name of the WKT geometry
#'   column. Passed to [infra_get_wkt_column()] during WKT detection.
#'   Default: NULL
#' @param quiet (optional, logical) If TRUE, all messages are silenced.
#'   Default: FALSE
#' @param ... Additional arguments passed to `infra_call_stack()` (e.g.
#'   `parent`).
#'
#' @returns data.frame with `attr(df, "validated") == TRUE`.
#' @family infra
#' @export
#' @autoglobal
#' @examples
#' df <- data.frame(longitude = c(1.0, 2.0, 3.0), latitude = c(4.0, 5.0, 6.0))
#' infra_validate_df(df)
infra_validate_df <- function(
  df = NULL,
  x_column = NULL,
  y_column = NULL,
  wkt_column = NULL,
  quiet = FALSE,
  ...
) {
  dots <- list(...)

  call_stack <- infra_call_stack(
    parent = dots$parent,
    child  = "spatialSpells::infra_validate_df()"
  )

  # Structural checks
  if (is.null(df)) {
    stop("\n", call_stack, ": argument 'df' cannot be NULL.", call. = FALSE)
  }

  if (!inherits(df, "data.frame")) {
    stop(
      "\n", call_stack,
      ": argument 'df' must be a data.frame, tibble, or data.table.",
      call. = FALSE
    )
  }

  if (nrow(df) == 0) {
    stop("\n", call_stack, ": argument 'df' has no rows.", call. = FALSE)
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

    # is.na() catches NaN as well
    x_na_count <- sum(is.na(x_vals))
    if (x_na_count > 0) {
      stop(
        "\n", call_stack,
        ": x-coordinate column '", x_col, "' has ", x_na_count,
        " NA/NaN value(s).",
        call. = FALSE
      )
    }

    y_na_count <- sum(is.na(y_vals))
    if (y_na_count > 0) {
      stop(
        "\n", call_stack,
        ": y-coordinate column '", y_col, "' has ", y_na_count,
        " NA/NaN value(s).",
        call. = FALSE
      )
    }

    x_inf_count <- sum(is.infinite(x_vals))
    if (x_inf_count > 0) {
      stop(
        "\n", call_stack,
        ": x-coordinate column '", x_col, "' has ", x_inf_count,
        " infinite value(s).",
        call. = FALSE
      )
    }

    y_inf_count <- sum(is.infinite(y_vals))
    if (y_inf_count > 0) {
      stop(
        "\n", call_stack,
        ": y-coordinate column '", y_col, "' has ", y_inf_count,
        " infinite value(s).",
        call. = FALSE
      )
    }

    if (length(unique(x_vals)) == 1 && length(unique(y_vals)) == 1) {
      warning(
        "\n", call_stack,
        ": all x and y coordinates are identical.",
        call. = FALSE
      )
    }

    attr(df, "validated") <- TRUE
    return(df)
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
      # "cannot find" means no WKT column exists — fall through to final stop.
      # Any other error (e.g. bad WKT content) must be re-thrown.
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

    attr(df, "validated") <- TRUE
    return(df)
  }

  stop(
    "\n", call_stack,
    ": argument 'df' has no recognisable x/y coordinate columns ",
    "(see 'spatialSpells::infra_x_column_names') and no WKT column ",
    "(names or content matching POINT/POLYGON/etc.).",
    call. = FALSE
  )
}
