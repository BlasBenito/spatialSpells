#' Guess name of the WKT geometry column in a data frame
#'
#' @description Returns the name of the WKT geometry column in a data frame.
#'   Detection proceeds in three levels: (1) user-supplied `wkt_column`, (2)
#'   name matching against `spatialSpells::infra_wkt_column_names`, and (3)
#'   value grep for WKT geometry type keywords in the first row. Every
#'   candidate is validated with [sf::st_as_sfc()] before being returned.
#'
#' @param df (required, data frame) Data frame to search for a WKT column.
#'   Default: NULL
#' @param wkt_column (optional, character string) Name of the column containing
#'   WKT geometries. If more than one element is provided, only the first is
#'   used. Default: NULL
#' @param quiet (optional, logical) If TRUE, all messages are silenced.
#'   Default: FALSE
#' @param ... Additional arguments passed to `infra_call_stack()` (e.g.
#'   `parent`).
#' @returns A character string with the detected column name.
#' @family infra
#' @export
#' @autoglobal
#' @examples
#' df <- data.frame(geometry = c("POINT (1 2)", "POINT (3 4)"), species = "A")
#' infra_get_wkt_column(df = df)
#'
#' df2 <- data.frame(foo = c("POINT (1 2)", "POINT (3 4)"), species = "B")
#' infra_get_wkt_column(df = df2)
infra_get_wkt_column <- function(
  df = NULL,
  wkt_column = NULL,
  quiet = FALSE,
  ...
) {
  dots <- list(...)

  call_stack <- infra_call_stack(
    parent = dots$parent,
    child = "spatialSpells::infra_get_wkt_column()"
  )

  if (is.null(df)) {
    stop(
      "\n",
      call_stack,
      ": argument 'df' cannot be NULL.",
      call. = FALSE
    )
  }

  if (!is.null(wkt_column)) {
    wkt_column <- as.character(wkt_column[1])
    if (wkt_column %in% names(df)) {
      wkt_column_clean <- janitor::make_clean_names(wkt_column)
      if (!wkt_column_clean %in% spatialSpells::infra_wkt_column_names) {
        if (!quiet) {
          message(
            "\n",
            call_stack,
            ": argument 'wkt_column' = '",
            wkt_column,
            "' has an unusual name (see 'spatialSpells::infra_wkt_column_names')"
          )
        }
      }
      .validate_wkt(df = df, col = wkt_column, call_stack = call_stack)
      return(wkt_column)
    } else {
      if (!quiet) {
        message(
          "\n",
          call_stack,
          ": argument 'wkt_column' with value '",
          wkt_column,
          "' is not in 'names(df)', trying to find the correct one."
        )
      }
    }
  }

  # Level 1: name matching
  columns_clean <- janitor::make_clean_names(string = names(df))
  names(columns_clean) <- names(df)

  idx <- which(columns_clean %in% spatialSpells::infra_wkt_column_names)[1]

  if (!is.na(idx)) {
    candidate <- names(columns_clean)[idx]
    .validate_wkt(df = df, col = candidate, call_stack = call_stack)
    return(candidate)
  }

  # Level 2: value grep
  first_row <- as.character(df[1, ])
  idx <- which(grepl("POINT|MULTIPOINT|POLYGON|MULTIPOLYGON", first_row, fixed = FALSE))[1]

  if (!is.na(idx)) {
    candidate <- names(df)[idx]
    .validate_wkt(df = df, col = candidate, call_stack = call_stack)
    return(candidate)
  }

  stop(
    "\n",
    call_stack,
    ": cannot find a valid WKT column in 'df'.",
    call. = FALSE
  )
}

#' @noRd
.validate_wkt <- function(df, col, call_stack) {
  result <- tryCatch(
    sf::st_as_sfc(df[[col]], quiet = TRUE),
    error = function(e) NULL
  )
  if (is.null(result)) {
    stop(
      "\n",
      call_stack,
      ": column '",
      col,
      "' was identified as a WKT candidate but ",
      "sf::st_as_sfc() could not parse it as valid WKT.",
      call. = FALSE
    )
  }
  invisible(result)
}
