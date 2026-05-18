#' Guess name of the longitude column in a data frame
#'
#' @description Returns the name of the longitude column in a data frame. If
#'   `x_column` is not provided, the function attempts to auto-detect it by
#'   matching cleaned column names against `spatialSpells::infra_x_column_names`.
#'   When `x_column` is provided but not found in `columns`, the function falls
#'   back to auto-detection and emits a message.
#'
#' @param columns (required, character vector) Column names of the target data
#'   frame. Default: NULL
#' @param x_column (optional, character string) Name of the column representing
#'   the x coordinate or longitude. If more than one element is provided, only
#'   the first is used. Default: NULL
#' @param quiet (optional, logical) If TRUE, all messages are silenced.
#'   Default: FALSE
#' @param ... Additional arguments passed to `infra_call_stack()` (e.g.
#'   `parent`).
#' @returns A character string with the detected column name.
#' @family infra
#' @export
#' @autoglobal
#' @examples
#' infra_get_x_column(columns = c("longitude", "latitude", "species"))
#' infra_get_x_column(
#'   columns = c("longitude", "latitude", "species"),
#'   x_column = "longitude"
#' )
infra_get_x_column <- function(
  columns = NULL,
  x_column = NULL,
  quiet = FALSE,
  ...
) {
  dots <- list(...)

  call_stack <- infra_call_stack(
    parent = dots$parent,
    child = "spatialSpells::infra_get_x_column()"
  )

  if (is.null(columns)) {
    stop(
      "\n",
      call_stack,
      ": argument 'columns' cannot be NULL.",
      call. = FALSE
    )
  }

  if (!is.null(x_column)) {
    x_column <- as.character(x_column[1])
    if (x_column %in% columns) {
      x_column_clean <- janitor::make_clean_names(x_column)
      if (x_column_clean %in% spatialSpells::infra_x_column_names) {
        return(x_column)
      } else {
        if (!quiet) {
          message(
            "\n",
            call_stack,
            ": argument 'x_column' = '",
            x_column,
            "' has an unusual name (see 'spatialSpells::infra_x_column_names')"
          )
        }
        return(x_column)
      }
    } else {
      if (!quiet) {
        message(
          "\n",
          call_stack,
          ": argument 'x_column' with value '",
          x_column,
          "' is not in 'columns', trying to find the correct one."
        )
      }
    }
  }

  columns_clean <- janitor::make_clean_names(
    string = columns
  )

  names(columns_clean) <- columns

  x_column_index <- which(
    columns_clean %in% spatialSpells::infra_x_column_names
  )[1]

  if (is.na(x_column_index)) {
    stop(
      "\n",
      call_stack,
      ": cannot find a valid x column.",
      call. = FALSE
    )
  }

  x_column <- names(columns_clean)[x_column_index]

  x_column
}
