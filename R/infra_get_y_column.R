#' Guess name of the latitude column in a data frame
#'
#' @description Returns the name of the latitude column in a data frame. If
#'   `y_column` is not provided, the function attempts to auto-detect it by
#'   matching cleaned column names against `spatialSpells::infra_y_column_names`.
#'   When `y_column` is provided but not found in `columns`, the function falls
#'   back to auto-detection and emits a message.
#'
#' @param columns (required, character vector) Column names of the target data
#'   frame. Default: NULL
#' @param y_column (optional, character string) Name of the column representing
#'   the y coordinate or latitude. If more than one element is provided, only
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
#' infra_get_y_column(columns = c("longitude", "latitude", "species"))
#' infra_get_y_column(
#'   columns = c("longitude", "latitude", "species"),
#'   y_column = "latitude"
#' )
infra_get_y_column <- function(
  columns = NULL,
  y_column = NULL,
  quiet = FALSE,
  ...
) {
  dots <- list(...)

  call_stack <- infra_call_stack(
    parent = dots$parent,
    child = "spatialSpells::infra_get_y_column()"
  )

  if (is.null(columns)) {
    stop(
      "\n",
      call_stack,
      ": argument 'columns' cannot be NULL.",
      call. = FALSE
    )
  }

  if (!is.null(y_column)) {
    y_column <- as.character(y_column[1])
    if (y_column %in% columns) {
      y_column_clean <- janitor::make_clean_names(y_column)
      if (y_column_clean %in% spatialSpells::infra_y_column_names) {
        return(y_column)
      } else {
        if (!quiet) {
          message(
            "\n",
            call_stack,
            ": argument 'y_column' = '",
            y_column,
            "' has an unusual name (see 'spatialSpells::infra_y_column_names')"
          )
        }
        return(y_column)
      }
    } else {
      if (!quiet) {
        message(
          "\n",
          call_stack,
          ": argument 'y_column' with value '",
          y_column,
          "' is not in 'columns', trying to find the correct one."
        )
      }
    }
  }

  columns_clean <- janitor::make_clean_names(
    string = columns
  )

  names(columns_clean) <- columns

  y_column_index <- which(
    columns_clean %in% spatialSpells::infra_y_column_names
  )[1]

  if (is.na(y_column_index)) {
    stop(
      "\n",
      call_stack,
      ": cannot find a valid y column.",
      call. = FALSE
    )
  }

  y_column <- names(columns_clean)[y_column_index]

  y_column
}
