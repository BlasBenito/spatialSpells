#' Get names of the x and y coordinate columns in a data frame
#'
#' @description Calls [infra_get_x_column()] and [infra_get_y_column()] and
#'   returns both results as a named character vector.
#'
#' @param columns (required, character vector) Column names of the target data
#'   frame. Default: NULL
#' @param x_column (optional, character string) Name of the x / longitude
#'   column. Passed directly to [infra_get_x_column()]. Default: NULL
#' @param y_column (optional, character string) Name of the y / latitude
#'   column. Passed directly to [infra_get_y_column()]. Default: NULL
#' @param quiet (optional, logical) If TRUE, all messages are silenced.
#'   Default: FALSE
#' @param ... Additional arguments passed to `infra_call_stack()` (e.g.
#'   `parent`).
#' @returns Named character vector with elements `"x_column"` and `"y_column"`.
#' @family infra
#' @export
#' @autoglobal
#' @examples
#' infra_get_xy_columns(columns = c("longitude", "latitude", "species"))
#' infra_get_xy_columns(
#'   columns = c("longitude", "latitude", "species"),
#'   x_column = "longitude",
#'   y_column = "latitude"
#' )
infra_get_xy_columns <- function(
  columns = NULL,
  x_column = NULL,
  y_column = NULL,
  quiet = FALSE,
  ...
) {
  dots <- list(...)

  call_stack <- infra_call_stack(
    parent = dots$parent,
    child = "spatialSpells::infra_get_xy_columns()"
  )

  x <- infra_get_x_column(
    columns  = columns,
    x_column = x_column,
    quiet    = quiet,
    parent   = call_stack
  )

  y <- infra_get_y_column(
    columns  = columns,
    y_column = y_column,
    quiet    = quiet,
    parent   = call_stack
  )

  if (x == y) {
    stop(
      "\n",
      call_stack,
      ": x and y coordinate columns resolved to the same column ('",
      x,
      "'). Provide distinct 'x_column' and 'y_column' arguments.",
      call. = FALSE
    )
  }

  c(x_column = x, y_column = y)
}
