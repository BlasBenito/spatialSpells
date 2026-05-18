#' Validate xy Matrix Input
#'
#' @description
#' Validates xy matrix or data.frame input. Consolidates validation logic used
#' by functions that accept xy coordinate matrices.
#'
#' @param xy (required, matrix or data.frame) Coordinate data with at least two
#'   columns (x/longitude and y/latitude). Default: NULL
#' @param ... (optional) For internal arguments only.
#'
#' @return The validated xy object unchanged.
#'
#' @details
#' Validation steps performed in order:
#' \enumerate{
#'   \item NULL check
#'   \item Type check (matrix or data.frame)
#'   \item Column count check (at least 2 columns)
#'   \item Zero rows check
#' }
#'
#' @family arg_validation
#' @autoglobal
#' @export
validate_xy <- function(
  xy = NULL,
  ...
) {
  dots <- list(...)

  call_stack <- infra_call_stack(
    parent = dots$parent,
    child = "spatialSpells::validate_xy()"
  )

  if (is.null(xy)) {
    stop(
      "\n",
      call_stack,
      ": argument 'xy' cannot be NULL.",
      call. = FALSE
    )
  }

  if (!is.matrix(xy) && !is.data.frame(xy)) {
    stop(
      "\n",
      call_stack,
      ": argument 'xy' must be a matrix or data frame.",
      call. = FALSE
    )
  }

  if (ncol(xy) < 2) {
    stop(
      "\n",
      call_stack,
      ": argument 'xy' must have at least 2 columns.",
      call. = FALSE
    )
  }

  n_rows <- nrow(xy)
  if (is.null(n_rows) || n_rows == 0) {
    stop(
      "\n",
      call_stack,
      ": argument 'xy' has no rows.",
      call. = FALSE
    )
  }

  xy
}
