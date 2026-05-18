#' Build call stack of function names for messages
#'
#' @description
#' Concatenates parent and child function names to improve message, warning, and error tracing.
#'
#' @param parent (optional, character) Name of the parent function. Default: NULL
#' @param child (optional, character) Name of the calling function. Default: NULL
#' @param ... (optional) Passes \code{parent} to functions that lack a \code{parent} parameter.
#'
#' @returns A character string with the formatted call stack, or \code{NULL} if both \code{parent} and \code{child} are \code{NULL}.
#' @export
#' @family infra
#' @examples
#' x <- infra_call_stack(
#'   parent = "parent_name",
#'   child = "child_function"
#' )
#'
#' message(x)
#' @autoglobal
infra_call_stack <- function(
  child = NULL,
  parent = NULL,
  ...
) {
  if (all(is.null(c(child, parent)))) {
    return(NULL)
  }

  if (is.null(parent) && !is.null(child)) {
    return(child)
  }

  hierarchy_symbol <- "\u2514\u2500\u2500 "
  spaces <- "    "

  spaces_multiplier <- length(
    regmatches(
      x = parent,
      m = gregexpr(
        pattern = hierarchy_symbol,
        text = parent
      )
    )[[1]]
  )

  spaces <- paste0(
    rep(x = "    ", times = spaces_multiplier),
    collapse = ""
  )

  parent <- paste0(
    parent,
    "\n",
    spaces,
    hierarchy_symbol,
    child
  )

  parent
}
