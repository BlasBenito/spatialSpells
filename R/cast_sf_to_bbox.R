#' Convert sf Object to Bounding Box sf Object
#' @description Extracts the bounding box from an sf object and returns it as a one-row sf data frame containing a rectangular polygon.
#' @param df (required, sf) An sf data frame. Default: `NULL`
#' @param ... (optional) For internal arguments only.
#' @return One-row sf data frame containing a rectangular polygon representing the bounding box of the input. Has the same CRS as input.
#' @details
#' This function:
#' \enumerate{
#'   \item Extracts the bounding box from the input sf object using `sf::st_bbox()`
#'   \item Converts the bbox to a rectangular polygon using `sf::st_as_sfc()`
#'   \item Returns as a one-row sf dataframe with the same CRS as input
#' }
#'
#' **Use cases:**
#' - Calculate bounding box area for default parameter calculation
#' - Visualize spatial extent of point data
#' - Quality control and validation of spatial data
#'
#' @examples
#' pts <- data.frame(x = c(-10, 0, 10), y = c(35, 40, 45))
#' pts_sf <- sf::st_as_sf(pts, coords = c("x", "y"), crs = 4326)
#'
#' bbox_sf <- cast_sf_to_bbox(pts_sf)
#' nrow(bbox_sf)  # 1
#'
#' sf::st_area(bbox_sf)
#' @family casting_functions
#' @autoglobal
#' @export
cast_sf_to_bbox <- function(
  df = NULL,
  ...
) {
  dots <- list(...)
  call_stack <- infra_call_stack(
    parent = dots$parent,
    child = "spatialSpells::cast_sf_to_bbox()"
  )

  # Validate input
  if (is.null(df)) {
    stop(
      "\n",
      call_stack,
      ": argument 'df' cannot be NULL.",
      call. = FALSE
    )
  }

  if (!inherits(df, "sf")) {
    stop(
      "\n",
      call_stack,
      ": argument 'df' must be an sf object.",
      call. = FALSE
    )
  }

  if (nrow(df) == 0) {
    stop(
      "\n",
      call_stack,
      ": argument 'df' has no rows.",
      call. = FALSE
    )
  }

  # Extract bounding box
  df_bbox <- df |>
    sf::st_bbox() |>
    sf::st_as_sfc() |>
    sf::st_sf()

  df_bbox
}
