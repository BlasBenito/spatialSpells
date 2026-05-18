library(sf)
library(spatialData)
library(dplyr)

df <- spatialData::vi_smol |>
  dplyr::mutate(
    latitude = as.vector(sf::st_coordinates(geometry)[, "Y"]),
    longitude = as.vector(sf::st_coordinates(geometry)[, "X"])
  ) |>
  sf::st_drop_geometry()

columns <- colnames(df)

dots <- list()
dots$parent <- "parent_function()"

infra_get_x_column(columns = colnames(df))


#NULL x_column
infra_get_x_column(
  columns = c("latitude", "longitude"),
  x_column = NULL
)

#wrong x_column
infra_get_x_column(
  columns = c("latitude", "longitude"),
  x_column = "none"
)

#user's mistake
infra_get_x_column(
  columns = c("latitude", "longitude"),
  x_column = "latitude"
)
