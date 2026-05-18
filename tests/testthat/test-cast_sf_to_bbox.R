sf_4326 <- suppressMessages(
  cast_df_to_sf(
    df = data.frame(
      longitude = c(-10.0, 0.0, 10.0),
      latitude  = c(35.0, 40.0, 45.0)
    )
  )
)

# Structural checks -----------------------------------------------------------

test_that("cast_sf_to_bbox: NULL input stops with informative message", {
  expect_error(
    cast_sf_to_bbox(df = NULL),
    regexp = "cannot be NULL"
  )
})

test_that("cast_sf_to_bbox: non-sf input stops with informative message", {
  expect_error(
    cast_sf_to_bbox(
      df = data.frame(longitude = c(1, 2), latitude = c(3, 4))
    ),
    regexp = "must be an sf"
  )
})

test_that("cast_sf_to_bbox: zero-row sf stops with informative message", {
  empty_sf <- sf_4326[0, ]
  expect_error(
    cast_sf_to_bbox(df = empty_sf),
    regexp = "has no rows"
  )
})

# Output checks ---------------------------------------------------------------

test_that("cast_sf_to_bbox: valid sf returns one-row sf", {
  result <- cast_sf_to_bbox(df = sf_4326)
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1L)
})

test_that("cast_sf_to_bbox: result geometry is a polygon", {
  result <- cast_sf_to_bbox(df = sf_4326)
  geom_type <- as.character(sf::st_geometry_type(result, by_geometry = FALSE))
  expect_true(geom_type %in% c("POLYGON", "MULTIPOLYGON"))
})

test_that("cast_sf_to_bbox: CRS is preserved from input", {
  result <- cast_sf_to_bbox(df = sf_4326)
  expect_equal(sf::st_crs(result), sf::st_crs(sf_4326))
})
