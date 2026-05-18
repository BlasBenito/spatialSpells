sf_point <- suppressMessages(
  cast_df_to_sf(
    df = data.frame(
      longitude = c(-10.0, 0.0, 10.0),
      latitude  = c(35.0, 40.0, 45.0)
    )
  )
)

sf_polygon <- sf::st_sf(
  geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    sf::st_polygon(list(rbind(c(2, 2), c(3, 2), c(3, 3), c(2, 3), c(2, 2))))
  ),
  crs = 4326
)

sf_linestring <- sf::st_sf(
  geometry = sf::st_sfc(
    sf::st_linestring(rbind(c(0, 0), c(1, 1), c(2, 0))),
    crs = 4326
  )
)

# Structural checks -----------------------------------------------------------

test_that("cast_df_to_xy: NULL stops with informative message", {
  expect_error(
    cast_df_to_xy(df = NULL),
    regexp = "cannot be NULL"
  )
})

test_that("cast_df_to_xy: non-sf input stops with informative message", {
  expect_error(
    cast_df_to_xy(df = data.frame(x = 1:3, y = 4:6)),
    regexp = "must be an sf"
  )
})

test_that("cast_df_to_xy: LINESTRING geometry stops with unsupported message", {
  expect_error(
    cast_df_to_xy(df = sf_linestring),
    regexp = "LINESTRING"
  )
})

# Output checks ---------------------------------------------------------------

test_that("cast_df_to_xy: POINT sf returns 2-column matrix", {
  result <- cast_df_to_xy(df = sf_point)
  expect_true(is.matrix(result))
  expect_equal(ncol(result), 2L)
  expect_equal(nrow(result), nrow(sf_point))
})

test_that("cast_df_to_xy: output column names are exactly 'x' and 'y'", {
  result <- cast_df_to_xy(df = sf_point)
  expect_equal(colnames(result), c("x", "y"))
})

test_that("cast_df_to_xy: POLYGON sf returns centroids as 2-column matrix", {
  result <- suppressWarnings(cast_df_to_xy(df = sf_polygon))
  expect_true(is.matrix(result))
  expect_equal(ncol(result), 2L)
  expect_equal(nrow(result), nrow(sf_polygon))
})

test_that("cast_df_to_xy: MULTIPOINT sf extracts all points", {
  sf_mp <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_multipoint(rbind(c(-10, 35), c(0, 40), c(10, 45))),
      crs = 4326
    )
  )
  result <- cast_df_to_xy(df = sf_mp)
  expect_true(is.matrix(result))
  expect_equal(nrow(result), 3L)
})

# Integration roundtrip -------------------------------------------------------

test_that("integration: df -> cast_df_to_sf -> cast_sf_to_bbox returns one-row sf polygon", {
  df <- data.frame(
    longitude = c(-10.0, 0.0, 10.0),
    latitude  = c(35.0, 40.0, 45.0)
  )
  sf_obj  <- suppressMessages(cast_df_to_sf(df = df))
  bbox_sf <- cast_sf_to_bbox(df = sf_obj)
  expect_s3_class(bbox_sf, "sf")
  expect_equal(nrow(bbox_sf), 1L)
  geom_type <- as.character(sf::st_geometry_type(bbox_sf, by_geometry = FALSE))
  expect_true(geom_type %in% c("POLYGON", "MULTIPOLYGON"))
})

test_that("integration: df -> cast_df_to_sf -> cast_df_to_xy -> cast_xy_to_xyz returns unit-sphere matrix", {
  df <- data.frame(
    longitude = c(-10.0, 0.0, 10.0),
    latitude  = c(35.0, 40.0, 45.0)
  )
  sf_obj <- suppressMessages(cast_df_to_sf(df = df))
  xy     <- cast_df_to_xy(df = sf_obj)
  xyz    <- cast_xy_to_xyz(xy = xy)
  expect_true(is.matrix(xyz))
  expect_equal(ncol(xyz), 3L)
  radii <- sqrt(xyz[, "x"]^2 + xyz[, "y"]^2 + xyz[, "z"]^2)
  expect_equal(radii, rep(1.0, nrow(xyz)), tolerance = 1e-12)
})
