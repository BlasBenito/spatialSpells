df_4326 <- data.frame(
  longitude = c(-10.0, 0.0, 10.0),
  latitude  = c(35.0, 40.0, 45.0)
)

df_3857 <- data.frame(
  x = c(-8e6, 0, 8e6),
  y = c(-5e6, 0, 5e6)
)

# Structural checks -----------------------------------------------------------

test_that("cast_df_to_sf: NULL df stops with informative message", {
  expect_error(
    cast_df_to_sf(df = NULL),
    regexp = "must be a data.frame"
  )
})

test_that("cast_df_to_sf: non-data.frame stops with informative message", {
  expect_error(
    cast_df_to_sf(df = 1:5),
    regexp = "must be a data.frame"
  )
})

test_that("cast_df_to_sf: zero-row df stops with informative message", {
  expect_error(
    cast_df_to_sf(
      df = data.frame(longitude = numeric(0), latitude = numeric(0))
    ),
    regexp = "has no rows"
  )
})

# Coordinate detection and conversion -----------------------------------------

test_that("cast_df_to_sf: valid lon/lat df with crs = NULL auto-detects EPSG 4326", {
  result <- suppressMessages(cast_df_to_sf(df = df_4326))
  expect_s3_class(result, "sf")
  expect_equal(sf::st_crs(result)$epsg, 4326L)
  expect_equal(nrow(result), 3L)
})

test_that("cast_df_to_sf: explicit crs = 4326 produces sf with correct CRS", {
  result <- cast_df_to_sf(df = df_4326, crs = 4326)
  expect_s3_class(result, "sf")
  expect_equal(sf::st_crs(result)$epsg, 4326L)
})

test_that("cast_df_to_sf: geometry type is POINT", {
  result <- suppressMessages(cast_df_to_sf(df = df_4326))
  expect_equal(
    as.character(sf::st_geometry_type(result, by_geometry = FALSE)),
    "POINT"
  )
})

test_that("cast_df_to_sf: non-numeric x column stops with column name and type", {
  df_char_x <- data.frame(
    longitude = c("a", "b", "c"),
    latitude  = c(35.0, 40.0, 45.0),
    stringsAsFactors = FALSE
  )
  expect_error(cast_df_to_sf(df = df_char_x), regexp = "longitude")
  expect_error(cast_df_to_sf(df = df_char_x), regexp = "character")
})

test_that("cast_df_to_sf: NA in coordinates stops with NA count", {
  df_na <- data.frame(
    longitude = c(-10.0, NA, 10.0),
    latitude  = c(35.0, 40.0, 45.0)
  )
  expect_error(cast_df_to_sf(df = df_na), regexp = "NA")
})

test_that("cast_df_to_sf: Inf in coordinates stops with Inf count", {
  df_inf <- data.frame(
    longitude = c(-10.0, Inf, 10.0),
    latitude  = c(35.0, 40.0, 45.0)
  )
  expect_error(cast_df_to_sf(df = df_inf), regexp = "infinite")
})

test_that("cast_df_to_sf: already-sf input is returned unchanged", {
  sf_input <- suppressMessages(cast_df_to_sf(df = df_4326))
  result <- cast_df_to_sf(df = sf_input)
  expect_identical(result, sf_input)
})

test_that("cast_df_to_sf: Web Mercator scale values auto-detected as EPSG 3857", {
  result <- suppressMessages(cast_df_to_sf(df = df_3857))
  expect_s3_class(result, "sf")
  expect_equal(sf::st_crs(result)$epsg, 3857L)
})

test_that("cast_df_to_sf: x_column and y_column overrides work", {
  df <- data.frame(
    my_x = c(-10.0, 0.0, 10.0),
    my_y = c(35.0, 40.0, 45.0)
  )
  result <- cast_df_to_sf(
    df       = df,
    x_column = "my_x",
    y_column = "my_y",
    crs      = 4326,
    quiet    = TRUE
  )
  expect_s3_class(result, "sf")
  expect_equal(sf::st_crs(result)$epsg, 4326L)
})

# WKT path -------------------------------------------------------------------

test_that("cast_df_to_sf: WKT column named 'geometry' returns sf", {
  df <- data.frame(
    geometry = c("POINT(1 2)", "POINT(3 4)"),
    species  = c("a", "b"),
    stringsAsFactors = FALSE
  )
  result <- cast_df_to_sf(df = df, crs = 4326, quiet = TRUE)
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2L)
})

test_that("cast_df_to_sf: WKT column detected by content returns sf", {
  df <- data.frame(
    geo_col = c("POINT(1 2)", "POINT(3 4)"),
    stringsAsFactors = FALSE
  )
  result <- cast_df_to_sf(df = df, crs = 4326, quiet = TRUE)
  expect_s3_class(result, "sf")
})

test_that("cast_df_to_sf: wkt_column parameter override returns sf", {
  df <- data.frame(
    my_geom = c("POINT(1 2)", "POINT(3 4)"),
    stringsAsFactors = FALSE
  )
  result <- cast_df_to_sf(df = df, wkt_column = "my_geom", crs = 4326, quiet = TRUE)
  expect_s3_class(result, "sf")
})

test_that("cast_df_to_sf: WKT path with explicit crs sets CRS", {
  df <- data.frame(
    geometry = c("POINT(1 2)", "POINT(3 4)"),
    stringsAsFactors = FALSE
  )
  result <- cast_df_to_sf(df = df, crs = 4326, quiet = TRUE)
  expect_equal(sf::st_crs(result)$epsg, 4326L)
})

test_that("cast_df_to_sf: WKT path with crs = NULL produces sf with no CRS", {
  df <- data.frame(
    geometry = c("POINT(1 2)", "POINT(3 4)"),
    stringsAsFactors = FALSE
  )
  result <- suppressMessages(cast_df_to_sf(df = df))
  expect_s3_class(result, "sf")
  expect_true(is.na(sf::st_crs(result)))
})
