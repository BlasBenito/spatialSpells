df_valid <- data.frame(
  longitude = c(1.0, 2.0, 3.0),
  latitude  = c(4.0, 5.0, 6.0)
)

# Structural checks -------------------------------------------------------

test_that("infra_validate_df: NULL df stops with informative message", {
  expect_error(
    infra_validate_df(df = NULL),
    regexp = "cannot be NULL"
  )
})

test_that("infra_validate_df: non-data.frame stops with informative message", {
  expect_error(
    infra_validate_df(df = 1:3),
    regexp = "must be a data.frame"
  )
})

test_that("infra_validate_df: zero-row df stops with informative message", {
  expect_error(
    infra_validate_df(
      df = data.frame(longitude = numeric(0), latitude = numeric(0))
    ),
    regexp = "has no rows"
  )
})

# X/Y path ----------------------------------------------------------------

test_that("infra_validate_df: valid lon/lat df returns with validated attribute", {
  result <- infra_validate_df(df = df_valid)
  expect_true(attr(result, "validated"))
})

test_that("infra_validate_df: character x column stops with column name and type", {
  df_char_x <- data.frame(
    longitude = c("a", "b", "c"),
    latitude  = c(1.0, 2.0, 3.0),
    stringsAsFactors = FALSE
  )
  expect_error(infra_validate_df(df = df_char_x), regexp = "longitude")
  expect_error(infra_validate_df(df = df_char_x), regexp = "character")
})

test_that("infra_validate_df: NA in y column stops with NA count", {
  df_na_y <- data.frame(
    longitude = c(1.0, 2.0, 3.0),
    latitude  = c(4.0, NA, 6.0)
  )
  expect_error(infra_validate_df(df = df_na_y), regexp = "1")
})

test_that("infra_validate_df: Inf in x column stops with count", {
  df_inf_x <- data.frame(
    longitude = c(1.0, Inf, 3.0),
    latitude  = c(4.0, 5.0, 6.0)
  )
  expect_error(infra_validate_df(df = df_inf_x), regexp = "infinite")
})

test_that("infra_validate_df: all identical coordinates emits warning, returns df", {
  df_identical <- data.frame(
    longitude = c(1.0, 1.0, 1.0),
    latitude  = c(2.0, 2.0, 2.0)
  )
  expect_warning(
    result <- infra_validate_df(df = df_identical),
    regexp = "identical"
  )
  expect_true(attr(result, "validated"))
})

# WKT path ----------------------------------------------------------------

test_that("infra_validate_df: WKT column named 'geometry' returns validated df", {
  df_wkt <- data.frame(
    geometry = c("POINT(1 2)", "POINT(3 4)", "POINT(5 6)"),
    species  = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  result <- infra_validate_df(df = df_wkt, quiet = TRUE)
  expect_true(attr(result, "validated"))
})

test_that("infra_validate_df: WKT detected by content returns validated df", {
  df_wkt_content <- data.frame(
    geo_col = c("POINT(1 2)", "POINT(3 4)", "POINT(5 6)"),
    stringsAsFactors = FALSE
  )
  result <- infra_validate_df(df = df_wkt_content, quiet = TRUE)
  expect_true(attr(result, "validated"))
})

test_that("infra_validate_df: unparseable WKT stops with column name in message", {
  df_bad_wkt <- data.frame(
    geometry = c("POINT(1 2)", "not_valid_wkt"),
    stringsAsFactors = FALSE
  )
  expect_error(
    infra_validate_df(df = df_bad_wkt),
    regexp = "geometry"
  )
})

test_that("infra_validate_df: geometrically invalid WKT emits message, not error", {
  # bowtie polygon: valid WKT syntax but geometrically invalid (self-intersecting)
  df_invalid_geom <- data.frame(
    geometry = "POLYGON((0 0, 1 1, 0 1, 1 0, 0 0))",
    stringsAsFactors = FALSE
  )
  expect_no_error(
    expect_message(
      result <- infra_validate_df(df = df_invalid_geom, quiet = FALSE),
      regexp = "invalid"
    )
  )
})

test_that("infra_validate_df: no x/y and no WKT stops with both mentioned", {
  df_no_coords <- data.frame(a = 1:3, b = 4:6)
  expect_error(
    infra_validate_df(df = df_no_coords),
    regexp = "no recognisable x/y"
  )
})

test_that("infra_validate_df: quiet = TRUE suppresses all messages", {
  df_invalid_geom <- data.frame(
    geometry = "POLYGON((0 0, 1 1, 0 1, 1 0, 0 0))",
    stringsAsFactors = FALSE
  )
  expect_no_message(
    infra_validate_df(df = df_invalid_geom, quiet = TRUE)
  )
})

test_that("infra_validate_df: wkt_column parameter is passed through", {
  df <- data.frame(
    my_geom  = c("POINT(1 2)", "POINT(3 4)"),
    stringsAsFactors = FALSE
  )
  result <- infra_validate_df(df = df, wkt_column = "my_geom", quiet = TRUE)
  expect_true(attr(result, "validated"))
})
