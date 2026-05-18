# Test data -------------------------------------------------------------------

df_named  <- data.frame(geometry = c("POINT (1 2)", "POINT (3 4)"), species = "A")
df_mixed  <- data.frame(Geometry = c("POINT (1 2)", "POINT (3 4)"), species = "A")
df_level2 <- data.frame(foo      = c("POINT (1 2)", "POINT (3 4)"), species = "B")
df_multi  <- data.frame(foo      = c("MULTIPOLYGON (((0 0, 1 0, 1 1, 0 0)))"),
                        species  = "B")
df_bad    <- data.frame(a = 1:3, b = letters[1:3])
df_invalid_wkt <- data.frame(geometry = c("NOT WKT AT ALL", "STILL BAD"), species = "A")

# infra_get_wkt_column() -------------------------------------------------------

test_that("infra_get_wkt_column: NULL df stops with informative message", {
  expect_error(
    infra_get_wkt_column(df = NULL),
    regexp = "cannot be NULL"
  )
})

test_that("infra_get_wkt_column: no recognisable WKT column stops", {
  expect_error(
    infra_get_wkt_column(df = df_bad),
    regexp = "cannot find a valid WKT column"
  )
})

test_that("infra_get_wkt_column: level 1 auto-detects lowercase 'geometry'", {
  expect_equal(
    infra_get_wkt_column(df = df_named),
    "geometry"
  )
})

test_that("infra_get_wkt_column: level 1 auto-detects mixed-case 'Geometry'", {
  expect_equal(
    infra_get_wkt_column(df = df_mixed),
    "Geometry"
  )
})

test_that("infra_get_wkt_column: level 2 detects WKT by value grep (POINT)", {
  expect_equal(
    infra_get_wkt_column(df = df_level2),
    "foo"
  )
})

test_that("infra_get_wkt_column: level 2 detects WKT by value grep (MULTIPOLYGON)", {
  expect_equal(
    infra_get_wkt_column(df = df_multi),
    "foo"
  )
})

test_that("infra_get_wkt_column: user supplies known wkt_column, returns silently", {
  expect_no_message(
    result <- infra_get_wkt_column(df = df_named, wkt_column = "geometry")
  )
  expect_equal(result, "geometry")
})

test_that("infra_get_wkt_column: user supplies unusual-name column, returns it + message", {
  expect_message(
    result <- infra_get_wkt_column(
      df         = df_level2,
      wkt_column = "foo",
      quiet      = FALSE
    ),
    regexp = "unusual name"
  )
  expect_equal(result, "foo")
})

test_that("infra_get_wkt_column: quiet = TRUE suppresses unusual-name message", {
  expect_no_message(
    infra_get_wkt_column(
      df         = df_level2,
      wkt_column = "foo",
      quiet      = TRUE
    )
  )
})

test_that("infra_get_wkt_column: wkt_column not in names(df) falls back to auto-detect + message", {
  expect_message(
    result <- infra_get_wkt_column(
      df         = df_named,
      wkt_column = "nonexistent",
      quiet      = FALSE
    ),
    regexp = "not in"
  )
  expect_equal(result, "geometry")
})

test_that("infra_get_wkt_column: column with invalid WKT stops with informative error", {
  expect_error(
    infra_get_wkt_column(df = df_invalid_wkt),
    regexp = "could not parse"
  )
})
