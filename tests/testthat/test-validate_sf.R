df_valid <- data.frame(
  longitude = c(-10.0, 0.0, 10.0),
  latitude  = c(35.0, 40.0, 45.0)
)

sf_valid <- suppressMessages(cast_df_to_sf(df = df_valid))

# Structural checks -----------------------------------------------------------

test_that("validate_sf: NULL stops with informative message", {
  expect_error(
    validate_sf(df = NULL),
    regexp = "cannot be NULL"
  )
})

test_that("validate_sf: zero-row data frame stops with informative message", {
  expect_error(
    validate_sf(
      df = data.frame(longitude = numeric(0), latitude = numeric(0))
    ),
    regexp = "has no rows"
  )
})

# Output checks ---------------------------------------------------------------

test_that("validate_sf: valid sf returns sf with validated attribute TRUE", {
  result <- validate_sf(df = sf_valid)
  expect_s3_class(result, "sf")
  expect_true(attr(result, "validated"))
})

test_that("validate_sf: valid data.frame is coerced to sf with validated attribute", {
  result <- suppressMessages(validate_sf(df = df_valid))
  expect_s3_class(result, "sf")
  expect_true(attr(result, "validated"))
})

test_that("validate_sf: data.frame with identical coordinates stops", {
  df_identical <- data.frame(
    longitude = c(1.0, 1.0, 1.0),
    latitude  = c(2.0, 2.0, 2.0)
  )
  expect_error(
    suppressMessages(validate_sf(df = df_identical)),
    regexp = "same location"
  )
})
