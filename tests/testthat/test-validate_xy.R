xy_valid <- cbind(x = c(-10.0, 0.0, 10.0), y = c(35.0, 40.0, 45.0))

# Structural checks -----------------------------------------------------------

test_that("validate_xy: NULL stops with informative message", {
  expect_error(
    validate_xy(xy = NULL),
    regexp = "cannot be NULL"
  )
})

test_that("validate_xy: vector (non-matrix, non-data.frame) stops with type message", {
  expect_error(
    validate_xy(xy = c(1, 2, 3)),
    regexp = "matrix or data frame"
  )
})

test_that("validate_xy: 1-column matrix stops with column count message", {
  expect_error(
    validate_xy(xy = matrix(1:3, ncol = 1)),
    regexp = "at least 2 columns"
  )
})

test_that("validate_xy: zero-row matrix stops with row count message", {
  expect_error(
    validate_xy(xy = matrix(numeric(0), nrow = 0, ncol = 2)),
    regexp = "has no rows"
  )
})

# Passing cases ---------------------------------------------------------------

test_that("validate_xy: valid 2-column matrix returns unchanged", {
  result <- validate_xy(xy = xy_valid)
  expect_identical(result, xy_valid)
})

test_that("validate_xy: valid 2-column data.frame returns unchanged", {
  df_xy <- as.data.frame(xy_valid)
  result <- validate_xy(xy = df_xy)
  expect_identical(result, df_xy)
})

test_that("validate_xy: matrix with more than 2 columns is accepted", {
  xyz <- cbind(xy_valid, z = rep(0, nrow(xy_valid)))
  result <- validate_xy(xy = xyz)
  expect_identical(result, xyz)
})
