xy_valid <- cbind(x = c(-10.0, 0.0, 10.0), y = c(35.0, 40.0, 45.0))

# Structural checks -----------------------------------------------------------

test_that("cast_xy_to_xyz: NULL input stops with informative message", {
  expect_error(
    cast_xy_to_xyz(xy = NULL),
    regexp = "cannot be NULL"
  )
})

test_that("cast_xy_to_xyz: non-matrix/non-data.frame stops with informative message", {
  expect_error(
    cast_xy_to_xyz(xy = c(1, 2, 3)),
    regexp = "matrix or data frame"
  )
})

# Output structure ------------------------------------------------------------

test_that("cast_xy_to_xyz: valid lon/lat matrix returns 3-column matrix", {
  result <- cast_xy_to_xyz(xy = xy_valid)
  expect_true(is.matrix(result))
  expect_equal(ncol(result), 3L)
  expect_equal(nrow(result), nrow(xy_valid))
})

test_that("cast_xy_to_xyz: output column names are x, y, z", {
  result <- cast_xy_to_xyz(xy = xy_valid)
  expect_equal(colnames(result), c("x", "y", "z"))
})

# Unit sphere properties ------------------------------------------------------

test_that("cast_xy_to_xyz: all result points lie on unit sphere", {
  result <- cast_xy_to_xyz(xy = xy_valid)
  radii <- sqrt(result[, "x"]^2 + result[, "y"]^2 + result[, "z"]^2)
  expect_equal(radii, rep(1.0, nrow(result)), tolerance = 1e-12)
})

test_that("cast_xy_to_xyz: origin (lon=0, lat=0) maps to (1, 0, 0)", {
  result <- cast_xy_to_xyz(xy = cbind(x = 0, y = 0))
  expect_equal(result[1, "x"], 1.0, tolerance = 1e-12)
  expect_equal(result[1, "y"], 0.0, tolerance = 1e-12)
  expect_equal(result[1, "z"], 0.0, tolerance = 1e-12)
})

test_that("cast_xy_to_xyz: north pole (lat=90) maps to z=1", {
  result <- cast_xy_to_xyz(xy = cbind(x = 0, y = 90))
  expect_equal(result[1, "z"], 1.0, tolerance = 1e-12)
})

test_that("cast_xy_to_xyz: data.frame input is accepted", {
  df_xy <- as.data.frame(xy_valid)
  result <- cast_xy_to_xyz(xy = df_xy)
  expect_true(is.matrix(result))
  expect_equal(ncol(result), 3L)
})
