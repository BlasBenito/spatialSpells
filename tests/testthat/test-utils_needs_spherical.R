xy_europe  <- cbind(x = c(-5.0, 0.0, 5.0),   y = c(45.0, 50.0, 55.0))
xy_dateline <- cbind(x = c(155.0, 160.0, 165.0), y = c(-20.0, 0.0, 20.0))
xy_poles    <- cbind(x = c(-10.0, 0.0, 10.0),  y = c(72.0, 75.0, 80.0))

# Structural checks -----------------------------------------------------------

test_that("utils_needs_spherical: NULL stops via validate_xy", {
  expect_error(
    utils_needs_spherical(xy = NULL),
    regexp = "cannot be NULL"
  )
})

# Return value ----------------------------------------------------------------

test_that("utils_needs_spherical: mid-latitude European data returns FALSE", {
  expect_false(utils_needs_spherical(xy = xy_europe))
})

test_that("utils_needs_spherical: data spanning past dateline threshold returns TRUE", {
  expect_true(utils_needs_spherical(xy = xy_dateline))
})

test_that("utils_needs_spherical: data near poles returns TRUE", {
  expect_true(utils_needs_spherical(xy = xy_poles))
})

# Threshold edge cases --------------------------------------------------------

test_that("utils_needs_spherical: lon exactly at threshold (150) returns FALSE", {
  xy_edge <- cbind(x = c(-150.0, 0.0, 150.0), y = c(0.0, 0.0, 0.0))
  expect_false(utils_needs_spherical(xy = xy_edge))
})

test_that("utils_needs_spherical: lon just above threshold (150+eps) returns TRUE", {
  xy_over <- cbind(x = c(0.0, 150.001), y = c(0.0, 0.0))
  expect_true(utils_needs_spherical(xy = xy_over))
})

test_that("utils_needs_spherical: lat exactly at threshold (70) returns FALSE", {
  xy_edge <- cbind(x = c(0.0, 10.0), y = c(70.0, 70.0))
  expect_false(utils_needs_spherical(xy = xy_edge))
})

test_that("utils_needs_spherical: lat just above threshold (70+eps) returns TRUE", {
  xy_over <- cbind(x = c(0.0, 10.0), y = c(70.001, 70.001))
  expect_true(utils_needs_spherical(xy = xy_over))
})
