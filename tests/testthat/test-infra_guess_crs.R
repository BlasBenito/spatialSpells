# infra_guess_crs() -----------------------------------------------------------

test_that("infra_guess_crs: WGS84 values return EPSG 4326", {
  result <- suppressMessages(
    infra_guess_crs(x = c(-10, 0, 10), y = c(35, 40, 45))
  )
  expect_equal(result, 4326L)
})

test_that("infra_guess_crs: EPSG 4326 detected from coordinate ranges emits message", {
  expect_message(
    infra_guess_crs(x = c(-10, 0, 10), y = c(35, 40, 45)),
    regexp = "4326"
  )
})

test_that("infra_guess_crs: lat/lon column names confirm 4326 detection", {
  expect_message(
    result <- infra_guess_crs(
      x = c(1, 2, 3), y = c(40, 41, 42),
      x_name = "lon", y_name = "lat"
    ),
    regexp = "column names"
  )
  expect_equal(result, 4326L)
})

test_that("infra_guess_crs: Web Mercator scale values return EPSG 3857", {
  result <- suppressMessages(
    infra_guess_crs(x = c(-8e6, 0, 8e6), y = c(-5e6, 0, 5e6))
  )
  expect_equal(result, 3857L)
})

test_that("infra_guess_crs: EPSG 3857 detection emits message", {
  expect_message(
    infra_guess_crs(x = c(-8e6, 0, 8e6), y = c(-5e6, 0, 5e6)),
    regexp = "3857"
  )
})

test_that("infra_guess_crs: UTM-scale values return NA_integer_ with message", {
  result <- suppressMessages(
    infra_guess_crs(x = c(500000, 510000), y = c(4500000, 4510000))
  )
  expect_equal(result, NA_integer_)
})

test_that("infra_guess_crs: unknown CRS emits informative message", {
  expect_message(
    infra_guess_crs(x = c(500000, 510000), y = c(4500000, 4510000)),
    regexp = "could not be detected"
  )
})

test_that("infra_guess_crs: non-lat/lon column names do not trigger 'column names' message", {
  expect_message(
    result <- infra_guess_crs(
      x = c(-10, 0, 10), y = c(35, 40, 45),
      x_name = "easting", y_name = "northing"
    ),
    regexp = "4326"
  )
  expect_no_message(
    infra_guess_crs(
      x = c(-10, 0, 10), y = c(35, 40, 45),
      x_name = "easting", y_name = "northing"
    ),
    message = "column names"
  )
  expect_equal(result, 4326L)
})
