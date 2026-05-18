good_cols  <- c("longitude", "latitude", "species")
mixed_cols <- c("Longitude", "Latitude", "species")
bad_cols   <- c("a", "b", "c")

# infra_get_x_column() ----------------------------------------------------------

test_that("infra_get_x_column: NULL columns stops with informative message", {
  expect_error(
    infra_get_x_column(columns = NULL),
    regexp = "cannot be NULL"
  )
})

test_that("infra_get_x_column: no recognisable x column stops", {
  expect_error(
    infra_get_x_column(columns = bad_cols),
    regexp = "cannot find a valid x column"
  )
})

test_that("infra_get_x_column: auto-detects lowercase 'longitude'", {
  expect_equal(
    infra_get_x_column(columns = good_cols),
    "longitude"
  )
})

test_that("infra_get_x_column: auto-detects mixed-case 'Longitude'", {
  expect_equal(
    infra_get_x_column(columns = mixed_cols),
    "Longitude"
  )
})

test_that("infra_get_x_column: user supplies known x_column, returns it silently", {
  expect_equal(
    infra_get_x_column(columns = good_cols, x_column = "longitude"),
    "longitude"
  )
})

test_that("infra_get_x_column: user supplies unusual-name column, returns it + message", {
  expect_message(
    result <- infra_get_x_column(
      columns = c("my_x", "latitude", "species"),
      x_column = "my_x",
      quiet = FALSE
    ),
    regexp = "unusual name"
  )
  expect_equal(result, "my_x")
})

test_that("infra_get_x_column: quiet = TRUE suppresses unusual-name message", {
  expect_no_message(
    infra_get_x_column(
      columns = c("my_x", "latitude", "species"),
      x_column = "my_x",
      quiet = TRUE
    )
  )
})

test_that("infra_get_x_column: x_column not in columns, falls back to auto-detection", {
  expect_message(
    result <- infra_get_x_column(
      columns = good_cols,
      x_column = "nonexistent",
      quiet = FALSE
    ),
    regexp = "not in 'columns'"
  )
  expect_equal(result, "longitude")
})

test_that("infra_get_x_column: mixed-case x_column does not trigger unusual-name message (Bug 2 regression)", {
  expect_no_message(
    infra_get_x_column(
      columns = mixed_cols,
      x_column = "Longitude",
      quiet = FALSE
    )
  )
})

# infra_get_y_column() ----------------------------------------------------------

test_that("infra_get_y_column: NULL columns stops with informative message", {
  expect_error(
    infra_get_y_column(columns = NULL),
    regexp = "cannot be NULL"
  )
})

test_that("infra_get_y_column: no recognisable y column stops", {
  expect_error(
    infra_get_y_column(columns = bad_cols),
    regexp = "cannot find a valid y column"
  )
})

test_that("infra_get_y_column: auto-detects lowercase 'latitude'", {
  expect_equal(
    infra_get_y_column(columns = good_cols),
    "latitude"
  )
})

test_that("infra_get_y_column: auto-detects mixed-case 'Latitude'", {
  expect_equal(
    infra_get_y_column(columns = mixed_cols),
    "Latitude"
  )
})

test_that("infra_get_y_column: user supplies known y_column, returns it silently", {
  expect_equal(
    infra_get_y_column(columns = good_cols, y_column = "latitude"),
    "latitude"
  )
})

test_that("infra_get_y_column: user supplies unusual-name column, returns it + message", {
  expect_message(
    result <- infra_get_y_column(
      columns = c("longitude", "my_y", "species"),
      y_column = "my_y",
      quiet = FALSE
    ),
    regexp = "unusual name"
  )
  expect_equal(result, "my_y")
})

test_that("infra_get_y_column: quiet = TRUE suppresses unusual-name message", {
  expect_no_message(
    infra_get_y_column(
      columns = c("longitude", "my_y", "species"),
      y_column = "my_y",
      quiet = TRUE
    )
  )
})

test_that("infra_get_y_column: y_column not in columns, falls back to auto-detection", {
  expect_message(
    result <- infra_get_y_column(
      columns = good_cols,
      y_column = "nonexistent",
      quiet = FALSE
    ),
    regexp = "not in 'columns'"
  )
  expect_equal(result, "latitude")
})

test_that("infra_get_y_column: mixed-case y_column does not trigger unusual-name message (Bug 2 regression)", {
  expect_no_message(
    infra_get_y_column(
      columns = mixed_cols,
      y_column = "Latitude",
      quiet = FALSE
    )
  )
})

# infra_get_xy_columns() --------------------------------------------------------

test_that("infra_get_xy_columns: auto-detects both columns", {
  expect_equal(
    infra_get_xy_columns(columns = good_cols),
    c(x_column = "longitude", y_column = "latitude")
  )
})

test_that("infra_get_xy_columns: works with mixed-case columns", {
  expect_equal(
    infra_get_xy_columns(columns = mixed_cols),
    c(x_column = "Longitude", y_column = "Latitude")
  )
})

test_that("infra_get_xy_columns: user supplies both columns explicitly, returns silently", {
  expect_no_message(
    result <- infra_get_xy_columns(
      columns  = good_cols,
      x_column = "longitude",
      y_column = "latitude"
    )
  )
  expect_equal(result, c(x_column = "longitude", y_column = "latitude"))
})

test_that("infra_get_xy_columns: unusual x and y names emit two messages", {
  cols <- c("my_x", "my_y", "species")
  expect_message(
    expect_message(
      result <- infra_get_xy_columns(
        columns  = cols,
        x_column = "my_x",
        y_column = "my_y",
        quiet    = FALSE
      ),
      regexp = "unusual name"
    ),
    regexp = "unusual name"
  )
  expect_equal(result, c(x_column = "my_x", y_column = "my_y"))
})

test_that("infra_get_xy_columns: quiet = TRUE suppresses both messages", {
  cols <- c("my_x", "my_y", "species")
  expect_no_message(
    infra_get_xy_columns(
      columns  = cols,
      x_column = "my_x",
      y_column = "my_y",
      quiet    = TRUE
    )
  )
})

test_that("infra_get_xy_columns: columns = NULL stops with error", {
  expect_error(
    infra_get_xy_columns(columns = NULL),
    regexp = "cannot be NULL"
  )
})

test_that("infra_get_xy_columns: no recognisable x column stops", {
  expect_error(
    infra_get_xy_columns(columns = bad_cols),
    regexp = "cannot find a valid x column"
  )
})

test_that("infra_get_xy_columns: no recognisable y column stops", {
  expect_error(
    infra_get_xy_columns(columns = c("longitude", "a", "b")),
    regexp = "cannot find a valid y column"
  )
})
