# infra_call_stack() ----------------------------------------------------------

test_that("infra_call_stack: NULL parent and NULL child returns NULL", {
  expect_null(infra_call_stack(parent = NULL, child = NULL))
})

test_that("infra_call_stack: child only returns child string unchanged", {
  result <- infra_call_stack(child = "spatialSpells::my_fn()")
  expect_equal(result, "spatialSpells::my_fn()")
})


test_that("infra_call_stack: parent + child produces hierarchy string", {
  result <- infra_call_stack(
    parent = "spatialSpells::parent_fn()",
    child  = "spatialSpells::child_fn()"
  )
  expect_true(grepl("spatialSpells::parent_fn()", result, fixed = TRUE))
  expect_true(grepl("spatialSpells::child_fn()", result, fixed = TRUE))
  expect_true(grepl("└──", result))
})

test_that("infra_call_stack: nested call increases indentation", {
  level1 <- infra_call_stack(
    parent = NULL,
    child  = "spatialSpells::fn1()"
  )
  level2 <- infra_call_stack(
    parent = level1,
    child  = "spatialSpells::fn2()"
  )
  level3 <- infra_call_stack(
    parent = level2,
    child  = "spatialSpells::fn3()"
  )

  # level2 has one hierarchy symbol; level3 has two
  symbol_count <- function(x) {
    lengths(regmatches(x, gregexpr("└──", x, fixed = TRUE)))
  }
  expect_equal(symbol_count(level2), 1L)
  expect_equal(symbol_count(level3), 2L)
})

test_that("infra_call_stack: result is a character string", {
  result <- infra_call_stack(
    parent = "spatialSpells::parent_fn()",
    child  = "spatialSpells::child_fn()"
  )
  expect_type(result, "character")
  expect_length(result, 1L)
})
