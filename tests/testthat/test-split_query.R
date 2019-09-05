test_that("split_query() stops when input is not a character vector", {
  expect_error(
    split_query(42),
    "^Unexpected"
  )
})

test_that("split_query() stops when input is has length > 1", {
  expect_error(
    split_query(c("SELECT w FROM x", "SELECT y FROM z")),
    "^Unexpected"
  )
})

test_that("split_query() stops when query does not begin with SELECT", {
  expect_error(
    split_query("ORDER BY z"),
    "begin"
  )
})
