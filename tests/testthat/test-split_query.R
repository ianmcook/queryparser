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

test_that("split_query() works on example query #1 with no whitespace separating keywords", {
  expect_equal(
    split_query("SELECT`foo`FROM`bar`;"),
    list(select = "`foo`", from = "`bar`")
  )
})

test_that("split_query() works on example query #2 with no whitespace separating keywords", {
  expect_equal(
    split_query("SELECT*FROM table;"),
    list(select = "*", from = "table")
  )
})

test_that("split_query() works on example query #3 with no whitespace separating keywords", {
  expect_equal(
    split_query("SELECT'literal'FROM table;"),
    list(select = "'literal'", from = "table")
  )
})
