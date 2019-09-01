test_that("parse_expression() fails on illegal function", {
  expect_error(
    parse_expression("system('ls')"),
    "^Unrecognized"
  )
})

test_that("parse_expression() fails on illegal function when function also used as a column name", {
  expect_error(
    parse_expression("system + system('ls')"),
    "^Unrecognized"
  )
})

test_that("parse_expression() fails on illegal function called without parentheses using %>%", {
  expect_error(
    parse_expression("'ls' %>% system"),
    "^Unrecognized"
  )
})

test_that("parse_expression(secure = FALSE) does not fail on illegal function", {
  expect_error(
    parse_expression("system('ls')", secure = FALSE),
    NA
  )
})

test_that("parse_query() fails with illegal function in SELECT clause", {
  expect_error(
    parse_query("SELECT rm(x) FROM y"),
    "^Unrecognized"
  )
})

test_that("parse_query() fails with illegal function in FROM clause", {
  expect_error(
    parse_query("SELECT x FROM Sys.getenv('y')"),
    "^Unrecognized"
  )
})

test_that("parse_query() fails with illegal function in WHERE clause", {
  expect_error(
    parse_query("SELECT x FROM y WHERE Sys.setenv(z = 'abc')"),
    "^Unrecognized"
  )
})

test_that("parse_query() fails with illegal function in GROUP BY clause", {
  expect_error(
    parse_query("SELECT SUM(x) FROM y GROUP BY print('abc')"),
    "^Unrecognized"
  )
})

test_that("parse_query() fails with illegal function in HAVING clause", {
  expect_error(
    parse_query("SELECT x, SUM(y) FROM z GROUP BY x HAVING COUNT(*) > getwd()"),
    "^Unrecognized"
  )
})

test_that("parse_query() fails with illegal function in ORDER BY clause", {
  expect_error(
    parse_query("SELECT x ORDER BY list.files('y')"),
    "^Unrecognized"
  )
})
