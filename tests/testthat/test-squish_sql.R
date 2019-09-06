test_that("squish_sql() removes tabs and newlines from a query", {
  expect_equal(
    squish_sql("\nSELECT x\n\tFROM y\r\n\tWHERE z\n\t WHERE w\n"),
    " SELECT x FROM y WHERE z WHERE w "
  )
})

test_that("squish_sql() removes tabs and newlines from an expression", {
  expect_equal(
    squish_sql("\nx BETWEEN \ny AND\r\n\tz\n"),
    " x BETWEEN y AND z "
  )
})

test_that("squish_sql() removes line comments from a query", {
  expect_equal(
    squish_sql("\nSELECT x - y  --x   \n\t FROM y--y' ' SELECT\r\n\tWHERE z--q\n\t WHERE w --???w\n"),
    " SELECT x - y FROM y WHERE z WHERE w "
  )
})

test_that("squish_sql() removes block comments from a query", {
  expect_equal(
    squish_sql("\nSELECT /*SELECT ' ' */ x\n\tFROM/*)*/  y\r\n\tWHERE   /*abc(((*/z\n\t WHERE w\n"),
    " SELECT x FROM y WHERE z WHERE w "
  )
})

test_that("squish_sql() removes line comments from an expression", {
  expect_equal(
    squish_sql("\nx / 2 BETWEEN --a\ny AND\r\n\tz\n -- b"),
    " x / 2 BETWEEN y AND z "
  )
})

test_that("squish_sql() removes block comments from an expression", {
  expect_equal(
    squish_sql("\nx*3 BETWEEN /*a AND x * 2 / 3 b*/  \ny AND /*' ' ! \"*/\r\n\tz\n"),
    " x*3 BETWEEN y AND z "
  )
})

test_that("squish_sql() stops on unclosed block comment", {
  expect_error(
    squish_sql("SELECT x FROM /* "),
    "unclosed"
  )
})

test_that("squish_sql() stops when input is not a character vector", {
  expect_error(
    squish_sql(42),
    "^Unexpected"
  )
})

test_that("squish_sql() stops when input is has length > 1", {
  expect_error(
    squish_sql(c("SELECT w FROM x", "SELECT y FROM z")),
    "^Unexpected"
  )
})
