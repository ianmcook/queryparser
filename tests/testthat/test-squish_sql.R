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
    squish_sql("\nSELECT x--x\n\tFROM y--y' ' SELECT\r\n\tWHERE z\n\t WHERE w --???w\n"),
    " SELECT x FROM y WHERE z WHERE w "
  )
})

test_that("squish_sql() removes block comments from a query", {
  expect_equal(
    squish_sql("\nSELECT /*SELECT ' ' */ x\n\tFROM y\r\n\tWHERE z\n\t WHERE w\n"),
    " SELECT x FROM y WHERE z WHERE w "
  )
})

test_that("squish_sql() removes line comments from an expression", {
  expect_equal(
    squish_sql("\nx BETWEEN --a\ny AND\r\n\tz\n -- b"),
    " x BETWEEN y AND z "
  )
})

test_that("squish_sql() removes block comments from an expression", {
  expect_equal(
    squish_sql("\nx BETWEEN /*a AND b*/  \ny AND /*' ' ! \"*/\r\n\tz\n"),
    " x BETWEEN y AND z "
  )
})

test_that("squish_sql() stops on unclosed block comment", {
  expect_error(
    squish_sql("SELECT x FROM /* "),
    "unclosed"
  )
})
