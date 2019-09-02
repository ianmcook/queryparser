test_that("parse_expression(tidy = FALSE) successfully parses an expression with SUM(scalar)", {
  expect_equal(
    parse_expression("SUM(1 + pi + sqrt(16) + cast('123' AS integer))", tidy = FALSE),
    str2lang("nrow(.) * (1 + pi + sqrt(16) + as.integer('123'))")
  )
})

test_that("parse_expression(tidy = TRUE) successfully parses an expression with SUM(scalar)", {
  expect_equal(
    parse_expression("SUM(1 + pi + sqrt(16) + cast('123' AS integer))", tidy = TRUE),
    str2lang("dplyr::n() * (1 + pi + sqrt(16) + as.integer('123'))")
  )
})

test_that("parse_expression(tidy = FALSE) successfully parses an expression with SUM(non-scalar)", {
  expect_equal(
    parse_expression("SUM(1 + pi + sqrt(16) + cast(x AS integer))", tidy = FALSE),
    str2lang("sum(1 + pi + sqrt(16) + as.integer(x), na.rm = TRUE)")
  )
})

test_that("parse_expression(tidy = TRUE) successfully parses an expression with SUM(non-scalar)", {
  expect_equal(
    parse_expression("SUM(1 + pi + sqrt(16) + cast(x AS integer))", tidy = TRUE),
    str2lang("sum(1 + pi + sqrt(16) + as.integer(x), na.rm = TRUE)")
  )
})

test_that("parse_expression(tidy = FALSE) successfully parses an expression with GROUP_CONCAT(scalar)", {
  expect_equal(
    parse_expression("GROUP_CONCAT(concat(cast(123 AS string), 'abc'), '|')", tidy = FALSE),
    str2lang("paste0(rep(paste0(as.character(123), 'abc'), times = nrow(.)), collapse = '|')")
  )
})

test_that("parse_expression(tidy = TRUE) successfully parses an expression with GROUP_CONCAT(scalar)", {
  expect_equal(
    parse_expression("GROUP_CONCAT(concat(cast(123 AS string), 'abc'), '|')", tidy = TRUE),
    str2lang("stringr::str_flatten(rep(stringr::str_c(as.character(123), 'abc'), times = dplyr::n()), collapse = '|')")
  )
})

test_that("parse_expression(tidy = FALSE) successfully parses an expression with GROUP_CONCAT(non-scalar)", {
  expect_equal(
    parse_expression("GROUP_CONCAT(concat(cast(123 AS string), 'abc'), '|')", tidy = FALSE),
    str2lang("paste0(rep(paste0(as.character(123), 'abc'), times = nrow(.)), collapse = '|')")
  )
})

test_that("parse_expression(tidy = TRUE) successfully parses an expression with GROUP_CONCAT(non-scalar)", {
  expect_equal(
    parse_expression("GROUP_CONCAT(concat(cast(x AS string), 'abc'), '|')", tidy = TRUE),
    str2lang("stringr::str_flatten(stringr::str_c(as.character(x), 'abc'), collapse = '|')")
  )
})
