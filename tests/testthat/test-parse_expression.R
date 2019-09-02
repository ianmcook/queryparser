test_that("parse_expression(tidy = FALSE) successfully parses test expression #1 with BETWEEN", {
  expect_equal(
    parse_expression("ifnull(x > 2, x between 2 and 3)", tidy = FALSE),
    str2lang("ifelse(is.na(x > 2), (x >= 2 & x <= 3), x > 2)")
  )
})

test_that("parse_expression(tidy = TRUE) successfully parses test expression #1 with BETWEEN", {
  expect_equal(
    parse_expression("ifnull(x > 2, x between 2 and 3)", tidy = TRUE),
    str2lang("ifelse(is.na(x > 2), dplyr::between(x, 2, 3), x > 2)")
  )
})

test_that("parse_expression(tidy = FALSE) successfully parses test expression #2 with CAST and BETWEEN", {
  expect_equal(
    parse_expression("x between cast(y as int) and z", tidy = FALSE),
    str2lang("(x >= as.integer(y) & x <= z)")
  )
})

test_that("parse_expression(tidy = TRUE) successfully parses test expression #2 with CAST and BETWEEN", {
  expect_equal(
    parse_expression("x between cast(y as int) and z", tidy = TRUE),
    str2lang("dplyr::between(x, as.integer(y), z)")
  )
})

test_that("parse_expression(tidy = FALSE) successfully parses test expression #3 with CAST and BETWEEN", {
  expect_equal(
    parse_expression("cast(x between y and z as char(1))", tidy = FALSE),
    str2lang("as.character((x >= y & x <= z))")
  )
})

test_that("parse_expression(tidy = TRUE) successfully parses test expression #3 with CAST and BETWEEN", {
  expect_equal(
    parse_expression("cast(x between y and z as char(1))", tidy = TRUE),
    str2lang("as.character(dplyr::between(x, y, z))")
  )
})


test_that("parse_expression(tidy = FALSE) successfully parses test expression #4 with CAST and BETWEEN", {
  expect_equal(
    parse_expression("if (1 + cast(2 + 3 + sqrt(4) as integer) between 7 and 11 - 1, 'a', 'b')", tidy = FALSE),
    str2lang("ifelse((1 + as.integer(2 + 3 + sqrt(4)) >= 7 & 1 + as.integer(2 + 3 + sqrt(4)) <= 11 - 1), \"a\", \"b\")")
  )
})

test_that("parse_expression(tidy = TRUE) successfully parses test expression #4 with CAST and BETWEEN", {
  expect_equal(
    parse_expression("if (1 + cast(2 + 3 + sqrt(4) as integer) between 7 and 11 - 1, 'a', 'b')", tidy = TRUE),
    str2lang("ifelse(dplyr::between(1 + as.integer(2 + 3 + sqrt(4)), 7, 11 - 1), \"a\", \"b\")")
  )
})

test_that("parse_expression(tidy = FALSE) successfully parses test expression #5 with CAST and NOT BETWEEN", {
  expect_equal(
    parse_expression("(cast((x) as decimal(2,9)) not between (y) and (z))", tidy = FALSE),
    str2lang("((as.numeric((x)) < (y) | as.numeric((x)) > (z)))")
  )
})

test_that("parse_expression(tidy = TRUE) successfully parses test expression #5 with CAST and NOT BETWEEN", {
  expect_equal(
    parse_expression("(cast((x) as decimal(2,9)) not between (y) and (z))", tidy = TRUE),
    str2lang("(!dplyr::between(as.numeric((x)), (y), (z)))")
  )
})

test_that("parse_expression(tidy = FALSE) successfully parses test expression #7 with coalesce()", {
  expect_equal(
    parse_expression("coalesce(w, x, y, z)", tidy = FALSE),
    str2lang("if (!is.na(w)) w else if (!is.na(x)) x else if (!is.na(y)) y else if (!is.na(z)) z else NA")
  )
})

test_that("parse_expression(tidy = FALSE) successfully parses test expression #7 with coalesce()", {
  expect_equal(
    parse_expression("coalesce(w, x, y, z)", tidy = TRUE),
    str2lang("dplyr::coalesce(w, x, y, z)")
  )
})

test_that("parse_expression() successfully parses test expression #8 with IN", {
  expect_equal(
    parse_expression("x IN (a,b,c)"),
    quote(x %in% c(a, b, c))
  )
})

test_that("parse_expression() successfully parses test expression #9 with NOT IN", {
  expect_equal(
    parse_expression("x NOT IN (a,b,c)"),
    quote(!(x %in% c(a, b, c)))
  )
})

test_that("parse_expression() successfully parses test expression #10 with three CASTs", {
  expect_equal(
    parse_expression("concat(cast(year as string), '-', cast(month as string), '-', cast(day as string))"),
    quote(paste0(as.character(year), "-", as.character(month), "-", as.character(day)))
  )
})

test_that("parse_expression(tidy = FALSE) successfully parses test expression #11 with three BETWEENs", {
  expect_equal(
    parse_expression("x between y and z and a between b and c and x + 2 between y - 1 and z + 5", tidy = FALSE),
    quote((x >= y & x <= z) & (a >= b & a <= c) & (x + 2 >= y - 1 & x + 2 <= z + 5))
  )
})

test_that("parse_expression(tidy = TRUE) successfully parses test expression #11 with three BETWEENs", {
  expect_equal(
    parse_expression("x between y and z and a between b and c and x + 2 between y - 1 and z + 5", tidy = TRUE),
    str2lang("dplyr::between(x, y, z) & dplyr::between(a, b, c) & dplyr::between(x + 2, y - 1, z + 5)")
  )
})

test_that("parse_expression(tidy = FALSE) successfully parses test expression #12 with three NOT BETWEENs", {
  expect_equal(
    parse_expression("x not between y and z and a not between b and c and x + 2 not between y - 1 and z + 5", tidy = FALSE),
    quote((x < y | x > z) & (a < b | a > c) & (x + 2 < y - 1 | x + 2 > z + 5))
  )
})

test_that("parse_expression(tidy = TRUE) successfully parses test expression #12 with three NOT BETWEENs", {
  expect_equal(
    parse_expression("x not between y and z and a not between b and c and x + 2 not between y - 1 and z + 5", tidy = TRUE),
    str2lang("!dplyr::between(x, y, z) & !dplyr::between(a, b, c) & !dplyr::between(x + 2, y - 1, z + 5)")
  )
})

test_that("parse_expression(tidy = FALSE) successfully parses test expression #13 with BETWEEN and NOT BETWEEN", {
  expect_equal(
    parse_expression("x not between y and z and a between b and c", tidy = FALSE),
    quote((x < y | x > z) & (a >= b & a <= c))
  )
})

test_that("parse_expression(tidy = TRUE) successfully parses test expression #13 with BETWEEN and NOT BETWEEN", {
  expect_equal(
    parse_expression("x not between y and z and a between b and c", tidy = TRUE),
    str2lang("!dplyr::between(x, y, z) & dplyr::between(a, b, c)")
  )
})

test_that("parse_expression(tidyverse = FALSE) stops when multiple expressions to non-SUM aggregate distinct", {
  expect_error(
    parse_expression("AVG(DISTINCT x, y)", tidyverse = FALSE),
    "^Multiple arguments"
  )
})

test_that("parse_expression(tidyverse = TRUE) stops when multiple expressions to non-SUM aggregate DISTINCT", {
  expect_error(
    parse_expression("AVG(DISTINCT x, y)", tidyverse = TRUE),
    "^Multiple arguments"
  )
})

test_that("parse_expression(tidyverse = FALSE) stops when multiple expressions to COUNT DISTINCT", {
  expect_error(
    parse_expression("COUNT(DISTINCT x, y)", tidyverse = FALSE),
    "^Multiple arguments"
  )
})

test_that("parse_expression(tidyverse = TRUE) succeeds when multiple expressions to COUNT DISTINCT", {
  expect_equal(
    parse_expression("COUNT(DISTINCT x, y)", tidyverse = TRUE),
    str2lang("dplyr::n_distinct(x, y, na.rm = TRUE)")
  )
})

test_that("parse_expression(tidyverse = FALSE) does not translate column names that match direct translated function names", {
  expect_equal(
    parse_expression("concat = 3", tidyverse = FALSE),
    str2lang("concat == 3")
  )
})

test_that("parse_expression(tidyverse = TRUE) does not translate column names that match direct translated function names", {
  expect_equal(
    parse_expression("upper(initcap)", tidyverse = TRUE),
    str2lang("stringr::str_to_upper(initcap)")
  )
})
