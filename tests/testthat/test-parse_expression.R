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
    parse_expression("coalesce(abs(w + 2), x, y, z)", tidy = FALSE),
    str2lang("ifelse(!is.na(abs(w + 2)), abs(w + 2), ifelse(!is.na(x), x, ifelse(!is.na(y), y, ifelse(!is.na(z), z, NA))))")
  )
})

test_that("parse_expression(tidy = TRUE) successfully parses test expression #7 with coalesce()", {
  expect_equal(
    parse_expression("coalesce(abs(w + 2), x, y, z)", tidy = TRUE),
    str2lang("dplyr::coalesce(abs(w + 2), x, y, z)")
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

test_that("parse_expression() successfully parses test expression #13 with IS NOT DISTINCT FROM", {
  expect_equal(
    parse_expression("x is not distinct from y"),
    str2lang("ifelse(is.na(x) | is.na(y), is.na(x) == is.na(y), x == y)")
  )
})

test_that("parse_expression() successfully parses test expression #14 with <=>", {
  expect_equal(
    parse_expression("x <=> y"),
    str2lang("ifelse(is.na(x) | is.na(y), is.na(x) == is.na(y), x == y)")
  )
})

test_that("parse_expression() successfully parses test expression #15 with IS DISTINCT FROM", {
  expect_equal(
    parse_expression("x is distinct from y"),
    str2lang("ifelse(is.na(x) | is.na(y), is.na(x) != is.na(y), x != y)")
  )
})

test_that("parse_expression(tidy = TRUE) successfully parses test expression #16 with CASE", {
  expect_equal(
    parse_expression(paste(
      "CASE size WHEN 'L' THEN 'large'",
      "WHEN 'M' THEN 'medium'",
      "WHEN 'S' THEN 'small' END"), tidy = TRUE),
    str2lang("dplyr::case_when(size == 'L' ~ 'large', size == 'M' ~ 'medium',
             size == 'S' ~ 'small')")
  )
})

test_that("parse_expression(tidy = TRUE) successfully parses test expression #17 with CASE", {
  expect_equal(
    parse_expression(paste(
      "CASE size WHEN 'L' THEN 'large'",
      "WHEN 'M' THEN 'medium'",
      "WHEN 'S' THEN 'small'",
      "ELSE 'other' END"), tidy = TRUE),
    str2lang("dplyr::case_when(size == 'L' ~ 'large', size == 'M' ~ 'medium',
      size == 'S' ~ 'small', TRUE ~ 'other')")
  )
})

test_that("parse_expression(tidy = TRUE) successfully parses test expression #18 with CASE", {
  expect_equal(
    parse_expression(paste(
      "CASE WHEN size >= 46 THEN 'other' WHEN size >= 42 THEN 'large'",
      "WHEN size >= 38 THEN 'medium'",
      "WHEN size >= 34 THEN 'small' END"), tidy = TRUE),
    str2lang("dplyr::case_when(size >= 46 ~ 'other', size >= 42 ~ 'large',
      size >= 38 ~ 'medium', size >= 34 ~ 'small')")
  )
})

test_that("parse_expression(tidy = TRUE) successfully parses test expression #19 with CASE", {
  expect_equal(
    parse_expression(paste(
      "CASE WHEN size >= 46 THEN 'other' WHEN size >= 42 THEN 'large'",
      "WHEN size >= 38 THEN 'medium'",
      "WHEN size >= 34 THEN 'small'",
      "ELSE 'other' END"), tidy = TRUE),
    str2lang("dplyr::case_when(size >= 46 ~ 'other', size >= 42 ~ 'large',
             size >= 38 ~ 'medium', size >= 34 ~ 'small', TRUE ~ 'other')")
  )
})

test_that("parse_expression(tidy = TRUE) stops on malformed CASE expression with no END", {
  expect_error(
    parse_expression(paste(
      "CASE WHEN size >= 46 THEN 'other' WHEN size >= 42 THEN 'large'",
      "WHEN size >= 38 THEN 'medium'",
      "WHEN size >= 34 THEN 'small'",
      "ELSE 'other'"), tidy = TRUE),
    "END"
  )
})

test_that("parse_expression(tidy = TRUE) stops on malformed CASE expression with no WHEN ... THEN", {
  expect_error(
    parse_expression("CASE ELSE 'other' END", tidy = TRUE),
    "WHEN"
  )
})

test_that("parse_expression(tidy = TRUE) stops on malformed CASE expression with WHEN missing THEN", {
  expect_error(
    parse_expression(paste(
      "CASE WHEN size >= 46 THEN 'other' WHEN size >= 42 THEN 'large'",
      "WHEN size >= 38 THEN 'medium'",
      "WHEN size >= 34",
      "ELSE 'other' END"), tidy = TRUE),
    "THEN"
  )
})

test_that("parse_expression(tidy = FALSE) stops when coalesce() has no arguments", {
  expect_error(
    parse_expression("coalesce()", tidy = FALSE),
    "argument"
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

test_that("parse_expression(tidyverse = FALSE) succeeds when one expression to COUNT DISTINCT", {
  expect_equal(
    parse_expression("COUNT(DISTINCT x)", tidyverse = FALSE),
    quote(sum(!is.na(unique(x))))
  )
})

test_that("parse_expression(tidyverse = TRUE) succeeds when one expression to COUNT DISTINCT", {
  expect_equal(
    parse_expression("COUNT(DISTINCT x)", tidyverse = TRUE),
    str2lang("dplyr::n_distinct(x, na.rm = TRUE)")
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

test_that("parse_expression() wraps `!` args in parentheses", {
  expect_equal(
    parse_expression("NOT NOT NOT (x OR y)"),
    quote(!(!(!(x | y))))
  )
})

test_that("parse_expression(tidyverse = FALSE) successfully translates BETWEEN with quoted operands", {
  expect_equal(
    parse_expression("'b' BETWEEN 'a' AND 'c'", tidyverse = FALSE),
    quote(("b" >= "a" & "b" <= "c"))
  )
})

test_that("parse_expression(tidyverse = TRUE) successfully translates BETWEEN with quoted operands", {
  expect_equal(
    parse_expression("'b' BETWEEN 'a' AND 'c'", tidyverse = TRUE),
    str2lang("dplyr::between('b', 'a', 'c')")
  )
})

test_that("parse_expression(tidyverse = FALSE) successfully translates NOT BETWEEN with quoted operands", {
  expect_equal(
    parse_expression("'b' NOT BETWEEN 'a' AND 'c'", tidyverse = FALSE),
    quote(("b" < "a" | "b" > "c"))
  )
})

test_that("parse_expression(tidyverse = TRUE) successfully translates NOT BETWEEN with quoted operands", {
  expect_equal(
    parse_expression("'b' NOT BETWEEN 'a' AND 'c'", tidyverse = TRUE),
    str2lang("!dplyr::between('b', 'a', 'c')")
  )
})

test_that("parse_expression() successfully translates expression with LIKE", {
  expect_equal(
    parse_expression("x LIKE 'a%d_f'"),
    quote(grepl("^a.*d.f$", x))
  )
})

test_that("parse_expression() stops when input is not a character vector", {
  expect_error(
    parse_expression(42),
    "^Unexpected"
  )
})

test_that("parse_expression() stops when input is has length > 1", {
  expect_error(
    parse_expression(c("sqrt(4)", "sqrt(9)")),
    "^Unexpected"
  )
})

test_that("parse_expression() properly handles quote character escaped by doubling", {
  expect_equal(
    parse_expression("x LIKE 'isn''t'"),
    quote(grepl("^isn't$", x))
  )
})

test_that("parse_expression() properly handles quote character escaped with backslash", {
  expect_equal(
    parse_expression("x LIKE 'isn\\'t'"),
    quote(grepl("^isn't$", x))
  )
})

test_that("parse_expression() stops when column name is an R reserved word in backticks", {
  expect_error(
    parse_expression("SUM(`break`)"),
    "reserved"
  )
})
