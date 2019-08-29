test_that("parse_expression(tidy = F) successfully parses test expression #1 with BETWEEN", {
  expect_equal(
    parse_expression("ifnull(x > 2, x between 2 and 3)", tidy = F),
    str2lang("ifelse(is.na(x > 2), (x >= 2 & x <= 3), x > 2)")
  )
})

test_that("parse_expression(tidy = T) successfully parses test expression #1 with BETWEEN", {
  expect_equal(
    parse_expression("ifnull(x > 2, x between 2 and 3)", tidy = T),
    str2lang("ifelse(is.na(x > 2), dplyr::between(x, 2, 3), x > 2)")
  )
})

test_that("parse_expression(tidy = F) successfully parses test expression #2 with CAST and BETWEEN", {
  expect_equal(
    parse_expression("x between cast(y as int) and z", tidy = F),
    str2lang("(x >= as.integer(y) & x <= z)")
  )
})

test_that("parse_expression(tidy = T) successfully parses test expression #2 with CAST and BETWEEN", {
  expect_equal(
    parse_expression("x between cast(y as int) and z", tidy = T),
    str2lang("dplyr::between(x, as.integer(y), z)")
  )
})

test_that("parse_expression(tidy = F) successfully parses test expression #3 with CAST and BETWEEN", {
  expect_equal(
    parse_expression("cast(x between y and z as char(1))", tidy = F),
    str2lang("as.character((x >= y & x <= z))")
  )
})

test_that("parse_expression(tidy = T) successfully parses test expression #3 with CAST and BETWEEN", {
  expect_equal(
    parse_expression("cast(x between y and z as char(1))", tidy = T),
    str2lang("as.character(dplyr::between(x, y, z))")
  )
})


test_that("parse_expression(tidy = F) successfully parses test expression #4 with CAST and BETWEEN", {
  expect_equal(
    parse_expression("if (1 + cast(2 + 3 + sqrt(4) as integer) between 7 and 11 - 1, 'a', 'b')", tidy = F),
    str2lang("ifelse((1 + as.integer(2 + 3 + sqrt(4)) >= 7 & 1 + as.integer(2 + 3 + sqrt(4)) <= 11 - 1), \"a\", \"b\")")
  )
})

test_that("parse_expression(tidy = T) successfully parses test expression #4 with CAST and BETWEEN", {
  expect_equal(
    parse_expression("if (1 + cast(2 + 3 + sqrt(4) as integer) between 7 and 11 - 1, 'a', 'b')", tidy = T),
    str2lang("ifelse(dplyr::between(1 + as.integer(2 + 3 + sqrt(4)), 7, 11 - 1), \"a\", \"b\")")
  )
})

test_that("parse_expression(tidy = F) successfully parses test expression #5 with CAST and NOT BETWEEN", {
  expect_equal(
    parse_expression("(cast((x) as decimal(2,9)) not between (y) and (z))", tidy = F),
    str2lang("((as.numeric((x)) < (y) | as.numeric((x)) > (z)))")
  )
})

test_that("parse_expression(tidy = T) successfully parses test expression #5 with CAST and NOT BETWEEN", {
  expect_equal(
    parse_expression("(cast((x) as decimal(2,9)) not between (y) and (z))", tidy = T),
    str2lang("(!dplyr::between(as.numeric((x)), (y), (z)))")
  )
})

test_that("parse_expression(tidy = F) successfully parses test expression #7 with coalesce()", {
  expect_equal(
    parse_expression("coalesce(w, x, y, z)", tidy = F),
    str2lang("if (!is.na(w)) w else if (!is.na(x)) x else if (!is.na(y)) y else if (!is.na(z)) z else NA")
  )
})

test_that("parse_expression(tidy = F) successfully parses test expression #7 with coalesce()", {
  expect_equal(
    parse_expression("coalesce(w, x, y, z)", tidy = T),
    str2lang("dplyr::coalesce(w, x, y, z)")
  )
})
