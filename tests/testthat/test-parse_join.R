test_that("parse_query() works on SQL-92-style (explicit) join with ON", {
  expect_equal(
    parse_query("SELECT y.w, z.x FROM y JOIN z ON y.a = z.b"),
    list(select = list(quote(y.w), quote(z.x)), from = structure(list(quote(y),
      quote(z)), join_types = "inner join", join_conditions = list(quote(y.a ==
      z.b))))
  )
})

test_that("parse_query() works on SQL-92-style (explicit) join with USING", {
  expect_equal(
    parse_query("SELECT y.w, z.x FROM y JOIN z USING (a)"),
    list(select = list(quote(y.w), quote(z.x)), from = structure(list(quote(y),
      quote(z)), join_types = "inner join", join_conditions = list(quote(y.a ==
      z.a))))
  )
})

test_that("parse_query() stops on SQL-89-style (implicit) join", {
  expect_error(
    parse_query("SELECT y.w, z.x FROM y, z WHERE y.a = z.b"),
    "implicit"
  )
})
