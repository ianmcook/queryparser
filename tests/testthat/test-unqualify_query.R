test_that("unqualify_query() is called and works as expected on example single-table query with aliases", {
  expect_equal(
    parse_query("SELECT f.year AS y, flights.month AS m, day AS d FROM flights f"),
    parse_query("SELECT year AS y, month AS m, day AS d FROM flights f"),
  )
})

test_that("unqualify_query() is called and works as expected on example single-table query without aliases", {
  expect_equal(
    parse_query("SELECT f.year, flights.month, day FROM flights f"),
    parse_query("SELECT year, month, day FROM flights f"),
  )
})

test_that("unqualify_query() works as expected when explicitly called on example join query", {
  expect_equal(
    unqualify_query(
      parse_query("SELECT f.year AS y, flights.month AS m, day AS d
                     FROM flights f JOIN planes p USING (tailnum)"),
      c("flights", "f")
    ),
    parse_query("SELECT year AS y, month AS m, day AS d
                   FROM flights f JOIN planes p USING (tailnum)"),
  )
})

test_that("unqualify_query() with except works as expected when explicitly called on example join query", {
  expect_equal(
    unqualify_query(
      parse_query("SELECT f.year AS y, flights.month AS m, day AS d, f.tailnum
                     FROM flights f JOIN planes p USING (tailnum)"),
      c("flights", "f"),
      c("f.tailnum", "p.tailnum")
    ),
    parse_query("SELECT year AS y, month AS m, day AS d, f.tailnum
                   FROM flights f JOIN planes p USING (tailnum)"),
  )
})
