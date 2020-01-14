test_that("unqualify_query() works as expected on example query", {
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
