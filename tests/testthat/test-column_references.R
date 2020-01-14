test_that("column_references() returns expected result on 'flights' and 'planes' left outer join example query", {
  expect_equal(
    {
      query <- "SELECT origin, dest,
          round(AVG(distance)) AS dist,
          round(COUNT(*)/10) AS flights_per_year,
          round(SUM(seats)/10) AS seats_per_year,
          round(AVG(arr_delay)) AS avg_arr_delay
        FROM fly.flights f LEFT OUTER JOIN fly.planes p
          ON f.tailnum = p.tailnum
        WHERE distance BETWEEN 300 AND 400
        GROUP BY origin, dest
        HAVING flights_per_year > 5000
        ORDER BY seats_per_year DESC
        LIMIT 6;"
      column_references(parse_query(query, tidy = TRUE))
    },
    c("origin", "dest", "distance", "seats", "arr_delay",
      "f.tailnum", "p.tailnum",
      "flights_per_year", "seats_per_year")
  )
})
