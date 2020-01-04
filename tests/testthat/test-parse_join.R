test_that("parse_query(tidy = TRUE) works on 'flights' and 'planes' left outer join example query", {
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
      parse_query(query, tidy = TRUE)
    },
    structure(list(select = structure(list(quote(origin), quote(dest),
      dist = quote(round(mean(distance, na.rm = TRUE))), flights_per_year = quote(round(dplyr::n()/10)),
      seats_per_year = quote(round(sum(seats, na.rm = TRUE)/10)),
      avg_arr_delay = quote(round(mean(arr_delay, na.rm = TRUE)))),
      aggregate = c(FALSE, FALSE, dist = TRUE, flights_per_year = TRUE,
      seats_per_year = TRUE, avg_arr_delay = TRUE)), from = structure(list(f = quote(fly.flights),
      p = quote(fly.planes)), join_types = "left outer join", join_conditions = list(quote(f.tailnum ==
      p.tailnum))), where = list(quote(dplyr::between(distance,
      300, 400))), group_by = list(quote(origin), quote(dest)),
      having = list(quote(flights_per_year > 5000)), order_by = structure(list(quote(dplyr::desc(seats_per_year))),
      aggregate = FALSE), limit = list(6)), aggregate = TRUE)
  )
})

test_that("parse_query() works on SQL-92-style (explicit) join with ON", {
  expect_equal(
    parse_query("SELECT y.w, z.x FROM y JOIN z ON y.a = z.b"),
    list(select = list(quote(y.w), quote(z.x)), from = structure(list(quote(y),
      quote(z)), join_types = "inner join", join_conditions = list(quote(y.a ==
      z.b))))
  )
})

test_that("parse_query() works on SQL-92-style (explicit) join with ON with parentheses", {
  expect_equal(
    parse_query("SELECT y.w, z.x FROM y JOIN z ON (y.a = z.b)"),
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

test_that("parse_query() works on join with aliases", {
  expect_equal(
    parse_query("SELECT y.a, z.`b`, `w`.c FROM why AS y LEFT OUTER JOIN zee 'z' ON y.a = z.b INNER JOIN dub w USING(c,d,e)"),
    list(select = list(quote(y.a), quote(z.b), quote(w.c)), from = structure(list(y = quote(why),
      z = quote(zee), w = quote(dub)), join_types = c("left outer join",
      "inner join"), join_conditions = list(quote(y.a == z.b),
      quote(z.c == w.c & z.d == w.d & z.e == w.e))))
  )
})

test_that("parse_query() works on CROSS JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x CROSS JOIN y"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "cross join", join_conditions = NA))
  )
})

test_that("parse_query() works on NATURAL JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x NATURAL JOIN y"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "natural inner join", join_conditions = NA))
  )
})

test_that("parse_query() works on NATURAL INNER JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x NATURAL INNER JOIN y"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "natural inner join", join_conditions = NA))
  )
})

test_that("parse_query() works on INNER JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x INNER JOIN y ON x.k = y.k"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "inner join", join_conditions = list(quote(x.k ==
      y.k))))
  )
})

test_that("parse_query() works on NATURAL OUTER JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x NATURAL OUTER JOIN y"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "natural left outer join", join_conditions = NA))
  )
})

test_that("parse_query() works on NATURAL LEFT OUTER JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x NATURAL LEFT OUTER JOIN y"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "natural left outer join", join_conditions = NA))
  )
})

test_that("parse_query() works on LEFT OUTER JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x LEFT OUTER JOIN y ON x.k = y.k"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "left outer join", join_conditions = list(quote(x.k ==
      y.k))))
  )
})

test_that("parse_query() works on NATURAL RIGHT OUTER JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x NATURAL RIGHT OUTER JOIN y"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "natural right outer join", join_conditions = NA))
  )
})

test_that("parse_query() works on RIGHT OUTER JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x RIGHT OUTER JOIN y ON x.k = y.k"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "right outer join", join_conditions = list(quote(x.k ==
      y.k))))
  )
})

test_that("parse_query() works on NATURAL FULL OUTER JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x NATURAL FULL OUTER JOIN y"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "natural full outer join", join_conditions = NA))
  )
})

test_that("parse_query() works on FULL OUTER JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x FULL OUTER JOIN y ON x.k = y.k"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "full outer join", join_conditions = list(quote(x.k ==
        y.k))))
  )
})

test_that("parse_query() works on OUTER JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x OUTER JOIN y ON x.k = y.k"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
    quote(y)), join_types = "left outer join", join_conditions = list(quote(x.k ==
    y.k))))
  )
})

test_that("parse_query() works on NATURAL LEFT SEMI JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x NATURAL LEFT SEMI JOIN y"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "natural left semi join", join_conditions = NA))
  )
})

test_that("parse_query() works on LEFT SEMI JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x LEFT SEMI JOIN y ON x.k = y.k"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "left semi join", join_conditions = list(quote(x.k ==                                                                                                                                            y.k))))
  )
})

test_that("parse_query() works on NATURAL RIGHT SEMI JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x NATURAL RIGHT SEMI JOIN y"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "natural right semi join", join_conditions = NA))
  )
})

test_that("parse_query() works on RIGHT SEMI JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x RIGHT SEMI JOIN y ON x.k = y.k"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "right semi join", join_conditions = list(quote(x.k ==                                                                                                                                            y.k))))
  )
})

test_that("parse_query() stops on SEMI JOIN without RIGHT or LEFT", {
  expect_error(
    parse_query("SELECT a, b FROM x SEMI JOIN y ON x.k = y.k"),
    "LEFT or RIGHT"
  )
})

test_that("parse_query() works on NATURAL LEFT ANTI JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x NATURAL LEFT ANTI JOIN y"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "natural left anti join", join_conditions = NA))
  )
})

test_that("parse_query() works on LEFT ANTI JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x LEFT ANTI JOIN y ON x.k = y.k"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "left anti join", join_conditions = list(quote(x.k ==                                                                                                                                            y.k))))
  )
})

test_that("parse_query() works on NATURAL RIGHT ANTI JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x NATURAL RIGHT ANTI JOIN y"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "natural right anti join", join_conditions = NA))
  )
})


test_that("parse_query() works on RIGHT ANTI JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x RIGHT ANTI JOIN y ON x.k = y.k"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "right anti join", join_conditions = list(quote(x.k ==                                                                                                                                            y.k))))
  )
})

test_that("parse_query() stops on ANTI JOIN without RIGHT or LEFT", {
  expect_error(
    parse_query("SELECT a, b FROM x ANTI JOIN y ON x.k = y.k"),
    "LEFT or RIGHT"
  )
})

test_that("parse_query() works on NATURAL LEFT JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x NATURAL LEFT JOIN y"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "natural left outer join", join_conditions = NA))
  )
})

test_that("parse_query() works on LEFT JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x LEFT JOIN y ON x.k = y.k"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "left outer join", join_conditions = list(quote(x.k ==                                                                                                                                       y.k))))
  )
})

test_that("parse_query() works on NATURAL RIGHT JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x NATURAL RIGHT JOIN y"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "natural right outer join", join_conditions = NA))
  )
})

test_that("parse_query() works on RIGHT JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x RIGHT JOIN y ON x.k = y.k"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "right outer join", join_conditions = list(quote(x.k ==                                                                                                                                       y.k))))
  )
})

test_that("parse_query() works on NATURAL FULL JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x NATURAL FULL JOIN y"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "natural full outer join", join_conditions = NA))
  )
})

test_that("parse_query() works on FULL JOIN", {
  expect_equal(
    parse_query("SELECT a, b FROM x FULL JOIN y ON x.k = y.k"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "full outer join", join_conditions = list(quote(x.k ==                                                                                                                                       y.k))))
  )
})

test_that("parse_query() works on JOIN with no modifying keywords", {
  expect_equal(
    parse_query("SELECT a, b FROM x JOIN y ON x.k = y.k"),
    list(select = list(quote(a), quote(b)), from = structure(list(quote(x),
      quote(y)), join_types = "inner join", join_conditions = list(quote(x.k ==                                                                                                                                       y.k))))
  )
})

test_that("parse_query() stops on LEFT INNER JOIN", {
  expect_error(
    parse_query("SELECT a, b FROM x LEFT INNER JOIN y ON x.k = y.k"),
    "Invalid"
  )
})

test_that("parse_query() stops on INNER OUTER JOIN", {
  expect_error(
    parse_query("SELECT a, b FROM x INNER OUTER JOIN y ON x.k = y.k"),
    "Invalid"
  )
})

test_that("parse_query() stops on OUTER INNER JOIN", {
  expect_error(
    parse_query("SELECT a, b FROM x OUTER INNER JOIN y ON x.k = y.k"),
    "Invalid"
  )
})

test_that("parse_query() stops on SQL-89-style (implicit) join", {
  expect_error(
    parse_query("SELECT y.w, z.x FROM y, z WHERE y.a = z.b"),
    "implicit"
  )
})

test_that("parse_query() stops on join with repeated AS in table alias", {
  expect_error(
    parse_query("SELECT a FROM bee AS AS b NATURAL JOIN see AS c"),
    "Repeated"
  )
})

test_that("parse_query() stops on join with no left table alias after AS", {
  expect_error(
    parse_query("SELECT a FROM bee AS NATURAL JOIN see AS c"),
    "Missing table alias"
  )
})

test_that("parse_query() stops on join with no right table alias after AS", {
  expect_error(
    parse_query("SELECT a FROM bee AS b NATURAL JOIN see AS"),
    "Missing table alias"
  )
})

test_that("parse_query() stops on join with missing right table reference before ON", {
  expect_error(
    parse_query("SELECT a FROM b JOIN ON b.x = c.y"),
    "Missing table reference"
  )
})

test_that("parse_query() stops on join with missing right table reference before USING", {
  expect_error(
    parse_query("SELECT a FROM b JOIN USING (z)"),
    "Missing table reference"
  )
})

test_that("parse_query() stops on join no parentheses after USING", {
  expect_error(
    parse_query("SELECT y.w, z.x FROM y JOIN z USING a"),
    "parenthes"
  )
})

test_that("parse_query() stops on join no column references in USING clause", {
  expect_error(
    parse_query("SELECT y.w, z.x FROM y JOIN z USING ()"),
    "Missing"
  )
})

test_that("parse_query() stops on join with required conditions missing", {
  expect_error(
    parse_query("SELECT y.w, z.x FROM y JOIN z"),
    "conditions"
  )
})

test_that("parse_query() stops on three-table join with required conditions missing", {
  expect_error(
    parse_query("SELECT a, b, c FROM x JOIN y JOIN z USING (t)"),
    "conditions"
  )
})

test_that("parse_query() stops on NATURAL JOIN with ON clause", {
  expect_error(
    parse_query("SELECT a, b FROM x NATURAL JOIN y ON x.k = y.k"),
    "Unexpected ON"
  )
})

test_that("parse_query() stops on NATURAL JOIN with USING clause", {
  expect_error(
    parse_query("SELECT a, b FROM x NATURAL JOIN y USING (k)"),
    "Unexpected USING"
  )
})

test_that("parse_query() stops when parentheses around table name in join", {
  expect_error(
    parse_query("SELECT a, b FROM c JOIN (d) USING (e)"),
    "parenthes"
  )
})

test_that("parse_query() stops when unexpected word or symbol in join", {
  expect_error(
    parse_query("SELECT a, b FROM c JOIN d USING (e) NOT f"),
    "Unexpected"
  )
})

test_that("parse_query() stops on incomplete join conditions", {
  expect_error(
    parse_query("SELECT a, b FROM x JOIN y ON x"),
    "Malformed"
  )
})

test_that("parse_query() stops on malformed join conditions #1", {
  expect_error(
    parse_query("SELECT a, b FROM x JOIN y ON x AND y"),
    "Malformed"
  )
})

test_that("parse_query() stops on malformed join conditions #2", {
  expect_error(
    parse_query("SELECT a, b FROM x JOIN y ON x AND y = q AND r"),
    "Malformed"
  )
})

test_that("parse_query() stops on disallowed values in USING", {
  expect_error(
    parse_query("SELECT a, b FROM x JOIN y USING(a = b)"),
    "column"
  )
})


test_that("parse_query() stops on join conditions with disallowed operators and/or functions", {
  expect_error(
    parse_query("SELECT a, b FROM x JOIN y ON sqrt(t) - 4 = 0"),
    "equality"
  )
})
