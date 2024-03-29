test_that("parse_query(tidy = TRUE) works on 'flights' example query", {
  expect_equal(
    {
      query <- "SELECT origin, dest,
          COUNT(flight) AS num_flts,
          round(AVG(distance)) AS dist,
          round(AVG(arr_delay)) AS avg_delay
        FROM flights
        WHERE distance BETWEEN 200 AND 300
          AND air_time IS NOT NULL
        GROUP BY origin, dest
        HAVING num_flts > 3000
        ORDER BY num_flts DESC, avg_delay DESC
        LIMIT 100;"
      parse_query(query, tidy = TRUE)
    },
  structure(list(select = structure(list(quote(origin), quote(dest),
    num_flts = quote(sum(!is.na(flight), na.rm = TRUE)), dist = quote(round(mean(distance,
    na.rm = TRUE))), avg_delay = quote(round(mean(arr_delay,
    na.rm = TRUE)))), aggregate = c(FALSE, FALSE, num_flts = TRUE,
    dist = TRUE, avg_delay = TRUE)), from = list(quote(flights)),
    where = list(str2lang("dplyr::between(distance, 200, 300) & !is.na(air_time)")),
    group_by = list(quote(origin), quote(dest)), having = list(
    quote(num_flts > 3000)), order_by = structure(list(str2lang("dplyr::desc(num_flts)"),
    str2lang("dplyr::desc(avg_delay)")), aggregate = c(FALSE,
    FALSE)), limit = list(100L)), aggregate = TRUE)
  )
})

test_that("parse_query(tidy = FALSE) works on 'flights' example query", {
  expect_equal(
    {
      query <- "SELECT origin, dest,
        COUNT(flight) AS num_flts,
        round(AVG(distance)) AS dist,
        round(AVG(arr_delay)) AS avg_delay
      FROM flights
      WHERE distance BETWEEN 200 AND 300
        AND air_time IS NOT NULL
      GROUP BY origin, dest
      HAVING num_flts > 3000
      ORDER BY num_flts DESC, avg_delay DESC
      LIMIT 100;"
      parse_query(query, tidy = FALSE)
    },
    structure(list(select = structure(list(quote(origin), quote(dest),
      num_flts = quote(sum(!is.na(flight), na.rm = TRUE)), dist = quote(round(mean(distance,
      na.rm = TRUE))), avg_delay = quote(round(mean(arr_delay,
      na.rm = TRUE)))), aggregate = c(FALSE, FALSE, num_flts = TRUE,
      dist = TRUE, avg_delay = TRUE)), from = list(quote(flights)),
      where = list(quote((distance >= 200 & distance <= 300) &
      !is.na(air_time))), group_by = list(quote(origin), quote(dest)),
      having = list(quote(num_flts > 3000)), order_by = structure(list(
      quote(-xtfrm(num_flts)), quote(-xtfrm(avg_delay))), aggregate = c(FALSE, FALSE)), limit = list(100L)), aggregate = TRUE)
  )
})

test_that("parse_query(tidy = TRUE) works on 'arcos' example query", {
  expect_equal(
    {
      query <- 'SELECT CAST(reporter_dea_no AS CHAR(9)) AS reporter_dea_no,
          reporter_bus_act, reporter_name, reporter_addl_co_info, reporter_address1,
          reporter_address2, reporter_city, CAST(reporter_state AS CHAR(2)) AS reporter_state,
          CAST(lpad(reporter_zip, 5, "0") AS CHAR(5)) AS reporter_zip, reporter_county,
          CAST(buyer_dea_no AS CHAR(9)) AS buyer_dea_no, buyer_bus_act, buyer_name, buyer_addl_co_info, buyer_address1,
          buyer_address2, buyer_city, CAST(buyer_state AS CHAR(2)) AS buyer_state,
          CAST(lpad(buyer_zip, 5, "0") AS CHAR(5)) AS buyer_zip,
          CAST(transaction_code AS CHAR(1)) AS transaction_code, CAST(drug_code AS CHAR(4)) AS drug_code, ndc_no, drug_name,
          quantity, unit, action_indicator, order_form_no, correction_no, strength, transaction_date,
          calc_base_wt_in_gm, dosage_unit, transaction_id, product_name, ingredient_name,
          CAST(measure AS CHAR(3)) AS measure, mme_conversion_factor, combined_labeler_name, revised_company_name,
          reporter_family, dos_str
        FROM arcos
        WHERE buyer_state NOT IN ("AE","GU","PW","MP","VI");'
      parse_query(query, tidy = TRUE)
    },
    list(select = list(reporter_dea_no = quote(as.character(reporter_dea_no)),
      quote(reporter_bus_act), quote(reporter_name), quote(reporter_addl_co_info),
      quote(reporter_address1), quote(reporter_address2), quote(reporter_city),
      reporter_state = quote(as.character(reporter_state)),
      reporter_zip = str2lang('as.character(
        stringr::str_pad(reporter_zip, 5, side = "left", pad = "0"))'), quote(reporter_county),
      buyer_dea_no = quote(as.character(buyer_dea_no)), quote(buyer_bus_act),
      quote(buyer_name), quote(buyer_addl_co_info), quote(buyer_address1),
      quote(buyer_address2), quote(buyer_city), buyer_state = quote(as.character(buyer_state)),
      buyer_zip = str2lang('as.character(
        stringr::str_pad(buyer_zip, 5, side = "left", pad = "0"))'),
      transaction_code = quote(as.character(transaction_code)),
      drug_code = quote(as.character(drug_code)), quote(ndc_no),
      quote(drug_name), quote(quantity), quote(unit), quote(action_indicator),
      quote(order_form_no), quote(correction_no), quote(strength),
      quote(transaction_date), quote(calc_base_wt_in_gm), quote(dosage_unit),
      quote(transaction_id), quote(product_name), quote(ingredient_name),
      measure = quote(as.character(measure)), quote(mme_conversion_factor),
      quote(combined_labeler_name), quote(revised_company_name),
      quote(reporter_family), quote(dos_str)), from = list(quote(arcos)),
      where = list(quote(!(buyer_state %in% c("AE", "GU", "PW",  "MP", "VI")))))
  )
})

test_that("parse_query(tidy = FALSE) works on 'arcos' example query", {
  expect_equal(
    {
      query <- 'SELECT CAST(reporter_dea_no AS CHAR(9)) AS reporter_dea_no,
        reporter_bus_act, reporter_name, reporter_addl_co_info, reporter_address1,
        reporter_address2, reporter_city, CAST(reporter_state AS CHAR(2)) AS reporter_state,
        CAST(lpad(reporter_zip, 5, "0") AS CHAR(5)) AS reporter_zip, reporter_county,
        CAST(buyer_dea_no AS CHAR(9)) AS buyer_dea_no, buyer_bus_act, buyer_name, buyer_addl_co_info, buyer_address1,
        buyer_address2, buyer_city, CAST(buyer_state AS CHAR(2)) AS buyer_state,
        CAST(lpad(buyer_zip, 5, "0") AS CHAR(5)) AS buyer_zip,
        CAST(transaction_code AS CHAR(1)) AS transaction_code, CAST(drug_code AS CHAR(4)) AS drug_code, ndc_no, drug_name,
        quantity, unit, action_indicator, order_form_no, correction_no, strength, transaction_date,
        calc_base_wt_in_gm, dosage_unit, transaction_id, product_name, ingredient_name,
        CAST(measure AS CHAR(3)) AS measure, mme_conversion_factor, combined_labeler_name, revised_company_name,
        reporter_family, dos_str
      FROM arcos
      WHERE buyer_state NOT IN ("AE","GU","PW","MP","VI");'
      parse_query(query, tidy = FALSE)
    },
    list(select = list(reporter_dea_no = quote(as.character(reporter_dea_no)),
      quote(reporter_bus_act), quote(reporter_name), quote(reporter_addl_co_info),
      quote(reporter_address1), quote(reporter_address2), quote(reporter_city),
      reporter_state = quote(as.character(reporter_state)),
      reporter_zip = quote(as.character(
        sprintf("%05s", reporter_zip))), quote(reporter_county),
      buyer_dea_no = quote(as.character(buyer_dea_no)),
      quote(buyer_bus_act), quote(buyer_name), quote(buyer_addl_co_info),
      quote(buyer_address1), quote(buyer_address2), quote(buyer_city),
      buyer_state = quote(as.character(buyer_state)), buyer_zip = quote(as.character(
        sprintf("%05s", buyer_zip))),
      transaction_code = quote(as.character(transaction_code)),
      drug_code = quote(as.character(drug_code)), quote(ndc_no),
      quote(drug_name), quote(quantity), quote(unit), quote(action_indicator),
      quote(order_form_no), quote(correction_no), quote(strength),
      quote(transaction_date), quote(calc_base_wt_in_gm), quote(dosage_unit),
      quote(transaction_id), quote(product_name), quote(ingredient_name),
      measure = quote(as.character(measure)), quote(mme_conversion_factor),
      quote(combined_labeler_name), quote(revised_company_name),
      quote(reporter_family), quote(dos_str)), from = list(quote(arcos)),
      where = list(quote(!(buyer_state %in% c("AE", "GU", "PW", "MP", "VI")))))
  )
})

test_that("parse_query(tidy = TRUE) works on 'flights' aggregate example query #1", {
  expect_equal(
    {
      query <- "SELECT year, month, day, origin, dest, carrier, flight
        FROM flights
        GROUP BY year, month, day, origin, dest, carrier, flight
        ORDER BY year, month, day, origin, dest, carrier, flight
        LIMIT 100"
      parse_query(query, tidy = TRUE)
    },
    structure(list(select = structure(list(quote(year), quote(month),
      quote(day), quote(origin), quote(dest), quote(carrier), quote(flight)), aggregate = c(FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)), from = list(quote(flights)),
      group_by = list(quote(year), quote(month), quote(day), quote(origin),
      quote(dest), quote(carrier), quote(flight)), order_by = structure(list(
      quote(year), quote(month), quote(day), quote(origin),
      quote(dest), quote(carrier), quote(flight)), aggregate = c(FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)), limit = list(
      100L)), aggregate = TRUE)
  )
})

test_that("parse_query(tidy = TRUE) works on 'flights' aggregate example query #2", {
  expect_equal(
    {
      query <- "SELECT year, month, day, origin, dest, carrier, flight
        FROM flights
        GROUP BY year, month, day, origin, dest, carrier, flight
        ORDER BY year * 10000 + month * 100 + day, origin, dest, carrier, flight
        LIMIT 100"
      parse_query(query, tidy = TRUE)
    },
    structure(list(select = structure(list(quote(year), quote(month),
      quote(day), quote(origin), quote(dest), quote(carrier), quote(flight)), aggregate = c(FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)), from = list(quote(flights)),
      group_by = list(quote(year), quote(month), quote(day), quote(origin),
      quote(dest), quote(carrier), quote(flight)), order_by = structure(list(
      quote(year * 10000 + month * 100 + day), quote(origin),
      quote(dest), quote(carrier), quote(flight)), aggregate = c(FALSE,
      FALSE, FALSE, FALSE, FALSE)), limit = list(100L)), aggregate = TRUE)
  )
})

test_that("parse_query(tidy = TRUE) works on 'flights' aggregate example query #3", {
  expect_equal(
    {
      query <- "SELECT year * 10000 + month * 100 + day, origin, dest, carrier, flight
        FROM flights
        GROUP BY year, month, day, origin, dest, carrier, flight
        ORDER BY year * 10000 + month * 100 + day, origin, dest, carrier, flight
        LIMIT 100"
      parse_query(query, tidy = TRUE)
    },
    structure(list(select = structure(list(quote(year * 10000 + month *
      100 + day), quote(origin), quote(dest), quote(carrier), quote(flight)), aggregate = c(FALSE,
      FALSE, FALSE, FALSE, FALSE)), from = list(quote(flights)), group_by = list(
      quote(year), quote(month), quote(day), quote(origin), quote(dest),
      quote(carrier), quote(flight)), order_by = structure(list(
      quote(year * 10000 + month * 100 + day), quote(origin), quote(dest),
      quote(carrier), quote(flight)), aggregate = c(FALSE, FALSE,
      FALSE, FALSE, FALSE)), limit = list(100L)), aggregate = TRUE)
  )
})

test_that("parse_query(tidy = TRUE) works on 'flights' aggregate example query #4", {
  expect_equal(
    {
      query <- "SELECT year y, year * 1000 + month * 100 + day, origin, dest, carrier, flight
        FROM flights
        GROUP BY year, month, day, origin, dest, carrier, flight
        ORDER BY y * 10000 + month * 100 + day, origin, dest, carrier, flight
        LIMIT 100"
      parse_query(query, tidy = TRUE)
    },
    structure(list(select = structure(list(y = quote(year), quote(year *
      1000 + month * 100 + day), quote(origin), quote(dest), quote(carrier),
      quote(flight)), aggregate = c(y = FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE)), from = list(quote(flights)), group_by = list(
      quote(year), quote(month), quote(day), quote(origin), quote(dest),
      quote(carrier), quote(flight)), order_by = structure(list(
      quote(y * 10000 + month * 100 + day), quote(origin), quote(dest),
      quote(carrier), quote(flight)), aggregate = c(FALSE, FALSE,
      FALSE, FALSE, FALSE)), limit = list(100L)), aggregate = TRUE)
  )
})

test_that("parse_query(tidy = TRUE) works on 'flights' SELECT DISTINCT example query #1", {
  expect_equal(
    {
      query <- "SELECT DISTINCT year, month, day, origin, dest, carrier, flight
        FROM flights
        ORDER BY year * 10000 + month * 100 + day, origin, dest, carrier, flight
        LIMIT 100"
      parse_query(query, tidy = TRUE)
    },
    list(select = structure(list(quote(year), quote(month), quote(day),
      quote(origin), quote(dest), quote(carrier), quote(flight)), distinct = TRUE),
      from = list(quote(flights)), order_by = list(quote(year *
      10000 + month * 100 + day), quote(origin), quote(dest),
      quote(carrier), quote(flight)), limit = list(100L))
  )
})

test_that("parse_query(tidy = TRUE) works on 'flights' SELECT DISTINCT example query #2", {
  expect_equal(
    {
      query <- "SELECT DISTINCT year y, month m, day d, origin, dest, carrier, flight
        FROM flights
        ORDER BY year * 10000 + month * 100 + day, origin, dest, carrier, flight
        LIMIT 100"
      parse_query(query, tidy = TRUE)
    },
    list(select = structure(list(y = quote(year), m = quote(month),
      d = quote(day), quote(origin), quote(dest), quote(carrier),
      quote(flight)), distinct = TRUE), from = list(quote(flights)),
      order_by = list(quote(year * 10000 + month * 100 + day),
      quote(origin), quote(dest), quote(carrier), quote(flight)),
      limit = list(100L))
  )
})

test_that("parse_query(tidy = TRUE) works on 'flights' SELECT DISTINCT example query #3", {
  expect_equal(
    {
      query <- "SELECT DISTINCT year y, year * 10000 + month * 100 + day, origin, dest, carrier, flight
        FROM flights
        ORDER BY year * 10000 + month * 100 + day, origin, dest, carrier, flight
        LIMIT 100"
      parse_query(query, tidy = TRUE)
    },
    list(select = structure(list(y = quote(year), quote(year * 10000 +
      month * 100 + day), quote(origin), quote(dest), quote(carrier),
      quote(flight)), distinct = TRUE), from = list(quote(flights)),
      order_by = list(quote(year * 10000 + month * 100 + day),
      quote(origin), quote(dest), quote(carrier), quote(flight)),
      limit = list(100L))

  )
})

test_that("parse_query(tidy = TRUE) works on 'flights' SELECT DISTINCT example query #4", {
  expect_equal(
    {
      query <- "SELECT DISTINCT year * 10000 + month * 100 + day, origin, dest, carrier, flight
        FROM flights
        ORDER BY year * 10000 + month * 100 + day, origin, dest, carrier, flight
        LIMIT 100"
      parse_query(query, tidy = TRUE)
    },
    list(select = structure(list(quote(year * 10000 + month * 100 +
      day), quote(origin), quote(dest), quote(carrier), quote(flight)), distinct = TRUE),
      from = list(quote(flights)), order_by = list(quote(year *
      10000 + month * 100 + day), quote(origin), quote(dest),
      quote(carrier), quote(flight)), limit = list(100L))
  )
})

test_that("parse_query(tidy = TRUE) works on 'flights' SELECT DISTINCT example query #5", {
  skip("currently unsupported")
  expect_success(
    {
      query <- "SELECT DISTINCT year y, year * 10000 + month * 100 + day, origin, dest, carrier, flight
        FROM flights
        ORDER BY y * 10000 + month * 100 + day, origin, dest, carrier, flight
        LIMIT 100"
      parse_query(query, tidy = TRUE)
    }
  )
})

test_that("parse_query() works on SELECT ALL example query", {
  expect_equal(
    parse_query("SELECT ALL year, month, day FROM flights"),
    list(select = list(quote(year), quote(month), quote(day)), from = list(
      quote(flights)))
  )
})

test_that("parse_query() works with NULLS FIRST in ORDER BY", {
  expect_equal(
    parse_query("SELECT x FROM df ORDER BY x NULLS FIRST"),
    list(select = list(quote(x)),
      from = list(quote(df)),
      order_by = list(quote(!is.na(x)), quote(x)))
  )
})

test_that("parse_query() works with NULLS LAST in ORDER BY", {
  expect_equal(
    parse_query("SELECT x FROM df ORDER BY x NULLS LAST"),
    list(select = list(quote(x)),
      from = list(quote(df)),
      order_by = list(quote(is.na(x)), quote(x)))
  )
})

test_that("parse_query() works with ASC/DESC and NULLS FIRST/LAST in ORDER BY", {
  expect_equal(
    parse_query("SELECT w, x, y, z FROM df
      ORDER BY w ASC NULLS FIRST, x DESC NULLS FIRST, y ASC NULLS LAST, z DESC NULLS LAST"),
    list(select = list(quote(w), quote(x), quote(y), quote(z)), from = list(quote(df)),
      order_by = list(quote(!is.na(w)), quote(w), quote(!is.na(x)),
      quote(-xtfrm(x)), quote(is.na(y)), quote(y), quote(is.na(z)),
      quote(-xtfrm(z))))
  )
})

test_that("parse_query(tidy = TRUE) works with ASC/DESC and NULLS FIRST/LAST in ORDER BY", {
  expect_equal(
    parse_query("SELECT w, x, y, z FROM df
        ORDER BY w ASC NULLS FIRST, x DESC NULLS FIRST, y ASC NULLS LAST, z DESC NULLS LAST",
      tidyverse = TRUE
    ),
    list(select = list(quote(w), quote(x), quote(y), quote(z)), from = list(quote(df)),
      order_by = list(quote(!is.na(w)), quote(w), quote(!is.na(x)),
      str2lang("dplyr::desc(x)"), quote(is.na(y)), quote(y), quote(is.na(z)),
      str2lang("dplyr::desc(z)")))
  )
})

test_that("parse_query() stops on positional column references in ORDER BY clause", {
  expect_error(
    parse_query("SELECT x, y, z FROM t ORDER BY 1 DESC, 2"),
    "^Positional"
  )
})

test_that("parse_query() stops when unvalid expression used in an aggregate query", {
  expect_error(
    parse_query("SELECT x FROM y GROUP BY z"),
    "aggregation"
  )
})

test_that("parse_query() stops when unvalid expression used in a SELECT DISTINCT query", {
  expect_error(
    parse_query("SELECT DISTINCT x FROM y ORDER BY z"),
    "DISTINCT"
  )
})

test_that("parse_query() stops on incomplete CAST expression", {
  expect_error(
    parse_query("SELECT CAST(x) FROM y"),
    "CAST"
  )
})

test_that("parse_query() stops on malformed CAST expression", {
  expect_error(
    parse_query("SELECT CAST(x) AS y FROM z"),
    "CAST"
  )
})

test_that("parse_query() stops on incomplete TRY_CAST expression", {
  expect_error(
    parse_query("SELECT TRY_CAST(x) FROM y"),
    "TRY_CAST"
  )
})

test_that("parse_query() stops on malformed TRY_CAST expression", {
  expect_error(
    parse_query("SELECT TRY_CAST(x) AS y FROM z"),
    "TRY_CAST"
  )
})

test_that("parse_query() stops on incomplete BETWEEN expression", {
  expect_error(
    parse_query("SELECT x FROM y WHERE a BETWEEN b"),
    "BETWEEN"
  )
})

test_that("parse_query() stops when invalid object name in FROM clause", {
  expect_error(
    parse_query("SELECT * FROM `1a`"),
    "^Invalid name"
  )
})

test_that("parse_query() stops when expression (not name) in FROM clause", {
  expect_error(
    parse_query("SELECT * FROM 1+2+3"),
    "^Invalid name"
  )
})

test_that("parse_query() stops when invalid namespace name in FROM clause", {
  expect_error(
    parse_query("SELECT * FROM `1a`::foo"),
    "^Invalid name"
  )
})

test_that("parse_query() stops when aggregate expression in WHERE clause", {
  expect_error(
    parse_query("SELECT x WHERE COUNT(*) > 2"),
    "^Aggregate"
  )
})

test_that("parse_query() stops when aggregate expression in GROUP BY clause", {
  expect_error(
    parse_query("SELECT x GROUP BY SUM(x)"),
    "^Aggregate"
  )
})

test_that("parse_query() stops when invalid use of ASC in ORDER BY clause", {
  expect_error(
    parse_query("SELECT x FROM y ORDER BY x, ASC"),
    "^Invalid"
  )
})

test_that("parse_query() stops when invalid use of DESC in ORDER BY clause", {
  expect_error(
    parse_query("SELECT x FROM y ORDER BY x, DESC"),
    "^Invalid"
  )
})

test_that("parse_query() stops when invalid use of NULLS FIRST in ORDER BY clause", {
  expect_error(
    parse_query("SELECT x FROM y ORDER BY x, NULLS FIRST"),
    "^Invalid"
  )
})

test_that("parse_query() stops when invalid use of NULLS LAST in ORDER BY clause", {
  expect_error(
    parse_query("SELECT x FROM y ORDER BY x, NULLS LAST"),
    "^Invalid"
  )
})

test_that("parse_query() stops when expression in LIMIT clause", {
  expect_error(
    parse_query("SELECT x FROM y LIMIT sqrt(100)"),
    "constant"
  )
})

test_that("parse_query() stops when column name in LIMIT clause", {
  expect_error(
    parse_query("SELECT x FROM y LIMIT z"),
    "constant"
  )
})

test_that("parse_query() stops when input is not a character vector", {
  expect_error(
    parse_query(42),
    "^Unexpected"
  )
})

test_that("parse_query() stops when input is has length > 1", {
  expect_error(
    parse_query(c("SELECT w FROM x", "SELECT y FROM z")),
    "^Unexpected"
  )
})

test_that("parse_query() stops when query does not begin with SELECT", {
  expect_error(
    parse_query("ORDER BY z"),
    "begin"
  )
})

test_that("parse_query() stops on OVER clauses", {
  expect_error(
    parse_query("SELECT SUM(x) OVER(PARTITION BY y) FROM z"),
    "OVER"
  )
})

test_that("parse_query() stops on UNION", {
  expect_error(
    parse_query("SELECT * FROM x UNION SELECT * FROM y"),
    "UNION"
  )
})

test_that("parse_query() stops on INTERSECT", {
  expect_error(
    parse_query("SELECT * FROM x INTERSECT SELECT * FROM y"),
    "INTERSECT"
  )
})

test_that("parse_query() stops on EXCEPT", {
  expect_error(
    parse_query("SELECT * FROM x EXCEPT SELECT * FROM y"),
    "EXCEPT"
  )
})

test_that("parse_query() stops on subquery", {
  expect_error(
    parse_query("SELECT * FROM table WHERE x IN (SELECT z FROM table2) t;"),
    "^Subqueries"
  )
})

test_that("parse_query() stops on unmatched quotation marks", {
  expect_error(
    parse_query("SELECT 'hello FROM x"),
    "unmatched"
  )
})

test_that("parse_query() stops on unmatched quotation parentheses", {
  expect_error(
    parse_query("SELECT sqrt(16 FROM y"),
    "unmatched"
  )
})

test_that("parse_query() stops on repated clause", {
  expect_error(
    parse_query("SELECT x FROM y WHERE z WHERE w"),
    "times"
  )
})

test_that("parse_query() stops on clauses in incorrect order", {
  expect_error(
    parse_query("SELECT x GROUP BY z FROM y"),
    "order"
  )
})

test_that("parse_query() removes table name prefixes in single-table queries", {
  expect_equal(
    {
      query <- "SELECT flights.year, flights.month, flights.day
        FROM flights
        WHERE flights.arr_delay <= 0
        ORDER BY flights.carrier"
      parse_query(query)
    },
    list(select = list(quote(year), quote(month), quote(day)), from = list(quote(flights)),
      where = list(quote(arr_delay <= 0)), order_by = structure(list(quote(carrier))))
  )
})

test_that("parse_query() removes table alias prefixes in single-table queries", {
  expect_equal(
    {
      query <- "SELECT f.year, f.month, f.day
        FROM flights f
        WHERE f.arr_delay <= 0
        ORDER BY f.carrier"
      parse_query(query)
    },
    list(select = list(quote(year), quote(month), quote(day)), from = list(f = quote(flights)),
      where = list(quote(arr_delay <= 0)), order_by = structure(list(quote(carrier))))
  )
})

test_that("parse_query() stops when column alias is disallowed", {
  expect_error(
    parse_query("SELECT x AS character FROM y"),
    "disallowed"
  )
})

test_that("parse_query() stops when table alias is disallowed", {
  expect_error(
    parse_query("SELECT x FROM y 'is'"),
    "disallowed"
  )
})

test_that("parse_query() succeeds when parentheses around table name", {
  expect_error(
    parse_query("SELECT * FROM (ex)"),
    NA
  )
})

test_that("parse_query() succeeds when parentheses around table name and alias after", {
  expect_error(
    parse_query("SELECT * FROM (ex) x"),
    NA
  )
})

test_that("parse_query() succeeds with test query #1 on syntactically invalid column name in backticks", {
  expect_error(
    parse_query("SELECT x.`y z` FROM x"),
    NA
  )
})

test_that("parse_query() succeeds with test query #2 on syntactically invalid column name in backticks", {
  expect_error(
    parse_query("SELECT sqrt(x.`y z`) FROM x"),
    NA
  )
})

test_that("parse_query() succeeds with test query #3 on syntactically invalid column name in backticks", {
  expect_error(
    parse_query("SELECT round(x.`y z`, 2) FROM x"),
    NA
  )
})

# generate result objects like this:
#result <- parse_query("SELECT ...")
#str2lang(queryparser:::deparse(result, width.cutoff = 500L, control = c("quoteExpressions", "showAttributes", "niceNames")))
