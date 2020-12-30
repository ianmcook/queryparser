test_that("expected result with no warnings when Unicode characters in literal strings", {
  skip_on_os("windows")
  skip_on_cran()
  expect_warning(
    result <- parse_query("select 'ä𝒷Ç', '𝓧𝕪𝖟'"),
    regexp = NA
  )
  expect_equal(
    result,
    structure(list(select = c("ä𝒷Ç", "𝓧𝕪𝖟")))
  )
})

test_that("expected result with no warnings when Unicode characters in column names", {
  skip_on_os("windows")
  skip_on_cran()
  expect_warning(
    result <- parse_query("select xyž, ⓑåᏒ, Ω"),
    regexp = NA
  )
  expect_equal(
    result,
    structure(list(select = list(str2lang("xyž"), str2lang("ⓑåᏒ"), str2lang("Ω"))))
  )
})


test_that("expected result with no warnings when Unicode characters in column aliases", {
  skip_on_os("windows")
  skip_on_cran()
  expect_warning(
    result <- parse_query("select 1 'xyž', 'foo' AS ⓑåᏒ, omega'Ω'"),
    regexp = NA
  )
  expect_equal(
    result,
    structure(list(select = list("xyž" = 1, "ⓑåᏒ"= "foo", "Ω" = quote(omega))))
  )
})

test_that("parse_query() works on example cast() query with Unicode characters", {
  skip_on_os("windows")
  skip_on_cran()
  expect_equal(
    parse_query("SELECT CAST(ⓓîßtãñçℰ AS int) FROM 𝖋𝖑𝖎𝖌𝖍𝖙𝖘"),
    list(select = structure(list(str2lang("as.integer(ⓓîßtãñçℰ)"))), from = list(str2lang("𝖋𝖑𝖎𝖌𝖍𝖙𝖘")))
  )
})

test_that("parse_query(tidy = TRUE) works on 'flights' example query with Unicode characters", {
  skip_on_os("windows")
  skip_on_cran()
  expect_equal(
    {
      #utflights <- flights %>% rename("ⓞⓡïgįñ" = "origin", "dèšⓣ" = "dest", "ⓓîßtãñçℰ" = "distance")
      query <- "SELECT ⓞⓡïgįñ, dèšⓣ,
          COUNT(flight) AS num_flts,
          round(AVG(ⓓîßtãñçℰ)) AS ⓓîßt,
          round(AVG(arr_delay)) AS avg_delay
        FROM utflights
        WHERE ⓓîßtãñçℰ BETWEEN 200 AND 300
          AND air_time IS NOT NULL
        GROUP BY ⓞⓡïgįñ, dèšⓣ
        HAVING num_flts > 3000
        ORDER BY num_flts DESC, avg_delay DESC
        LIMIT 100;"
      parse_query(query, tidy = TRUE)
    },
    structure(list(select = structure(list(str2lang("ⓞⓡïgįñ"),str2lang("dèšⓣ"),
      num_flts = quote(sum(!is.na(flight))), "ⓓîßt" = str2lang("round(mean(ⓓîßtãñçℰ, na.rm = TRUE))"),
      avg_delay = quote(round(mean(arr_delay,
      na.rm = TRUE)))), aggregate = c(FALSE, FALSE, num_flts = TRUE,
      "ⓓîßt" = TRUE, avg_delay = TRUE)), from = list(quote(utflights)),
      where = list(str2lang("dplyr::between(ⓓîßtãñçℰ, 200, 300) & !is.na(air_time)")),
      group_by = list(str2lang("ⓞⓡïgįñ"), str2lang("dèšⓣ")), having = list(
      quote(num_flts > 3000)), order_by = structure(list(str2lang("dplyr::desc(num_flts)"),
      str2lang("dplyr::desc(avg_delay)")), aggregate = c(FALSE,
      FALSE)), limit = list(100L)), aggregate = TRUE)
  )
})
