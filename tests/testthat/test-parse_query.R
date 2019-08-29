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
        HAVING num_flts > 5000
        ORDER BY num_flts DESC, avg_delay DESC
        LIMIT 100;"
      parse_query(query, tidy = TRUE)
    },
    list(select = list(quote(origin), quote(dest), num_flts = quote(sum(!is.na(flight))),
      dist = quote(round(mean(distance, na.rm = TRUE))), avg_delay = quote(round(mean(arr_delay,
      na.rm = TRUE)))), from = list(quote(flights)), where = list(
      str2lang("dplyr::between(distance, 200, 300) && !is.na(air_time)")), group_by = list(
      quote(origin), quote(dest)), having = list(quote(num_flts >
      5000)), order_by = list(str2lang("dplyr::desc(num_flts)"), str2lang("dplyr::desc(avg_delay)")),
      limit = list(100L))
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
      HAVING num_flts > 5000
      ORDER BY num_flts DESC, avg_delay DESC
      LIMIT 100;"
      parse_query(query, tidy = FALSE)
    },
    list(select = list(quote(origin), quote(dest), num_flts = quote(sum(!is.na(flight))),
      dist = quote(round(mean(distance, na.rm = TRUE))), avg_delay = quote(round(mean(arr_delay,
      na.rm = TRUE)))), from = list(quote(flights)), where = list(
      quote((distance >= 200 & distance <= 300) && !is.na(air_time))), group_by = list(
      quote(origin), quote(dest)), having = list(quote(num_flts >
      5000)), order_by = structure(list(quote(num_flts), quote(avg_delay)), descreasing = c(TRUE,
      TRUE)), limit = list(100L))
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
