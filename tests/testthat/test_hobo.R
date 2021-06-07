
testthat::test_that("separate dates works", {
  expect_true(hbw_detect_separate_date_time("Date;Time;sensor"))
  expect_true(hbw_detect_separate_date_time("Date,Time,sensor"))
  expect_true(hbw_detect_separate_date_time(list("infoline", "Date;Time;sensor", "monday;3:00 PM;-3")))
  expect_false(hbw_detect_separate_date_time("Date Time,sensor"))
  expect_false(hbw_detect_separate_date_time(list("infoline", "Date Time();sensor", "monday;3:00 PM;-3")))
  expect_error(hbw_detect_separate_date_time(c("cats","sheep","horses")))
})


testthat::test_that("time zone detected from header line", {
  expect_equal(detect_time_zone_from_header_line(hobo_header_1), "-0700")
})


testthat::test_that("header lines identified", {
  expect_true(is_header_line(hobo_header_1, FALSE, FALSE, ","))
  expect_true(is_header_line(hobo_header_2, FALSE, TRUE, "\t"))
})


testthat::test_that("Identify data headers", {
  expect_true(is_data_header('Temp (°C)  #9724974', TRUE))
  expect_true(is_data_header('"Temp, °F (LGR S/N: 9724974, SEN S/N: 9724974)"', FALSE))
  expect_false(is_data_header('Coupler Detached', TRUE))
  expect_false(is_data_header('Coupler Detached', FALSE))
  expect_false(is_data_header('', TRUE))
  expect_false(is_data_header(NA, TRUE))
})


testthat::test_that("Identify date format", {
  expect_equal(hbw_detect_date_format(hobo_lines_1), "DMY")
  expect_equal(hbw_detect_date_format(hobo_lines_2), "MDY")
  expect_equal(hbw_detect_date_format(hobo_lines_3), "MDY")
})


testthat::test_that("'No quotes or commas' property can be detected", {
  expect_true(hbw_detect_no_quotes_commas(hobo_lines_2))
  expect_false(hbw_detect_no_quotes_commas(hobo_lines_1))
  expect_false(hbw_detect_no_quotes_commas(c('"Plot Title: 9724974","#","Date","Time, GMT-07:00"','1,08/18/10,02:00:00 PM,20.865')))
  expect_false(hbw_detect_no_quotes_commas("\"#\",\"Date\",\"Time, GMT-07:00\",\"Temp, °C (LG"))
  })

