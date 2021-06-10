
testthat::test_that("separate dates works", {
  expect_true(hbw_detect_separate_date_time("Date;Time;sensor"))
  expect_true(hbw_detect_separate_date_time("Date,Time,sensor"))
  expect_true(hbw_detect_separate_date_time(list("infoline", "Date;Time;sensor", "monday;3:00 PM;-3")))
  expect_false(hbw_detect_separate_date_time("Date Time,sensor"))
  expect_false(hbw_detect_separate_date_time(list("infoline", "Date Time();sensor", "monday;3:00 PM;-3")))
  expect_error(hbw_detect_separate_date_time(c("cats","sheep","horses")))
})


testthat::test_that("time zone detected from header line", {
  expect_equal(hbw_detect_time_zone_from_header_line(hobo_header_1), "-0700")
})


testthat::test_that("header lines identified", {
  expect_true(hbw_is_header_line(hobo_header_1, FALSE, FALSE, ","))
  expect_true(hbw_is_header_line(hobo_header_2, FALSE, TRUE, "\t"))
})


testthat::test_that("Identify data headers", {
  expect_true(hbw_is_data_header('Temp (°C)  #9724974'))
  expect_true(hbw_is_data_header('"Temp, °F (LGR S/N: 9724974, SEN S/N: 9724974)"'))
  expect_true(hbw_is_data_header('Temp...C....9724974'))
  expect_false(hbw_is_data_header('Coupler Detached'))
  expect_false(hbw_is_data_header('Coupler Detached'))
  expect_false(hbw_is_data_header(''))
  expect_false(hbw_is_data_header(NA))

})


testthat::test_that("Identify date format", {
  expect_equal(hbw_detect_date_format(hobo_lines_1), "DMY")
  expect_equal(hbw_detect_date_format(hobo_lines_2), "MDY")
  expect_equal(hbw_detect_date_format(hobo_lines_3), "MDY")
})


testthat::test_that("Plot details detected", {
  expect_false(hbw_detect_include_plot_details("1,08/18/10 02:00:00  PM,69.557,69.771,Logged,,,"))
  expect_false(hbw_detect_include_plot_details("Plot Title: 9724974"))
  expect_false(hbw_detect_include_plot_details("Plot Title: 9724974"))
  expect_true(hbw_detect_include_plot_details(c("08/18/10 17:00:00.0\t7.920	8.394\t\t\t\t\tDevice Info")))

})


testthat::test_that("Hobo details returned in the expected way", {
  expect_equal(ncol(read_hoboware_details(hobo_file_2, hobo.config.classic())), 2)
  expect_equal(read_hoboware_details(hobo_file_2, hobo.config.classic())[1,1], "Series")
  expect_equal(as.numeric(read_hoboware_details(hobo_file_2, hobo.config.classic())[4,2]), 1.09)
  expect_equal(read_hoboware_details(hobo_file_3, hobo.config.defaults()), NA)

})

testthat::test_that("'No quotes or commas' property can be detected", {
  expect_true(hbw_detect_no_quotes_commas(hobo_lines_2))
  expect_false(hbw_detect_no_quotes_commas(hobo_lines_1))
  expect_false(hbw_detect_no_quotes_commas(c('"Plot Title: 9724974","#","Date","Time, GMT-07:00"','1,08/18/10,02:00:00 PM,20.865')))
  expect_false(hbw_detect_no_quotes_commas("\"#\",\"Date\",\"Time, GMT-07:00\",\"Temp, °C (LG"))
  })

testthat::test_that("datetime headers detected", {
  expect_true(hbw_is_datetime_header("Date"))
  expect_true(hbw_is_datetime_header("Time"))
  expect_true(hbw_is_datetime_header("Date.Time"))
  expect_true(hbw_is_datetime_header("Date.Time, GMT-07:00"))
  expect_false(hbw_is_datetime_header("Coupler Detached (LGR S/N: 9724974)"))
  expect_false(hbw_is_datetime_header("Coupler Detached (LGR S/N: 9724974)"))
  })

testthat::test_that("time format detected", {
  fmt1 <- hbw_parse_time_format("3,08/18/10,04:00:00 PM,6.458,6.611,,,,")

  expect_equal(fmt1$dsep, "/")
  expect_equal(fmt1$dtdelim, ",")
  expect_equal(fmt1$ampm, "PM")
  expect_equal(fmt1$deci, "")

  fmt2 <- hbw_parse_time_format("3,08-18-10,04:00:00.0,6.458,6.611,,,,")
  expect_equal(fmt2$dsep, "-")
  expect_equal(fmt2$ampm, "")
  expect_equal(fmt2$dtdelim, ",")
  expect_equal(fmt2$dec, ".")

  fmt3 <- hbw_parse_time_format("09/28/10 21:00:00.0\t0.934\t0.880\t\t\t\t")
  expect_equal(fmt3$dsep,"/")
  expect_equal(fmt3$dtdelim, " ")
})
