files_dir <- testthat::test_path("test_files")

testthat::test_that("separate dates works", {
  expect_true(hbw_detect_separate_date_time("Date;Time;sensor"))
  expect_true(hbw_detect_separate_date_time("Date,Time,sensor"))
  expect_true(hbw_detect_separate_date_time(list("infoline", "Date;Time;sensor", "monday;3:00 PM;-3")))
  expect_false(hbw_detect_separate_date_time("Date Time,sensor"))
  expect_false(hbw_detect_separate_date_time(list("infoline", "Date Time();sensor", "monday;3:00 PM;-3")))
  expect_error(hbw_detect_separate_date_time(c("cats","sheep","horses")))
})


testthat::test_that("time format detection works", {
  expect_false(hbw_detect_time_format_24h(list("2,08/18/10,03:00:00 PM,20.174,20.317,,,,",
                                           "3,08/18/10,04:00:00 PM,6.458,6.611,,,,")))

})
