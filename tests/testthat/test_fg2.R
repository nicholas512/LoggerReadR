files_dir <- testthat::test_path("test_files")

## Test files
FG2_1 <- file.path(files_dir, "FG2_399.csv")

testthat::test_that("header recognition works", {
  expect_true(is_fg2_header("NO,TIME,#1(oC),#2(oC),#3(oC),#4(oC),#5(oC),HK-TEMP(oC),HK-BAT(V)"))
  expect_false(is_fg2_header("2,05.09.2018 16:00:00,22.1139,22.2239,22.161,22.0981,22.2318,23.86,3.629"))
})

testthat::test_that("data line recognition works", {
  expect_false(is_fg2_observation("NO,TIME,#1(oC),#2(oC),#3(oC),#4(oC),#5(oC),HK-TEMP(oC),HK-BAT(V)"))
  expect_true(is_fg2_observation("2,05.09.2018 16:00:00,22.1139,22.2239,22.161,22.0981,22.2318,23.86,3.629"))
})

testthat::test_that("Files can be read (integration test)", {
  expect_equal(read_fg2(FG2_1)$`#1(oC)`[1], 22.1375)
})
