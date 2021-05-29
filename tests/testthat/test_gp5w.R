files_dir <- testthat::test_path("test_files")

## Test files
GP5_1 <- file.path(files_dir, "GP5W.csv")
GP5_2 <- file.path(files_dir, "GP5W_260.csv")
GP5_3 <- file.path(files_dir, "GP5W_270.csv")

testthat::test_that("header recognition works", {
  expect_true(is_gp5w_header("No,Time,#1:oC,#HK-Bat:V"))
  expect_false(is_gp5w_header("1,30.04.2015 15:36:51,22.6166,3.633"))
})

testthat::test_that("data line recognition works", {
  expect_false(is_gp5w_observation("No,Time,#1:oC,#HK-Bat:V"))
  expect_true(is_gp5w_observation("1,30.04.2015 15:36:51,22.6166,3.633"))
})

testthat::test_that("Files can be read (integration test", {
  expect_equal(read_gp5w(GP5_1)$`#1:oC`[1], 22.6166)
  expect_equal(read_gp5w(GP5_2)$`#1:oC`[1], 23.86)
  expect_equal(read_gp5w(GP5_3)$`#1:oC`[1], 22.61)
})

testthat::test_that("padding works as expected", {
  expect_equal(pad_missing_separators(list("1,2,3"), 5, ","), "1,2,3,,")
  expect_equal(pad_missing_separators(list("1,2,3"), 3, ","), "1,2,3")
})
