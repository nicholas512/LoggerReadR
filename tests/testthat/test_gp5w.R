test_that("header recognition works", {
  expect_true(is_gp5w_header("No,Time,#1:oC,#HK-Bat:V"))
  expect_false(is_gp5w_header("1,30.04.2015 15:36:51,22.6166,3.633"))
})

test_that("data line recognition works", {
  expect_false(is_gp5w_observation("No,Time,#1:oC,#HK-Bat:V"))
  expect_true(is_gp5w_observation("1,30.04.2015 15:36:51,22.6166,3.633"))
})
