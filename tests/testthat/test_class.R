context("calb_aa3")
library(seal.econum)


test_that("class is not aa3", {
  expect_error(calb_aa3(x = iris), "class is not aa3")
})
