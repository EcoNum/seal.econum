context("plot_aa3")
library(seal.econum)


test_that("class is not aa3", {
  expect_error(plot_aa3(aa3_combine = iris), "class is not aa3")
})
