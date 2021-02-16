#context("check-output")  # Our file is called "test-check_output.R"
library(testthat)        # load testthat package
library(NPIstats)       # load our package

# Test whether we get the same lower and upper prob


test_that("Test best.pair Lower",{
  eps <- 0.0001
  data1<-split(BreakdownTimes$times, BreakdownTimes$group)
  expected <- 0.5372
  actual <- best.pair(data1$X, data1$Y)[1]
  expect_lt(abs(expected - actual), eps)
})



test_that("Test best.pair Upper",{
  eps <- 0.0001
  data1<-split(BreakdownTimes$times, BreakdownTimes$group)
  expected <- 0.7273
  actual <- best.pair(data1$X, data1$Y)[2]
  expect_lt(abs(expected - actual), eps)
})
