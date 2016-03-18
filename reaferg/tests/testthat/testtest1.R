library(smseglm)
library(stringr)
context("Smoothly Segemented LM")

test_that("str_length is number of character",{
  expect_equal(str_length("a"),1)
  expect_equal(str_length("ab"),2)
  expect_equal(str_length("abc"),3)
})
  