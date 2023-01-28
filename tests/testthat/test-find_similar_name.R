require(stringr)
require(dplyr)
require(comparator)
require(readr)

test_that("n names are output", {
  expect_length(find_similar_name("Daniel", 5), 5)
})

test_that("no duplicate names", {
  expect_length(unique(find_similar_name("Daniel", 10)), 10)
})

test_that("repeated calls generate different names",{
  expect_false(isTRUE(all.equal(find_similar_name("Daniel"), find_similar_name("Daniel"))))
  # https://stackoverflow.com/questions/12111863/expect-not-equal-in-pkgtestthat
})

test_that("similarity seems to be working", {
  expect_true(any(str_detect(find_similar_name("Daniel", 3), "^D")))
})
