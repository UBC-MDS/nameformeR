require(testthat)
require(tidyverse)
require(stringr)

test_find_name <- function() {
  # Test that the names are in the same length
  test_that("The list provides different length of names", {
    expect_equal(length(unique(nchar(find_name("F", "A", 3)))), 1)
    })

  # Test that the names are start with the same letter
  test_that("The list provides different initial letter of names", {
    expect_equal(length(unique(substr(find_name("F", "A", 3), 1, 1))), 1)
  })

  # Test that there are no duplicate names in output
  test_that("The list should not have duplicated names", {
    expect_equal(length(unique(find_name("F", "A", 3))), 10)
  })

  # Test that the output list size
  test_that("The list cannot has 10 output if the names does not exist", {
    expect_equal(length(find_name("F", "A", 1)), 0)
  })
}

test_find_name()

