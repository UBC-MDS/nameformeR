require(testthat)
require(tidyverse)
require(stringr)

test_find_unisex_name <- function() {
    # Test that the characters in the output are all alphabetic
    result <- find_unisex_name(bar = 0.02, limit = 10)
    test_that("output should contain only alphabetic characters", {
        expect_true(all(grepl("^[[:alpha:]]+$", result, perl = TRUE)))
    })

    # Test that the length of the output is correct
    result <- find_unisex_name(bar = 0.02, limit = 5)
    test_that("output should have length of 5", {
        expect_equal(length(result), 5)
    })

    # Test that no duplicate names are found in the output
    result <- find_unisex_name(bar = 0.02, limit = 10)
    test_that("output should not have any duplicate names", {
        expect_equal(length(unique(result)), 10)
    })

    # Test that changing bar value create new name list
    result1 <- find_unisex_name(bar = 0.02, limit = 10)
    result2 <- find_unisex_name(bar = 0.01, limit = 10)
    test_that("output should be different for different bar value", {
        expect_not_identical(result1, result2)
    })
}

test_find_unisex_name()
