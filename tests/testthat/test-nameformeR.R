require(testthat)
require(stringr)
require(dplyr)
require(comparator)
require(readr)
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

test_find_old_name <- function() {
  # Test that length of list is as expected
  test_that("The list provides different length of names", {
    expect_equal(length(find_old_name("1980s", limit=3)), 3)
  })

  # Test that repeated calls with given seed generate same names
  test_that("The lists generated with given seed are not the same.", {
    first_call = find_old_name("1980s",seed=123)
    second_call = find_old_name("1980s",seed=123)
    expect_equal(first_call, second_call)
  })

  # Test that repeated calls generate different names
  test_that("The list generated is not random", {
    first_call = find_old_name("1980s")
    second_call = find_old_name("1980s")
    expect_equal(sum(first_call == second_call),0)
  })

  # Test that name is actually of given sex
  test_that("Does not give name of right sex", {
    URL <-  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv"
    data <-  readr::read_csv(URL,col_types = readr::cols())
    data <- data |> dplyr::filter(sex == "M") |> dplyr::pull(name)
    call <- find_old_name("1980s", sex = "M")
    expect_equal(sum(call %in% data), 10)
  })
}

test_find_similar_name <- function() {
  # Test that name is in required length
  test_that("n names are output", {
    expect_length(find_similar_name("Daniel", 5), 5)
  })

  # Test that name is not duplicated in one call
  test_that("no duplicate names", {
    expect_length(unique(find_similar_name("Daniel", 10)), 10)
  })

  # Test that name is not duplicated in different call
  test_that("repeated calls generate different names",{
    expect_false(isTRUE(all.equal(find_similar_name("Daniel"), find_similar_name("Daniel"))))
    # https://stackoverflow.com/questions/12111863/expect-not-equal-in-pkgtestthat
  })

  # Test that names are similar to the given name
  test_that("similarity seems to be working", {
    expect_true(any(str_detect(find_similar_name("Daniel", 3), "^D")))
  })
}

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
    expect_equal(sum(result1 == result2),0)
  })
}

test_find_name()

test_find_old_name()

test_find_similar_name()

test_find_unisex_name()


