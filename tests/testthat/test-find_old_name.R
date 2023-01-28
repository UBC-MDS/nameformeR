require(testthat)
require(tidyverse)
require(stringr)

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
    data <-  read_csv(URL,col_types = cols())
    data <- data |> filter(sex == "M") |> pull(name)
    call = find_old_name("1980s", sex = "M")
    expect_equal(sum(call %in% data), 10)
  })
}

test_find_old_name()


