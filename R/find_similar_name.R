#' Generate a random list of names that sound similar to a given user input name.
#' Uses Match rating approach algorithm to determine similarity
#'
#' @param match_name A character vector that is the name for comparison; output names will sound like this one
#' @param limit An integer vector that is the number of names in the output list. Default 10
#'
#' @return A vector containing random suggested similar names based on the given name.
#' @export
#'
#' @examples
#' >>> find_similar_name('Elizabeth', 5)
#' >>> [1] "Elizabet" "Elijah"   "Elvis"    "Gidget"   "Kelcie"

library(dplyr)
library(comparator)

find_similar_name <- function(match_name, limit=10) {

  # Data loading and cleaning
  url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv"
  data <- read.csv(url) |>
    filter(n >= 100) |>
    distinct(name) |>
    filter(name != match_name)

  # Calculate similarity scores
  data <- data |>
    mutate(similarity = JaroWinkler()(name, match_name)) |>
    mutate(weight = (similarity*2) ** 10)

  # Sample n names
  sample <- slice_sample(data, n = limit, weight_by = data$weight) |>
    arrange(desc(weight))
  sample$name
}

print(find_similar_name("Elizabeth", 5))

library(testthat)
library(stringr)

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
