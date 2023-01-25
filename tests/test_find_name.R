library(testthat)
library(nameformeR)

test_find_name <- function() {
  # Test the input types
  expect_error(find_name(1, "A", 3), "sex needs to be a string.")
  expect_error(find_name("F", 5, 3), "init needs to be a string.")
  expect_error(find_name("F", "A", "a"), "length needs to be an int type.")

  # Test the input values
  expect_error(find_name("a", "A", 3), "sex should be either 'F'/'f' or 'M'/'m'.")
  expect_error(find_name("F", "qw", 3), "init should be a single character.")
  expect_error(find_name("F", "?", 3), "init should be an alphabet.")
  expect_error(find_name("F", "A", 0), "length should be larger than 0.")

  # Test that the names are in the same length
  names <- find_name("F", "A", 3)
  assert_equal(length(unique(nchar(names))), 1, 'The list provides different length of names')

  # Test that the names match the required length
  assert_equal(next(iter(unique(nchar(names)))), 3, 'The list does not provide the correct length of names')

  # Test that the names are start with the same letter
  assert_equal(length(unique(substr(names, 1, 1))), 1, 'The list provides different initial letter of names')

  # Test that the names match the required initial letter
  assert_equal(next(iter(unique(substr(names, 1, 1)))), 'A', 'The list does not provide the correct initial letter of names')

  # Test that there are no duplicate names in output
  assert_equal(length(unique(names)), 10, 'The list should not have duplicated names')

  # Test that the output list size
  assert_equal(length(find_name("F", "A", 1)), 0, 'The list cannot has 10 output if the names does not exist')
}
