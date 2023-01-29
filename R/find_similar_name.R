require(dplyr, quietly = TRUE)
require(comparator, quietly = TRUE)
require(readr, quietly = TRUE)
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
#' find_similar_name('Elizabeth', 5)
find_similar_name <- function(match_name, limit=10) {
  n <- NULL
  name <- NULL
  similarity <- NULL
  weight <- NULL
  # Data loading and cleaning
  url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv"
  data <- readr::read_csv(url) |>
    dplyr::filter(n >= 100) |>
    dplyr::distinct(name) |>
    dplyr::filter(name != match_name)

  # Calculate similarity scores
  data <- data |>
    dplyr::mutate(similarity = comparator::JaroWinkler()(name, match_name)) |>
    dplyr::mutate(weight = (similarity*2) ** 10)

  # Sample n names
  sample <- dplyr::slice_sample(data, n = limit, weight_by = data$weight) |>
    dplyr::arrange(dplyr::desc(weight))
  sample$name
}
