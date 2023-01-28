#' Generate the a random set of 10 suggested baby names based on the given limitations.
#'
#' @param sex A character vector which is sex of baby's name
#' @param init A character vector which the initial of baby's name
#' @param len A double vector that is the length of baby's name
#'
#' @return A list contains random suggested names based on the given limitation.
#' @export
#'
#' @examples
#' find_name('F', 'A', 3)
#> [1] "Ace" "Aya" "Ann" "Amy" "Aja" "Ada" "Avi" "Ari" "Axl" "Ani"

library(tidyverse)

find_name <- function(sex, init, length) {
  # Check input type of sex
  if (!is.character(sex)) {
    stop("sex needs to be a string.")
  }

  # Check input type of init
  if (!is.character(init)) {
    stop("init needs to be a string.")
  }

  # Check input type of length
  if (!is.numeric(length)) {
    stop("length needs to be an int type.")
  }

  # Check input value of sex
  if (!(sex == 'F' || sex == 'M' || sex == 'f' || sex == 'm')) {
    stop("sex should be either 'F'/'f' or 'M'/'m'.")
  }

  # Check input value of init
  if (nchar(init) != 1) {
    stop("init should be a single character.")
  }

  # Check input value of length
  if (length <= 0) {
    stop("length should be larger than 0.")
  }

  # Data loading and cleaning
  url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv"
  raw_df <- read.csv(url)
  raw_df <- raw_df %>% filter(n >= 100) # Keep only names that had at least 100 births for a single gender in a single year

  # Filter data based on the arguments
  df_sex <- raw_df %>% filter(sex == toupper(sex))
  df_init <- df_sex %>% filter(str_detect(name, paste0("^", toupper(init))))
  df_len <- df_init %>% filter(str_length(name) == length)

  # Create name list and randomly select 10 names
  name_list <- unique(df_len$name)
  if (length(name_list) < 10) {
    sampled <- sample(name_list, size = length(name_list))
  } else {
    sampled <- sample(name_list, size = 10)
  }

  return (sampled)
}

