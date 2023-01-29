require(testthat,quietly = TRUE)
require(tidyverse,quietly = TRUE)
require(stringr,quietly = TRUE)
require(readr,quietly = TRUE)

#' Generate the a random set of 10 suggested neutral baby names based on the given limitation and baby names in the past years.
#'
#' @param limit A float controls the minimum proportion of the neutral names in a single year in the dataframe.
#' @param count The length of the output with a default value of 10.
#' @return  A word list containing random suggested neutral names based on the given limitation
#' If the length of the word list is less than the limitation, it will return all the names in the word list
#' @export
#'
#' @examples
#' find_unisex_name(0.002,limit=10)
find_unisex_name <- function(bar, limit = 10) {
  
  # check input value
  if (!(bar > 0 && bar < 1)) {
    stop("bar value should be within 0 and 1")
  }
  
  # Load data
  url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv"
  data <- readr::read_csv(url)
  
  # data wrangling
  bar_data_boy <- data[(data$prop > bar) & (data$sex == "M"), ] # select all the boy names with prop greater than bar
  bar_data_girl <- data[(data$prop > bar) & (data$sex == "F"), ] # select all the girl names with prop greater than bar
  target_names <- intersect(bar_data_girl$name, bar_data_boy$name) # a vector of unisex name 
  
  # Create name list
  common_unisex_names <- c("Skylar", "Azariah", "Royal", "Hayden", "Emerson", 
                           "Rowan", "Baylor", "Dakota", "River", "Emory","Jessie",
                           "Marion","Jackie","Alva","Ollie","Jodie","Cleo","Kerry") # add default names if limit is too high
  if (length(target_names) < limit) {
    result <- c(target_names, sample(common_unisex_names, limit - length(target_names)))
  } 
    else {
    result <- sample(target_names, limit)
  }
  
  return(result)
}
