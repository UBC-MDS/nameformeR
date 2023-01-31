require(stringr,quietly = TRUE)
require(readr,quietly = TRUE)
require(comparator, quietly = TRUE)
require(dplyr, quietly = TRUE)
#' Generate the a random set of 10 suggested baby names based on the given limitations.
#'
#' @param sex A character vector which is sex of baby's name
#' @param init A character vector which the initial of baby's name
#' @param length A double vector that is the length of baby's name
#'
#' @return A list contains random suggested names based on the given limitation.
#' @export
#'
#' @examples
#' find_name('F', 'A', 3)
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
  name <- NULL
  n <- NULL
  url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv"
  raw_df <- readr::read_csv(url,col_types = readr::cols())
  raw_df <- raw_df |> dplyr::filter(n >= 100) # Keep only names that had at least 100 births for a single gender in a single year

  # Filter data based on the arguments
  df_sex <- raw_df |> dplyr::filter(sex == toupper(sex))
  df_init <- df_sex |> dplyr::filter(stringr::str_detect(name, paste0("^", toupper(init))))
  df_len <- df_init |> dplyr::filter(stringr::str_length(name) == length)

  # Create name list and randomly select 10 names
  name_list <- unique(df_len$name)
  if (length(name_list) < 10) {
    sampled <- sample(name_list, size = length(name_list))
  } else {
    sampled <- sample(name_list, size = 10)
  }

  return (sampled)
}

#' Generate the a random set of 10 suggested neutral(by default) baby names based on the given time period and sex.
#'
#' @param tp A character vector that specifying time period from 1880 to 2018
#' @param limit A double vector that is the number of name in the output list. Default 10
#' @param sex A character vector that is the aimed sex of the name in the output. Default "uni"
#' @param seed A seed for generating same output. Default NULL
#'
#' @return A list contains random suggested old names based on the given limitation.
#' @export
#'
#' @examples
#' find_old_name('1980s')
find_old_name <- function(tp, limit=10, sex="uni", seed=NULL) {
  name <- NULL
  URL <-  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv"
  data <-  readr::read_csv(URL,col_types = readr::cols())
  # Data wrangling
  data <- data |> dplyr::mutate(tp = dplyr::case_when(
    dplyr::between(year, 1880, 1890) ~ '1880s',
    dplyr::between(year, 1890, 1900) ~ '1890s',
    dplyr::between(year, 1900, 1910) ~ '1900s',
    dplyr::between(year, 1910, 1920) ~ '1910s',
    dplyr::between(year, 1920, 1930) ~ '1920s',
    dplyr::between(year, 1930, 1940) ~ '1930s',
    dplyr::between(year, 1940, 1950) ~ '1940s',
    dplyr::between(year, 1950, 1960) ~ '1950s',
    dplyr::between(year, 1960, 1970) ~ '1960s',
    dplyr::between(year, 1970, 1980) ~ '1970s',
    dplyr::between(year, 1980, 1990) ~ '1980s',
    dplyr::between(year, 1990, 2000) ~ '1990s',
    dplyr::between(year, 2000, 2010) ~ '2000s',
    dplyr::between(year, 2010, 2020) ~ '2010s',
  ))

  # Setting seed.
  if (!is.null(seed)){
    set.seed(seed)
  }
  if (!(tp %in% c('1880s','1890s','1900s','1910s','1920s','1930s','1940s','1950s','1960s','1970s','1980s','1990s','2000s','2010s')) | !(sex %in% c("uni",'M','F'))){
    stop("Sorry, please enter valid time periods/sex!")
  }

  df <- data |> dplyr::filter(tp == {{tp}})
  if (sex == "uni"){
    f <- df |> dplyr::filter(sex=="F") |> dplyr::pull(name)
    m <- df |> dplyr::filter(sex=="M") |> dplyr::pull(name)
    uni_df <- intersect(f,m)
    if (length(uni_df) < limit){
      uni_df
    }else{
      sample(uni_df,size=limit,replace = FALSE)
    }

  }else{
    r <- df |> dplyr::filter(sex=={{sex}}) |> dplyr::pull(name)
    sample(r,size=limit,replace = FALSE)
  }
}

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
  data <- readr::read_csv(url,col_types = readr::cols()) |>
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

#' Generate the a random set of 10 suggested neutral baby names based on the given limitation and baby names in the past years.
#'
#' @param limit A float controls the minimum proportion of the neutral names in a single year in the dataframe.
#' @param bar The length of the output with a default value of 10.
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
  data <- readr::read_csv(url,col_types = readr::cols())

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
