require(tidyverse,quietly = TRUE)
require(readr,quietly = TRUE)
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
