% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nameformeR.R
\name{find_name}
\alias{find_name}
\title{Generate the a random set of 10 suggested baby names based on the given limitations.}
\usage{
find_name(sex, init, length)
}
\arguments{
\item{sex}{A character vector which is sex of baby's name}

\item{init}{A character vector which the initial of baby's name}

\item{length}{A double vector that is the length of baby's name}
}
\value{
A list contains random suggested names based on the given limitation.
}
\description{
Generate the a random set of 10 suggested baby names based on the given limitations.
}
\examples{
find_name('F', 'A', 3)
}
