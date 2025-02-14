% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forest_structure.R
\name{BA_Calculator}
\alias{BA_Calculator}
\title{Basal Area Calculation for Individual Tree Species}
\usage{
BA_Calculator(data, dbh_col)
}
\arguments{
\item{data}{A data frame containing tree data.}

\item{dbh_col}{A string specifying the column name containing tree DBH values in centimeters.}
}
\value{
A numeric vector representing the basal area for each tree in square meters.
}
\description{
This function calculates the basal area (BA) for each tree based on its diameter at breast height (DBH).
The calculation is performed in square meters using the formula:
\eqn{BA = \pi \times (DBH / 200)^2}.
}
\examples{
data <- data.frame(dbh = 30)
results <- BA_Calculator(data, dbh_col = "dbh")
 print(results)
}
