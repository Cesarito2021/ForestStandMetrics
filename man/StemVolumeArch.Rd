% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/architecture.R
\name{StemVolumeArch}
\alias{StemVolumeArch}
\title{Stem Volume Architecture}
\usage{
StemVolumeArch(data, dbh_col, th_col, specie_col)
}
\arguments{
\item{data}{A data frame containing the tree data.}

\item{dbh_col}{The name of the column in the data frame containing the diameter at breast height (DBH) values.}

\item{th_col}{The name of the column in the data frame containing the total height (TH) values.}

\item{specie_col}{The name of the column in the data frame containing the tree species names.}
}
\value{
A numeric value of the stem volume for the given species, or a message if the species is not registered.
}
\description{
**Note:** This function is currently not accessible. To calculate stem volume, please use the
`StemVolumeCalculator` function instead.
}
\details{
This function calculates the stem volume of trees based on diameter at breast height (DBH),
total height (TH), and the species name. The formula used for the calculation depends on the
species, and the function supports a variety of common tree species. It is designed to be used
in forestry analysis for estimating biomass and carbon storage potential.
}
\examples{
data <- data.frame(DBH = c(30), Height = c(15), Species = c("silver fir"))
results <- StemVolumeArch(data, "DBH", "Height", "Species")
print(results)
(Rest of the documentation)
}
