% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standing_lying_deadwood.R
\name{SDT_SNAG_Calculator}
\alias{SDT_SNAG_Calculator}
\title{Deadwood Volume Calculation for Individual Components (Standing and Snag)}
\usage{
SDT_SNAG_Calculator(
  data,
  TH_tot_col,
  DBH_col,
  L_tot_col,
  Dhalf_col,
  SDT_SNAG_option
)
}
\arguments{
\item{data}{A data frame containing the tree measurement data. It should include columns for
total height (TH), diameter at breast height (DBH), total length of snags (L_tot), and diameter at half the length (Dhalf).}

\item{TH_tot_col}{A character string specifying the column name for total height (m).}

\item{DBH_col}{A character string specifying the column name for diameter at breast height (DBH in cm).}

\item{L_tot_col}{A character string specifying the column name for total length (m) of snags.}

\item{Dhalf_col}{A character string specifying the column name for the diameter at half the length (cm) of snags.}

\item{SDT_SNAG_option}{A character string to specify which volume to calculate:
"sdt" for standing deadwood, "snag" for snags, or "both" to calculate the volume for both types of deadwood.}
}
\value{
A numeric vector with the calculated volume(s) in cubic meters, depending on the specified option:
        - "sdt" returns the volume of standing deadwood.
        - "snag" returns the volume of snags.
        - "both" returns the sum of both volumes.
}
\description{
This function calculates the volume of standing deadwood (SDT) or snags based on tree measurements.
Depending on the selected option, it can compute the volume for standing deadwood, snags, or both types of deadwood.
}
\examples{
data <- data.frame(TH_tot= c(10, 15), DBH = c(30, 40), L_tot = c(8, 12), Dhalf = c(20, 30))
# Calculate Standing Deadwood Volume
SDT_SNAG_Calculator(data, TH_tot_col = "TH_tot", DBH_col = "DBH", L_tot_col = "L_tot", Dhalf_col = "Dhalf", SDT_SNAG_option = "sdt")
# Calculate Snag Volume
SDT_SNAG_Calculator(data, TH_tot_col = "TH_tot", DBH_col = "DBH", L_tot_col = "L_tot", Dhalf_col = "Dhalf", SDT_SNAG_option = "snag")
# Calculate Both Standing Deadwood and Snag Volumes
SDT_SNAG_Calculator(data, TH_tot_col = "TH_tot", DBH_col = "DBH", L_tot_col = "L_tot", Dhalf_col = "Dhalf", SDT_SNAG_option = "both")
}
