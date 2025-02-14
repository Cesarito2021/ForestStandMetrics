% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standing_lying_deadwood.R
\name{LDT_CWD_Calculator}
\alias{LDT_CWD_Calculator}
\title{Deadwood Volume Calculation for Individual Components (Lying and CWD)}
\usage{
LDT_CWD_Calculator(
  data,
  TH_tot_col,
  DBH_col,
  L_tot_col,
  Dhalf_col,
  LDT_CWD_option
)
}
\arguments{
\item{data}{A data frame containing the tree measurement data. The data should include the relevant columns specified
for the calculation.}

\item{TH_tot_col}{A character string specifying the column name for the total height (m).}

\item{DBH_col}{A character string specifying the column name for diameter at breast height (DBH in cm).}

\item{L_tot_col}{A character string specifying the column name for the total length (m) of coarse woody debris (CWD).}

\item{Dhalf_col}{A character string specifying the column name for the diameter at half the length (cm) of CWD.}

\item{LDT_CWD_option}{A character string indicating which volume to calculate:
"ldt" for lying deadwood, "cwd" for coarse woody debris, or "both" for both volumes.}
}
\value{
A numeric vector with the calculated volume(s) in cubic meters, depending on the specified option.
        If "ldt" is chosen, the function returns the volume of lying deadwood.
        If "cwd" is chosen, it returns the volume of coarse woody debris.
        If "both" is selected, it returns the sum of both volumes.
}
\description{
This function calculates the volume of deadwood for either lying deadwood (LDT), coarse woody debris (CWD), or both.
The calculation uses input tree measurements such as total height (TH), diameter at breast height (DBH),
total length of coarse woody debris (L_tot), and diameter at half the length (Dhalf). Depending on the selected option,
the function computes the respective volume and returns the results.
}
\examples{
data <- data.frame(TH_tot = c(10, 15), DBH = c(30, 40), L_tot = c(8, 12), Dhalf = c(20, 30))
# Calculate Lying Deadwood Volume
LDT_CWD_Calculator(data, TH_tot_col = "TH_tot", DBH_col = "DBH", L_tot_col = "L_tot", Dhalf_col = "Dhalf", LDT_CWD_option = "ldt")
# Calculate Coarse Woody Debris Volume
LDT_CWD_Calculator(data, TH_tot_col = "TH_tot", DBH_col = "DBH", L_tot_col = "L_tot", Dhalf_col = "Dhalf", LDT_CWD_option = "cwd")
# Calculate Both Volumes (Lying Deadwood and Coarse Woody Debris)
LDT_CWD_Calculator(data, TH_tot_col = "TH_tot", DBH_col = "DBH", L_tot_col = "L_tot", Dhalf_col = "Dhalf", LDT_CWD_option = "both")
}
