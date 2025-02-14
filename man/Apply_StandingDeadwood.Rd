% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standing_lying_deadwood.R
\name{Apply_StandingDeadwood}
\alias{Apply_StandingDeadwood}
\title{Deadwood Volume Calculation (Standing and Snag) for Multiple Plots and Forest Management Practices}
\usage{
Apply_StandingDeadwood(
  data,
  ForManInt_option,
  ForManInt,
  plot_option,
  plot_col,
  SDT_SNAG_option,
  L_tot_col,
  Dhalf_col,
  TH_tot_col,
  DBH_col,
  plot_area
)
}
\arguments{
\item{data}{A data frame containing tree attributes for each plot. Must include relevant columns such as DBH, tree height, and forest management intervention information.
Additional parameters for SDT or Snag calculations (L_tot, Dhalf, etc.) are required as well.}

\item{ForManInt_option}{A character string ("Yes" or "No") indicating whether forest management intervention
is considered in the dataset. Defaults to "No" if not provided. If "Yes", the column specified in `ForManInt` is used to group results.}

\item{ForManInt}{A character string representing the name of the column in the dataset that specifies the
forest management intervention (e.g., thinning, clearcut). Used if `ForManInt_option` is "Yes".}

\item{plot_option}{A character string ("Yes" or "No") indicating whether plot-level information is available in the dataset. Defaults to "No" if not provided.
If "Yes", the data will be grouped by plot and summarized accordingly.}

\item{plot_col}{A character string representing the name of the column in the dataset that specifies the plot identifier. Used if `plot_option` is "Yes".}

\item{SDT_SNAG_option}{A character string specifying whether to calculate the volume for Standing Deadwood ("sdt") or Snag ("snag").
The corresponding calculation will be applied based on this option.}

\item{L_tot_col}{A character string representing the name of the column in the dataset for the total length of the tree (L_tot), used for Snag calculations.}

\item{Dhalf_col}{A character string representing the name of the column in the dataset for the half-diameter of the tree (Dhalf), used for Snag calculations.}

\item{TH_tot_col}{A character string representing the name of the column in the dataset for the total height of the tree (TH_tot), used for SDT calculations.}

\item{DBH_col}{A character string representing the name of the column in the dataset for the diameter at breast height (DBH), used for both SDT and Snag calculations.}

\item{plot_area}{A numeric value representing the area of each plot in square meters, used to calculate the volume per hectare.}
}
\value{
A list containing two data frames:
\item{output_plot}{A data frame summarizing the calculated volumes at the plot level, including volume per hectare.}
\item{output_ForManInt}{A data frame summarizing the calculated volumes by forest management intervention, including the average volume per hectare across plots.}
}
\description{
This function calculates the volume of Standing Deadwood (SDT) or Snags based on tree attributes such as
diameter at breast height (DBH), tree height, total length of the tree (L_tot), and half-diameter (Dhalf).
The calculation is performed for each plot, and results can be grouped by forest management intervention (ForManInt).
Additionally, the function provides plot-level and forest management intervention summaries for the calculated volumes.
}
\details{
This function calculates the volume of Standing Deadwood (SDT) or Snags based on the provided tree attributes.
  The calculation differs for SDT and Snags, with specific parameters required for each. For SDT, height and DBH are used,
  while Snag volume requires additional parameters like L_tot and Dhalf. The function can group results by plot or
  forest management intervention based on the user-specified options.
}
\examples{
# Example 1: Using the "sdt" option
data_sdt <- as.data.frame(list(
  ForManInt = c("Intervention_A","Intervention_A","Intervention_A","Intervention_B","Intervention_B","Intervention_B"),
  plot_col = c("Plot_1", "Plot_1","Plot_1","Plot_2","Plot_2","Plot_2"),
  TH_tot_col = c(15, 18, 19, 25, 26, 32),  # Tree height in meters
  DBH_col = c(10, 15, 18, 30, 35, 45)     # Diameter at breast height (DBH) in cm
))
plot_area <- 530
SDT_SNAG_option <- "sdt"
results_sdt <- Apply_StandingDeadwood(
  data = data_sdt,
  ForManInt_option = "Yes",
  ForManInt = "ForManInt",
  plot_option = "Yes",
  plot_col = "plot_col",
  SDT_SNAG_option = SDT_SNAG_option,
  L_tot_col = NULL,
  Dhalf_col = NULL,
  TH_tot_col = "TH_tot_col",
  DBH_col = "DBH_col",
  plot_area = plot_area
)
print(results_sdt)

# Example 2: Using the "snag" option
data_snag <- as.data.frame(list(
  ForManInt = c("Intervention_A","Intervention_A","Intervention_A","Intervention_B","Intervention_B","Intervention_B"),
  plot_col = c("Plot_1", "Plot_1","Plot_1","Plot_2","Plot_2","Plot_2"),
  L_tot_col = c(15, 18, 19, 25, 26, 32),  # Total length in meters
  Dhalf_col = c(10, 15, 18, 30, 35, 45)  # Half diameter in cm
))
plot_area <- 530
SDT_SNAG_option <- "snag"
results_snag <-  Apply_StandingDeadwood(
  data = data_snag,
  ForManInt_option = "Yes",
  ForManInt = "ForManInt",
  plot_option = "Yes",
  plot_col = "plot_col",
  SDT_SNAG_option = SDT_SNAG_option,
  L_tot_col = "L_tot_col",
  Dhalf_col = "Dhalf_col",
  TH_tot_col = NULL,
  DBH_col = NULL,
  plot_area = plot_area
)
print(results_snag)
}
