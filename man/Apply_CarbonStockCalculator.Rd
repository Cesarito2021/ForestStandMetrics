% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forest_structure.R
\name{Apply_CarbonStockCalculator}
\alias{Apply_CarbonStockCalculator}
\title{Carbon Stock Calculation for Multiple Plots and Forest Management Practices}
\usage{
Apply_CarbonStockCalculator(
  data,
  ForManInt_option,
  ForManInt,
  plot_option,
  plot_col,
  DomTreeSpecies,
  vol_col
)
}
\arguments{
\item{data}{A data frame containing the forest inventory data. The data should include columns for tree volume,
species information, and optionally, forest management interventions and plot identifiers.}

\item{ForManInt_option}{A character string indicating whether forest management intervention data is included ("Yes"/"No").
If "Yes", the function will consider the column specified in the `ForManInt` argument for calculations.}

\item{ForManInt}{A character string representing the name of the column in `data` that contains information on forest management
interventions (e.g., thinning, clearcut). This column is used if `ForManInt_option` is "Yes".}

\item{plot_option}{A character string indicating whether plot information is included in the data ("Yes"/"No"). If "Yes",
the function will process the data at the plot level and include plot-level summaries in the output.}

\item{plot_col}{A character string representing the name of the column in `data` that holds the plot identifiers.
This column is used if `plot_option` is "Yes".}

\item{DomTreeSpecies}{A character string representing the name of the column in `data` that contains the dominant tree species information.
This column helps identify species-specific carbon stock and biomass.}

\item{vol_col}{A character string representing the name of the column in `data` that contains the tree volume (in cubic meters per hectare).
The volume is used in the calculation of AGB and CS.}
}
\value{
A list containing two data frames:
  \itemize{
    \item \code{output_plot}: The original dataset with added columns for calculated above-ground biomass (AGB) and carbon stock (CS) at the plot level.
    \item \code{output_ForManInt}: A summarized dataset with mean values of AGB and CS grouped by forest management intervention type.
  }
}
\description{
This function calculates the above-ground biomass (AGB) and carbon stock (CS) for forest plots,
considering forest management interventions (ForManInt) and the dominant tree species (DomTreeSpecies).
It summarizes the data by forest management intervention type, calculating the mean values for carbon stock and biomass.
The function can handle different types of forest management interventions and provides both plot-level and intervention-level summaries.
}
\details{
This function calculates AGB using standard allometric equations and then estimates the carbon stock (CS) based on AGB.
  The function can compute these metrics for each plot, and also group and summarize by forest management intervention type.
  This is useful for understanding the effects of different forest management strategies on carbon sequestration and biomass accumulation.
}
\examples{
# Example dataset
data <- data.frame(
  ForManInt = c("Intervention_A", "Intervention_A", "Intervention_A",
                "Intervention_B", "Intervention_B", "Intervention_B"),
  DomTreeSpecies = c("larches", "larches", "larches",
                     "larches", "larches", "larches"),
  plot = c("Plot1", "Plot1", "Plot1", "Plot2", "Plot2", "Plot2"),
  vol_m3_ha = c(120.5, 85.3, 150.2, 95.7, 74.2, 84.2)
)

# Apply the Carbon Stock Calculator
results <- Apply_CarbonStockCalculator(
  data = data,
  ForManInt_option = "Yes",   # Include forest management intervention
  ForManInt = "ForManInt",
  plot_option = "Yes",        # Include plot-level information
  plot_col = "plot",
  DomTreeSpecies = "DomTreeSpecies",
  vol_col = "vol_m3_ha"
)

# View the calculated results
print(results)

}
