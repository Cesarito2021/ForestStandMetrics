
#' Calculate the Structural Diversity Index (CI) for a given dataset of trees.
#'
#' This function computes the CI based on tree height, basal area, and species composition
#' from a specified plot area. It is useful for ecologists and forest managers assessing
#' the diversity and structural complexity of forest stands.
#'
#' @param data A data frame containing tree measurements.
#' @param th_col A string representing the name of the column for tree heights.
#' @param ba_col A string representing the name of the column for basal area.
#' @param specie_col A string representing the name of the column for tree species.
#' @param plot_area A numeric value representing the area of the plot in square meters.
#'
#' @return A numeric value representing the Structural Diversity Index (CI).
#'
#' @examples
#' # Example usage:
#'data <- as.data.frame(list(th = c(15, 20, 25, 10, 30),
#'                           ba = c(5, 10, 15, 5, 20),
#'                           specie = c("fir", "european beech", "fir", "european beech", "european beech")))
#'
#'results<- CI_plot_1000(data, "th", "ba", "specie", plot_area = 100)
#'
#'print(results)
#'
#' @export
CI_plot_1000 <- function(data, th_col, ba_col, specie_col, plot_area) {
  # Select relevant columns and rename
  input <- data %>% dplyr::select(any_of(c(th_col, ba_col, specie_col)))
  colnames(input) <- c("th", "ba", "specie")

  # Calculate the mean of the three highest tree heights (scaled to a tenth of a hectare)
  th_top3_mean <- input[["th"]] %>% sort(decreasing = TRUE) %>% head(3) %>% mean()

  # Calculate total basal area per hectare and scale to a tenth of a hectare
  ba_sum <- sum(input[["ba"]]) * ((10000 / plot_area) / 10)

  # Calculate tree density per hectare and scale to a tenth of a hectare
  td_count <- round(nrow(input) * ((10000 / plot_area) / 10))

  # Count distinct species
  Ntree <- n_distinct(input[["specie"]])

  # Calculate the Structural Diversity Index (CI)
  CI <- (th_top3_mean * ba_sum * td_count * Ntree) / 1000

  return(CI)
}


#' Stand Density Index (SDI) Calculation
#'
#' This function calculates the Stand Density Index (SDI) for a forest plot based on tree density
#' and quadratic mean diameter (QMD). The SDI is commonly used to assess stand density in forest
#' management, and it can be calculated for different forest types, such as even-aged or
#' uneven-aged stands.
#'
#' The formula for SDI is:
#' \deqn{SDI = (N * (QMD^b))}, where:
#' \itemize{
#'   \item N is the tree density (trees per hectare),
#'   \item QMD is the quadratic mean diameter (cm),
#'   \item b is a scaling exponent, typically set to 1.605 (default).
#' }
#'
#' The SDI provides an estimate of the stand's density, which helps evaluate its competition and growth potential.
#'
#' @param N A numeric value representing tree density (trees per hectare).
#' @param G A numeric value representing the total basal area (m² per hectare), used to compute QMD.
#' @param b A numeric value for the scaling exponent (default is 1.605). This parameter adjusts the relationship
#'        between tree density and diameter.
#' @param type A character string specifying the type of forest stand. Options are:
#'        \itemize{
#'          \item "even-aged" for even-aged stands (default),
#'          \item "uneven-aged" for uneven-aged stands.
#'        }
#'
#' @return A numeric value representing the calculated Stand Density Index (SDI) for the given forest stand.
#'
#' @examples
#' N <- 500
#' G <- 40
#' calculate_SDI(N, G)  # for even-aged stand
#'
#' N2 <- 300
#' G2 <- 30
#' calculate_SDI(N2, G2, type = "uneven-aged")  # for uneven-aged stand
#'
#' @export
calculate_SDI <- function(N, G, b = 1.605, type = "even-aged") {
  # Helper function to calculate quadratic mean diameter
  qmd_ha = function(ba, tpa) {
    qmd = sqrt((ba / tpa) / 0.00007854)
    return(qmd)
  }

  if (type == "even-aged") {
    # Calculate the quadratic mean diameter for even-aged stands
    dm = qmd_ha(G, N)
    SDI <- N * (dm / 25)^b
  } else if (type == "uneven-aged") {
    # Calculate SDI for uneven-aged stands
    SDI <- sum(N * (qmd_ha(G, N) / 25)^b)
  } else {
    stop("Invalid stand type. Use 'even-aged' for even-aged or 'uneven-aged' for uneven-aged stands.")
  }

  return(SDI)
}


#' Calculate the Vertical Evenness Index (VEm) for Forest Plots
#'
#' This function calculates the Vertical Evenness Index (VEm), which is a measure of the vertical
#' distribution of biomass in a forest plot. The index considers both tree height and basal area
#' to evaluate how evenly the biomass is distributed across different vertical strata.
#'
#' @param data A data frame containing tree data with height and basal area information.
#' The data should include at least one column for tree heights and one for basal area values.
#' @param th_col A character string specifying the column name containing tree heights (m).
#' @param ba_col A character string specifying the column name containing basal area values (m²).
#'
#' @return A numeric value representing the Vertical Evenness Index (VEm). Higher values indicate
#' a more evenly distributed vertical biomass structure, while lower values indicate a more concentrated
#' biomass distribution at certain vertical strata.
#'
#' @details The Vertical Evenness Index (VEm) is calculated by assessing how evenly the biomass
#' (as determined by basal area) is distributed across different height strata within the forest.
#' A higher VEm value suggests a more evenly distributed biomass profile, which is often a desirable
#' trait in forest ecosystems for resilience and biodiversity. The formula for the calculation depends on
#' the specific method for dividing the plot into height classes and the biomass distribution in those classes.
#'
#' @examples
#' # Example usage
#' data <- data.frame(th = c(10, 15, 20, 25), ba = c(0.2, 0.5, 0.7, 1.0))
#' results <- calc_VEm(data, th_col = "th", ba_col = "ba")
#'
#' print(results)
#'
#' @import vegan
#' @export
calc_VEm <- function(data, th_col, ba_col) {

  # Step 1: Find the maximum height to set thresholds for strata
  max_height <- max(data[[th_col]], na.rm = TRUE)
  thresholds <- c(0.2, 0.5, 0.8) * max_height

  # Step 2: Assign each tree to a layer based on height
  data <- data %>%
    mutate(strato = case_when(
      .data[[th_col]] <= thresholds[1] ~ "s1",
      .data[[th_col]] <= thresholds[2] & .data[[th_col]] > thresholds[1] ~ "s2",
      .data[[th_col]] <= thresholds[3] & .data[[th_col]] > thresholds[2] ~ "s3",
      .data[[th_col]] > thresholds[3] ~ "s4"
    ))

  # Step 3: Calculate the total basal area for each stratum
  basal_area_by_stratum <- data %>%
    group_by(strato) %>%
    summarize(total_basal_area = sum(.data[[ba_col]], na.rm = TRUE))

  # Step 4: Calculate the proportion of each stratum
  total_basal_area <- sum(basal_area_by_stratum$total_basal_area, na.rm = TRUE)
  basal_area_by_stratum <- basal_area_by_stratum %>%
    mutate(proportion = total_basal_area / total_basal_area)

  # Step 5: Apply the Shannon-Wiener index
  shannon_index <- diversity(basal_area_by_stratum$total_basal_area, index = "shannon")

  # Step 6: Standardize the index based on the number of strata (4 in this case)
  evenness_index <- shannon_index / log(4)

  return(evenness_index)
}


#' Forest Structure and Species Diversity Indices
#'
#' This function calculates various forest diversity indices and tree structural metrics,
#' including Simpson and Shannon diversity indices for diameter at breast height (dbh),
#' tree height (th), and species richness. It also computes the Gini index for basal area and
#' diameter, the coefficient of variation for basal area, and the Vertical Evenness Index (VEm).
#' Additionally, the function calculates the Stand Density Index (SDI).
#'
#' @param data A data frame containing the forest inventory data. It should include columns for
#'   diameter at breast height (dbh), tree height (th), basal area (ba), and species.
#' @param dbh_col A character string specifying the column name for diameter at breast height (dbh)
#'   in centimeters.
#' @param th_col A character string specifying the column name for tree height (th) in meters.
#' @param ba_col A character string specifying the column name for basal area (ba) in square meters.
#' @param specie_col A character string specifying the column name for species names.
#' @param plot_area A numeric value indicating the area of the forest plot in square meters (m²).
#'
#' @return A data frame containing the calculated diversity and structural indices for the forest plot.
#'   The returned data frame includes:
#'   - Simpson's diversity index (for dbh, th, and species richness)
#'   - Shannon's diversity index (for dbh, th, and species richness)
#'   - Gini index for basal area and diameter
#'   - Coefficient of variation for basal area
#'   - Vertical Evenness Index (VEm)
#'   - Stand Density Index (SDI)
#'
#' @details This function performs multiple calculations on a given forest inventory dataset to
#'   assess forest structural complexity and species diversity. The calculated indices help in
#'   understanding the spatial and vertical distribution of trees and their structural variability.
#'   These metrics are useful for forest management, ecological assessments, and biodiversity studies.
#'
#' @examples
#' # Example usage:
#' data <- as.data.frame(list(
#'   dbh_col = c(10, 15, 18, 30, 35, 45),     # Diameter at breast height (DBH) in cm
#'   th_col = c(15, 18, 19, 25, 26, 32),      # Tree height in meters
#'   ba_col = c(0.007854, 0.017671, 0.025446, 0.070685, 0.096211, 0.159154),  # Basal Area (calculated)
#'   specie_col = c("norway spruce", "norway spruce", "norway spruce", "norway spruce", "norway spruce", "norway spruce")
#' ))
#' plot_area = 530
#' results <- ForStrSpecDiv(data = data,
#'                         dbh_col = "dbh_col",
#'                         th_col = "th_col",
#'                         ba_col = "ba_col",
#'                         specie_col = "specie_col",
#'                         plot_area = plot_area)
#'
#' print(results)
#'
#' @import vegan
#' @import DescTools
#  Note: no @export tag here.
ForStrSpecDiv_Arch <- function(data, dbh_col, th_col, ba_col, specie_col, plot_area) {

  # Select relevant columns from the data
  input <- data %>% dplyr::select(any_of(c(dbh_col, th_col, ba_col, specie_col)))
  colnames(input) <- c("dbh", "th", "ba", "specie")

  # Simpson and Shannon diversity indices for dbh
  simpson_dbh <- diversity(input[["dbh"]], "simpson")
  shannon_dbh <- diversity(input[["dbh"]], "shannon")
  sd_dbh <- sd(input[["dbh"]])

  # Shannon diversity for tree height (th)
  shannon_th <- diversity(input[["th"]], "shannon")
  sd_th <- sd(input[["th"]])

  # Gini index for basal area (ba)
  Gini_G <- Gini(input[["ba"]], unbiased = FALSE)

  # Coefficient of Variation for basal area
  CV_G <- sd(input[["ba"]]) / mean(input[["ba"]])

  # Gini index for dbh
  Gini_dbh <- Gini(input[["dbh"]], unbiased = FALSE)

  # Shannon and Simpson diversity indices for species
  shannon_specie <- diversity(as.numeric(table(input[["specie"]])), "shannon")
  simpson_specie <- diversity(as.numeric(table(input[["specie"]])), "simpson")

  # Calculate 1000 tree index (CI_1000)
  CI_1000 <- CI_plot_1000(data = input, th_col = "th", ba_col = "ba", specie_col = "specie", plot_area)

  # Calculate total basal area and number of trees per hectare
  total_ba <- sum(input[["ba"]]) * (10000 / plot_area)  # Total basal area per hectare
  Ntree <- round(nrow(input) * (10000 / plot_area))  # Trees per hectare

  # Calculate Stand Density Index (SDI)
  SDI <- calculate_SDI(N = Ntree, G = total_ba, b = 1.605, type = "even-aged")

  # Vertical Evenness Index (VEm)
  VEm <- calc_VEm(input, "th", "ba")

  # Variance of difference between Shannon indices of dbh and th
  VarDH <- sqrt((diversity(input[["dbh"]], "shannon") - diversity(input[["th"]], "shannon"))^2)

  # Combine results into a summary vector
  res <- cbind(simpson_dbh, shannon_dbh, sd_dbh, Gini_dbh, Gini_G, CV_G, sd_th, shannon_th, CI_1000,
               shannon_specie, simpson_specie, SDI, VEm, VarDH)
  colnames(res) <- c("SI_dbh", "SH_dbh", "SD_dbh", "GI_dbh", "GI_ba", "CV_ba", "SD_th", "SH_th",
                     "CI_1000", "SH_sp", "SI_sp", "SDI", "VEm", "VarDH")

  # Summarize input data for mean dbh, mean th, total basal area, and number of species
  input_sum <- input %>% summarise(Mean_dbh = mean(dbh), Mean_th = mean(th), Sum_ba = sum(ba),
                                   N_sp = n_distinct(specie))

  # Merge summary statistics with the diversity indices results
  merged <- cbind(input_sum, res)

  # Round numerical output for readability
  output <- as.data.frame(merged) %>% mutate_if(is.numeric, round, 2)

  return(output)
}

#' Forest Structure and Species Diversity Indices for Forest Stand
#'
#' This function calculates various forest diversity indices and tree structural metrics,
#' including Simpson and Shannon diversity indices for diameter at breast height (dbh),
#' tree height (th), and species richness. It also computes the Gini index for basal area and
#' diameter, the coefficient of variation for basal area, and the Vertical Evenness Index (VEm).
#' Additionally, the function calculates the Stand Density Index (SDI).
#'
#' @param data A data frame containing the forest inventory data. It should include columns for
#'   diameter at breast height (dbh), tree height (th), basal area (ba), and species.
#' @param dbh_col A character string specifying the column name for diameter at breast height (dbh)
#'   in centimeters.
#' @param th_col A character string specifying the column name for tree height (th) in meters.
#' @param ba_col A character string specifying the column name for basal area (ba) in square meters.
#' @param specie_col A character string specifying the column name for species names.
#' @param plot_area A numeric value indicating the area of the forest plot in square meters (m²).
#'
#' @return A data frame containing the calculated diversity and structural indices for the forest plot.
#'   The returned data frame includes:
#'   - Simpson's diversity index (for dbh, th, and species richness)
#'   - Shannon's diversity index (for dbh, th, and species richness)
#'   - Gini index for basal area and diameter
#'   - Coefficient of variation for basal area
#'   - Vertical Evenness Index (VEm)
#'   - Stand Density Index (SDI)
#'
#' @details This function performs multiple calculations on a given forest inventory dataset to
#'   assess forest structural complexity and species diversity. The calculated indices help in
#'   understanding the spatial and vertical distribution of trees and their structural variability.
#'   These metrics are useful for forest management, ecological assessments, and biodiversity studies.
#'
#' @examples
#' # Example usage:
#' data <- as.data.frame(list(
#'   dbh_col = c(10, 15, 18, 30, 35, 45),     # Diameter at breast height (DBH) in cm
#'   th_col = c(15, 18, 19, 25, 26, 32),      # Tree height in meters
#'   ba_col = c(0.007854, 0.017671, 0.025446, 0.070685, 0.096211, 0.159154),  # Basal Area (calculated)
#'   specie_col = c("norway spruce", "norway spruce", "norway spruce", "norway spruce", "norway spruce", "norway spruce")
#' ))
#' plot_area = 530
#' results <- ForStrSpecDiv(data = data,
#'                         dbh_col = "dbh_col",
#'                         th_col = "th_col",
#'                         ba_col = "ba_col",
#'                         specie_col = "specie_col",
#'                         plot_area = plot_area)
#'
#' print(results)
#'
#' @import vegan
#' @import DescTools
#' @export
ForStrSpecDiv <- function(data, dbh_col, th_col, ba_col, specie_col, plot_area) {
  ForStrSpecDiv_Arch(data, dbh_col, th_col, ba_col, specie_col, plot_area)
}

#' Apply Forest Structure and Species Diversity Calculation
#'
#' This function calculates forest structure and species diversity metrics for forest plots,
#' including basal area (BA) and other related attributes, based on tree diameter at breast height (dbh),
#' tree height (th), and species data. The calculation is performed for each plot, and the results can be grouped
#' by forest management intervention. It returns summaries at both the plot level and the forest management intervention level,
#' allowing for comprehensive analysis of forest structure and species diversity across plots and interventions.
#'
#' @param data A data frame containing tree attributes for each plot. Must include relevant
#'   columns such as tree diameter (dbh), tree height (th), species, and forest management intervention information.
#' @param ForManInt_option A string ("Yes" or "No") indicating whether forest management intervention
#'   is considered in the dataset. If "Yes", the column specified in `ForManInt` is used to group results.
#' @param ForManInt A string representing the name of the column in the dataset that specifies the
#'   forest management intervention (e.g., thinning, clearcut, etc.). Used only if `ForManInt_option` is "Yes".
#' @param plot_option A string ("Yes" or "No") indicating if the dataset contains multiple plots.
#'   Defaults to "No" if not provided. If "Yes", data will be grouped by plot and summarized accordingly.
#' @param plot_col A string representing the name of the column in the dataset that specifies the plot identifier.
#'   Used only if `plot_option` is "Yes".
#' @param dbh_col A string representing the name of the column in the dataset that specifies the diameter at breast height (dbh) of the trees.
#' @param th_col A string representing the name of the column in the dataset that specifies the height of the trees.
#' @param specie_col A string representing the name of the column in the dataset that specifies the species of the trees.
#' @param plot_area A numeric value representing the area of each plot in square meters, used for calculating per-hectare metrics.
#'
#' @return A list containing two data frames:
#' \item{output_plot_df}{A data frame summarizing the calculated forest structure and species diversity metrics at the plot level,
#' including basal area, species diversity, and other related attributes.}
#' \item{output_ForManInt}{A data frame summarizing the calculated metrics by forest management intervention,
#' including the average values across plots for each intervention type.}
#'
#' @details This function calculates various forest structure and species diversity metrics for each plot.
#'   The main metric calculated is basal area (BA), which is derived from the diameter at breast height (dbh) of each tree.
#'   Additionally, species diversity is measured, providing insights into the distribution of species across plots.
#'   Results can be grouped by forest management interventions (if specified), and per-hectare metrics are calculated based on
#'   the area of the plot provided in `plot_area`. The function provides summaries at both the plot level and the forest management intervention level.
#'
#' @examples
#' # Example usage with test data:
#' data <- as.data.frame(list(
#'   ForManInt = c("Intervention_A", "Intervention_A", "Intervention_A", "Intervention_B", "Intervention_B", "Intervention_B"),
#'   plot_col = c("Plot_1", "Plot_1", "Plot_1", "Plot_2", "Plot_2", "Plot_2"),
#'   dbh_col = c(10, 15, 18, 30, 35, 45),    # Diameter at breast height (DBH) in cm
#'   th_col = c(15, 18, 19, 25, 26, 32),     # Tree height in meters
#'   specie_col = c("norway spruce", "norway spruce", "norway spruce", "norway spruce", "norway spruce", "norway spruce")
#' ))
#' plot_area = 530
#'
#' results <- Apply_ForStrSpecDiv(
#'   data = data,
#'   ForManInt_option = "Yes",
#'   ForManInt = "ForManInt",
#'   plot_option = "Yes",
#'   plot_col = "plot_col",
#'   dbh_col = "dbh_col",
#'   th_col = "th_col",
#'   specie_col = "specie_col",
#'   plot_area = plot_area
#' )
#' # View the calculated results
#' print(results)
#'
#' @import dplyr
#' @importFrom dplyr %>%
#' @export
Apply_ForStrSpecDiv <- function(data,ForManInt_option, ForManInt,plot_option, plot_col, dbh_col,th_col,specie_col,plot_area) {
  # check if the ForManInt are included in the dataset
  if(ForManInt_option== "No"){
    data[[ForManInt]] <- "No_ForManInt"
  }
  # check if the ForManInt are included in the dataset
  if(plot_option== "No"){
    data[[plot_col]] <- "1"
  }
  #
  required_columns <- c(ForManInt,plot_col,dbh_col,th_col,specie_col)
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("The following columns are missing in the dataset:", paste(missing_columns, collapse = ", ")))
  }
  #
  input <- data %>% dplyr::select(all_of(required_columns))
  colnames(input) <- c("ForManInt", "plot","dbh", "th", "specie")
  # ****************************************************************************
  # BA calculation
  # ****************************************************************************
  num_rows <- nrow(input)
  columns_to_replace <- c("ba")
  # Apply StemVolumeCalculator row by row
  for (i in 1:num_rows) {
    raw_data <- input[i, ]
    result <- BA_Calculator(raw_data,"dbh")
    input[i, columns_to_replace] <- result
  }
  #
  # ****************************************************************************
  # BA calculation
  # ****************************************************************************
  input$plot <- as.factor(input$plot)
  input_group <- input%>%group_split(plot)
  # ***************************************************************************
  #  Plot analysis
  # ***************************************************************************
  output_plot <- list()
  for (i in 1:length(input_group)) {
    output_plot[[i]] <- ForStrSpecDiv(data = input_group[[i]],dbh_col="dbh",th_col="th",ba_col="ba",specie_col="specie",plot_area=plot_area)  # Pass the actual values
    output_plot[[i]]$plot <-input_group[[i]][["plot"]][1]
    output_plot[[i]]$ForManInt <-input_group[[i]][["ForManInt"]][1]
  }
  #
  output_plot_df <-do.call(rbind,output_plot)
  #output_plot_df_corrected <- output_plot_df %>% dplyr::select(ID,Ntree,everything())%>%data.frame()
  #
  output_plot_df <- output_plot_df %>%
    mutate(across(where(is.numeric), ~ round(., 2))) %>%
    mutate(ID = row_number()) %>%
    select(ID, ForManInt,plot, everything())
  # ***************************************************************************
  #  ForManInt
  # ***************************************************************************
  head(output_plot_df)
  # Grouping and Summarizing
  output_ForManInt <- output_plot_df %>%
    group_by(ForManInt) %>%
    select(-ID) %>%
    summarise(
      TotalPlots = n_distinct(plot),
      across(where(is.numeric), mean, na.rm = TRUE, .names = "Mean_{.col}"),
      .groups = "drop"
    )
  # Convert integers to numeric
  output_ForManInt <- output_ForManInt %>% mutate(across(where(is.integer), as.numeric))
  # Round numeric columns and add ID
  output_ForManInt <- output_ForManInt %>%
    mutate(across(where(is.numeric), ~ round(., 2))) %>%
    mutate(ID = row_number()) %>%
    select(ID, everything())
  # return(raw_data)
  #
  return(list(output_plot_df,output_ForManInt))
}
