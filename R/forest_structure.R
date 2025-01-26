#' Stem Volume Calculation for Individual Tree Species
#'
#' This function calculates the stem volume of trees based on diameter at breast height (DBH),
#' total height (TH), and the species name. The formula used for the calculation depends on the
#' species, and the function supports a variety of common tree species. It is designed to be used
#' in forestry analysis for estimating biomass and carbon storage potential.
#'
#' @param data A data frame containing the tree data.
#' @param dbh_col The name of the column in the data frame containing the diameter at breast height (DBH) values.
#' @param th_col The name of the column in the data frame containing the total height (TH) values.
#' @param specie_col The name of the column in the data frame containing the tree species names.
#' @return A numeric value of the stem volume for the given species, or a message if the species is not registered.
#' @examples
#' data <- data.frame(DBH = c(30), Height = c(15), Species = c("silver fir"))
#' results <- StemVolumeCalculator(data, "DBH", "Height", "Species")
#' print(results)
#' @export
StemVolumeCalculator <- function(data, dbh_col, th_col, specie_col) {
  StemVolumeArch(data, dbh_col, th_col, specie_col)
}


#' Carbon Stock Calculation for Forest Stand
#'
#' This function calculates the Above-Ground Biomass (AGB) and Carbon Stock (CS)
#' for forest stands based on volume, area, and typology.
#'
#' @param data A data frame containing forest inventory data.
#' @param category_col A string specifying the column name for the category (e.g., "stands").
#' @param typology_col A string specifying the column name for the forest typology.
#' @param vol_col A string specifying the column name for the volume (in m3/ha).
#' @param area_col A numeric value or string specifying the area column (in ha).
#'
#' @return A named vector with Above-Ground Biomass (AGB) and Carbon Stock (CS) in tons.
#' If the typology is not registered, it returns an error message.
#'
#' @examples
#' data <- data.frame(category = "stands", typology = "norway spruce", volume = 0.5)
#' area_ha <- 1
#' results <- CarbonStockCalculator(data, "stands", "typology", "volume", area_ha)
#' print(results)
#' @export
CarbonStockCalculator <- function(data, category_col, typology_col, vol_col, area_col) {
  CarbonStockArch(data, category_col, typology_col, vol_col, area_col)
}

#' Basal Area Calculation for Individual Tree Species
#'
#' This function calculates the basal area (BA) for each tree based on its diameter at breast height (DBH).
#' The calculation is performed in square meters using the formula:
#' \eqn{BA = \pi \times (DBH / 200)^2}.
#'
#' @param data A data frame containing tree data.
#' @param dbh_col A string specifying the column name containing tree DBH values in centimeters.
#' @return A numeric vector representing the basal area for each tree in square meters.
#' @examples
#' data <- data.frame(dbh = 30)
#' results <- BA_Calculator(data, dbh_col = "dbh")
#'  print(results)
#' @export
BA_Calculator <- function(data, dbh_col) {
  dbh_cm <- as.numeric(data[[dbh_col]])

  # Check for valid diameter values
  if (any(dbh_cm <= 0, na.rm = TRUE)) {
    stop("The diameter must be greater than 0.")
  }

  # Calculate Basal Area in square meters
  BA_m2 <- round(pi * (dbh_cm / 200)^2, 3)
  return(BA_m2)
}

#' Dominant Tree Species Detection in Forest Stands Based on Basal Area
#'
#' This function identifies the dominant tree species in a forest stand and classifies the stand as either "Pure" or
#' "Mixed" based on basal area accumulation. The classification of the stand as "Pure" is based on the condition that
#' one species contributes more than 50% of the total basal area in the stand. If no species exceeds this threshold,
#' the stand is classified as "Mixed". The dominant species is identified as the species with the highest basal area
#' percentage if the stand is "Pure". For "Mixed" stands, the dominant species is classified as "other broadleaf/coniferous".
#'
#' @param data A data frame containing tree data with a column for species and basal area.
#' @param BA_col The name of the column in the data frame representing basal area (e.g., "Sum_BA_m2").
#' @param specie_col The name of the column in the data frame representing tree species (e.g., "specie").
#'
#' @return A character vector containing:
#' \item{Pure_Mixed_Stands}{The type of stand: "Pure" if a single species dominates with more than 50% of the total
#' basal area, "Mixed" otherwise.}
#' \item{DomTreeSpecies}{The name of the dominant species if the stand is "Pure", or "other broadleaf/coniferous"
#' if the stand is "Mixed".}
#'
#' @details
#' To classify the plot as "Pure" or "Mixed", the function calculates the total basal area of the stand and
#' determines the percentage of basal area contributed by each species. If any species exceeds 50% of the total basal area,
#' it is considered the dominant species, and the stand is classified as "Pure". In cases where no species exceeds
#' 50%, the stand is classified as "Mixed". For mixed stands, the dominant tree species is labeled as "other broadleaf/coniferous"
#' to represent the mixed composition of the stand.
#'
#' @examples
#' data <- data.frame(specie_col = c("Pine", "Oak", "Birch"), BA_col = c(30, 20, 5))
#' results <- DominantTreeByPlot(data=data, BA_col="BA_col", specie_col="specie_col")
#' print(results)
#'
#' @export
DominantTreeByPlot <- function(data, BA_col, specie_col) {
  # Check if specified columns are present in the data
  if (!BA_col %in% colnames(data)) {
    stop(paste("Column", BA_col, "is missing from the data."))
  }
  if (!specie_col %in% colnames(data)) {
    stop(paste("Column", specie_col, "is missing from the data."))
  }

  # Calculate percentage of Basal Area
  total_BA <- sum(data[[BA_col]], na.rm = TRUE)
  data$per <- (data[[BA_col]] / total_BA) * 100

  # Find the dominant species
  max_BA_species <- data[which.max(data$per), specie_col]
  dominant_species <- ifelse(max(data$per, na.rm = TRUE) > 50, max_BA_species, "Mixed")

  # Determine stand type and dominant species
  if (dominant_species == "Mixed") {
    return(c("Mixed", "other broadleaf/coniferous"))
  } else {
    return(c("Pure", dominant_species))
  }
}


#' Stem Volume Calculation for Multiple Plots and Forest Management Practices
#'
#' This function calculates stem volume, basal area, and identifies dominant tree species
#' based on forest inventory data. It also provides summaries at the forest management intervention
#' level and plot level, including volume per hectare, basal area, and other relevant metrics.
#' The function is particularly useful for forest management applications, allowing for efficient
#' analysis of inventory data and intervention strategies.
#'
#' @param data A data frame containing the forest inventory data. The data should include columns for
#'   diameter at breast height (DBH), tree height, tree species, and optional columns for forest
#'   management interventions and plot information.
#' @param ForManInt_option A string specifying whether forest management intervention data is provided
#'   ("Yes" or "No"). If "Yes", the function will use the intervention data in calculations.
#' @param ForManInt A string representing the name of the column containing forest management intervention
#'   data (e.g., "Thinning", "Clearcut"). This column is used when `ForManInt_option` is "Yes".
#' @param plot_option A string specifying whether plot data is provided ("Yes" or "No"). If "Yes", the
#'   function will include plot-level summaries in the output.
#' @param plot_col A string specifying the name of the column representing the plot identifier. Used
#'   when `plot_option` is "Yes".
#' @param plot_area A numeric value representing the total area of the plot in hectares. This is used
#'   for calculating plot-level data, such as volume and basal area per hectare.
#' @param dbh_col A string specifying the name of the column representing diameter at breast height (DBH)
#'   in centimeters.
#' @param th_col A string specifying the name of the column representing tree height in meters.
#' @param specie_col A string specifying the name of the column representing tree species.
#'
#' @return A list of two data frames:
#' \itemize{
#'   \item \code{output_plot}: A data frame with plot-level data, including total stem volume, basal area,
#'   dominant tree species, and other metrics calculated per hectare.
#'   \item \code{output_ForManInt}: A data frame summarizing forest management interventions (e.g., thinning)
#'   and their impact on forest structure, including volume changes, basal area modifications, and species composition.
#' }
#'
#' @details This function is designed to support forest management practices by providing both plot-level
#'   and intervention-level summaries. It is useful for assessing the effectiveness of forest interventions
#'   and understanding the structural changes in forest stands. The calculations include stem volume, basal area,
#'   and dominant species identification, enabling comprehensive forest assessments.
#'
#' @examples
#' data <- as.data.frame(list(
#'   ForManInt = c("Intervention_A","Intervention_A","Intervention_A","Intervention_B","Intervention_B","Intervention_B"),
#'   plot_col = c("Plot_1", "Plot_1","Plot_1","Plot_2","Plot_2","Plot_2"),
#'   dbh_col = c(10,15,18,30,35,45),    # Diameter at breast height (DBH) in cm
#'   th_col = c(15, 18,19,25,26,32),     # Tree height in meters
#'   specie_col = c("norway spruce", "norway spruce","norway spruce","norway spruce", "norway spruce","norway spruce")
#' ))
#' plot_area = 530
#'
#' results <-  Apply_StemVolumeCalculator(
#'   data = data,
#'   ForManInt_option = "Yes",
#'   ForManInt = "ForManInt",
#'   plot_option = "Yes",
#'   plot_col = "plot_col",
#'   plot_area = 530,
#'   dbh_col = "dbh_col",
#'   th_col = "th_col",
#'   specie_col = "specie_col"
#' )
#' # View the calculated results
#' print(results)
#' @export
Apply_StemVolumeCalculator <- function(data, ForManInt_option, ForManInt, plot_option, plot_col, plot_area, dbh_col, th_col, specie_col) {
  # Check if ForManInt is included in the dataset and set default if not
  if (ForManInt_option == "No") {
    data[[ForManInt]] <- "No_ForManInt"
  }
    # Check if plot information is included in the dataset and set default if not
  if (plot_option == "No") {
    data[[plot_col]] <- "1"
  }
    # Select the columns within the dataset
  required_columns <- c(ForManInt, plot_col, dbh_col, th_col, specie_col)
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("The following columns are missing in the dataset:", paste(missing_columns, collapse = ", ")))
  }
  # Select and rename relevant columns
  data <- data %>%
    dplyr::select(all_of(c(plot_col, dbh_col, th_col, specie_col, ForManInt))) %>%
    rename(plot = all_of(plot_col), dbh = all_of(dbh_col), th = all_of(th_col), specie = all_of(specie_col))
  # Stem Volume Calculation
  data$vol_dm3 <- NA
  num_rows <- nrow(data)
  for (i in 1:num_rows) {
    raw_data <- data[i, ]
    result <- StemVolumeCalculator(data = raw_data, dbh_col = "dbh", th_col = "th", specie_col = "specie")
    data[i, "vol_dm3"] <- result
  }
  # Summarize Volume per Plot
  output_vol <- data %>%
    group_by(plot) %>%
    reframe(
      ForManInt = unique(ForManInt),
      vol_m3_plot = sum(vol_dm3 * 0.001, na.rm = TRUE)
    ) %>%
    mutate(vol_m3_ha = vol_m3_plot * (10000 / plot_area)) %>%
    data.frame()

  output_vol <- output_vol %>% mutate(across(where(is.character), as.factor))
  # Basal area (m2) Calculation - step1
  for (i in 1:num_rows) {
    raw_data <- data[i, ]
    result <- BA_Calculator(raw_data, "dbh")
    data[i, "BA_m2"] <- result
  }
  data <- data %>% mutate(across(where(is.character), as.factor))
   # Basal area (m2) Calculation step2
    aa <- data %>%
    group_by(across(all_of(c("plot", "specie")))) %>%
    summarise(Sum_BA_m2 = sum(BA_m2, na.rm = TRUE), .groups = "drop")
   # Dominant Tree Species detection
    add <- aa %>%
      group_by(plot) %>%
      do({
        result <- DominantTreeByPlot(., BA_col = "Sum_BA_m2", specie_col = "specie")
        tibble(Pure_Mixed_Stands = result[[1]], DomTreeSpecies = result[[2]])
      }) %>%
      ungroup()
    # Data Set organization
    output_plot <- left_join(output_vol, add, by = "plot") %>%
      select(ForManInt, everything()) %>%
      mutate(across(where(is.numeric), ~ round(., 2))) %>%
      mutate(ID = row_number()) %>%
      select(ID, everything())
  # Forest Management Intervention Summary
  output_ForManInt <- output_vol %>%
    group_by(across(all_of(c("ForManInt")))) %>%
    summarise(
      Mean_vol_m3_plot = mean(vol_m3_plot, na.rm = TRUE),
      Mean_vol_m3_ha = mean(vol_m3_ha, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(where(is.integer), as.numeric))
 # Data Set organization
  output_ForManInt <- output_ForManInt %>%
    mutate(across(where(is.numeric), ~ round(., 2))) %>%
    mutate(ID = row_number()) %>%
    select(ID, everything())
  # Return the results as two list
  return(list(output_plot, output_ForManInt))
}


#' Carbon Stock Calculation for Multiple Plots and Forest Management Practices
#'
#' This function calculates the above-ground biomass (AGB) and carbon stock (CS) for forest plots,
#' considering forest management interventions (ForManInt) and the dominant tree species (DomTreeSpecies).
#' It summarizes the data by forest management intervention type, calculating the mean values for carbon stock and biomass.
#' The function can handle different types of forest management interventions and provides both plot-level and intervention-level summaries.
#'
#' @param data A data frame containing the forest inventory data. The data should include columns for tree volume,
#'   species information, and optionally, forest management interventions and plot identifiers.
#' @param ForManInt_option A character string indicating whether forest management intervention data is included ("Yes"/"No").
#'   If "Yes", the function will consider the column specified in the `ForManInt` argument for calculations.
#' @param ForManInt A character string representing the name of the column in `data` that contains information on forest management
#'   interventions (e.g., thinning, clearcut). This column is used if `ForManInt_option` is "Yes".
#' @param plot_option A character string indicating whether plot information is included in the data ("Yes"/"No"). If "Yes",
#'   the function will process the data at the plot level and include plot-level summaries in the output.
#' @param plot_col A character string representing the name of the column in `data` that holds the plot identifiers.
#'   This column is used if `plot_option` is "Yes".
#' @param DomTreeSpecies A character string representing the name of the column in `data` that contains the dominant tree species information.
#'   This column helps identify species-specific carbon stock and biomass.
#' @param vol_col A character string representing the name of the column in `data` that contains the tree volume (in cubic meters per hectare).
#'   The volume is used in the calculation of AGB and CS.
#'
#' @return A list containing two data frames:
#'   \itemize{
#'     \item \code{output_plot}: The original dataset with added columns for calculated above-ground biomass (AGB) and carbon stock (CS) at the plot level.
#'     \item \code{output_ForManInt}: A summarized dataset with mean values of AGB and CS grouped by forest management intervention type.
#'   }
#'
#' @details This function calculates AGB using standard allometric equations and then estimates the carbon stock (CS) based on AGB.
#'   The function can compute these metrics for each plot, and also group and summarize by forest management intervention type.
#'   This is useful for understanding the effects of different forest management strategies on carbon sequestration and biomass accumulation.
#'
#' @examples
#' # Example dataset
#' data <- data.frame(
#'   ForManInt = c("Intervention_A", "Intervention_A", "Intervention_A",
#'                 "Intervention_B", "Intervention_B", "Intervention_B"),
#'   DomTreeSpecies = c("larches", "larches", "larches",
#'                      "larches", "larches", "larches"),
#'   plot = c("Plot1", "Plot1", "Plot1", "Plot2", "Plot2", "Plot2"),
#'   vol_m3_ha = c(120.5, 85.3, 150.2, 95.7, 74.2, 84.2)
#' )
#'
#' # Apply the Carbon Stock Calculator
#' results <- Apply_CarbonStockCalculator(
#'   data = data,
#'   ForManInt_option = "Yes",   # Include forest management intervention
#'   ForManInt = "ForManInt",
#'   plot_option = "Yes",        # Include plot-level information
#'   plot_col = "plot",
#'   DomTreeSpecies = "DomTreeSpecies",
#'   vol_col = "vol_m3_ha"
#' )
#'
#' # View the calculated results
#' print(results)
#'
#' @import dplyr
#' @export
Apply_CarbonStockCalculator <- function(data, ForManInt_option, ForManInt, plot_option, plot_col, DomTreeSpecies, vol_col) {
  # Check if ForManInt is included in the dataset and set default if not
  if (ForManInt_option == "No") {
    data[[ForManInt]] <- "No_ForManInt"
  }
  # Check if plot information is included in the dataset and set default if not
  if (plot_option == "No") {
    data[[plot_col]] <- "1"
  }
  # Ensure required columns are present
  required_columns <- c(ForManInt, DomTreeSpecies, plot_col, vol_col)
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("The following columns are missing in the dataset:", paste(missing_columns, collapse = ", ")))
  }
  # Prepare data for plot analysis
  output_plot <- data %>% dplyr::select(all_of(required_columns))
  colnames(output_plot) <- c("ForManInt", "DomTreeSpecies", "plot", "vol_m3_ha")
  output_plot$plot <- as.factor(output_plot$plot)
  # Initialize columns for AGB and CS
  output_plot$AGB_tn_ha <- NA
  output_plot$CS_tn_ha <- NA
  # Apply CarbonStockCalculator to each plot
  for (i in 1:nrow(output_plot)) {
    temp <- output_plot[i, , drop = FALSE]#
    result <- CarbonStockCalculator(data = temp, category_col=NULL, typology_col="DomTreeSpecies", vol_col="vol_m3_ha", area_col = 1)
    if (!is.null(result)) {
      output_plot[i, c("AGB_tn_ha", "CS_tn_ha")] <- result
    }
  }
  # Summarize data by forest management intervention type
  output_ForManInt <- output_plot %>%
    group_by(across(all_of(c("ForManInt")))) %>%
    summarise(
      TotalPlots = n_distinct(plot),
      Mean_vol_m3_ha = mean(vol_m3_ha, na.rm = TRUE),
      Mean_AGB_tn_ha = mean(AGB_tn_ha, na.rm = TRUE),
      Mean_CS_tn_ha = mean(CS_tn_ha, na.rm = TRUE),
      .groups = "drop"
    )
  # Data Set organization
  output_plot <- output_plot %>%
    mutate(across(where(is.numeric), ~ round(., 2))) %>%
    mutate(ID = row_number()) %>%
    select(ID, everything())

  # Data Set organization
  output_ForManInt <- output_ForManInt %>%
    mutate(across(where(is.integer), as.numeric)) %>%
    mutate(across(where(is.numeric), ~ round(., 2))) %>%
    mutate(ID = row_number()) %>%
    select(ID, everything())
  # Return the results as two list
  return(list(output_plot, output_ForManInt))
}
