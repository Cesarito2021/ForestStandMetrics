#' Calculate Stem Volume for Various Tree Species
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
#' data <- data.frame(DBH = c(30, 25, 45), Height = c(15, 18, 20), Species = c("silver fir", "cypress", "oak"))
#' StemVolumeCalculator(data, "DBH", "Height", "Species")
#' @export
StemVolumeCalculator <-function(data, dbh_col, th_col, specie_col){
  # Check if the columns exist and retrieve their values, ensuring they are numeric
  dbh <- as.numeric(data[[dbh_col]])
  th <- as.numeric(data[[th_col]])
  specie <- tolower(trimws(data[[specie_col]]))  # Remove extra spaces and convert to lowercase
  #
  # Handle missing or invalid values
  if (is.na(dbh) || is.na(th) || specie == "") {
    return(NA)  # Return NA for invalid data
  }
  if(specie %in% c("abies alba mill","silver fir")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (-1.8381)+(3.7836*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))+(3.9934*10^-1*as.numeric(dbh))
    return(c(vol))
  }
  #2
  else if(specie %in% c("cupressus spp","cypress")){
    # vume del fusto e dei rami grossi
    vol = (-2.6735)+(3.6590*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))+(6.4725*10^-1*as.numeric(dbh))
    return(c(vol))
  }
  #3
  else if(specie %in% c("larix decidua mill","european larch")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (-1.6519*10)+(2.9979*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))+(3.1506*as.numeric(dbh))
    return(c(vol))
  }
  #4
  else if(specie %in% c("picea abies k",	"norway spruce")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (-9.1298)+(3.4866*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))+(1.4633*as.numeric(dbh))
    return(c(vol))
  }
  #5
  else if(specie %in% c("pinus cembra l","arolla pine")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (2.8521)+(3.9504*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))
    return(c(vol))
  }
  #6
  else if(specie %in% c("pinus halepensis mill",	"aleppo pine")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (-1.2508*10^-1)+(3.1518*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))+(2.3748*as.numeric(dbh))
    return(c(vol))
  }
  #7
  else if(specie %in% c("pinus laricio poiret",	"corsican pine")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (6.4383)+(3.8594*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))
    return(c(vol))
  }
  #8
  else if(specie %in% c("pinus nigra arn","	black pine")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (-2.1480*10)+(3.3448*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))+(2.9088*as.numeric(dbh))
    return(c(vol))
  }
  #9
  else if(specie %in% c("pinus pinaster ait",	"maritime pine")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (2.9963)+(3.8302*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))
    return(c(vol))
  }
  #10
  else if(specie %in% c("pinus pinea l",	"stone pine")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (-4.0404*10^-1)+(4.1113*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))
    return(c(vol))
  }
  #11
  else if(specie %in% c("pinus sylvestris l","scots pine")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (3.1803)+(3.9899*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))
    return(c(vol))
  }
  #12
  else if(specie %in% c("exotic pines group")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (2.6279)+(3.3389*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))
    return(c(vol))
  }
  #13
  else if(specie %in% c("pseudotsuga menziesii franco",	"douglas fir")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (-7.9946)+(3.3343*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))+(1.2186*as.numeric(dbh))
    return(c(vol))
  }
  #14
  else if(specie %in% c("small conifers")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (2.1414)+(3.4914*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))
    return(c(vol))
  }
  #15
  else if(specie %in% c("acer spp","maple")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (1.6905)+(3.7082*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))
    return(c(vol))
  }
  #16
  else if(specie %in% c("alnus spp","alder")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (-2.2932*10)+(3.2641*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))+(2.9991*as.numeric(dbh))
    return(c(vol))
  }
  #17
  else if(specie %in% c("carpinus-ostrya spp",	"hornbeam")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (-1.4983)+(3.8828*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))
    return(c(vol))
  }
  #18
  else if(specie %in% c("castanea sativa mill",	"sweet chestnut")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (-2.0010)+(3.6524*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))+(7.4466*10^-1*as.numeric(dbh))
    return(c(vol))
  }
  #19
  else if(specie %in% c("eucalyptus spp","eucalyptus")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (-1.3789)+(4.5811*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))
    return(c(vol))
  }
  #20
  else if(specie %in% c("fagus sylvatica L",	"european Beech")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (8.1151*10^-1)+(3.8965*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))
    return(c(vol))
  }
  #21
  else if(specie %in% c("fraxinus spp",	"ash")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (-1.1137*10^-1)+(3.9108*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))
    return(c(vol))
  }
  #22
  else if(specie %in% c("quercus cerris L",	"turkey oak")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (-4.3221*10^-2)+(3.8079*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))
    return(c(vol))
  }
  #23
  else if(specie %in% c("quercus ilex L",	"holm oak")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (-2.2219)+(3.9685*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))+(6.2762*10^-1*as.numeric(dbh))
    return(c(vol))
  }
  #24
  else if(specie %in% c("quercus pubescens willd",	"downy oak")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (5.1025*10^-1)+(4.5184*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))+(-3.6026*10^-1*as.numeric(dbh))
    return(c(vol))
  }
  #25
  else if(specie %in% c("robinia pseudoacacia l","black Locust")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (-2.1214)+(3.7123*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))+(1.4296*10^-1*as.numeric(dbh))
    return(c(vol))
  }
  #26
  else if(specie %in% c("salix spp",	"willow")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (-2.3140)+(3.8926*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))
    return(c(vol))
  }
  #27
  else if(specie %in% c("other broadleaves group",	"other broadleaves")){
    # GS (m3 ha-1) *BEF*WBD (t.d.m. m-3 f.v.) *A (ha)
    vol = (2.3118)+(3.1278*10^-2)*(as.numeric(dbh)^2)*(as.numeric(th))+(3.7159*10^-1*as.numeric(dbh))
    return(c(vol))
  }
  else {
    return("TreeSpecies not registered")
  }
}



#' Carbon Stock Calculator
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
#' data <- data.frame(category = "stands",typology = "norway spruce",volume =0.5,area = 1)
#' CarbonStockCalculator(data, "stands", "typology", "volume", "area")
#'
#' #' @export
CarbonStockCalculator <- function(data, category_col, typology_col, vol_col, area_col) {
  category <- tolower(trimws(data[[category_col]]))
  typology <- tolower(trimws(data[[typology_col]]))
  vol <- as.numeric(data[[vol_col]])
  area <- as.numeric(data[[area_col]])

  if (category == "stands") {
    # Norway Spruce
    if (typology %in% c("norway spruce")) {
      agb <- vol * 1.29 * 0.38 * area
      cs <- agb * 0.5
      return(c(AGB = agb, CS = cs))
    }
    # Silver Fir
    else if (typology %in% c("silver fir")) {
      agb <- vol * 1.34 * 0.38 * area
      cs <- agb * 0.5
      return(c(AGB = agb, CS = cs))
    }
    # European Larch
    else if (typology %in% c("larches", "european larch")) {
      agb <- vol * 1.22 * 0.56 * area
      cs <- agb * 0.5
      return(c(AGB = agb, CS = cs))
    }
    # Mountain Pines, Black Pine, etc.
    else if (typology %in% c("mountain pines", "arolla pine", "black pine", "scots pine")) {
      agb <- vol * 1.53 * 0.47 * area
      cs <- agb * 0.5
      return(c(AGB = agb, CS = cs))
    }
    # Mediterranean Pines
    else if (typology %in% c("mediterranean pines", "aleppo pine", "corsican pine", "maritime pine", "stone pine")) {
      agb <- vol * 1.53 * 0.53 * area
      cs <- agb * 0.5
      return(c(AGB = agb, CS = cs))
    }
    # Other Conifers
    else if (typology %in% c("other conifers", "cypress", "exotic pines group", "douglas fir", "small conifers", "eucalyptus")) {
      agb <- vol * 1.37 * 0.43 * area
      cs <- agb * 0.5
      return(c(AGB = agb, CS = cs))
    }
    # European Beech
    else if (typology %in% c("european beech")) {
      agb <- vol * 1.36 * 0.61 * area
      cs <- agb * 0.5
      return(c(AGB = agb, CS = cs))
    }
    # Turkey Oak
    else if (typology %in% c("turkey oak")) {
      agb <- vol * 1.45 * 0.69 * area
      cs <- agb * 0.5
      return(c(AGB = agb, CS = cs))
    }
    # Other Oaks
    else if (typology %in% c("other oaks", "holm oak", "downy oak")) {
      agb <- vol * 1.42 * 0.67 * area
      cs <- agb * 0.5
      return(c(AGB = agb, CS = cs))
    }
    # Other Broadleaves
    else if (typology %in% c("other broadleaves", "maple", "alder", "hornbeam", "sweet chestnut", "ash", "black locust", "willow")) {
      agb <- vol * 1.47 * 0.53 * area
      cs <- agb * 0.5
      return(c(AGB = agb, CS = cs))
    }
    # Partial Total
    else if (typology %in% c("partial total")) {
      agb <- vol * 1.42 * 0.67 * area
      cs <- agb * 0.5
      return(c(AGB = agb, CS = cs))
    }
    # Typology not registered
    else {
      return("Typology not registered")
    }
  } else {
    return("Category not recognized")
  }
}


#' Calculate Basal Area
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
#' BA_Calculator(data, dbh_col = "dbh")
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

#' Detect Dominant Tree Species
#'
#' This function detects the dominant tree species in a stand based on basal area. If more than 50% of the
#' stand's basal area is made up of one species, it is considered the dominant species.
#'
#' @param data A data frame containing tree data with a column for species and basal area.
#' @return A character vector containing the type of stand ("Pure" or "Mixed") and the dominant species or "Mixed" if no species exceeds 50% of the basal area.
#' @examples
#' data <- data.frame(specie = c("Pine", "Oak", "Birch"),Sum_BA_m2 = c(30, 20, 5))
#' DomTreeSpeciesDetection(data)
#' @export
DomTreeSpeciesDetection <- function(data) {
  # Check if 'specie' column is missing
  if (!"specie" %in% colnames(data)) {
    stop("Column 'specie' is missing from the data.")
  }

  # Check if 'Sum_BA_m2' column is missing
  if (!"Sum_BA_m2" %in% colnames(data)) {
    stop("Column 'Sum_BA_m2' is missing from the data.")
  }
  # Calculate percentage of Basal Area
  total_BA <- sum(data$Sum_BA_m2, na.rm = TRUE)
  data$per <- (data$Sum_BA_m2 / total_BA) * 100

  # Find the dominant species
  max_BA_species <- data[which.max(data$per), "specie"]
  dominant_species <- ifelse(max(data$per, na.rm = TRUE) > 50, max_BA_species, "Mixed")

  # Determine stand type and dominant species
  if (dominant_species == "Mixed") {
    return(c("Mixed", "other broadleaf/coniferous"))
  } else {
    return(c("Pure", dominant_species))
  }
}



#' Apply Stem Volume Calculator and Forest Management Interventions
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
#' \dontrun{
#' data <- read.csv("forest_inventory.csv")
#' result <- Apply_StemVolumeCalculator(data, "Yes", "Thinning", "Yes", "plot_id", 10, "dbh", "height", "species")
#' }
#'
#' @export

Apply_StemVolumeCalculator <- function(data, ForManInt_option, ForManInt, plot_option, plot_col, plot_area, dbh_col, th_col, specie_col) {

  # Check if the necessary columns are included in the dataset
  if (ForManInt_option == "No") {
    data[[ForManInt]] <- "No_ForManInt"
  }
  if (plot_option == "No") {
    data[[plot_col]] <- "1"
  }

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

  # BA Calculation
  for (i in 1:num_rows) {
    raw_data <- data[i, ]
    result <- BA_Calculator(raw_data, "dbh")
    data[i, "BA_m2"] <- result
  }

  data <- data %>% mutate(across(where(is.character), as.factor))

  # Dominant Tree Species Detection
  aa <- data %>%
    group_by(across(all_of(c("plot", "specie")))) %>%
    summarise(Sum_BA_m2 = sum(BA_m2, na.rm = TRUE), .groups = "drop")

  add <- aa %>%
    group_by(plot) %>%
    do({
      result <- DomTreeSpeciesDetection(.)
      tibble(Pure_Mixed_Stands = result[1], DomTreeSpecies = result[2])
    }) %>%
    ungroup()

  # Forest Management Intervention Summary
  output_ForManInt <- output_vol %>%
    group_by(across(all_of(c("ForManInt")))) %>%
    summarise(
      Mean_vol_m3_plot = mean(vol_m3_plot, na.rm = TRUE),
      Mean_vol_m3_ha = mean(vol_m3_ha, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(where(is.integer), as.numeric))

  # Join Results and Round Numeric Columns
  output_plot <- left_join(output_vol, add, by = "plot") %>%
    select(ForManInt, everything()) %>%
    mutate(across(where(is.numeric), ~ round(., 2))) %>%
    mutate(ID = row_number()) %>%
    select(ID, everything())

  output_ForManInt <- output_ForManInt %>%
    mutate(across(where(is.numeric), ~ round(., 2))) %>%
    mutate(ID = row_number()) %>%
    select(ID, everything())

  return(list(output_plot, output_ForManInt))
}


#' Apply Carbon Stock Calculator to Forest Inventory Data
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
#' \dontrun{
#' data <- read.csv("forest_inventory.csv")
#' result <- Apply_CarbonStockCalculator(data, "Yes", "Thinning", "Yes", "plot_id", "species", "volume")
#' }
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
    temp <- output_plot[i, ]
    result <- CarbonStockCalculator(data = temp, category_col = "stands", typology_col = "DomTreeSpecies", vol_col = "vol_m3_ha", area_col = 1)

    if (!is.null(result)) {
      output_plot[i, c("AGB_tn_ha", "CS_tn_ha")] <- result
    }
  }

  # Round numeric values and add an ID column
  output_plot <- output_plot %>%
    mutate(across(where(is.numeric), ~ round(., 2))) %>%
    mutate(ID = row_number()) %>%
    select(ID, everything())

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

  # Convert integers to numeric and round
  output_ForManInt <- output_ForManInt %>%
    mutate(across(where(is.integer), as.numeric)) %>%
    mutate(across(where(is.numeric), ~ round(., 2))) %>%
    mutate(ID = row_number()) %>%
    select(ID, everything())

  # Return the results as a list
  return(list(output_plot, output_ForManInt))
}


