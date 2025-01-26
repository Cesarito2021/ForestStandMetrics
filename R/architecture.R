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
#' results <- StemVolumeArch(data, "DBH", "Height", "Species")
#' print(results)
#' (Rest of the documentation)
#  Note: no @export tag here.
StemVolumeArch <-function(data, dbh_col, th_col, specie_col){
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

#' Carbon Stock Architecture
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
#' results <- CarbonStockArch(data, "stands", "typology", "volume", area_ha)
#' print(results)
#' (Rest of the documentation)
#  Note: no @export tag here.
CarbonStockArch <- function(data, category_col=NULL, typology_col, vol_col, area_col) {
  if (!is.null(category_col)) {
    category <- tolower(trimws(category_col))
  } else {
    category <- "stands"
  } # unique category
  #
  typology <- tolower(trimws(data[[typology_col]]))
  vol <- as.numeric(data[[vol_col]])
  area <- as.numeric(area_col)# unique number

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

#' Lying and Standing Deadwood Volume Architecture
#'
#' This function calculates the volume of deadwood for either lying deadwood (LDT), coarse woody debris (CWD), or both.
#' The calculation uses input tree measurements such as total height (TH), diameter at breast height (DBH),
#' total length of coarse woody debris (L_tot), and diameter at half the length (Dhalf). Depending on the selected option,
#' the function computes the respective volume and returns the results.
#'
#' @param data A data frame containing the tree measurement data. The data should include the relevant columns specified
#' for the calculation.
#' @param TH_tot_col A character string specifying the column name for the total height (m).
#' @param DBH_col A character string specifying the column name for diameter at breast height (DBH in cm).
#' @param L_tot_col A character string specifying the column name for the total length (m) of coarse woody debris (CWD).
#' @param Dhalf_col A character string specifying the column name for the diameter at half the length (cm) of CWD.
#' @param LDT_CWD_option A character string indicating which volume to calculate:
#'        "ldt" for lying deadwood, "cwd" for coarse woody debris, or "both" for both volumes.
#'
#' @return A numeric vector with the calculated volume(s) in cubic meters, depending on the specified option.
#'         If "ldt" is chosen, the function returns the volume of lying deadwood.
#'         If "cwd" is chosen, it returns the volume of coarse woody debris.
#'         If "both" is selected, it returns the sum of both volumes.
#'
#' @examples
#' data <- data.frame(TH_tot = c(10, 15), DBH = c(30, 40), L_tot = c(8, 12), Dhalf = c(20, 30))
#' # Calculate Lying Deadwood Volume
#' LDT_CWD_Calculator(data, TH_tot_col = "TH_tot", DBH_col = "DBH", L_tot_col = "L_tot", Dhalf_col = "Dhalf", LDT_CWD_option = "ldt")
#' # Calculate Coarse Woody Debris Volume
#' LDT_CWD_Calculator(data, TH_tot_col = "TH_tot", DBH_col = "DBH", L_tot_col = "L_tot", Dhalf_col = "Dhalf", LDT_CWD_option = "cwd")
#' # Calculate Both Volumes (Lying Deadwood and Coarse Woody Debris)
#' LDT_CWD_Calculator(data, TH_tot_col = "TH_tot", DBH_col = "DBH", L_tot_col = "L_tot", Dhalf_col = "Dhalf", LDT_CWD_option = "both")
#  Note: no @export tag here.
LDT_CWD_Arch <- function(data, TH_tot_col = NULL, DBH_col = NULL, L_tot_col = NULL, Dhalf_col = NULL, LDT_CWD_option) {
  if (!LDT_CWD_option %in% c("ldt", "cwd", "both")) {
    stop("Invalid LDT_CWD_option. Choose 'ldt', 'cwd', or 'both'.")
  }

  ldt_vol_m3 <- NA
  cwd_vol_m3 <- NA

  if (LDT_CWD_option == "ldt") {
    if (is.null(TH_tot_col) || is.null(DBH_col)) {
      stop("For 'ldt', both TH_tot_col and DBH_col must be provided.")
    }
    TH_tot_m <- as.numeric(data[[TH_tot_col]])
    DBH_cm <- as.numeric(data[[DBH_col]])
    DBH_m <- DBH_cm / 100
    factor <- 0.5
    ldt_vol_m3 <- factor * TH_tot_m * (pi / 4) * (DBH_m^2)
    return(ldt_vol_m3)

  } else if (LDT_CWD_option == "cwd") {
    if (is.null(L_tot_col) || is.null(Dhalf_col)) {
      stop("For 'cwd', both L_tot_col and Dhalf_col must be provided.")
    }
    L_tot_m <- as.numeric(data[[L_tot_col]])
    Dhalf_cm <- as.numeric(data[[Dhalf_col]])
    Dhalf_m <- Dhalf_cm / 100
    cwd_vol_m3 <- (pi / 4) * L_tot_m * (Dhalf_m^2)
    return(cwd_vol_m3)

  } else if (LDT_CWD_option == "both") {
    if (is.null(TH_tot_col) || is.null(DBH_col) || is.null(L_tot_col) || is.null(Dhalf_col)) {
      stop("For 'both', all input columns must be provided.")
    }
    TH_tot_m <- as.numeric(data[[TH_tot_col]])
    DBH_cm <- as.numeric(data[[DBH_col]])
    L_tot_m <- as.numeric(data[[L_tot_col]])
    Dhalf_cm <- as.numeric(data[[Dhalf_col]])
    DBH_m <- DBH_cm / 100
    Dhalf_m <- Dhalf_cm / 100
    factor <- 0.5
    ldt_vol_m3 <- factor * TH_tot_m * (pi / 4) * (DBH_m^2)
    cwd_vol_m3 <- (pi / 4) * L_tot_m * (Dhalf_m^2)
    return(c(cwd_vol_m3, ldt_vol_m3))
  }
}

#' Deadwood Volume Calculation for Individual Components (Lying and CWD)
#'
#' This function calculates the volume of standing deadwood (SDT) or snags based on tree measurements.
#' Depending on the selected option, it can compute the volume for standing deadwood, snags, or both types of deadwood.
#'
#' @param data A data frame containing the tree measurement data. It should include columns for
#' total height (TH), diameter at breast height (DBH), total length of snags (L_tot), and diameter at half the length (Dhalf).
#' @param TH_tot_col A character string specifying the column name for total height (m).
#' @param DBH_col A character string specifying the column name for diameter at breast height (DBH in cm).
#' @param L_tot_col A character string specifying the column name for total length (m) of snags.
#' @param Dhalf_col A character string specifying the column name for the diameter at half the length (cm) of snags.
#' @param SDT_SNAG_option A character string to specify which volume to calculate:
#'        "sdt" for standing deadwood, "snag" for snags, or "both" to calculate the volume for both types of deadwood.
#'
#' @return A numeric vector with the calculated volume(s) in cubic meters, depending on the specified option:
#'         - "sdt" returns the volume of standing deadwood.
#'         - "snag" returns the volume of snags.
#'         - "both" returns the sum of both volumes.
#'
#' @examples
#' data <- data.frame(TH_tot= c(10, 15), DBH = c(30, 40), L_tot = c(8, 12), Dhalf = c(20, 30))
#' # Calculate Standing Deadwood Volume
#' SDT_SNAG_Calculator(data, TH_tot_col = "TH_tot", DBH_col = "DBH", L_tot_col = "L_tot", Dhalf_col = "Dhalf", SDT_SNAG_option = "sdt")
#' # Calculate Snag Volume
#' SDT_SNAG_Calculator(data, TH_tot_col = "TH_tot", DBH_col = "DBH", L_tot_col = "L_tot", Dhalf_col = "Dhalf", SDT_SNAG_option = "snag")
#' # Calculate Both Standing Deadwood and Snag Volumes
#' SDT_SNAG_Calculator(data, TH_tot_col = "TH_tot", DBH_col = "DBH", L_tot_col = "L_tot", Dhalf_col = "Dhalf", SDT_SNAG_option = "both")
#  Note: no @export tag here.
SDT_SNAG_Arch <- function(data, TH_tot_col = NULL, DBH_col = NULL, L_tot_col = NULL, Dhalf_col = NULL, SDT_SNAG_option) {
  if (!SDT_SNAG_option %in% c("sdt", "snag", "both")) {
    stop("Invalid SDT_SNAG_option. Choose 'sdt', 'snag', or 'both'.")
  }

  sdt_vol_m3 <- NA
  snag_vol_m3 <- NA

  if (SDT_SNAG_option == "sdt") {
    if (is.null(TH_tot_col) || is.null(DBH_col)) {
      stop("For 'sdt', both TH_tot_col and DBH_col must be provided.")
    }
    TH_tot_m <- as.numeric(data[[TH_tot_col]])
    DBH_cm <- as.numeric(data[[DBH_col]])
    DBH_m <- DBH_cm / 100
    factor <- 0.5
    sdt_vol_m3 <- factor * TH_tot_m * (pi / 4) * (DBH_m^2)
    return(sdt_vol_m3)

  } else if (SDT_SNAG_option == "snag") {
    if (is.null(L_tot_col) || is.null(Dhalf_col)) {
      stop("For 'snag', both L_tot_col and Dhalf_col must be provided.")
    }
    L_tot_m <- as.numeric(data[[L_tot_col]])
    Dhalf_cm <- as.numeric(data[[Dhalf_col]])
    Dhalf_m <- Dhalf_cm / 100
    snag_vol_m3 <- (pi / 4) * L_tot_m * (Dhalf_m^2)
    return(snag_vol_m3)

  } else if (SDT_SNAG_option == "both") {
    if (is.null(TH_tot_col) || is.null(DBH_col) || is.null(L_tot_col) || is.null(Dhalf_col)) {
      stop("For 'both', all input columns must be provided.")
    }
    TH_tot_m <- as.numeric(data[[TH_tot_col]])
    DBH_cm <- as.numeric(data[[DBH_col]])
    L_tot_m <- as.numeric(data[[L_tot_col]])
    Dhalf_cm <- as.numeric(data[[Dhalf_col]])
    DBH_m <- DBH_cm / 100
    Dhalf_m <- Dhalf_cm / 100
    factor <- 0.5
    sdt_vol_m3 <- factor * TH_tot_m * (pi / 4) * (DBH_m^2)
    snag_vol_m3 <- (pi / 4) * L_tot_m * (Dhalf_m^2)
    return(c(snag_vol_m3, sdt_vol_m3))
  }
}

#' Deadwood Volume Calculator using Lombardi's Formula Architecture
#'
#' This function calculates the total volume of deadwood (including LDT, SDT, CWD, Snags, and Stumps)
#' using Lombardi's formula. The formula requires tree height, maximum diameter, and
#' minimum diameter measurements to estimate the volume of deadwood in cubic meters.
#' This method is widely used in forestry for estimating the volume of various deadwood types in a forest stand.
#'
#' @param data A data frame containing the tree measurement data. The data frame should include columns
#'        for tree height (H_Len), maximum diameter (D_max), and minimum diameter (D_min).
#' @param H_Len_col A character string specifying the column name for tree height (m).
#' @param D_max_col A character string specifying the column name for the maximum diameter (cm).
#' @param D_min_col A character string specifying the column name for the minimum diameter (cm).
#'
#' @return A numeric vector containing the calculated deadwood volume(s) in cubic meters for each tree
#'         or for the entire dataset, depending on how the function is applied.
#'         The result represents the total volume of deadwood for each observation.
#'
#' @examples
#' data <- data.frame(H_Len = c(10, 12), D_max = c(30, 35), D_min = c(20, 25))
#' All_Deadwood_Calculator(data, H_Len_col = "H_Len", D_max_col = "D_max", D_min_col = "D_min")
#'
#' # Example with a dataset including multiple trees:
#' data_multiple <- data.frame(H_Len = c(10, 12, 14), D_max = c(30, 35, 40), D_min = c(20, 25, 28))
#' All_Deadwood_Calculator(data_multiple, H_Len_col = "H_Len", D_max_col = "D_max", D_min_col = "D_min")
# Note: no @export tag here.
All_Deadwood_Arch <- function(data, H_Len_col = NULL, D_max_col = NULL, D_min_col = NULL) {
  # Validate required columns
  if (is.null(H_Len_col) || is.null(D_max_col) || is.null(D_min_col)) {
    stop("For deadwood volume calculation, H_Len_col, D_max_col, and D_min_col must be provided.")
  }

  # Extract and convert the input data
  H_Len_m <- as.numeric(data[[H_Len_col]])
  D_max_m <- (as.numeric(data[[D_max_col]]) / 100) / 2  # Convert to radius (m)
  D_min_m <- (as.numeric(data[[D_min_col]]) / 100) / 2  # Convert to radius (m)

  # Apply Lombardi's formula for deadwood volume calculation
  volume_m3 <- (pi * H_Len_m / 3) * (D_max_m^2 + D_min_m^2 + (D_max_m * D_min_m))

  return(volume_m3)
}
