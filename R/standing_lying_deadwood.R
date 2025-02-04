#' Deadwood Volume Calculation for Individual Components (Lying and CWD)
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
#' @export
LDT_CWD_Calculator <- function(data, TH_tot_col, DBH_col, L_tot_col, Dhalf_col, LDT_CWD_option) {
  LDT_CWD_Arch(data = data, TH_tot_col, DBH_col, L_tot_col, Dhalf_col, LDT_CWD_option)
}

#'Deadwood Volume Calculation for Individual Components (Standing and Snag)
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
#' @export
SDT_SNAG_Calculator <- function(data, TH_tot_col, DBH_col, L_tot_col, Dhalf_col, SDT_SNAG_option) {
  SDT_SNAG_Arch(data, TH_tot_col, DBH_col, L_tot_col, Dhalf_col, SDT_SNAG_option)
}

#' Deadwood Volume Calculation for Individual Components (Double Input for Diameters: Dmax and Dmin)
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
#' @export
All_Deadwood_Calculator <- function(data, H_Len_col, D_max_col, D_min_col) {
  All_Deadwood_Arch(data, H_Len_col, D_max_col, D_min_col)
}

#' Deadwood Volume Calculation (Lying and CWD) for Multiple Plots and Forest Management Practices
#'
#' This function calculates the volumes of Lying Deadwood (LDT) or Coarse Woody Debris (CWD)
#' based on tree attributes such as diameter at breast height (DBH), tree height, and
#' additional parameters like the total length of the tree (L_tot) and its half-diameter
#' (Dhalf). The calculation is performed for each plot, and the function can group the results
#' by forest management interventions.
#'
#' @param data A data frame containing tree attributes for each plot. The data must include
#'   relevant columns such as DBH, tree height, and forest management intervention information.
#'   Additional parameters for the volume calculations, such as L_tot and Dhalf, are also required for CWD.
#' @param ForManInt_option A character string ("Yes" or "No") indicating whether forest management intervention
#'   data is available in the dataset. Defaults to "No" if not provided. If "Yes", the column specified in
#'   the `ForManInt` argument will be used to group the results.
#' @param ForManInt A character string representing the name of the column in the dataset that specifies the
#'   forest management intervention (e.g., thinning, clearcut). Used if `ForManInt_option` is "Yes".
#' @param plot_option A character string ("Yes" or "No") indicating whether plot-level information is available.
#'   Defaults to "No" if not provided. If "Yes", the data will be grouped by plots and summarized accordingly.
#' @param plot_col A character string representing the name of the column in the dataset that specifies the plot identifier.
#'   Used if `plot_option` is "Yes".
#' @param LDT_CWD_option A character string specifying which type of deadwood to calculate ("ldt" for Lying Deadwood or
#'   "cwd" for Coarse Woody Debris). The corresponding calculation will be applied based on this option.
#' @param L_tot_col A character string representing the name of the column in the dataset for the total tree length
#'   (L_tot), used in the CWD volume calculation.
#' @param Dhalf_col A character string representing the name of the column in the dataset for the half-diameter
#'   (Dhalf), used in the CWD volume calculation.
#' @param TH_tot_col A character string representing the name of the column in the dataset for the total tree height
#'   (TH_tot), used in the LDT volume calculation.
#' @param DBH_col A character string representing the name of the column in the dataset for the diameter at breast height
#'   (DBH), used in the LDT volume calculation.
#' @param plot_area A numeric value representing the area of the plot in square meters. This is used to calculate
#'   the volume per hectare.
#'
#' @return A list containing two data frames:
#' \item{output_plot}{A data frame with the calculated volume for each plot, including the volume of LDT or CWD per hectare.}
#' \item{output_ForManInt}{A summary data frame showing the average volume of LDT or CWD per hectare, grouped by forest management intervention.}
#'
#' @details This function allows for the calculation of volumes of Lying Deadwood (LDT) or Coarse Woody Debris (CWD)
#'   based on tree attributes, and can group results by plot or forest management intervention type. The specific
#'   calculation for LDT or CWD depends on the user-specified option (`LDT_CWD_option`).
#'   The volume per hectare is calculated by considering the area of the plot.
#'
#' @examples
#' # Example 1: Using "ldt" option
#' data_ldt <- as.data.frame(list(
#'   ForManInt = c("Intervention_A","Intervention_A","Intervention_A","Intervention_B","Intervention_B","Intervention_B"),
#'   plot_col = c("Plot_1", "Plot_1","Plot_1","Plot_2","Plot_2","Plot_2"),
#'   TH_tot_col= c(15, 18,19,25,26,32),     # Tree height in meters
#'   DBH_col = c(10,15,18,30,35,45)         # Diameter at breast height (DBH) in cm
#' ))
#'
#' plot_area <- 530
#' LDT_CWD_option <- "ldt"
#'
#' results_ldt  <- Apply_LyingDeadwood(data = data_ldt,
#'                     ForManInt_option = "Yes",
#'                     ForManInt = "ForManInt",
#'                     plot_option = "Yes",
#'                     plot_col = "plot_col",
#'                     LDT_CWD_option = LDT_CWD_option,
#'                     L_tot_col = NULL,
#'                     Dhalf_col = NULL,
#'                     TH_tot_col = "TH_tot_col",
#'                     DBH_col = "DBH_col",
#'                     plot_area = plot_area)
#'print(results_ldt)
#' # Example 2: Using "cwd" option
#' data_cwd <- as.data.frame(list(
#'   ForManInt = c("Intervention_A","Intervention_A","Intervention_A","Intervention_B","Intervention_B","Intervention_B"),
#'   plot_col = c("Plot_1", "Plot_1","Plot_1","Plot_2","Plot_2","Plot_2"),
#'   L_tot_col= c(15, 18,19,25,26,32),     # Tree height in meters
#'   Dhalf_col = c(10,15,18,30,35,45)       # Half-diameter (cm)
#' ))
#'
#' plot_area <- 530
#' LDT_CWD_option <- "cwd"
#'
#' results_cwd <- Apply_LyingDeadwood(data = data_cwd,
#'                     ForManInt_option = "Yes",
#'                     ForManInt = "ForManInt",
#'                     plot_option = "Yes",
#'                     plot_col = "plot_col",
#'                     LDT_CWD_option = LDT_CWD_option,
#'                     L_tot_col = "L_tot_col",
#'                     Dhalf_col = "Dhalf_col",
#'                     TH_tot_col = NULL,
#'                     DBH_col = NULL,
#'                     plot_area = plot_area)
#'print(results_cwd)
#' @import dplyr
#' @importFrom dplyr %>%
#' @export
Apply_LyingDeadwood <- function(data, ForManInt_option, ForManInt, plot_option, plot_col, LDT_CWD_option, L_tot_col, Dhalf_col, TH_tot_col, DBH_col, plot_area) {
  # Check if forest management intervention (ForManInt) is specified; if not, set a default value
  if (ForManInt_option == "No") {
    data[[ForManInt]] <- "No_ForManInt"
  }

  # Check if a single plot is being analyzed; if not, assign all rows to a default "plot 1"
  if (plot_option == "No") {
    data[[plot_col]] <- "1"
  }

  # Select required columns based on the Lying Deadwood or Coarse Woody Debris option
  required_columns <- switch(LDT_CWD_option,
                             "ldt" = c(ForManInt, plot_col, TH_tot_col, DBH_col),
                             "cwd" = c(ForManInt, plot_col, L_tot_col, Dhalf_col)
  )
  # Check for missing columns in the dataset
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("The following columns are missing in the dataset:", paste(missing_columns, collapse = ", ")))
  }
  #
  if (LDT_CWD_option == "cwd" ) {
    output <- data %>%
      dplyr::select(all_of(required_columns)) %>%
      rename_with(~ c("ForManInt", "plot","L_tot_m", "Dhalf_cm")[seq_along(.)])
  } else{
    output <- data %>%
      dplyr::select(all_of(required_columns)) %>%
      rename_with(~ c("ForManInt", "plot","TH_tot_m", "DBH_cm")[seq_along(.)])
  }
  # Ensure the plot column is treated as a factor
  output$plot <- as.factor(output$plot)

  if (LDT_CWD_option == "cwd" && !is.null(output)) {
    output <- output %>%
      mutate(CWD_m3 = NA)
  } else if (LDT_CWD_option == "ldt" && !is.null(output)) {
    output <- output %>%
      mutate( LDT_m3 = NA)
  }
  # Calculate volumes for each row using the helper function

  for (i in seq_len(nrow(output))) {
    temp <- output[i, ]

    result <- LDT_CWD_Calculator(
      data = output[1,],
      TH_tot_col = "TH_tot_m",
      DBH_col = "DBH_cm",
      L_tot_col = "L_tot_m",
      Dhalf_col = "Dhalf_cm",
      LDT_CWD_option = LDT_CWD_option
    )

    # Assign results to the respective columns
    if (LDT_CWD_option == "cwd" && !is.null(result)) {
      output[i, "CWD_m3"] <- result
    } else if (LDT_CWD_option == "ldt" && !is.null(result)) {
      output[i, "LDT_m3"] <- result
    }
  }
  # Assign results to the respective columns
  if (LDT_CWD_option == "cwd" && !is.null(output)) {
    output_plot <- output %>%
      group_by(plot) %>%
      summarise(
        ForManInt = first(ForManInt),
        CWD_m3_plot = sum(CWD_m3, na.rm = TRUE)
      )
    # Convert plot totals to per-hectare values
    output_plot <- output_plot %>%
      mutate(
        CWD_m3_ha = CWD_m3_plot * (10000 / plot_area)

      ) %>%
      mutate(across(where(is.numeric), ~ round(., 3))) %>%
      mutate(ID = row_number()) %>%
      select(ID, everything())

  } else if (LDT_CWD_option == "ldt" && !is.null(output)) {
    output_plot <- output %>%
      group_by(plot) %>%
      summarise(
        ForManInt = first(ForManInt),
        LDT_m3_plot = sum(LDT_m3, na.rm = TRUE)
      )
    # Convert plot totals to per-hectare values
    output_plot <- output_plot %>%
      mutate(
        LDT_m3_ha = LDT_m3_plot * (10000 / plot_area)
      ) %>%
      mutate(across(where(is.numeric), ~ round(., 3))) %>%
      mutate(ID = row_number()) %>%
      select(ID, everything())
  }
  ##
  vector <- c("cwd", "ldt")
  # Assign results to the respective columns
  if (LDT_CWD_option == "cwd" && !is.null(output_plot)) {
    # Summarize results by forest management intervention
    output_ForManInt <- output_plot %>%
      group_by(ForManInt) %>%
      summarise(
        TotalPlots = n_distinct(plot),
        Mean_CWD_m3_ha = mean(CWD_m3_ha, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(across(where(is.numeric), ~ round(., 2))) %>%
      mutate(ID = row_number()) %>%
      select(ID,everything())
    #
  } else if (LDT_CWD_option == "ldt" && !is.null(output_plot)) {
    # Summarize results by forest management intervention
    output_ForManInt <- output_plot %>%
      group_by(ForManInt) %>%
      summarise(
        TotalPlots = n_distinct(plot),
        Mean_LDT_m3_ha = mean(LDT_m3_ha, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(across(where(is.numeric), ~ round(., 2))) %>%
      mutate(ID = row_number()) %>%
      select(ID,everything())
  }
  # Return both plot-level and forest-management summaries
  return(list(output_plot, output_ForManInt))
}


#' Deadwood Volume Calculation (Standing and Snag) for Multiple Plots and Forest Management Practices
#'
#' This function calculates the volume of Standing Deadwood (SDT) or Snags based on tree attributes such as
#' diameter at breast height (DBH), tree height, total length of the tree (L_tot), and half-diameter (Dhalf).
#' The calculation is performed for each plot, and results can be grouped by forest management intervention (ForManInt).
#' Additionally, the function provides plot-level and forest management intervention summaries for the calculated volumes.
#'
#' @param data A data frame containing tree attributes for each plot. Must include relevant columns such as DBH, tree height, and forest management intervention information.
#'   Additional parameters for SDT or Snag calculations (L_tot, Dhalf, etc.) are required as well.
#' @param ForManInt_option A character string ("Yes" or "No") indicating whether forest management intervention
#'   is considered in the dataset. Defaults to "No" if not provided. If "Yes", the column specified in `ForManInt` is used to group results.
#' @param ForManInt A character string representing the name of the column in the dataset that specifies the
#'   forest management intervention (e.g., thinning, clearcut). Used if `ForManInt_option` is "Yes".
#' @param plot_option A character string ("Yes" or "No") indicating whether plot-level information is available in the dataset. Defaults to "No" if not provided.
#'   If "Yes", the data will be grouped by plot and summarized accordingly.
#' @param plot_col A character string representing the name of the column in the dataset that specifies the plot identifier. Used if `plot_option` is "Yes".
#' @param SDT_SNAG_option A character string specifying whether to calculate the volume for Standing Deadwood ("sdt") or Snag ("snag").
#'   The corresponding calculation will be applied based on this option.
#' @param L_tot_col A character string representing the name of the column in the dataset for the total length of the tree (L_tot), used for Snag calculations.
#' @param Dhalf_col A character string representing the name of the column in the dataset for the half-diameter of the tree (Dhalf), used for Snag calculations.
#' @param TH_tot_col A character string representing the name of the column in the dataset for the total height of the tree (TH_tot), used for SDT calculations.
#' @param DBH_col A character string representing the name of the column in the dataset for the diameter at breast height (DBH), used for both SDT and Snag calculations.
#' @param plot_area A numeric value representing the area of each plot in square meters, used to calculate the volume per hectare.
#'
#' @return A list containing two data frames:
#' \item{output_plot}{A data frame summarizing the calculated volumes at the plot level, including volume per hectare.}
#' \item{output_ForManInt}{A data frame summarizing the calculated volumes by forest management intervention, including the average volume per hectare across plots.}
#'
#' @details This function calculates the volume of Standing Deadwood (SDT) or Snags based on the provided tree attributes.
#'   The calculation differs for SDT and Snags, with specific parameters required for each. For SDT, height and DBH are used,
#'   while Snag volume requires additional parameters like L_tot and Dhalf. The function can group results by plot or
#'   forest management intervention based on the user-specified options.
#'
#' @examples
#' # Example 1: Using the "sdt" option
#' data_sdt <- as.data.frame(list(
#'   ForManInt = c("Intervention_A","Intervention_A","Intervention_A","Intervention_B","Intervention_B","Intervention_B"),
#'   plot_col = c("Plot_1", "Plot_1","Plot_1","Plot_2","Plot_2","Plot_2"),
#'   TH_tot_col = c(15, 18, 19, 25, 26, 32),  # Tree height in meters
#'   DBH_col = c(10, 15, 18, 30, 35, 45)     # Diameter at breast height (DBH) in cm
#' ))
#' plot_area <- 530
#' SDT_SNAG_option <- "sdt"
#'results_sdt <- Apply_StandingDeadwood(
#'   data = data_sdt,
#'   ForManInt_option = "Yes",
#'   ForManInt = "ForManInt",
#'   plot_option = "Yes",
#'   plot_col = "plot_col",
#'   SDT_SNAG_option = SDT_SNAG_option,
#'   L_tot_col = NULL,
#'   Dhalf_col = NULL,
#'   TH_tot_col = "TH_tot_col",
#'   DBH_col = "DBH_col",
#'   plot_area = plot_area
#' )
#' print(results_sdt)
#'
#' # Example 2: Using the "snag" option
#' data_snag <- as.data.frame(list(
#'   ForManInt = c("Intervention_A","Intervention_A","Intervention_A","Intervention_B","Intervention_B","Intervention_B"),
#'   plot_col = c("Plot_1", "Plot_1","Plot_1","Plot_2","Plot_2","Plot_2"),
#'   L_tot_col = c(15, 18, 19, 25, 26, 32),  # Total length in meters
#'   Dhalf_col = c(10, 15, 18, 30, 35, 45)  # Half diameter in cm
#' ))
#' plot_area <- 530
#' SDT_SNAG_option <- "snag"
#'results_snag <-  Apply_StandingDeadwood(
#'   data = data_snag,
#'   ForManInt_option = "Yes",
#'   ForManInt = "ForManInt",
#'   plot_option = "Yes",
#'   plot_col = "plot_col",
#'   SDT_SNAG_option = SDT_SNAG_option,
#'   L_tot_col = "L_tot_col",
#'   Dhalf_col = "Dhalf_col",
#'   TH_tot_col = NULL,
#'   DBH_col = NULL,
#'   plot_area = plot_area
#' )
#' print(results_snag)
#' @import dplyr
#' @importFrom dplyr %>%
#' @export
Apply_StandingDeadwood <- function(data, ForManInt_option, ForManInt,plot_option, plot_col, SDT_SNAG_option, L_tot_col, Dhalf_col, TH_tot_col, DBH_col, plot_area) {
  # Check if forest management intervention (ForManInt) is specified; if not, set a default value
  if (ForManInt_option == "No") {
    data[[ForManInt]] <- "No_ForManInt"
  }

  # Check if a single plot is being analyzed; if not, assign all rows to a default "plot 1"
  if (plot_option == "No") {
    data[[plot_col]] <- "1"
  }

  # Select required columns based on the Lying Deadwood or Coarse Woody Debris option
  required_columns <- switch(SDT_SNAG_option,
                             "sdt" = c(ForManInt, plot_col, TH_tot_col, DBH_col),
                             "snag" = c(ForManInt, plot_col, L_tot_col, Dhalf_col)
  )
  # Check for missing columns in the dataset
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("The following columns are missing in the dataset:", paste(missing_columns, collapse = ", ")))
  }
  #
  if (SDT_SNAG_option == "snag" ) {
    output <- data %>%
      dplyr::select(all_of(required_columns)) %>%
      rename_with(~ c("ForManInt", "plot","L_tot_m", "Dhalf_cm")[seq_along(.)])
  } else{
    output <- data %>%
      dplyr::select(all_of(required_columns)) %>%
      rename_with(~ c("ForManInt", "plot","TH_tot_m", "DBH_cm")[seq_along(.)])
  }
  # Ensure the plot column is treated as a factor
  output$plot <- as.factor(output$plot)

  if (SDT_SNAG_option == "snag" && !is.null(output)) {
    output <- output %>%
      mutate(SNAG_m3 = NA)
  } else if (SDT_SNAG_option == "sdt" && !is.null(output)) {
    output <- output %>%
      mutate( SDT_m3 = NA)
  }
  # Calculate volumes for each row using the helper function

  for (i in seq_len(nrow(output))) {
    temp <- output[i, ]

    result <- SDT_SNAG_Calculator(
      data = output[1,],
      TH_tot_col = "TH_tot_m",
      DBH_col = "DBH_cm",
      L_tot_col = "L_tot_m",
      Dhalf_col = "Dhalf_cm",
      SDT_SNAG_option = SDT_SNAG_option
    )

    # Assign results to the respective columns
    if (SDT_SNAG_option == "snag" && !is.null(result)) {
      output[i, "SNAG_m3"] <- result
    } else if (SDT_SNAG_option == "sdt" && !is.null(result)) {
      output[i, "SDT_m3"] <- result
    }
  }
  # Assign results to the respective columns
  if (SDT_SNAG_option == "snag" && !is.null(output)) {
    output_plot <- output %>%
      group_by(plot) %>%
      summarise(
        ForManInt = first(ForManInt),
        SNAG_m3_plot = sum(SNAG_m3, na.rm = TRUE)
      )
    # Convert plot totals to per-hectare values
    output_plot <- output_plot %>%
      mutate(
        SNAG_m3_ha = SNAG_m3_plot * (10000 / plot_area)

      ) %>%
      mutate(across(where(is.numeric), ~ round(., 3))) %>%
      mutate(ID = row_number()) %>%
      select(ID, everything())

  } else if (SDT_SNAG_option == "sdt" && !is.null(output)) {
    output_plot <- output %>%
      group_by(plot) %>%
      summarise(
        ForManInt = first(ForManInt),
        SDT_m3_plot = sum(SDT_m3, na.rm = TRUE)
      )
    # Convert plot totals to per-hectare values
    output_plot <- output_plot %>%
      mutate(
        SDT_m3_ha = SDT_m3_plot * (10000 / plot_area)
      ) %>%
      mutate(across(where(is.numeric), ~ round(., 3))) %>%
      mutate(ID = row_number()) %>%
      select(ID, everything())
  }
  ##
  vector <- c("snag", "sdt")
  # Assign results to the respective columns
  if (SDT_SNAG_option == "snag" && !is.null(output_plot)) {
    # Summarize results by forest management intervention
    output_ForManInt <- output_plot %>%
      group_by(ForManInt) %>%
      summarise(
        TotalPlots = n_distinct(plot),
        Mean_SNAG_m3_ha = mean(SNAG_m3_ha, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(across(where(is.numeric), ~ round(., 2))) %>%
      mutate(ID = row_number()) %>%
      select(ID,everything())
    #
  } else if (SDT_SNAG_option == "sdt" && !is.null(output_plot)) {
    # Summarize results by forest management intervention
    output_ForManInt <- output_plot %>%
      group_by(ForManInt) %>%
      summarise(
        TotalPlots = n_distinct(plot),
        Mean_SDT_m3_ha = mean(SDT_m3_ha, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(across(where(is.numeric), ~ round(., 2))) %>%
      mutate(ID = row_number()) %>%
      select(ID,everything())
  }
  # Return both plot-level and forest-management summaries
  return(list(output_plot, output_ForManInt))
}


#' Deadwood Volume Calculation (Double Input for Diameters: Dmax and Dmin) for Multiple Plots and Forest Management Practices
#'
#' This function calculates the volume of deadwood for forest plots based on tree attributes
#' such as tree height, maximum and minimum diameters. The calculation is performed for each plot,
#' and the function can group the results by forest management interventions. It returns summaries
#' at both the plot level and the forest management intervention level, including volume per hectare.
#'
#' @param data A data frame containing tree attributes for each plot. Must include relevant
#'   columns such as tree height, maximum and minimum diameter, and forest management intervention information.
#' @param ForManInt_option A string ("Yes" or "No") indicating whether forest management intervention
#'   is considered in the dataset. If "Yes", the column specified in `ForManInt` is used to group results.
#' @param ForManInt A string representing the name of the column in the dataset that specifies the
#'   forest management intervention (e.g., thinning, clearcut, etc.). Used only if `ForManInt_option` is "Yes".
#' @param plot_option A string ("Yes" or "No") indicating if the dataset contains multiple plots.
#'   Defaults to "No" if not provided. If "Yes", data will be grouped by plot and summarized accordingly.
#' @param plot_col A string representing the name of the column in the dataset that specifies the plot identifier.
#'   Used only if `plot_option` is "Yes".
#' @param H_Len_col A string representing the name of the column in the dataset that specifies the total length of the tree (used in volume calculation).
#' @param D_max_col A string representing the name of the column in the dataset that specifies the maximum diameter of the tree.
#' @param D_min_col A string representing the name of the column in the dataset that specifies the minimum diameter of the tree.
#' @param plot_area A numeric value representing the area of each plot in square meters, used to calculate the volume per hectare.
#'
#' @return A list containing two data frames:
#' \item{output_plot}{A data frame summarizing the calculated volumes at the plot level, including the volume per hectare.}
#' \item{output_ForManInt}{A data frame summarizing the calculated volumes by forest management intervention, including the average volume per hectare across plots.}
#'
#' @details This function calculates the volume of deadwood for forest plots using tree attributes such as
#'   maximum and minimum diameters, total height (if required), and forest management intervention (if available).
#'   The volume is computed for each tree, and results can be summarized by plot and/or forest management intervention.
#'   The function also provides per-hectare volume estimates, depending on the plot size specified.
#'
#' @examples
#' # Example: Applying deadwood calculations
#' data <- as.data.frame(list(
#'   ForManInt = c("Intervention_A","Intervention_A","Intervention_A","Intervention_B","Intervention_B","Intervention_B"),
#'   plot_col = c("Plot_1", "Plot_1","Plot_1","Plot_2","Plot_2","Plot_2"),
#'   H_Len_col = c(5.5, 6.5, 4.5, 7.5, 8.5, 7),    # Deadwood length in meters
#'   D_max_col = c(15, 16, 14, 19, 22, 25),        # Maximum diameter in cm
#'   D_min_col = c(10.5, 11.5, 12, 10, 12, 15)     # Minimum diameter in cm
#' ))
#'
#' plot_area <- 530
#'
#' results <- Apply_All_Deadwood(
#'   data = data,
#'   ForManInt_option = "Yes",
#'   ForManInt = "ForManInt",
#'   plot_option = "Yes",
#'   plot_col = "plot_col",
#'   H_Len_col = "H_Len_col",
#'   D_max_col = "D_max_col",
#'   D_min_col = "D_min_col",
#'   plot_area = plot_area
#' )
#'
#' print(results)
#' @import dplyr
#' @importFrom dplyr %>%
#' @export
Apply_All_Deadwood <- function(data, ForManInt_option, ForManInt,plot_option, plot_col,H_Len_col, D_max_col , D_min_col, plot_area) {
  # Check if forest management intervention (ForManInt) is specified; if not, set a default value
  if (ForManInt_option == "No") {
    data[[ForManInt]] <- "No_ForManInt"
  }
  # Check if a single plot is being analyzed; if not, assign all rows to a default "plot 1"
  if (plot_option == "No") {
    data[[plot_col]] <- "1"
  }
  # Select required columns based on the Lying Deadwood or Coarse Woody Debris option
  required_columns <- c(ForManInt, plot_col,H_Len_col, D_max_col , D_min_col)
  # Check for missing columns in the dataset
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("The following columns are missing in the dataset:", paste(missing_columns, collapse = ", ")))
  }
  #
  output <- data %>%
    dplyr::select(all_of(required_columns)) %>%
    rename_with(~ c("ForManInt", "plot","H_Len_m", "D_max_cm" , "D_min_cm")[seq_along(.)])
  head(output)
  # Ensure the plot column is treated as a factor
  output$plot <- as.factor(output$plot)
  ##
  output <- output %>%
    mutate(DEADWOOD_m3 = NA)
  # Calculate volumes for each row using the helper function

  for (i in seq_len(nrow(output))) {
    temp <- output[i, ]
    result <- All_Deadwood_Calculator(temp, H_Len_col = "H_Len_m", D_max_col = "D_max_cm", D_min_col = "D_min_cm")
    # Assign results to the respective columns
    output[i, "DEADWOOD_m3"] <- result
  }
  # Assign results to the respective columns
  output_plot <- output %>%
    group_by(plot) %>%
    summarise(
      ForManInt = first(ForManInt),
      DEADWOOD_m3_plot = sum(DEADWOOD_m3, na.rm = TRUE)
    )
  # Convert plot totals to per-hectare values
  output_plot <- output_plot %>%
    mutate(
      DEADWOOD_m3_ha = DEADWOOD_m3_plot * (10000 / plot_area)

    ) %>%
    mutate(across(where(is.numeric), ~ round(., 3))) %>%
    mutate(ID = row_number()) %>%
    select(ID, everything())
  # Summarize results by forest management intervention
  output_ForManInt <- output_plot %>%
    group_by(ForManInt) %>%
    summarise(
      TotalPlots = n_distinct(plot),
      Mean_DEADWOOD_m3_ha = mean(DEADWOOD_m3_ha, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(where(is.numeric), ~ round(., 2))) %>%
    mutate(ID = row_number()) %>%
    select(ID,everything())

  # Return both plot-level and forest-management summaries
  return(list(output_plot, output_ForManInt))
}


