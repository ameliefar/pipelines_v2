#' Construct standard format for data from Moulis, France
#'
#' A pipeline to produce the standard format for the nest box population in Moulis, France, administered by Alexis Chaine.
#'
#' This section provides details on data management choices that are unique to this data.
#' For a general description of the standard format,
#' please see \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf}{here}.
#'
#'\strong{speciesID}: Primarily Great tits and Blue tits. Some breeding information for Marsh tits, Coal tits, Crested tits and Eurasian Nuthatches.
#' All other species (not associated with breeding information) were removed
#'
#'\strong{individualID}: a character string of length 7 where the first character is either a number or the letter "V" and
#'the last six characters are all numbers. Starting 2025, there could also be a character string composed of 8 numbers.
#'
#'\strong{studyID}: one population "MOU-1"
#'
#'\strong{plotID}: six main sites which are divided into woodlot. After discussing with data custodian, plotID refers to woodlot
#'
#'\strong{treatmentID}: most of experiments (including cross-fostering experiments) are not reported in the pipeline.
#'Contact data administrator for further details related to experiments
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 6 .csv files or 6 data frames in the standard format.
#'@export

format_MOU <- function(db = choose_directory(),
                       path = ".",
                       species = NULL,
                       optional_variables = NULL,
                       pop = NULL,
                       output_type = 'R') {


  # Force choose_directory() if used
  force(db)

  ## Determine species and population codes for filtering
  if(is.null(species)) {

    species_filter <- NULL

  } else {

    species_filter <- species

  }

  if(is.null(pop)){

    pop_filter <- NULL

  } else {

    pop_filter <- pop

  }

 ## Set options
  if(!is.null(optional_variables) & "all" %in% optional_variables) optional_variables <- names(unlist(unname(utility_variables)))

  start_time <- Sys.time()

  message("Importing primary data...")

  ## Read in nest data






  #### BROOD DATA
  message("Compiling brood information...")
  Brood_data_temp <- create_brood_BRG(nest_data, cap_data, loc_data,
                                      optional_variables = optional_variables)

  #### CAPTURE DATA
  message("Compiling capture information...")
  Capture_data_temp <- create_capture_BRG(cap_data, Brood_data_temp,
                                          optional_variables = optional_variables)

  #### INDIVIDUAL DATA
  message("Compiling individual information...")
  Individual_data_temp <- create_individual_BRG(Capture_data_temp,
                                                optional_variables = optional_variables)

  #### MEASUREMENT DATA
  message("Compiling measurement information...")
  Measurement_data_temp <- create_measurement_BRG(Capture_data_temp) #new in v2.0

  #### LOCATION DATA
  message("Compiling location information...")
  Location_data_temp <- create_location_BRG(nest_data, loc_data)

  #### EXPERIMENT DATA
  message("Compiling experiment information...")
  Experiment_data_temp <- create_experiment_BRG(Brood_data_temp) #new in v2.0


  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  #### PROCESSING FINAL DATA TO EXPORT

  ## Brood data
  Brood_data <- Brood_data_temp %>%

    ## Remove any NAs from critical columns
    dplyr::filter_at(vars(broodID,
                          siteID,
                          observedLayYear,
                          speciesID), dplyr::all_vars(!is.na(.))) %>%

    ## Add rowID
    dplyr::mutate(row = 1:dplyr::n()) %>%

    ## Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Brood_data[0, !(names(data_templates$v2.0$Brood_data) %in% names(.))] %>%
                       tibble::add_row()) %>%

    ## Keep only necessary columns
    dplyr::select(names(data_templates$v2.0$Brood_data), dplyr::contains(names(utility_variables$Brood_data),
                                                                         ignore.case = FALSE))


  ## Capture data
  Capture_data <- Capture_data_temp %>%

    ## Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Capture_data[0, !(names(data_templates$v2.0$Capture_data) %in% names(.))] %>%
                       tibble::add_row()) %>%

    ## Remove any NAs from critical columns
    dplyr::filter_at(vars(captureID,
                          captureSiteID,
                          individualID,
                          speciesID,
                          captureYear,
                          captureMonth,
                          captureDay), dplyr::all_vars(!is.na(.))) %>%

    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%

    ## Reorder columns
    dplyr::select(names(data_templates$v2.0$Capture_data), dplyr::contains(names(utility_variables$Capture_data),
                                                                           ignore.case = FALSE))


  ## Individual data
  Individual_data <- Individual_data_temp %>%

    ## Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Individual_data[0, !(names(data_templates$v2.0$Individual_data) %in% names(.))] %>%
                       tibble::add_row()) %>%

    ## Remove any NAs from critical columns
    dplyr::filter_at(vars(siteID,
                          individualID,
                          speciesID,
                          tagYear), dplyr::all_vars(!is.na(.))) %>%

    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%

    ## Reorder columns
    dplyr::select(names(data_templates$v2.0$Individual_data), dplyr::contains(names(utility_variables$Individual_data),
                                                                              ignore.case = FALSE))



  ## Measurement data
  Measurement_data <- Measurement_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(data_templates$v2.0$Measurement_data))) %>%

    ## Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Measurement_data[0, !(names(data_templates$v2.0$Measurement_data) %in% names(.))] %>%
                       tibble::add_row()) %>%

    ## Remove any NAs from critical columns
    dplyr::filter_at(vars(siteID),
                     all_vars(!is.na(.))) %>%

    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%

    ## Reorder columns
    dplyr::select(names(data_templates$v2.0$Measurement_data)) %>%
    dplyr::ungroup()



  ## Location data
  Location_data <- Location_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(data_templates$v2.0$Location_data))) %>%

    ## Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Location_data[0, !(names(data_templates$v2.0$Location_data) %in% names(.))] %>%
                       tibble::add_row()) %>%

    ## Remove any NAs from critical columns
    dplyr::filter_at(vars(locationID,
                          siteID),
                     all_vars(!is.na(.))) %>%

    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%

    ## Reorder columns
    dplyr::select(names(data_templates$v2.0$Location_data))  %>%
    dplyr::ungroup()

  ## Experiment data
  Experiment_data <- Experiment_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(data_templates$v2.0$Experiment_data))) %>%

    ## Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Experiment_data[0, !(names(data_templates$v2.0$Experiment_data) %in% names(.))] %>%
                       tibble::add_row()) %>%

    ## Remove any NAs from critical columns
    dplyr::filter_at(vars(siteID),
                     all_vars(!is.na(.))) %>%

    ## Reorder columns
    dplyr::select(names(data_templates$v2.0$Experiment_data))  %>%
    dplyr::ungroup()


  ## Filter to keep only desired Species if specified for Brood, Capture, and Individual tables
  if(!is.null(species_filter)){

    Brood_data <- Brood_data %>%
      dplyr::filter(.data$speciesID %in% species_filter & !(is.na(.data$speciesID)))

    Capture_data <- Capture_data %>%
      dplyr::filter(.data$speciesID %in% species_filter & !(is.na(.data$speciesID)))

    Individual_data <- Individual_data %>%
      dplyr::filter(.data$speciesID %in% species_filter & !(is.na(.data$speciesID)))

  }

  ## Filter to keep only desired Pops if specified for Brood, Capture, Individual, Measurement, Location and Experiment tables
  if(!is.null(pop_filter)){

    Brood_data <- Brood_data %>%
      dplyr::filter(.data$siteID %in% pop_filter & !(is.na(.data$siteID)))

    Capture_data <- Capture_data %>%
      dplyr::filter(.data$captureSiteID %in% pop_filter & !(is.na(.data$captureSiteID)))

    Measurement_data <- Measurement_data %>%
      dplyr::filter(.data$siteID %in% pop_filter & !(is.na(.data$siteID)))

    Individual_data <- Individual_data %>%
      dplyr::filter(.data$siteID %in% pop_filter & !(is.na(.data$siteID)))

    Location_data <- Location_data %>%
      dplyr::filter(.data$siteID %in% pop_filter & !(is.na(.data$siteID)))

    Experiment_data <- Experiment_data %>%
      dplyr::filter(.data$siteID %in% pop_filter & !(is.na(.data$siteID)))

  }

  #### EXPORT DATA

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_MOU.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_MOU.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_MOU.csv"), row.names = F)

    utils::write.csv(x = Measurement_data, file = paste0(path, "\\Measurement_data_MOU.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_MOU.csv"), row.names = F)

    utils::write.csv(x = Experiment_data, file = paste0(path, "\\Experiment_data_MOU.csv"), row.names = F)

    invisible(NULL)

  }

  if(output_type == "R"){

    message("Returning R objects...")

    return(list(Brood_data = Brood_data,
                Capture_data = Capture_data,
                Individual_data = Individual_data,
                Measurement_data = Measurement_data,
                Location_data = Location_data,
                Experiment_data = Experiment_data))

  }

}
  }
