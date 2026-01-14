#' Construct standard format for data from Strasbourg, France
#'
#' A pipeline to produce the standard format for the hole nesting bird populations in North-Eastern France in and around Strasbourg
#' (Strasbourg, Roberstau, Wantzenau) administered by Sylvie Massemin and Josefa Bleu
#' (Institut Pluridisciplinaire Hubert Curien - CNRS UMR 7178 & Université de Strasbourg).
#'
#' This pipeline is built using SPI-Birds' \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf}{standard format v2.0.0}.
#'
#' This section provides details on data management choices that are unique to these data.
#'
#' \strong{exactAge, minimumAge}: based on age when tagged. exactAge is calculated for individuals tagged as "PUL". Individuals tagged as "2A" were assigned "subadult"
#' age (their age is known and refers as "as second year of life") and individuals tagged as "+2A" were assigned "adult". This helps calculting minimumAge. All others
#' values ("?", "+2?", "2A?") were set to NAs. Check with data custodian that "?" could never be assigned to an individual tagged before fledging.
#'
#' \strong{capturePhysical}: all individuals are assumed to be physical captured.
#'
#' \strong{individualID}: individuals are banded with metal ring with 7 digits (adults for both species, chicks before 2022) or a "V" followed by 6 digits (for chicks starting 2022)
#'
#' \strong{brood data}: Empty nestboxes or nestboxes occupied by other species ("MAMM") are removed
#'
#' \strong{observedClutchType}: Only classified as "1" or "2" in the original dataset. Most likely correspond to breeding attempt ("1" or "2") in the associated nestbox
#' (regardless of female ID or first egg laid on the site)
#'
#' \strong{location data}: locationID for capture event (with mist net or clap-net) are assumed to be at the same coordinates for each site (only one ID per site)
#'
#' \strong{habitatID}: roughly defined based on data information "G1" for forest population (WAN), "J1" for downtown population (STR) and "J2" for suburban population (ROB)
#'#'
#' \strong{locationID}: generate a second row for the same nest box when there was a gap in monitoring nest box (e.g. if nest box was monitored since 2014, but not
#' monitored in 2019, the first row indicates startYear as 2014 and endYear as 2018; the second row indicates startYear as 2020 and endYear as NA)
#'
#' \strong{Experiment data}: Experiment data described based on custodian information
#'
#' \strong{ExperimentID}: accidental events which may affect breeding attempt are reported and detailed
#'
#'
#' @inheritParams pipeline_params
#'
#' @return Generates either 6 .csv files or 6 data frames in the standard format (v2.0.0).
#' @export

format_STR <- function(db = choose_directory(),
                       path = ".",
                       species = NULL,
                       optional_variables = NULL,
                       pop = NULL,
                       output_type = 'R'){

  #Force choose_directory() if used
  force(db)


  #### Determine species and population codes for filtering
  if(is.null(species)){

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
  nest_data <- readxl::read_xlsx(path = paste0(db, "/STR_PrimaryData_Brood.xlsx"),
                                 guess_max = 5000,
                                 col_types = "text") %>%
    janitor::clean_names() %>%
    janitor::remove_empty(which = "rows") %>%

    # Remove rows from empty nest boxes across the season or used by other species than birds (ants, hornets)
    dplyr::filter(!(is.na(.data$espece)  | espece == "MAMM")) %>%

    ## Rename and process columns
    dplyr::mutate(dplyr::across(where(is.character),
                                ~dplyr::na_if(., ".")),
                  studyID = dplyr::case_when(.data$zone == "CV" ~ "STR-1",
                                             .data$zone == "Foret" ~ "WAN-1",
                                             TRUE ~ "ROB-1"),
                  siteID = dplyr::case_when(.data$zone == "CV" ~ "STR",
                                            .data$zone == "Foret" ~ "WAN",
                                            TRUE ~ "ROB"),
                  Year = as.integer(.data$saison),
                  speciesID = dplyr::case_when(.$espece == "PARCAE" ~ species_codes$speciesID[which(species_codes$speciesCode == 10002)],
                                               .$espece == "PARMAJ" ~ species_codes$speciesID[which(species_codes$speciesCode == 10001)],
                                               TRUE ~ NA_character_), #remove very specific case (unfamiliar species, mixed-brood instances, or error)
                  plotID = paste(.data$siteID, toupper(.data$site), sep = "_"),
                  nestID = stringr::str_extract(.data$id_nichee, "(?<=^[A-Z]{4}).*(?=_[0-9]{4}_[12]$)"), #regular expression to extract nestbox ID exactly as mentioned in id_nichee (slightly more reliable and corresponding to info in capture data)
                  locationID = paste(toupper(.data$site), .data$nestID, "NB", sep = "_"),
                  observedLayDate = suppressWarnings(as.Date(janitor::excel_numeric_to_date(as.numeric(.data$date_ponte)), format = "%Y-%m-%d")),
                  observedHatchDate = suppressWarnings(as.Date(janitor::excel_numeric_to_date(as.numeric(.data$date_eclosion)), format = "%Y-%m-%d")),
                  observedFledgeDate = suppressWarnings(as.Date(janitor::excel_numeric_to_date(as.numeric(.data$date_envol)), format = "%Y-%m-%d")),
                  observedClutchSize = suppressWarnings(as.integer(.data$tp)),
                  observedBroodSize = suppressWarnings(as.integer(.data$p_eclos)),
                  observedNumberFledged = suppressWarnings(as.integer(.data$p_envol)),
                  maleID = .data$id_male,
                  femaleID = .data$id_femelle,
                  observedClutchType = dplyr::case_when(.data$ponte == "1" ~ "first", #Check with data custodians, I don't think "1" and "2" correspond to Clutch type
                                                        .data$ponte == "2" ~ "second",
                                                        TRUE ~ "replacement"),
                  experimentID = dplyr::case_when(stringr::str_detect(.data$remarques, "probleme nichoir|probleme_nichoir|mort pdt la capture") ~ "br_1",
                                                  stringr::str_detect(.data$remarques, "Comportement poussins") ~ "br_2",
                                                  stringr::str_detect(.data$remarques, "Manip IMMUNO") ~ "br_3",
                                                  stringr::str_detect(.data$remarques, "Vitamines E") ~ "br_4",
                                                  stringr::str_detect(.data$remarques, "thermic stress") ~ "br_5",
                                                  TRUE ~ NA_character_),
                  experimentType = dplyr::case_when(experimentID %in% c("br_1") ~ "non_experimental",
                                                    experimentID %in% c("br_2") ~ "behavioural_experiment",
                                                    experimentID %in% c("br_3") ~ "injection",
                                                    experimentID %in% c("br_4") ~ "supplemented_feeding",
                                                    experimentID %in% c("br_5") ~ "thermic_stress",
                                                    TRUE ~ NA_character_),
                  treatmentDetails = dplyr::case_when(experimentID == "br_1" ~ "accidental event resulting in failing breeding event",
                                                      experimentID == "br_2" ~ "10 minutes of handling nestling to film their behaviour before fledging event",
                                                      experimentID == "br_3" ~ "immunity challenge experiment on nestling, by injecting lipopolysaccharid before fledging event; nestlings in the brood either received LPS injection or control injection",
                                                      experimentID == "br_4" ~ "nestling food supplemented with vitamin E; nestlings in the brood were either supplemented with vitamin E, or with control food, or not supplemented at all",
                                                      experimentID == "br_5" ~ "thermic stress experiment",
                                                      TRUE ~ NA_character_)) %>%
    dplyr::group_by(.data$Year, .data$locationID) %>%
    ## Create a unique identifier for breeding event (independently of the one provided by the data owner)
    dplyr::mutate(broodID2 = dplyr::case_when(dplyr::n() > 1 & .data$observedClutchType == "second" ~ paste(id_nichoir, Year,  "2", sep = "_"),
                                              TRUE ~ paste(id_nichoir, Year, "1", sep = "_"))) %>%
    dplyr::ungroup() %>%
    ## Arrange
    dplyr::arrange(.data$siteID, .data$Year, .data$locationID, .data$plotID, .data$broodID2)




  ## Read in capture data

  capture_data <- readxl::read_xlsx(path = paste0(db, "STR_PrimaryData_Capture.xlsx"),
                                    guess_max = 5000,
                                    col_types = "text") %>%
    janitor::clean_names() %>%
    janitor::remove_empty(which = "rows") %>%

    ## Rename and process columns
    dplyr::mutate(dplyr::across(where(is.character),
                                ~dplyr::na_if(., "NA")),
                  studyID = dplyr::case_when(.data$zone == "CV" ~ "STR-1",
                                             .data$zone == "Foret" ~ "WAN-1",
                                             TRUE ~ "ROB-1"),
                  siteID = dplyr::case_when(.data$zone == "CV" ~ "STR",
                                            .data$zone == "Foret" ~ "WAN",
                                            TRUE ~ "ROB"),
                  speciesID = dplyr::case_when(.$espece == "PARCAE" ~ species_codes$speciesID[which(species_codes$speciesCode == 10002)],
                                               .$espece == "PARMAJ" ~ species_codes$speciesID[which(species_codes$speciesCode == 10001)],
                                               TRUE ~ NA_character_),
                  captureDate = suppressWarnings(dplyr::case_when(stringr::str_detect(.data$date, "/") ~ as.Date(.data$date, format = "%d/%m/%Y"),
                                                                  TRUE ~ as.Date(janitor::excel_numeric_to_date(as.numeric(.data$date)), format = "%Y-%m-%d"))),
                  captureTime = dplyr::case_when(stringr::str_detect(.data$heure, "^[[:digit:]]{2}[:][[:digit:]]{2}[::][[:digit:]]{2}$") ~ format(as.POSIXct(.data$heure, format = "%H:%M:%OS"), format = "%H:%M"),
                                                 TRUE ~ suppressWarnings(format(as.POSIXct(as.numeric(.data$heure) * 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M"))), #timezone is set to Paris time zone for summer time (CEST)
                  individualID = .data$bague,
                  plotID = paste(siteID, toupper(.data$site), sep = "_"),
                  captureType = dplyr::case_when(.data$type_capture == "AU NID" ~ "nestbox",
                                                 .data$type_capture == "CAGE-PIEGE" ~ "clap-net",
                                                 TRUE ~ "mist-net"),
                  locationID = dplyr::case_when(.data$type_capture == "AU NID" ~ paste(toupper(.data$site), tolower(.data$nichoir), "NB", sep = "_"),
                                                TRUE ~ paste(toupper(.data$site), "MN", sep = "_")),
                  Tarsus = suppressWarnings(round(as.numeric(.data$lt), 2)),
                  Wing_Length = suppressWarnings(round(as.numeric(.data$lp), 1)),
                  Head_Beak_Length = suppressWarnings(round(as.numeric(.data$tb), 1)),
                  Fat_Score = suppressWarnings(round(as.numeric(.data$ad), 0)),
                  Mass = suppressWarnings(round(as.numeric(.data$ma), 1)),
                  HandlingAgr = suppressWarnings(round(as.numeric(.data$agressivite), 1)),
                  Age = dplyr::case_when(.data$age == "PUL" ~ "chick",
                                         .data$age == "2A" ~ "subadult",
                                         .data$age == "+2A" ~ "adult",
                                         TRUE ~ NA_character_),
                  chickAge = dplyr::case_when(.data$Age == "chick" ~ 15,
                                              TRUE ~ NA_integer_),
                  observedSex = dplyr::case_when(.data$sexe == "?" ~ "U",
                                                 is.na(.data$sexe) ~ "U",
                                                 TRUE ~ sexe),
                  capturePhysical = TRUE, #check with data custodian
                  captureAlive = dplyr::case_when(.data$action == "Reprise" ~ FALSE,
                                                  TRUE ~ TRUE),
                  releaseAlive = dplyr::case_when(.data$es == "MORT" ~ FALSE,
                                                  .data$action == "Reprise" ~ FALSE,
                                                  TRUE ~ TRUE),)  %>%
    dplyr::group_by(.data$bg) %>%
    # Anonymize observers
    dplyr::mutate(recordedBy = paste0("obs_", dplyr::cur_group_id())) %>%
    dplyr::ungroup()



  # Read in nestbox data

  loc_data <- readr::read_delim(paste0(db, "/STR_PrimaryData_Location.csv"), show_col_types = FALSE) %>%
    # Convert all column names to snake case
    janitor::clean_names() %>%
    dplyr::mutate(LocationID_join = paste(toupper(.data$site), tolower(.data$nichoir), "NB", sep = "_")) %>%
    dplyr::mutate(studyID = dplyr::case_when(.data$zone == "CV" ~ "STR-1",
                                             .data$zone == "Foret" ~ "WAN-1",
                                             TRUE ~ "ROB-1"),
                  siteID = dplyr::case_when(.data$zone == "CV" ~ "STR",
                                            .data$zone == "Foret" ~ "WAN",
                                            TRUE ~ "ROB"),
                  habitatID = dplyr::case_when(.data$zone == "CV" ~ "J1", #Check with data custodians for more details
                                               .data$zone == "Foret" ~ "G1",
                                               TRUE ~ "J2"),
                  decimalLatitude = .data$latitude,
                  decimalLongitude = .data$longitude,
                  startYear = dplyr::case_when(!is.na(.data$date_pose) ~ as.integer(.data$date_pose),
                                               TRUE ~ NA_integer_),
                  endYear = dplyr::case_when(!is.na(.data$date_retrait) ~ as.integer(.data$date_retrait),
                                             .data$remarques == "Non suivi depuis 2023" ~ 2022, #check with data custodians if this works
                                             TRUE ~ NA_integer_),
                  locationType = "nest",
                  locationDetails = dplyr::case_when(.data$type == "Schwegler" ~ "Schwegler nesting box",
                                                     .data$type == "Bois" ~ "Wooden nesting box",
                                                     .data$type == "Balcon" ~ "Balcony nesting box",
                                                     TRUE ~ NA_character_),
                  elevation = NA_real_,
                  # Add a column to detect rows that need to be duplicated to add information about changing nest box type or monitoring gaps
                  count = dplyr::case_when(stringr::str_detect(.data$remarques, "non suivi 2|bois jusqu|Balcon jusque") ~ 2,
                                           TRUE ~ 1)) %>%
    # Duplicate rows (based on count)
    tidyr::uncount(.data$count) %>%
    # For each location ID
    dplyr::group_by(.data$LocationID_join) %>%
    # ... Add number 2 for duplicated rows (used afterwards to filter the right row)
    dplyr::mutate(count = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    # Change information on location type when nest box type was changed
    dplyr::mutate(locationDetails = dplyr::case_when(stringr::str_detect(.data$remarques, "bois jusqu'") & count == 1 ~ "Wooden nesting box",
                                                     stringr::str_detect(.data$remarques, "Balcon jusque") & count == 1 ~ "Balcony nesting box",
                                                     TRUE ~ locationDetails),
                  # Adjust startYear and endYear for case with monitoring gaps ("non suivi") or changes in nest box type ("bois jusqu'", "Balcon jusque")
                  endYear = dplyr::case_when(count == 1 & stringr::str_detect(.data$remarques, "non suivi 2020|non suivi 2020, 2021|non suivi 2020,2021|bois jusqu'en 2020") ~ 2019,
                                             count == 1 & stringr::str_detect(.data$remarques, "non suivi 2019,2020,2021") ~ 2018,
                                             count == 1 & stringr::str_detect(.data$remarques, "non suivi 2021") ~ 2020,
                                             count == 1 & stringr::str_detect(.data$remarques, "bois jusqu'en 2018") ~ 2018,
                                             count == 1 & stringr::str_detect(.data$remarques, "Balcon jusque 2023") ~ 2022,
                                             TRUE ~ endYear),
                  startYear = dplyr::case_when(count == 2 & stringr::str_detect(.data$remarques, "non suivi 2019,2020,2021|non suivi 2020, 2021|non suivi 2021") ~ 2022,
                                               count == 2 & stringr::str_detect(.data$remarques, "non suivi 2020") ~ 2021,
                                               count == 2 & stringr::str_detect(.data$remarques, "bois jusqu'en 2020") ~ 2021,
                                               count == 2 & stringr::str_detect(.data$remarques, "bois jusqu'en 2018") ~ 2019,
                                               count == 2 & stringr::str_detect(.data$remarques, "Balcon jusque 2023") ~ 2023,
                                               TRUE ~ startYear)) %>%
    # For each location ID
    dplyr::group_by(.data$LocationID_join) %>%
    # ... create a location ID
    dplyr::mutate(locationID = paste(.data$LocationID_join, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::select(locationID, locationType, locationDetails, studyID, siteID, decimalLatitude, decimalLongitude, elevation, startYear, endYear, habitatID)



  #### BROOD DATA
  message("Compiling brood information...")
  Brood_data_temp <- create_brood_STR(nest_data,
                                      optional_variables = optional_variables)

  #### CAPTURE DATA
  message("Compiling capture information...")
  Capture_data_temp <- create_capture_STR(capture_data,
                                          optional_variables = optional_variables)

  #### INDIVIDUAL DATA
  message("Compiling individual information...")
  Individual_data_temp <- create_individual_STR(Capture_data_temp, Brood_data_temp,
                                                optional_variables = optional_variables)

  #### MEASUREMENT DATA
  message("Compiling measurement information...")
  Measurement_data_temp <- create_measurement_STR(Capture_data_temp) #new in v2.0

  #### LOCATION DATA
  message("Compiling location information...")
  Location_data_temp <- create_location_STR(Capture_data_temp, loc_data)

  #### EXPERIMENT DATA
  message("Compiling experiment information...")
  Experiment_data_temp <- create_experiment_STR(Brood_data_temp) #new in v2.0


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

    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%

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

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_STR.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_STR.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_STR.csv"), row.names = F)

    utils::write.csv(x = Measurement_data, file = paste0(path, "\\Measurement_data_STR.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_STR.csv"), row.names = F)

    utils::write.csv(x = Experiment_data, file = paste0(path, "\\Experiment_data_STR.csv"), row.names = F)

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



#### --------------------------------------------------------------------------~
#### FUNCTIONS
#### --------------------------------------------------------------------------~

#' Create brood data table in Strasbourg, France.
#'
#' @param nest_data Data frame of nest data from Strasbourg, France.
#'
#'
#' @return A data frame.
#'
create_brood_STR <- function(nest_data,
                             species_filter,
                             optional_variables) {

  ## Combine primary data to create brood data
  Brood_data_temp <- nest_data %>%

    ##Remove non-occupied nestboxes or unidentified species
    dplyr::filter(!is.na(.data$speciesID)) %>%

    ## Create additional variables
    dplyr::mutate(broodID = paste(.data$Year, 1:dplyr::n(), sep = "-"),
                  observedLayYear = as.integer(.data$Year),
                  observedLayMonth = as.integer(lubridate::month(.data$observedLayDate)),
                  observedLayDay = as.integer(lubridate::day(.data$observedLayDate)),
                  observedHatchYear = as.integer(.data$Year),
                  observedHatchMonth = as.integer(lubridate::month(.data$observedHatchDate)),
                  observedHatchDay = as.integer(lubridate::day(.data$observedHatchDate)),
                  observedFledgeYear = as.integer(.data$Year),
                  observedFledgeMonth = as.integer(lubridate::month(.data$observedFledgeDate)),
                  observedFledgeDay = as.integer(lubridate::day(.data$observedFledgeDate))) %>%

    ## Calculate optional variables
    {if("breedingSeason" %in% optional_variables) calc_season(data = .,
                                                              season = .data$Year)
      else .} %>%

    {if("calculatedClutchType" %in% optional_variables)  calc_clutchtype(data = ., na.rm = FALSE,
                                                                         protocol_version = "2.0")
      else .} %>%

    {if("nestAttemptNumber" %in% optional_variables) calc_nestattempt(data = .,
                                                                      season = .data$breedingSeason)
      else .}  %>%

    ## Set improperly formatted IDs to NA
    dplyr::mutate(dplyr::across(c(femaleID,
                                  maleID),
                                ~ dplyr::case_when(nchar(.) %in% c(7,8) & stringr::str_detect(., "^(V|[0-9])+[:digit:]+$") ~ .,
                                                   TRUE ~ NA_character_))) %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(data_templates$v2.0$Brood_data)), tidyselect::everything())

  return(Brood_data_temp)

}



#' Create capture data table for Strasbourg, France.
#'
#' @param capture_data, Data frame of individuals (adults and nestlings) ringing records from Strasbourg, France.
#'
#' @return A data frame.

create_capture_STR <- function(capture_data,
                               species_filter,
                               optional_variables) {


  Capture_data_temp <- capture_data %>%
    dplyr::mutate(captureYear = as.integer(lubridate::year(.data$captureDate)),
                  captureMonth = as.integer(lubridate::month(.data$captureDate)),
                  captureDay = as.integer(lubridate::day(.data$captureDate)),
                  captureSiteID = .data$siteID,
                  releaseSiteID = .data$siteID,
                  capturePlotID = .data$plotID,
                  releasePlotID = .data$plotID,
                  captureLocationID = .data$locationID,
                  releaseLocationID = .data$locationID,
                  treatmentID = NA_character_,
                  releaseTagID = .data$individualID,
                  observedAge = .data$Age,
                  chickAge = as.integer(.data$chickAge)) %>%
    dplyr::arrange(individualID, captureYear, captureMonth, captureDay, captureTime) %>%
    dplyr::group_by(individualID) %>%
    dplyr::mutate(ntime = 1:n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(captureTagID = dplyr::case_when(ntime == 1 ~ NA_character_,
                                                  TRUE ~ .data$individualID)) %>%

    ## Set improperly formatted IDs to NA and filter
    dplyr::mutate(individualID = dplyr::case_when(nchar(.data$individualID) %in% c(7,8) &
                                                    stringr::str_detect(.data$individualID, "^(V|[0-9])+[:digit:]+$")  ~ .data$individualID,
                                                  TRUE ~ NA_character_)) %>%
    dplyr::filter(!is.na(.data$individualID)) %>%

    ## Create captureID
    ## Arrange
    dplyr::arrange(.data$captureYear, .data$individualID, .data$captureDate) %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::mutate(captureID = paste(.data$individualID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup()  %>%

    ## Calculate age
    {if("exactAge" %in% optional_variables | "minimumAge" %in% optional_variables) calc_age(data = .,
                                                                                            ID = .data$individualID,
                                                                                            Age = .data$observedAge,
                                                                                            Date = .data$captureDate,
                                                                                            Year = .data$Year,
                                                                                            protocol_version = "2.0")
      else .}  %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(data_templates$v2.0$Capture_data)), tidyselect::everything())

  return(Capture_data_temp)

}



#' Create individual table for Strasbourg, France.
#'
#' @param Capture_data_temp  Capture data output from Strasbourg, France
#'
#' @param Brood_data_temp Brood data output from Strasbourg, France
#'
#' @return A data frame.

create_individual_STR <- function(Capture_data_temp,
                                  Brood_data_temp,
                                  species_filter,
                                  optional_variables){

  ## Several steps to ensure the association of chick to their assigned broodID

  #Step 1. merging information from capture_data_temp and Brood_data_temp (broadly)
  broad_merging <- Capture_data_temp %>%
    dplyr::filter(is.na(.data$captureTagID) & .data$Age == "chick") %>%
    dplyr::mutate(Year = lubridate::year(as.Date(.data$captureDate))) %>%
    dplyr::select(individualID, locationID, Year, captureDate) %>%
    dplyr::left_join(Brood_data_temp %>%
                       dplyr::mutate(bandingDate = suppressWarnings(as.Date(janitor::excel_numeric_to_date(as.numeric(.data$date_baguage_p)),
                                                                            format = "%Y-%m-%d"))) %>%
                       dplyr::select(broodID, bandingDate, observedHatchDate, locationID, Year),
                     by = c("locationID", "Year"),
                     relationship = "many-to-many")

  #Step 2. targetting the right brood within a locationID and Year
  #         (comparing hatching date or banding date from brood_data to captureDate in capture_data)
  target_merging <- broad_merging %>%
    dplyr::mutate(diff_hatch = as.numeric(.data$captureDate - .data$observedHatchDate),
                  diff_band  = as.numeric(.data$captureDate - .data$bandingDate),
                  diff_ref = dplyr::case_when(!is.na(.data$observedHatchDate) ~ .data$diff_hatch,
                                              is.na(.data$observedHatchDate) & !is.na(.data$bandingDate) ~ .data$diff_band,
                                              TRUE ~ NA_real_),

                  diff_rule = dplyr::case_when(!is.na(.data$observedHatchDate) ~ .data$diff_ref >= 0 & .data$diff_ref <= 25,
                                               is.na(.data$observedHatchDate) ~ abs(.data$diff_ref) <= 15,
                                               TRUE ~ FALSE))

  #Step 3. ensuring each chick is assigned to the right brood
  broodAssignment <- target_merging %>%
    dplyr::filter(.data$diff_rule) %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::slice_min(abs(.data$diff_ref), n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  #Step 4. Including brood assignment in capture table
  Individual_data_temp <- dplyr::left_join(Capture_data_temp, broodAssignment, by = "individualID", relationship = "many-to-many") %>%
    # Arrange data chronologically for each individual
    dplyr::arrange(.data$captureID) %>%
    dplyr::group_by(.data$individualID) %>%
    ## Format and create new data columns
    dplyr::mutate(siteID = .data$captureSiteID,
                  tagYear = dplyr::if_else(is.na(.data$captureTagID), .data$captureYear, NA_integer_),
                  tagMonth = dplyr::if_else(is.na(.data$captureTagID), .data$captureMonth, NA_integer_),
                  tagDay = dplyr::if_else(is.na(.data$captureTagID), .data$captureDay, NA_integer_),
                  tagStage = dplyr::if_else(is.na(.data$captureTagID), .data$observedAge, NA_character_),
                  tagSiteID = .data$captureSiteID,
                  geneticSex = NA_character_) %>%

    ## Add broodID and control speciesID
    dplyr::group_by(.data$individualID) %>%
    dplyr::mutate(broodIDLaid = purrr::map_chr(.x = list(unique(stats::na.omit(.data$broodID))),
                                               .f = ~{
                                                 if(length(..1) != 1){
                                                   return(NA_character_)
                                                 } else if(length(..1) == 1){
                                                   return(..1)
                                                 }
                                               }),
                  broodIDFledged = .data$broodIDLaid,
                  speciesID = purrr::map_chr(.x = list(unique(stats::na.omit(.data$speciesID))),
                                             .f = ~{
                                               if(length(..1) == 0){
                                                 return(NA_character_)
                                               } else if(length(..1) == 1){
                                                 return(..1)
                                               } else {
                                                 return("CCCCCC")
                                               }
                                             }))  %>%

    ## Keep distinct records by siteID and individualID
    dplyr::distinct(.data$siteID, .data$individualID, .keep_all = TRUE) %>%

    ## Arrange
    dplyr::arrange(.data$captureID) %>%
    dplyr::ungroup() %>%

    # Add optional variables
    {if("calculatedSex" %in% optional_variables) calc_sex(individual_data = .,
                                                          capture_data = Capture_data_temp)
      else .} %>%


    ## Reorder columns
    dplyr::select(dplyr::any_of(names(data_templates$v2.0$Individual_data)), tidyselect::everything())

  return(Individual_data_temp)

}



#' Create measurement data table for Strasbourg, France.
#'
#' @param Capture_data_temp Data frame of nest data from Strasbourg, France.
#'
#' @return A data frame.

create_measurement_STR <- function(Capture_data_temp) {

  ## Build measuremtent data based on capture data
  Measurement_data_temp <- Capture_data_temp %>%

    ## Format and create new data table
    dplyr::mutate(recordID = .data$captureID,
                  siteID = .data$captureSiteID,
                  measurementDeterminedYear = .data$captureYear,
                  measurementDeterminedMonth = .data$captureMonth,
                  measurementDeterminedDay = .data$captureDay,
                  measurementDeterminedTime = .data$captureTime,
                  Handling_Docility = .data$HandlingAgr) %>%

    ## Transform measurement columns into rows
    tidyr::pivot_longer(cols = c("Tarsus",
                                 "Wing_Length",
                                 "Head_Beak_Length",
                                 "Fat_Score",
                                 "Mass",
                                 "Handling_Docility"),
                        names_to = "measurementType",
                        values_to = "measurementValue",
                        values_drop_na = TRUE) %>%

    ## Create new variables
    dplyr::mutate(measurementSubject = "capture",
                  measurementAccuracy = NA_real_,
                  measurementUnit = dplyr::case_when(.data$measurementType == "Mass" ~ "g",
                                                     .data$measurementType %in% c("Handling_Docility", "Fat_Score") ~ "no unit",
                                                     TRUE ~ "mm"),
                  measurementMethod = dplyr::case_when(.data$measurementType == "Tarsus" ~ "alternative",
                                                       .data$measurementType == "Head_Beak_Length" ~ "length from the back of the head to the tip of the beak",
                                                       .data$measurementType == "Wing_Length" ~ "flattened, maximum chord from ESF guidelines",
                                                       .data$measurementType == "Fat_Score" ~ "fat score from ESF guidelines",
                                                       .data$measurementType == "Handling_Docility" ~ "behavioral score (0 to 3) of docility in hand",
                                                       TRUE ~ NA_character_),
                  # Convert measurementType to lower case & space-separated
                  # (e.g., wingLength -> wing length)
                  measurementType = tolower(gsub("([[:upper:]])", "\\1", .data$measurementType)),
                  measurementType = stringr::str_replace_all(string = .data$measurementType,
                                                             pattern = "\\_",
                                                             replacement = " ")) %>%

    ## Arrange
    dplyr::arrange(.data$measurementDeterminedYear,
                   .data$measurementDeterminedMonth,
                   .data$measurementDeterminedDay) %>%

    ## Create measurementID
    dplyr::mutate(measurementID = 1:dplyr::n()) %>%


    ## Reorder columns
    dplyr::select(dplyr::any_of(names(data_templates$v2.0$Measurement_data)), tidyselect::everything())

  return(Measurement_data_temp)

}


#' Create location data table for Strasbourg, France.
#'
#' @param loc_data Data frame of nestbox location records from Strasbourg, France.
#'
#' @param Capture_data_temp Capture data output from Strasbourg, France
#'
#' @return A data frame.


create_location_STR <- function(Capture_data_temp,
                                loc_data) {

  ## Create table with information related to mistnet captures
  loc_mn <- Capture_data_temp %>%
    dplyr::filter(.data$captureType != "nestbox") %>%
    dplyr::arrange(.data$plotID, .data$captureYear) %>%
    dplyr::group_by(.data$plotID) %>%
    dplyr::mutate(locationID = paste(.data$plotID, "MN", sep = "_"),
                  startYear = first(.data$captureYear),
                  endYear = last(.data$captureYear),
                  locationType = "capture",
                  locationDetails = .data$captureType,
                  habitatID = dplyr::case_when(.data$siteID == "STR" ~ "J1",
                                               .data$siteID == "WAN" ~ "G1",
                                               TRUE ~ "J2"),) %>%
    dplyr::distinct(.data$locationID, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::select(locationID, locationType, locationDetails, studyID, siteID, habitatID, startYear, endYear)

  ## Create table with nest box information
  Location_data_temp <- dplyr::bind_rows(loc_data, loc_mn) %>%
    dplyr::mutate(dplyr::across(c(startYear, endYear), as.integer)) %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(data_templates$v2.0$Location_data)), tidyselect::everything())

  return(Location_data_temp)

}


#' Create experiment data table for Strasbourg, France.
#'
#' @param Brood_data_temp Data frame of nest data from Strasbourg, France.
#'
#' @return A data frame.

create_experiment_STR <- function(Brood_data_temp) {


  Experiment_data <- Brood_data_temp %>%
    dplyr::select("experimentID",
                  "studyID",
                  "siteID",
                  "experimentType",
                  "treatmentDetails",
                  treatmentStartYear = "observedLayYear",
                  treatmentEndYear = "observedLayYear") %>%
    dplyr::mutate(treatmentID = paste(.data$treatmentStartYear, .data$siteID, .data$experimentID, sep = "_"),
                  treatmentStage = "nestlings",
                  treatmentStartMonth =  NA_integer_,
                  treatmentStartDay = NA_integer_,
                  treatmentStartTime = NA_character_,
                  treatmentEndMonth = NA_integer_,
                  treatmentEndDay =  NA_integer_,
                  treatmentEndTime = NA_character_,
                  recordedBy =  NA_character_,
                  reference = NA_character_) %>%
    dplyr::filter(!is.na(.data$experimentID)) %>%
    # Remove duplicates
    dplyr::distinct(.data$treatmentID,
                    .keep_all = TRUE)

  return(Experiment_data)

}

