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
#'\strong{plotID}: six main sites which are divided into woodlot. plotID refers to the six main sites.
#'Additional plots refering to mistnet captures were renamed as "miscellaneous"
#'
#'\strong{locationID}: for nestbox location, when available, the first part of locationID refers to the name of the woodlot inside the plot indicated in plotID.
#'Otherwise, the first part of locationID refers to plotID.
#'Similarly, for mistnet capture location, when available, the first part of locationID refers to a specific place within the plot indicated in plotID.
#'Otherwise, the first part of locationID refers to plotID.
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

  nest_data <- readxl::read_xlsx(path = paste0(db, "MOU_PrimaryData_LHT.xlsx"),
                                 guess_max = 5000,
                                 col_types = "text") %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Rename and process columns
    dplyr::mutate(dplyr::across(where(is.character),
                                ~dplyr::na_if(., "NA")),
                  studyID = "MOU-1",
                  siteID = "MOU",
                  Year = as.integer(.data$Year),
                  speciesID = dplyr::case_when(.data$Species == "G" ~ species_codes$speciesID[which(species_codes$speciesCode == 10001)],
                                               .data$Species == "B" ~ species_codes$speciesID[which(species_codes$speciesCode == 10002)],
                                               .data$Species == "M" ~ species_codes$speciesID[which(species_codes$speciesCode == 10008)],
                                               .data$Species == "C" ~ species_codes$speciesID[which(species_codes$speciesCode == 10005)],
                                               .data$Species == "CR" ~ species_codes$speciesID[which(species_codes$speciesCode == 10012)],
                                               .data$Species == "NUT" ~ species_codes$speciesID[which(species_codes$speciesCode == 10001)],
                                               TRUE ~ NA_character_), #remove potential specific case (unfamilar species, mixed-brood, errors)

                #After discussing with data custodian, merge three columns related to laying date
                  observedLayDate = dplyr::case_when(!is.na(.data$LayDate) ~ as.Date(.data$LayDate, format = "%d/%m/%Y"),
                                                     !is.na(.data$LayDateCorrect) ~ as.Date(.data$LayDateCorrect, format = "%d/%m/%Y"),
                                                     TRUE ~ as.Date(.data$LayDateError, format = "%d/%m/%Y")),

                #After discussing with data custodian, merge three columns related to hatch date
                  observedHatchDate = dplyr::case_when(!is.na(.data$HatchDate) ~ as.Date(.data$HatchDate, format = "%d/%m/%Y"),
                                                       !is.na(.data$HatchDateCorrect) ~ as.Date(.data$HatchDateCorrect, format = "%d/%m/%Y"),
                                                       TRUE ~ as.Date(.data$HatchDateError, format = "%d/%m/%Y")),
                  observedClutchSize = suppressWarnings(as.integer(.data$EggNb)),
                  observedBroodSize = suppressWarnings(as.integer(.data$HatchNb)),
                  observedFledgeDate = as.Date(.data$FledgeDate, format = "%d/%m/%Y"),
                  observedNumberFledged = suppressWarnings(as.integer(.data$FledgeNb)),

              #After discussing with data custodian, merge the twho colums related to Nest number
                  nestID = dplyr::case_when(!is.na(.data$Nest) ~ toupper(.data$Nest),
                                            TRUE ~ toupper(.data$NestRdata)),
                  femaleID = .data$FBand,
                  maleID = .data$MBand,
                  treatmentID = dplyr::case_when(!is.na(.data$CrossFosterDate) ~ "cross-fostering",
                                                 !is.na(.data$PredatorPresentationDate) ~ "predation_exp",
                                                 TRUE ~ NA_character_)) %>%
    dplyr::select(NestRdata, Nest, nestID, speciesID, studyID, siteID, Year, NestAttempt, observedLayDate, observedClutchSize, observedHatchDate, observedBroodSize,
                  observedFledgeDate, observedNumberFledged, ChickBandDate, FBandDate, femaleID, MBandDate, maleID, treatmentID)


  ## Read in capture data

  capture_data <- readxl::read_xlsx(path = paste0(db, "MOU_PrimaryData_Captures.xlsx"),
                                    guess_max = 5000,
                                    col_types = "text") %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Remove first and last columns that are useless here
    dplyr::select(-c(1:3, 42:43)) %>%

    ## Rename and process columns
    dplyr::mutate(dplyr::across(where(is.character),
                                ~dplyr::na_if(., "NA"))) %>%

    dplyr::mutate(studyID = "MOU-1",
                  siteID = "MOU",
                  Year = as.integer(.data$Year),
                  speciesID = dplyr::case_when(.data$Species %in% c("Great", "GT", "great", "PM") ~ species_codes$speciesID[which(species_codes$speciesCode == 10001)],
                                               .data$Species %in% c("Blue tit", "Blue", "blue", "CC") ~ species_codes$speciesID[which(species_codes$speciesCode == 10002)],
                                               .data$Species %in% c("MT", "Marshtit", "Marsh", "PARPAL") ~ species_codes$speciesID[which(species_codes$speciesCode == 10008)],
                                               .data$Species %in% c("Coal", "coal") ~ species_codes$speciesID[which(species_codes$speciesCode == 10005)],
                                               .data$Species %in% c("Crested", "crested") ~ species_codes$speciesID[which(species_codes$speciesCode == 10012)],
                                               .data$Species %in% c("Nuthatch") ~ species_codes$speciesID[which(species_codes$speciesCode == 10001)],
                                               TRUE ~ NA_character_), #remove all other birds most likely captured in mist net)
                  nestID = .data$NestNumber,
                  captureSite = stringr::str_trim(stringr::str_to_title(Site)),

                  #Normalize errors related to entering date in Excel from different computers
                  captureDate =  suppressWarnings(dplyr::case_when(stringr::str_detect(Date, "/") ~ as.Date(Date, format = "%d/%m/%Y"),
                                                                   TRUE ~ as.Date(janitor::excel_numeric_to_date(as.numeric(Date)), format = "%Y-%m-%d"))),
                  individualID = .data$BandNumber,
                  sex = tolower(.data$Sex),

                  #Normalize values related to sex
                  observedSex = dplyr::case_when(stringr::str_detect(.data$sex, fixed("?")) ~ NA_character_,
                                                 stringr::str_detect(.data$sex, "undetermined") ~ NA_character_,
                                                 stringr::str_detect(.data$sex, "m|male") ~ "M",
                                                 stringr::str_detect(.data$sex, "f|female") ~ "F",
                                                 TRUE ~ NA_character_),

                  #Normalize values related to age
                  age = tolower(stringr::str_trim(.data$Age)),
                  observedAge = dplyr::case_when(stringr::str_detect(.data$age, fixed("?")) ~ NA_character_,
                                                 stringr::str_detect(.data$age, fixed("+")) ~ "adult",
                                                 stringr::str_detect(.data$age, fixed(">")) ~ "adult",
                                                 stringr::str_detect(.data$age, "ad") ~ "adult",
                                                 stringr::str_detect(.data$age, fixed("-")) ~ "subadult",
                                                 stringr::str_detect(.data$age, "1a|vol") ~ "subadult",
                                                 stringr::str_detect(.data$age, "pul|pil|j") ~ "chick",
                                                 TRUE ~ NA_character_),

                  #Normalize errorsd related to entering time in Excel from different computers
                  captureTime = suppressWarnings(dplyr::case_when(stringr::str_detect(.data$Time, "^[[:digit:]]{2}[h]$") ~ gsub("h", ":00", .data$Time),
                                                                  stringr::str_detect(.data$Time, "^[[:digit:]]{2}[h][[:digit:]]{2}$") ~ gsub("h", ":", .data$Time),
                                                                  TRUE ~ format(as.POSIXct(as.numeric(.data$Time) * 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M"))),
                  #Normalize and anonymize observer's name
                  recordedBy = dplyr::case_when(is.na(.data$Bander) | .data$Bander %in% c("No Value", "No", "Yes", "None", "?") ~ NA_character_,
                                                .data$Bander == "alexis" ~ "ASC",
                                                .data$Bander == "ESheldon" ~ "ES",
                                                .data$Bander %in% c("Phil heeb", "Philippe") ~ "PH",
                                                .data$Bander == "Stepane" ~ "S",
                                                TRUE ~ stringr::str_trim(stringr::str_to_upper(.data$Bander))),
                  ct = tolower(.data$Recapture),

                  #Retrieve information related to sight or RFID identification
                  capturePhysical = dplyr::case_when(stringr::str_detect(.data$ct, "seen|rfid|mvga|oamv") ~ FALSE,
                                                     TRUE ~ TRUE),
                  captureAlive = dplyr::case_when(stringr::str_detect(.data$ct, "dead") ~ FALSE,
                                                  TRUE ~ TRUE),


                  Tarsus = suppressWarnings(round(as.numeric(.data$Tarsus), 2)),
                  WingLength = suppressWarnings(round(as.numeric(.data$WingChord), 1)),
                  HeadLength = suppressWarnings(round(as.numeric(.data$Head), 1)),
                  FatScore = suppressWarnings(round(as.numeric(.data$Fat), 0)),
                  TailLength = suppressWarnings(round(as.numeric(.data$Tail), 1)),
                  Mass = suppressWarnings(round(as.numeric(.data$Weight), 1)),) %>%

    # Select variables to be used in the pipeline
    dplyr::select(individualID, studyID, siteID, Year, speciesID, nestID, Site, ct, captureSite, captureDate, observedSex, age, observedAge, captureTime, recordedBy,
                  capturePhysical, captureAlive, WingLength, Tarsus, HeadLength, FatScore, Mass, TailLength)

  # Read in nestbox data
  loc_data <- readxl::read_xlsx(path = paste0(db, "MOU_PrimaryData_Locations.xlsx"),
                                guess_max = 5000,
                                col_types = "text") %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    dplyr::mutate(Nest = toupper(.data$Nest),
                  NestRdata = toupper(.data$NestRdata),
                  Year = as.integer(.data$Year),
                  Latitude = suppressWarnings(round(as.numeric(.data$Latitude), 5)),
                  Longitude = suppressWarnings(round(as.numeric(.data$Longitude), 5)),
                  Elevation = suppressWarnings(round(as.numeric(.data$Elevation), 5))) %>%
    dplyr::select(Nest, NestRdata, Year, Elevation, Latitude, Longitude, Site, Woodlot)



  #### BROOD DATA
  message("Compiling brood information...")
  Brood_data_temp <- create_brood_MOU(nest_data, loc_data,
                                      optional_variables = optional_variables)

  #### CAPTURE DATA
  message("Compiling capture information...")
  Capture_data_temp <- create_capture_MOU(capture_data, loc_data, Brood_data_temp,
                                          optional_variables = optional_variables)

  #### INDIVIDUAL DATA
  message("Compiling individual information...")
  Individual_data_temp <- create_individual_MOU(Capture_data_temp,
                                                optional_variables = optional_variables)

  #### LOCATION DATA
  message("Compiling location information...")
  Location_data_temp <- create_location_MOU(loc_data, Capture_data_temp)

  #### MEASUREMENT DATA
  message("Compiling measurement information...")
  Measurement_data_temp <- create_measurement_MOU(Capture_data_temp)

  #### EXPERIMENT DATA
  message("Compiling experiment information...")
  Experiment_data_temp <- create_experiment_MOU(Brood_data_temp)


  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))


  #### PROCESSING FINAL DATA TO EXPORT

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

    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%

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



#### --------------------------------------------------------------------------~
#### FUNCTIONS
#### --------------------------------------------------------------------------~

#' Create brood data table in Moulis, France.
#'
#' @param nest_data Data frame of nest data from Moulis, France.
#'
#' @param loc_data Data frame of nestbox location records from Moulis, France.
#'
#' @return A data frame.
#'
create_brood_MOU <- function(nest_data, loc_data,
                             species_filter,
                             optional_variables) {

  ## Combine primary data to create brood data
  Brood_data_temp <- nest_data %>%

    ##Remove non-occupied nestboxes or unidentified species
    dplyr::filter(!is.na(.data$speciesID)) %>%

    ## Join location data to normalise information about plot_ID and location_ID
    dplyr::left_join(loc_data %>%
                       dplyr::select(Nest, NestRdata, Year, Site, Woodlot),
                     by = c("nestID" = "Nest", "Year"),
                     relationship = "many-to-one") %>%

    ## Create variables related to location
    dplyr::mutate(plotID = .data$Site,
                  locationID = dplyr::case_when(is.na(.data$Woodlot) ~ paste(.data$plotID, .data$nestID, "NB", sep = "_"),
                                                TRUE ~ paste(.data$Woodlot, .data$nestID, "NB", sep = "_")))  %>%


    dplyr::arrange(.data$siteID, .data$Year, .data$plotID, .data$locationID) %>%

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
                  observedFledgeDay = as.integer(lubridate::day(.data$observedFledgeDate)),
                  observedClutchType = dplyr::case_when(.data$NestAttempt == "1" ~ "first",
                                                        .data$NestAttempt == "2" ~ "second",
                                                        TRUE ~ NA_character_)) %>%

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


#' Create capture data table for Bergen, Norway.
#'
#' @param capture_data, Data frame of individuals (adults and nestlings) ringing records from Moulis, France.
#'
#' @param loc_data, Data frame of nestbox location records from Moulis, France.
#'
#' @param Brood_data_temp, Data frame of brood data created from primary data from Moulis, France.
#'
#' @return A data frame.

create_capture_MOU <- function(capture_data, loc_data,
                               Brood_data_temp,
                               species_filter,
                               optional_variables) {


  ## Extract primary data related to individuals that were not captured in the nest
  capture_adults <- capture_data %>%
    dplyr::filter(is.na(.data$observedAge) | .data$observedAge != "chick") %>%
    dplyr::mutate(plot = identify_plotID_MOU(.data$captureSite),
                  Site = stringr::str_replace_all(Site, " ", "_"),
                  captureType = dplyr::case_when(!is.na(.data$nestID) & tolower(.data$nestID) != "filet" ~ "nestbox",
                                                 TRUE ~ "mistnet")) %>%
    dplyr::left_join(loc_data %>%
                       dplyr::select(Nest, NestRdata, Year, Woodlot, Site),
                     by = c("nestID" = "NestRdata", "Year")) %>%
    dplyr::mutate(plotID = dplyr::case_when(!is.na(.data$Site.y) ~ .data$Site.y,
                                            TRUE ~ .data$plot),
                  locationID = dplyr::case_when(.data$captureType == "nestbox" & is.na(.data$Woodlot) ~ paste(.data$plotID, .data$Nest, "NB", sep = "_"),
                                                .data$captureType == "nestbox" & !is.na(.data$Woodlot) ~ paste(.data$Woodlot, .data$Nest, "NB", sep = "_"),
                                                TRUE ~ paste(.data$Site.x, "MN", sep = "_")))

    ## Extract primary data related to individuals that were captured in the nest
    capture_chicks <- capture_data %>%
      dplyr::filter(.data$observedAge == "chick") %>%
      dplyr::left_join(loc_data %>%
                         dplyr::select(Nest, NestRdata, Year, Woodlot, Site),
                       by = c("nestID" = "NestRdata", "Year")) %>%
      dplyr::mutate(plotID = .data$Site.y,
                    locationID = dplyr::case_when(is.na(.data$Woodlot) ~ paste(.data$plotID, .data$Nest, "NB", sep = "_"),
                                                  TRUE ~ paste(.data$Woodlot, .data$Nest, "NB", sep = "_"))) %>%
      dplyr::left_join(Brood_data_temp %>%
                         dplyr::select(Year,
                                       plotID,
                                       locationID,
                                       broodID,
                                       speciesID,
                                       observedLayDay,
                                       observedLayMonth),
                       by = c("Year", "plotID", "locationID"),
                       relationship = "many-to-many") %>%
      dplyr::filter(!is.na(.data$speciesID.y) & !is.na(.data$plotID) & nchar(.data$individualID) %in% c(7,8)) %>%
      dplyr::mutate(observedLayDate = paste(.data$Year, .data$observedLayMonth, .data$observedLayDay, sep = "-"),
                    diff = as.Date(.data$captureDate, format = "%Y-%m-%d") - as.Date(.data$observedLayDate, format = "%Y-%m-%d")) %>%
      dplyr::filter(.data$diff < 50 & .data$diff > 0) %>%
      dplyr::select(individualID, studyID, siteID, Year, speciesID = "speciesID.y", nestID, Site.x, ct,
                    captureSite, captureDate, observedSex, age, observedAge, captureTime, recordedBy, capturePhysical,
                    captureAlive, WingLength, Tarsus, HeadLength, FatScore, Mass, TailLength, Nest, Woodlot, Site.y,
                    plotID, locationID, broodID)




    Capture_data_temp <- dplyr::bind_rows(capture_adults, capture_chicks) %>%
      dplyr::mutate(captureYear = as.integer(Year),
                    captureMonth = as.integer(lubridate::month(.data$captureDate)),
                    captureDay = as.integer(lubridate::day(.data$captureDate)),
                    captureSiteID = .data$siteID,
                    releaseSiteID = .data$siteID,
                    capturePlotID = .data$plotID,
                    releasePlotID = .data$plotID,
                    captureLocationID = .data$locationID,
                    releaseLocationID = .data$locationID,
                    releaseAlive = .data$captureAlive,
                    chickAge = NA_integer_,
                    treatmentID = NA_character_,
                    releaseTagID = .data$individualID) %>%
      dplyr::filter(!is.na(.data$plotID)) %>%
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
      dplyr::arrange(.data$Year, .data$individualID, .data$captureDate) %>%
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


#' Create individual table for Moulis, France.
#'
#' @param Capture_data_temp  Capture data output from Moulis, France
#'
#' @return A data frame.

create_individual_MOU <- function(Capture_data_temp,
                                  species_filter,
                                  optional_variables){

  ## Create individual data from capture data
  Individual_data_temp <- Capture_data_temp %>%

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


#' Create measurement data table for Moulis, France.
#'
#' @param Capture_data_temp Capture data output from Moulis, France
#'
#' @return A data frame.

create_measurement_MOU <- function(Capture_data_temp) {

  ## Build measuremtent data based on capture data
  Measurement_data_temp <- Capture_data_temp %>%

    ## Format and create new data table
    dplyr::mutate(recordID = .data$captureID,
                  siteID = .data$captureSiteID,
                  measurementDeterminedYear = .data$captureYear,
                  measurementDeterminedMonth = .data$captureMonth,
                  measurementDeterminedDay = .data$captureDay) %>%

    ## Transform measurement columns into rows
    tidyr::pivot_longer(cols = c("WingLength",
                                 "Tarsus",
                                 "HeadLength",
                                 "FatScore",
                                 "Mass",
                                 "TailLength"),
                        names_to = "measurementType",
                        values_to = "measurementValue",
                        values_drop_na = TRUE) %>%
    dplyr::filter(!(measurementValue %in% c("No Value", "NA"))) %>%

    ## Create new variables
    dplyr::mutate(measurementSubject = "capture",
                  measurementAccuracy = NA_real_,
                  measurementUnit = dplyr::case_when(.data$measurementType == "Mass" ~ "g",
                                                     .data$measurementType == "FatScore" ~ "no unit",
                                                     TRUE ~ "mm"),
                  measurementMethod = dplyr::case_when(.data$measurementType == "Tarsus" ~ "tarsus-length, from ESF guideline, but higher than the notch of the metatarsus (values slightly higher than usual)",
                                                       .data$measurementType == "WingLength" ~ "flattened, maximum chord from ESF guidelines",
                                                       .data$measurementType == "HeadLength" ~ "distance from the back of the skull and the tip of the bill",
                                                       .data$measurementType == "FatScore" ~ "from ESF guidelines, from 0 to 3",
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


#' Create location data table for Moulis, France.
#'
#' @param loc_data Data frame of nestbox location records from Moulis, France.
#'
#' @param Capture_data_temp Capture data output from Moulis, France
#'
#' @return A data frame.


create_location_MOU <- function(loc_data,
                                Capture_data_temp) {

  ## Create table with information related to mistnet captures
  loc_mn <- Capture_data_temp %>%
    dplyr::filter(captureType == "mistnet") %>%
    dplyr::arrange(.data$Site.x, .data$Year) %>%
    dplyr::group_by(.data$Site.x) %>%
    dplyr::mutate(locationID = paste(.data$Site.x, "MN", sep = "_"),
                  studyID = "MOU-1",
                  siteID = "MOU",
                  startYear = first(.data$Year),
                  endYear = last(.data$Year),
                  locationType = "capture",
                  locationDetails = "mistnet") %>%
    dplyr::distinct(.data$locationID, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(endYear = dplyr::case_when(.data$endYear == 2023 ~ NA_integer_,
                                             TRUE ~ .data$endYear)) %>%
    dplyr::select(locationID, locationType, locationDetails, studyID, siteID, startYear, endYear)

  ## Create table with nest box information
  Location_data_temp <- loc_data %>%
    dplyr::arrange(.data$Nest, .data$Year, .data$Site) %>%

    dplyr::group_by(.data$Nest) %>%
    dplyr::mutate(locationID = dplyr::case_when(is.na(.data$Woodlot) ~ paste(.data$Site, .data$Nest, "NB", sep = "_"),
                                                TRUE ~ paste(.data$Woodlot, .data$Nest, "NB", sep = "_")),
                  studyID = "MOU-1",
                  siteID = "MOU",
                  startYear = first(.data$Year),
                  endYear = last(.data$Year),
                  locationType = "nest",
                  locationDetails = "Schwegler nesting box",
                  decimalLatitude = round(as.numeric(Latitude), 4),
                  decimalLongitude = round(as.numeric(Longitude), 4),
                  elevation = round(as.numeric(Elevation), 4),
                  habitatID = NA_character_) %>%
    dplyr::distinct(.data$Nest, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(endYear = dplyr::case_when(.data$endYear == 2023 ~ NA_integer_,
                                             TRUE ~ .data$endYear)) %>%
    ## Add table with mistnet information
    dplyr::bind_rows(loc_mn) %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(data_templates$v2.0$Location_data)), tidyselect::everything())

  return(Location_data_temp)

}

#' Create experiment data table for Moulis, France.
#'
#' @param Brood_data_temp Data frame of nest data from Moulis, France.
#'
#' @return A data frame.

create_experiment_MOU <- function(Brood_data_temp) {

  # Experiments are poorly reported in the main dataset - contact data custodians for details
  Experiment_data_temp <- Brood_data_temp %>%

    # Drop broods without treatmentID
    dplyr::filter(!is.na(.data$treatmentID)) %>%
    dplyr::select(treatID = "treatmentID",
                  treatmentStartYear = "observedLayYear",
                  "studyID",
                  "siteID") %>%
    dplyr::mutate(treatmentID = paste(.data$treatID, .data$treatmentStartYear, sep = "_"),
                  experimentID = .data$treatID,
                  experimentType = dplyr::case_when(.data$treatmentID == "cross-fostering" ~ "transfert",
                                                    TRUE ~ "behavioural experiment"),
                  treatmentDetails = "Contact data custodian for details",
                  treatmentEndYear = .data$treatmentStartYear) %>%
    dplyr::distinct(.data$treatmentID, .keep_all = TRUE) %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(data_templates$v2.0$Experiment_data)), tidyselect::everything())

  return(Experiment_data_temp)

}


#' Filter cap_data to remove data corresponding to Toulouse populations
#' And identify/reorganize mistnet plot by plotID.
#'
#' Plots in MOU are translated into one of 7 plotID for mistnet plots,
#' to standardise these plots before further informations (GPS coordinates)
#'
#' @param variable "Site" name.
#'
#' @return Various characters corresponding to the main six monitored plots or miscellaneous
#' when not associated with one of the main plots
#' @export
#'
#' @examples
#' identify_plotID_MOU("Lab")
identify_plotID_MOU <- function(variable){

  dplyr::case_when(variable %in% c("Lab", "L1", "Moulis Lab", "L1_moulis", "Labo") ~ "Labo",
                   variable %in% c("Mou2", "Mou3", "Mou1", "M1", "Mou5", "M2", "Moulis") ~ "Moulis",
                   variable %in% c("Afr Garden", "Galeyafr", "Galey", "Andy", "G1", "Gj") ~ "Galey",
                   variable %in% c("H1", "Castera Bas", "Castera", "Arrech", "Arrech North", "H4",
                                   "H6", "H7", "H5") ~ "Castera",
                   variable %in% c("Ces1", "Ces3", "Ces4", "Ces2", "Asc Garden", "Cescau Haut",
                                   "Cescau 1", "Cescau", "Cesc 4", "C1", "C4", "C5", "C", "C3", "C2",
                                   "Cescau Asc") ~ "Cescau",
                   variable %in% c("Balacet", "Ba", "B") ~ "Balacet",
                   variable %in% c("Montegut", "Sor", "Aubert", "Capsour", "Cap Sour", "Ledar",
                                   "Castillon", "Cap De Sour", "Montjoie", "Gajan",
                                   "Grotte Aliou", "Mj", "Antras", "Villargein", "Callivert") ~ "Miscellaneous",
                   variable %in% c("Enfa", "Majouret", "Jjap", "Jpla", "Brienne", "Jjtl",
                                   "Jarplatlse", "No_value", "Au", "An", "No Value",
                                   "Colomiers", "A", "Ensfea", "Nev", "Lb", "Ups", "At",
                                   "La", "Lc", "Jdp", "Jj", "Jp", "Obs") ~ NA_character_,
                   TRUE ~ NA_character_)

}

