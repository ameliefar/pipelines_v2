#'Construct standard format for data from Bergen, Norway
#'
#'A pipeline to produce the standard format for the nest box population in Bergen, Norway, administered by Adele Mennerat.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf}{here}.
#'
#'\strong{speciesID}: Primarily Great tits and Blue tits.
#'
#'\strong{individualID}: Should be a character string of length 7 where the first two characters are either a letter or number and the last five characters are all numbers.
#'
#'\strong{studyID}: one population "BRG-1" at the moment -
#'
#'\strong{plotID}: two main sites Milde & Langeskogen - the primary data has an upper level of description (variable "Location") but
#'after discussing with the data custodian, it seems useless, as all nestboxes have a unique number (should avoid special character issue)
#'
#'\strong{decimalLatitude}: waiting for file with GPS coordinates for all nestboxes (data custodian confirmed it exists and they'll send it to me)
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 6 .csv files or 6 data frames in the standard format.
#'@export

format_BRG <- function(db = choose_directory(),
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
  nest_data <- readxl::read_xlsx(path = paste0(db, "/BRG_PrimaryData.xlsx"),
                                 guess_max = 5000,
                                 sheet = "Broods",
                                 col_types = "text") %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Rename and process columns
    dplyr::mutate(dplyr::across(where(is.character),
                                ~dplyr::na_if(., "."))) %>%
    dplyr::transmute(studyID = "BRG-1",
                     siteID = "BRG",
                     Year = as.integer(.data$Year),
                     speciesID = dplyr::case_when(.data$Species == "Kj\u00f8ttmeis"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10001)],
                                                .data$Species == "Bl\u00e5meis"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10002)],
                                                .data$Species == "Svarthvitfluesnapper"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10003)],
                                                .data$Species == "Svartmeis"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10005)],
                                                TRUE ~ NA_character_), #remove very specific case (unfamiliar species, mixed-brood instances, or error)
                     plotID = .data$Site,
                     locationID = paste(.data$Site, .data$Nestbox, "NB", sep = "_"),
                     observedLayDate = suppressWarnings(as.Date(as.numeric(.data$LayDate),
                                                                origin = as.Date(paste0(.data$Year, "-03-31")))),
                     observedHatchDate = suppressWarnings(as.Date(as.numeric(.data$HatchDate),
                                                                  origin = as.Date(paste0(.data$Year, "-03-31")))),
                     observedClutchSize = suppressWarnings(as.integer(.data$ClutchSize)),
                     observedBroodSize = suppressWarnings(as.integer(.data$BroodSize)),
                     observedNumberFledged = suppressWarnings(as.integer(.data$Fledged)),
                     HabitatType = tolower(.data$Vegetation),
                     treatmentID = tolower(.data$Experiment)) %>% #anticipate new column "Experiment" as discussed with data custodian

    dplyr::arrange(.data$siteID, .data$Year, .data$plotID, .data$locationID)


  ## Read in chick data
  chick_data <- suppressWarnings(readxl::read_xlsx(path = paste0(db, "/BRG_PrimaryData.xlsx"),
                                                   sheet = "Chicks",
                                                   guess_max = 5000,
                                                   col_types = "text")) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Rename and process columns
    dplyr::mutate(dplyr::across(where(is.character),
                                ~dplyr::na_if(., "."))) %>%
    dplyr::transmute(studyID = "BRG-1",
                     siteID = "BRG",
                     Year = as.integer(.data$Year),
                     speciesID = dplyr::case_when(.data$Species == "Kj\u00f8ttmeis"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10001)],
                                                .data$Species == "Bl\u00e5meis"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10002)],
                                                .data$Species == "Svarthvitfluesnapper"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10003)],
                                                .data$Species == "Svartmeis"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10005)],
                                                TRUE ~ NA_character_), #remove very specific case (unfamiliar species, mixed-brood instances, or error)
                     plotID = .data$Site,
                     locationID = paste(.data$Site, .data$Nestbox, "NB", sep = "_"),
                     captureDate = suppressWarnings(as.Date(paste(.data$Year, .data$Month, .data$Day, sep = "-"))), #keep this to sort data
                     captureYear = .data$Year,
                     captureMonth = as.integer(.data$Month),
                     captureDay = as.integer(.data$Day),
                     individualID = .data$Ring,
                     captureTagID = NA_character_,
                     Age = tolower(.data$Age),
                     chickAge = as.integer(.data$ChickAge),
                     captureTime = gsub("--", ":00", .data$Time),
                     recordedBy = .data$ObserverId,
                     mass = round(suppressWarnings(as.numeric(.data$Weight)), 1),
                     tarsus = round(suppressWarnings(as.numeric(.data$Tarsus)), 2),
                     capturePhysical = TRUE) # chicks are always handled thus physically captured


  ## Read in adult data
  adult_data <- suppressWarnings(readxl::read_xlsx(path = paste0(db, "/BRG_PrimaryData.xlsx"),
                                                   sheet = "Adults",
                                                   guess_max = 5000,
                                                   col_types = "text")) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Rename and process columns
    dplyr::mutate(dplyr::across(where(is.character),
                                ~dplyr::na_if(., "."))) %>%
    dplyr::transmute(studyID = "BRG-1",
                     siteID = "BRG",
                     Year = as.integer(.data$Year),
                     speciesID = dplyr::case_when(.data$Species == "Kj\u00f8ttmeis"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10001)],
                                                .data$Species == "Bl\u00e5meis"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10002)],
                                                .data$Species == "Svarthvitfluesnapper"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10003)],
                                                .data$Species == "Svartmeis"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10005)],
                                                TRUE ~ NA_character_), #anticipate instances of processing adults from other species
                     plotID = .data$Site,
                     locationID = paste(.data$Site, .data$Nestbox, "NB", sep = "_"),
                     captureDate = suppressWarnings(as.Date(paste(.data$Year, .data$Month, .data$Day, sep = "-"))), #keep this to sort data
                     captureYear = .data$Year,
                     captureMonth = as.integer(.data$Month),
                     captureDay = as.integer(.data$Day),
                     individualID = .data$Ring,
                     captureTagID = dplyr::case_when(.data$Control == "new" ~ NA_character_,
                                              TRUE ~ .data$Ring),
                     observedSex = .data$Sex,
                     recordedBy = .data$ObserverId,
                     Age = dplyr::case_when(.data$ObsAge == "juv" ~ "subadult",
                                            .data$ObsAge == "ad" ~ "adult"),
                     captureTime = dplyr::case_when(stringr::str_detect(.data$Time, "--") ~ gsub("--", ":00", .data$Time), #account for cases when only hour was provided
                                                    TRUE ~ paste0(substr(.data$Time,1,2), ":", substr(.data$Time,3,4))),
                     capturePhysical = dplyr::if_else(stringr::str_detect(.data$Comment, "ID from color"), FALSE, TRUE),
                     mass = round(suppressWarnings(as.numeric(.data$Weight)), 1),
                     wingLength = as.numeric(.data$WingLength),
                     tarsus = round(suppressWarnings(as.numeric(.data$Tarsus)), 2))

  #### BROOD DATA
  message("Compiling brood information...")
  Brood_data_temp <- create_brood_BRG(nest_data, chick_data, adult_data,
                                      optional_variables = optional_variables)

  #### CAPTURE DATA
  message("Compiling capture information...")
  Capture_data_temp <- create_capture_BRG(chick_data, adult_data, Brood_data_temp,
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
  Location_data_temp <- create_location_BRG(nest_data)

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

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_BRG.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_BRG.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_BRG.csv"), row.names = F)

    utils::write.csv(x = Measurement_data, file = paste0(path, "\\Measurement_data_BRG.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_BRG.csv"), row.names = F)

    utils::write.csv(x = Experiment_data, file = paste0(path, "\\Experiment_data_BRG.csv"), row.names = F)

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

#' Create brood data table in Bergen, Norway.
#'
#' @param nest_data Data frame of nest data from Bergen, Norway.
#'
#' @param chick_data Data frame of chick ringing records from Bergen, Norway.
#'
#' @param adult_data Data frame of adult ringing records from Bergen, Norway.
#'
#' @return A data frame.

create_brood_BRG <- function(nest_data, chick_data, adult_data,
                             species_filter,
                             optional_variables) {

  ## Combine primary data to create brood data
  Brood_data_temp <- nest_data %>%

    ## Join summarized chick data
    dplyr::left_join(chick_data %>%
                       dplyr::select(Year,
                                     plotID,
                                     locationID,
                                     individualID) %>%
                       dplyr::group_by(.data$Year, .data$plotID, .data$locationID) %>%
                       dplyr::summarise(nbChicks = n()),
                     by = c("Year", "plotID", "locationID")) %>%

    ## Join adult data to get info on parents
    dplyr::left_join(adult_data %>%
                       dplyr::select(Year,
                                     plotID,
                                     locationID,
                                     individualID,
                                     observedSex) %>%
                       tidyr::pivot_wider(id_cols = c(Year,
                                                      plotID,
                                                      locationID),
                                          values_from = individualID,
                                          names_from = observedSex) %>%
                       dplyr::rename(femaleID = "F",
                                     maleID = "M"),
                     by = c("Year", "plotID", "locationID")) %>%

    dplyr::arrange(.data$siteID, .data$Year, .data$plotID, .data$locationID) %>%

    ## Create additional variables
    dplyr::mutate(broodID = paste(.data$Year, 1:dplyr::n(), sep = "-"),
                  observedLayYear = as.integer(lubridate::year(.data$observedLayDate)),
                  observedLayMonth = as.integer(lubridate::month(.data$observedLayDate)),
                  observedLayDay = as.integer(lubridate::day(.data$observedLayDate)),
                  observedHatchYear = as.integer(.data$Year),
                  observedHatchMonth = as.integer(lubridate::month(.data$observedHatchDate)),
                  observedHatchDay = as.integer(lubridate::day(.data$observedHatchDate)),
                  observedFledgeYear = as.integer(.data$Year),
                  observedClutchType = "first") %>%  #check with data custodian if cases of second/replacement clutch

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
                                ~dplyr::case_when(stringr::str_detect(., "^[A-Z0-9]{2}[0-9]{5}$") ~ .,
                                                  TRUE ~ NA_character_))) %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(data_templates$v2.0$Brood_data)), tidyselect::everything())

  return(Brood_data_temp)

}

#' Create capture data table for Bergen, Norway.
#'
#' @param chick_data, Data frame of chick ringing records from Bergen, Norway.
#'
#' @param adult_data, Data frame of adult ringing records from Bergen, Norway.
#'
#' @param Brood_data_temp, Data frame of brood data created from primary data from Bergen, Norway.
#'
#' @return A data frame.

create_capture_BRG <- function(chick_data, adult_data, Brood_data_temp,
                               species_filter,
                               optional_variables) {


  ## Combine primary data to create capture data
  Capture_data_temp <- adult_data %>%

    ## Bind chick data
    dplyr::bind_rows(chick_data %>%
                       dplyr::left_join(Brood_data_temp %>%
                                          dplyr::select(Year,
                                                        plotID,
                                                        locationID,
                                                        broodID),
                                        by = c("Year", "plotID", "locationID"))) %>%

    ## Create new columns
    dplyr::mutate(releaseTagID = .data$individualID,
                  captureSiteID = .data$siteID,
                  releaseSiteID = .data$siteID, #no instance of transfert/cross-fostering
                  capturePlotID  = .data$plotID,
                  releasePlotID  = .data$plotID,
                  captureAlive = TRUE,
                  releaseAlive = TRUE) %>%

    ## Set improperly formatted IDs to NA and filter
    dplyr::mutate(individualID = dplyr::case_when(stringr::str_detect(.data$individualID,  "^[A-Z0-9]{2}[0-9]{5}$") ~ .data$individualID,
                                            TRUE ~ NA_character_)) %>%
    dplyr::filter(!is.na(.data$individualID)) %>%

    ## Create captureID
    ## Arrange
    dplyr::arrange(.data$Year, .data$individualID, .data$captureDate) %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::mutate(captureID = paste(.data$individualID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup() %>%

    ## Calculate age
    {if("exactAge" %in% optional_variables | "minimumAge" %in% optional_variables) calc_age(data = .,
                                                                                            ID = .data$individualID,
                                                                                            Age = .data$Age,
                                                                                            Date = .data$captureDate,
                                                                                            Year = .data$Year,
                                                                                            protocol_version = "2.0")
      else .}  %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(data_templates$v2.0$Capture_data)), tidyselect::everything())

  return(Capture_data_temp)

}



#' Create individual table for Bergen, Norway.
#'
#' @param Capture_data_temp Capture data output from Bergen, Norway
#'
#' @return A data frame.

create_individual_BRG <- function(Capture_data_temp,
                                  species_filter,
                                  optional_variables){

  ## Create individual data from capture data
  Individual_data_temp <- Capture_data_temp %>%

    ## Format and create new data columns
    dplyr::mutate(siteID = .data$captureSiteID,
                  tagYear = dplyr::if_else(is.na(.data$captureTagID), .data$captureYear, NA_integer_),
                  tagMonth = dplyr::if_else(is.na(.data$captureTagID), .data$captureMonth, NA_integer_),
                  tagDay = dplyr::if_else(is.na(.data$captureTagID), .data$captureDay, NA_integer_),
                  tagStage = dplyr::if_else(is.na(.data$captureTagID), .data$Age, NA_character_),
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



#' Create measurement data table for Bergen, Norway.
#'
#' @param Capture_data_temp Data frame of nest data from Bergen, Norway.
#'
#' @return A data frame.

create_measurement_BRG <- function(Capture_data_temp) {

  ## Build measuremtent data based on capture data
  Measurement_data_temp <- Capture_data_temp %>%

    ## Format and create new data table
    dplyr::mutate(recordID = .data$captureID,
                  siteID = .data$captureSiteID,
                  measurementDeterminedYear = .data$captureYear,
                  measurementDeterminedMonth = .data$captureMonth,
                  measurementDeterminedDay = .data$captureDay) %>%

    ## Transform measurement columns into rows
    tidyr::pivot_longer(cols = c("tarsus",
                                 "wingLength",
                                 "mass"),
                        names_to = "measurementType",
                        values_to = "measurementValue",
                        values_drop_na = TRUE) %>%

    ## Create new variables
    dplyr::mutate(measurementSubject = "capture",
                  measurementAccuracy = NA_real_,
                  measurementUnit = dplyr::case_when(.data$measurementType == "mass" ~ "g",
                                                     TRUE ~ "mm"),
                  measurementMethod = dplyr::case_when(.data$measurementType == "tarsus" ~ "alternative",
                                                       .data$measurementType == "wingLength" ~ "flattened, maximum chord from ESF guidelines",
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



#' Create location data table for Bergen, Norway.
#'
#' @param nest_data Data frame of nest data from Bergen, Norway.
#'
#' @return A data frame.

create_location_BRG <- function(nest_data) {

  ## Build location data based on nest data
  Location_data_temp <- nest_data %>%

    ## Summarize information for each nest box
    dplyr::group_by(.data$siteID, .data$locationID) %>%
    dplyr::mutate(locationType = "NB",
                  locationDetails = "Schwegler nesting box",
                  startYear = min(.data$Year, na.rm = TRUE),
                  endYear = NA_integer_,
                  decimalLatitude = 60.25, #global coordinates until I get the file with nestbox coordinates
                  decimalLongitude = 5.26,
                  habitatID = dplyr::case_when(.data$HabitatType == "deciduous" ~ "G1",
                                               .data$HabitatType == "evergreen" ~ "G2",
                                               TRUE ~ "G4")) %>%

    ## Keep distinct records
    dplyr::distinct(.data$siteID, .data$locationID, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(data_templates$v2.0$Location_data)), tidyselect::everything())

    return(Location_data_temp)

}


#' Create experiment data table for Bergen, Norway.
#'
#' @param Brood_data_temp Data frame of nest data from Bergen, Norway.
#'
#' @return A data frame.

create_experiment_BRG <- function(Brood_data_temp) {

  # No experiment so far - may change in the future years
  Experiment_data_temp <- Brood_data_temp %>%

    # Drop broods without treatmentID
    dplyr::filter(!is.na(.data$treatmentID)) %>%
    dplyr::select("treatmentID",
                  treatmentStartYear = "observedLayYear",
                  "studyID",
                  "siteID") %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(data_templates$v2.0$Experiment_data)), tidyselect::everything())

  return(Experiment_data_temp)

}
