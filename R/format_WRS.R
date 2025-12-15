
#'Construct standard format for data from Warsaw, Poland
#'
#'A pipeline to produce the standard format for the nest box population in Warsaw, Poland, administered by Marta Szulkin.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.pdf}{here}.
#'
#'\strong{speciesID}: Only PARMAJ and CYACAE are entered in the Capture and Individual tables.
#'
#'\strong{individualID}: Should be a character string of length 7
#'
#'\strong{captureDate}: for chicks that failed to fledge (Fledged == 0 in raw data), captureDate was calculated as 10 days after DateD15
#' DateD15 corresponds to the date when chicks were 15 days old (and processed).
#'
#'\strong{treatmentID}: there are no reported experiments, so there is no experiment table
#'
#'\strong{endYear}: the data administrator indicated ending the long-term monitoring program in 2025 so endYear is set to "2025"
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 5 .csv files or 5 data frames in the standard format. (no experiment table)
#'@export
#

format_WRS <- function(db = choose_directory(),
                       path = ".",
                       species = NULL,
                       pop = NULL,
                       output_type = 'R'){

  #Force choose_directory() if used
  force(db)

  start_time <- Sys.time()

  message("Importing primary data...")

  #### Force user to select directory
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

  start_time <- Sys.time()

  ## Set options
  options(dplyr.summarise.inform = FALSE)

  ## Read in primary data from nest sheet



  ## Read in nest data
  nest_data <- readxl::read_xlsx(path = paste0(db, "/WRS_PrimaryData.xlsx"),
                                 guess_max = 5000,
                                 sheet = "nests",
                                 col_types = "text",
                                 .name_repair = "minimal") %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Reformat and rename columns
    dplyr::mutate(dplyr::across(where(is.character),
                                ~ dplyr::na_if(., "NA")),
                  studyID = "WRS-1",
                  siteID = "WRS",
                  Year = as.integer(.data$Year),
                  speciesID = dplyr::case_when(.data$Species == "GT" ~ species_codes$speciesID[which(species_codes$speciesCode == 10001)],
                                               .data$Species == "BT" ~ species_codes$speciesID[which(species_codes$speciesCode == 10002)],
                                               TRUE ~ NA_character_), #remove potential specific case (unfamilar species, mixed-brood, errors)
                  plotID = paste(.data$siteID, .data$Site, sep = "_"),
                  locationID = .data$NestboxId,
                  observedLayYear = as.integer(.data$Year),
                  observedLayDate = suppressWarnings(as.Date(as.numeric(.data$LAyDAte),
                                                             origin = as.Date(paste0(.data$Year, "-03-31")))),
                  observedLayMonth = as.integer(lubridate::month(.data$observedLayDate)),
                  observedLayDay = as.integer(lubridate::day(.data$observedLayDate)),
                  observedHatchDate = suppressWarnings(as.Date(as.numeric(.data$Hd),
                                                               origin = as.Date(paste0(.data$Year, "-03-31")))),
                  observedHatchYear = as.integer(lubridate::year(.data$observedHatchDate)),
                  observedHatchMonth = as.integer(lubridate::month(.data$observedHatchDate)),
                  observedHatchDay = as.integer(lubridate::day(.data$observedHatchDate)),
                  observedClutchSize = suppressWarnings(as.integer(.data$Cs)),
                  observedBroodSize = suppressWarnings(as.integer(.data$NrHAtched)),
                  observedNumberFledged = suppressWarnings(as.integer(.data$NrFledged)),
                  Latitude = as.numeric(.data$Lat),
                  Longitude = as.numeric(.data$Long),
                  HabitatType = .data$UrbanRural,
                  NumberEggs = suppressWarnings(as.integer(.data$NrEggsWeighed)),
                  AvgEggMass = suppressWarnings(round(as.numeric(.data$EggMAssTot)/.data$NumberEggs, 3))) %>%
    dplyr::group_by(.data$Year, .data$NestboxId) %>%
    ## Create a unique identifier for breeding event (independently of the one provided by the data owner)
    dplyr::mutate(broodID2 = paste(Year, NestboxId, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup() %>%
    ## Arrange
    dplyr::arrange(.data$siteID, .data$Year, .data$locationID, .data$plotID, .data$broodID2)


  ## Read in primary data from chicks
  chick_data <- suppressWarnings(readxl::read_xlsx(path = paste0(db, "/WRS_PrimaryData.xlsx"),
                                                   guess = 5000,
                                                   sheet = "chicks",
                                                   .name_repair = "minimal")) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    dplyr::mutate(dplyr::across(where(is.character),
                                ~ dplyr::na_if(., "NA")),
                  studyID = "WRS-1",
                  siteID = "WRS",
                  Year = as.integer(.data$Year),
                  speciesID = dplyr::case_when(.data$Species == "GT" ~ species_codes$speciesID[which(species_codes$speciesCode == 10001)],
                                               .data$Species == "BT" ~ species_codes$speciesID[which(species_codes$speciesCode == 10002)],
                                               TRUE ~ NA_character_), #remove potential specific case (unfamilar species, mixed-brood, errors),
                  plotID = paste(.data$siteID, .data$Site, sep = "_"),
                  locationID = .data$NestboxId,
                  individualID = .data$RingId,
                  observedHatchDate = suppressWarnings(as.Date(as.numeric(.data$Hd),
                                                               origin = as.Date(paste0(.data$Year, "-03-31")))),

                  ## Handling different date formats in Excel
                  ## If chicks die before banding, the CaptureDate is set to the last day it was handled.
                  captureDate = suppressWarnings(dplyr::case_when(grepl("-|/", .data$D15Date) ~ lubridate::mdy(.data$D15Date, quiet = TRUE),
                                                                  !is.na(janitor::excel_numeric_to_date(as.numeric(.data$D15Date))) ~ lubridate::ydm(janitor::excel_numeric_to_date(as.numeric(.data$D15Date)), quiet = TRUE),
                                                                  !is.na(.data$WeightD15) ~ .data$observedHatchDate + 15L,
                                                                  !is.na(.data$observedHatchDate) ~ .data$observedHatchDate,
                                                                  TRUE ~ lubridate::NA_Date_)),
                  Tarsus = suppressWarnings(as.numeric(.data$TarsusD15)),
                  Mass = suppressWarnings(as.numeric(.data$WeightD15)),
                  chickAge = dplyr::case_when(!is.na(.data$WeightD15) ~ 15L,
                                              TRUE ~ NA_integer_),
                  observedSex = NA_character_,
                  captureAlive = TRUE,
                  releaseAlive = TRUE,
                  observedAge = "chick")

  ## Create new rows for every chick that did not fledge
  chick_dead <- chick_data %>%
    dplyr::filter(.data$Fledged == "0") %>%
    dplyr::mutate(captureDate = .data$captureDate + lubridate::days(10), # considering they were found dead when checking fledging event (~10 days after banding chicks)
                  captureAlive = FALSE,
                  releaseAlive = FALSE) %>%
    dplyr::select(individualID, studyID, siteID, Year, speciesID, locationID, plotID, captureDate, UniqueBreedingEvent, observedAge, observedSex, captureAlive, releaseAlive)

  ## Bind new dataframe to existed one (on chicks)
  chick_data <- bind_rows(chick_data, chick_dead)


  ## Read in primary data from adults
  adult_data <- suppressWarnings(readxl::read_xlsx(path = paste0(db, "/WRS_PrimaryData.xlsx"),
                                                   guess = 5000,
                                                   sheet = "parents",
                                                   col_types = "text")) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    dplyr::mutate(dplyr::across(where(is.character), #I need to clean species
                                ~dplyr::na_if(., "NA")),
                  studyID = "WRS-1",
                  siteID = "WRS",
                  Year = as.integer(.data$Year),
                  Species = stringr::str_to_upper(stringr::str_remove(.data$Species, "`")),
                  speciesID = dplyr::case_when(.data$Species == "GT" ~ species_codes$speciesID[which(species_codes$speciesCode == 10001)],
                                               .data$Species == "BT" ~ species_codes$speciesID[which(species_codes$speciesCode == 10002)],
                                               TRUE ~ NA_character_), #remove potential specific case (unfamilar species, mixed-brood, errors),
                  plotID = paste(.data$siteID, .data$Site, sep = "_"),
                  locationID = .data$NestboxId,
                  individualID = .data$RingId,
                  ## Handling different date formats in Excel
                  captureDate = suppressWarnings(dplyr::case_when(grepl("-|/", .data$Date) ~ lubridate::mdy(.data$Date, quiet = TRUE),
                                                                  !is.na(janitor::excel_numeric_to_date(as.numeric(.data$Date))) ~ lubridate::ydm(janitor::excel_numeric_to_date(as.numeric(.data$Date)), quiet = TRUE),
                                                                  TRUE ~ lubridate::NA_Date_)),
                  ## Handling different time formats in Excel
                  captureTime = suppressWarnings(case_when(grepl(":", .data$Hour) ~ as.character(.data$Hour),
                                                           TRUE ~ format(as.POSIXct(Sys.Date() + as.numeric(.data$Hour)), "%H:%M", tz="UTC"))),
                  ## Standardize age
                  observedAge = dplyr::case_when(.data$Age == 2 ~ "subadult",
                                                 toupper(.data$Age) == "PO2" ~ "adult"),
                  ## Standardize sex
                  observedSex = dplyr::case_when(is.na(.data$Sex) ~ "U",
                                                 TRUE ~ .data$Sex),

                  ## Ensure measurements are in numeric class
                  Tarsus = suppressWarnings(as.numeric(.data$Tarsus)),
                  Mass = suppressWarnings(as.numeric(.data$Weight)),
                  WingLength = suppressWarnings(as.numeric(.data$WingLength)),

                  captureAlive = TRUE,
                  releaseAlive = TRUE)




  #### BROOD DATA
  message("Compiling brood information...")
  Brood_data_temp <- create_brood_WRS(nest_data, chick_data, adult_data)

  #### CAPTURE DATA
  message("Compiling capture information...")
  Capture_data_temp <- create_capture_WRS(chick_data, adult_data)

  #### INDIVIDUAL DATA
  message("Compiling individual information...")
  Individual_data_temp <- create_individual_WRS(Capture_data_temp, Brood_data_temp)

  #### MEASUREMENT DATA
  message("Compiling measurement data")
  Measurement_data_temp <- create_measurement_WRS(Capture_data_temp) #new in v2.0

  #### LOCATION DATA
  message("Compiling location information...")
  Location_data_temp <- create_location_WRS(nest_data)

  # #### EXPERIMENT DATA
  # message("Compiling experiment information...")
  # Experiment_data_temp <- create_experiment_WRS(Brood_data_temp) #new in v2.0

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
  # Experiment_data <- Experiment_data_temp %>%
  #
  #   ## Keep only necessary columns
  #   dplyr::select(dplyr::contains(names(data_templates$v2.0$Experiment_data))) %>%
  #
  #   ## Add missing columns
  #   dplyr::bind_cols(data_templates$v2.0$Experiment_data[0, !(names(data_templates$v2.0$Experiment_data) %in% names(.))] %>%
  #                      tibble::add_row()) %>%
  #
  #   ## Remove any NAs from critical columns
  #   dplyr::filter_at(vars(siteID),
  #                    all_vars(!is.na(.))) %>%
  #
  #   ## Reorder columns
  #   dplyr::select(names(data_templates$v2.0$Experiment_data))  %>%
  #   dplyr::ungroup()
  #

  ## Filter to keep only desired Species if specified for Brood, Capture, and Individual tables
  if(!is.null(species_filter)){

    Brood_data <- Brood_data %>%
      dplyr::filter(.data$speciesID %in% species_filter & !(is.na(.data$speciesID)))

    Capture_data <- Capture_data %>%
      dplyr::filter(.data$speciesID %in% species_filter & !(is.na(.data$speciesID)))

    Individual_data <- Individual_data %>%
      dplyr::filter(.data$speciesID %in% species_filter & !(is.na(.data$speciesID)))

  }

  ## Filter to keep only desired Studies if specified for Brood, Capture, Individual, Measurement, Location and Experiment tables
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

    # Experiment_data <- Experiment_data %>%
    #   dplyr::filter(.data$siteID %in% pop_filter & !(is.na(.data$siteID)))

  }

  #### EXPORT DATA

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_WRS.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_WRS.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_WRS.csv"), row.names = F)

    utils::write.csv(x = Measurement_data, file = paste0(path, "\\Measurement_data_WRS.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_WRS.csv"), row.names = F)

    #utils::write.csv(x = Experimental_data, file = paste0(path, "\\Experimental_data_WRS.csv"), row.names = F)


    invisible(NULL)

  }

  if(output_type == "R"){

    message("Returning R objects...")

    return(list(Brood_data = Brood_data,
                Capture_data = Capture_data,
                Individual_data = Individual_data,
                Measurement_data = Measurement_data,
                Location_data = Location_data#,
                #Experiment_data = Experiment_data
    ))

  }

}


#### --------------------------------------------------------------------------~
#### FUNCTIONS
#### --------------------------------------------------------------------------~


#' Create brood data table in Warsaw, Poland.
#'
#' @param nest_data Data frame of nest data from Warsaw, Poland.
#'
#' @param chick_data Data frame of chick ringing records from Warsaw, Poland.
#'
#' @param adult_data Data frame of adult ringing records from Warsaw, Poland.
#'
#' @return A data frame.

create_brood_WRS <- function(nest_data, chick_data, adult_data) {

  ## Combine primary data
  ## TODO: Check on tarsus method
  Brood_data_temp <- nest_data %>%

    ## Keep only records with sufficient information
    dplyr::filter(!is.na(.data$UniqueBreedingEvent) & !is.na(.data$speciesID)) %>%

    dplyr::left_join(adult_data %>%
                       dplyr::select(UniqueBreedingEvent,
                                     observedSex,
                                     individualID) %>%
                       stats::na.omit() %>%

                       ## A few cases where the same individuals were caught multiple times for a single breeding event
                       ## Keeping only distinct records by breeding event and sex
                       ## TODO: Check about whether this is robust
                       dplyr::distinct(.data$UniqueBreedingEvent, .data$observedSex, .keep_all = T) %>%
                       tidyr::pivot_wider(id_cols = UniqueBreedingEvent,
                                          values_from = individualID,
                                          names_from = observedSex) %>%
                       dplyr::rename(femaleID = "F",
                                     maleID = "M",
                                     unknown = "U"), #account for rare case of unknown sex (will be suppressed later)
                     by = c("broodID2" = "UniqueBreedingEvent")) %>%

    dplyr::arrange(.data$studyID, .data$Year, .data$plotID, .data$locationID) %>%

    ## Create broodID
    dplyr::mutate(broodID = paste(.data$locationID, 1:dplyr::n(), sep = "-")) %>%  ## Calculate optional variables

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
                                ~dplyr::case_when(nchar(.) == 7 ~ .,
                                                  TRUE ~ NA_character_))) %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(data_templates$v2.0$Brood_data)), tidyselect::everything())

  return(Brood_data_temp)


}


#' Create capture data table for Warsaw, Poland.
#'
#' @param chick_data, Data frame of chick ringing records from Warsaw, Poland.
#'
#' @param adult_data, Data frame of adult ringing records from Warsaw, Poland.
#'
#' @return A data frame.

create_capture_WRS <- function(chick_data, adult_data) {

  Capture_data_temp <- dplyr::bind_rows(adult_data, chick_data) %>%


    ## Create new columns
    dplyr::mutate(captureSiteID = .data$siteID,
                  releaseSiteID = .data$siteID,
                  capturePlotID = .data$plotID,
                  releasePlotID = .data$plotID,
                  captureLocationID = .data$locationID,
                  releaseLocationID = .data$locationID,
                  captureYear = .data$Year,
                  captureMonth = suppressWarnings(as.integer(lubridate::month(.data$captureDate))),
                  captureDay = suppressWarnings(as.integer(lubridate::day(.data$captureDate))),
                  capturePhysical = TRUE,
                  releaseTagID = individualID) %>%
    ## Set improperly formatted IDs to NA
    dplyr::mutate(individualID = dplyr::case_when(nchar(.data$individualID) == 7 ~ .data$individualID,
                                                  TRUE ~ NA_character_)) %>%
    ## Remove individuals with no ID
    dplyr::filter(!is.na(.data$individualID)) %>%

    ## Create captureTagID and captureID
    ## arrange by Date
    dplyr::arrange(.data$Year, .data$individualID, .data$captureDate) %>%
    dplyr::group_by(.data$individualID) %>%
    ## captureTagID is "NA" the first time the individual is captured
    dplyr::mutate(captureTagID = dplyr::case_when(1:dplyr::n() == "1" ~ NA_character_,
                                                  TRUE ~ individualID),
                  captureID = paste(.data$individualID, 1:dplyr::n(), sep = "_")) %>%
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


#' Create individual table for Warsaw, Poland.
#'
#' @param Capture_data_temp Capture data output from Warsaw, Poland
#'
#' @param Brood_data_temp Brood data output from Warsaw, Poland
#'
#' @return A data frame.

create_individual_WRS <- function(Capture_data_temp, Brood_data_temp){

  ## Create individual data
  Individual_data_temp <- Capture_data_temp %>%

    #### Format and create new data columns
    dplyr::mutate(siteID = .data$captureSiteID,
                  tagYear = dplyr::if_else(is.na(.data$captureTagID), .data$captureYear, NA_integer_),
                  tagMonth = dplyr::if_else(is.na(.data$captureTagID), .data$captureMonth, NA_integer_),
                  tagDay = dplyr::if_else(is.na(.data$captureTagID), .data$captureDay, NA_integer_),
                  tagStage = dplyr::if_else(is.na(.data$captureTagID), .data$observedAge, NA_character_),
                  tagSiteID = .data$captureSiteID,
                  geneticSex = NA_character_) %>%

    ## Arrange
    dplyr::arrange(.data$individualID, .data$captureDate) %>%

    ## Control speciesID
    dplyr::group_by(individualID) %>%
    dplyr::mutate(speciesID = purrr::map_chr(.x = list(unique(stats::na.omit(.data$speciesID))),
                                             .f = ~{
                                               if(length(..1) == 0){
                                                 return(NA_character_)
                                               } else if(length(..1) == 1){
                                                 return(..1)
                                               } else {
                                                 return("CCCCCC")
                                               }
                                             }))  %>%

    ## Join Brood data for Individuals banded as chicks
    dplyr::mutate(brood_record = dplyr::case_when(.data$tagStage == "chick" &
                                                    .data$tagYear == .data$captureYear &
                                                    !is.na(.data$captureLocationID) ~ "yes",
                                                  TRUE ~ NA_character_)) %>%

    ## Only join BroodID to chick records
    dplyr::left_join(Brood_data_temp %>%
                       dplyr::mutate(brood_record = "yes") %>%
                       dplyr::select(brood_record,
                                     broodID,
                                     broodID2),
                     by = c("brood_record", "UniqueBreedingEvent" = "broodID2")) %>%

    ## Add BroodID information
    ## Only one unique (non NA) BroodID per individual
    dplyr::group_by(.data$individualID) %>%
    dplyr::mutate(broodIDLaid = purrr::map_chr(.x = list(unique(stats::na.omit(.data$broodID))),
                                               .f = ~{
                                                 if(length(..1) != 1){
                                                   return(NA_character_)
                                                 } else if(length(..1) == 1){
                                                   return(..1)
                                                 }
                                               }),
                  broodIDFledged = .data$broodIDLaid) %>%

    ## Keep distinct records by studyID and individualID
    dplyr::distinct(.data$studyID, .data$individualID, .keep_all = TRUE) %>%

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


#' Create measurement data table for Warsaw, Poland.
#'
#' @param Capture_data_temp Data frame of adult and chick data from Warsaw, Poland.
#'
#' @return A data frame.

create_measurement_WRS <- function(Capture_data_temp) {

  ## Build measurement data based on capture data
  Measurement_data_temp <- Capture_data_temp %>%

    ## Format and create new data table
    dplyr::mutate(recordID = .data$captureID,
                  siteID = .data$captureSiteID,
                  measurementDeterminedYear = .data$captureYear,
                  measurementDeterminedMonth = .data$captureMonth,
                  measurementDeterminedDay = .data$captureDay,
                  measurementDeterminedTime = .data$captureTime) %>%

    ## Transform measurement columns into rows
    tidyr::pivot_longer(cols = c("Tarsus",
                                 "WingLength",
                                 "Mass"),
                        names_to = "measurementType",
                        values_to = "measurementValue",
                        values_drop_na = TRUE) %>%

    ## Create new variables
    dplyr::mutate(measurementSubject = "capture",
                  measurementAccuracy = NA_real_,
                  measurementUnit = dplyr::case_when(.data$measurementType == "Mass" ~ "g",
                                                     TRUE ~ "mm"),
                  measurementMethod = dplyr::case_when(.data$measurementType == "Tarsus" ~ "alternative",
                                                       .data$measurementType == "WingLength" ~ "flattened, maximum chord from ESF guidelines",
                                                       TRUE ~ NA_character_),
                  # Convert measurementType to lower case & space-separated
                  # (e.g., WingLength -> wing length)
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





#' Create location data table for Warsaw, Poland.
#'
#' @param nest_data Data frame of nest data from Warsaw, Poland.
#'
#' @return A data frame.

create_location_WRS <- function(nest_data) {

  ## Build location data based on nest data
  Location_data_temp <- nest_data %>%

    ## Need to first remove trailing 0s from Lat/Lon
    dplyr::mutate(dplyr::across(c("Latitude", "Longitude"), ~ sub("^0+", "", .))) %>%

    ## Summarize information for each nest box
    dplyr::group_by(.data$studyID, .data$siteID, .data$locationID) %>%
    dplyr::reframe(locationType = "nest",
                   locationDetails = paste("Nestbox", .data$NestType, sep = "_"),
                   startYear = min(.data$Year, na.rm = TRUE),
                   endYear = suppressWarnings(as.integer(2025)),

                   ## Keep lat/lon with the most digits for each box
                   decimalLatitude = as.numeric(.data$Latitude[which.max(nchar(.data$Latitude))]),
                   decimalLongitude = as.numeric(.data$Longitude[which.max(nchar(.data$Longitude))]),
                   #
                   ## Based on metadata provided within the primary data
                   ## PAL described as  "surburban village" (I used "J2" = "Low density buildings" from EUNIS)
                   ## KPN = forest (national park) (I used "G3" = "Coniferous woodland" from EUNIS and description of the park )
                   ## others classified as "J1" = "Buildings of cities, towns and villages"  from EUNIS habitat classification
                   habitatID = dplyr::case_when(.data$Site == "KPN" ~ "G3",
                                                .data$Site == "PAL" ~ "J2",
                                                TRUE ~ "J1")) %>%

    ## Keep distinct records
    dplyr::distinct(.data$studyID, .data$locationID, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(data_templates$v2.0$Location_data)), tidyselect::everything())


  return(Location_data_temp)

}

#' Create experiment data table for Warsaw, Poland.
#'
#' @param empty table. There is no experiment in the dataset
#'
#' @return A data frame.

# create_experiment_WRS <- function(Brood_data_temp) {
#
# Experiment_data_temp <- Brood_data_temp %>%
#   # Drop broods without treatmentID
#   dplyr::filter(!is.na(.data$treatmentID)) %>%
#   dplyr::select("treatmentID",
#                 treatmentStartYear = "observedLayYear",
#                 "studyID",
#                 "siteID") %>%
#
#   ## Reorder columns
#   dplyr::select(dplyr::any_of(names(data_templates$v2.0$Experiment_data)), tidyselect::everything())
#
# return(Experiment_data_temp)
#
# }
