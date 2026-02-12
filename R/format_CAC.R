#'Construct standard format for data from Can Cata, Spain
#'
#'A pipeline to produce the standard format for the nest box population in Can Cata, Spain, administered by Juan Carlos Senar
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.pdf}{here}.
#'
#'\strong{CaptureDate}:
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_CAC <- function(db = choose_directory(),
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

  ## Use the specified pop filter
  if(is.null(pop)){

    pop_filter <- NULL


  } else {

    pop_filter <- pop

  }

  start_time <- Sys.time()

  ## Set options
  if(!is.null(optional_variables) & "all" %in% optional_variables) optional_variables <- names(unlist(unname(utility_variables)))

  ## Read in primary data
cac_data <- readxl::read_xlsx(path = paste0(db, "/CAC_PrimaryData.xlsx"), guess = 5000) %>%
  janitor::clean_names(case = "upper_camel") %>%
  janitor::remove_empty(which = "rows") %>%

  ## Reformat
  dplyr::mutate(Year = as.integer(.data$Year),
                studyID = "CAC-1",
                siteID = "CAC",
                plotID = "CAC",
                observedLayDate = suppressWarnings(janitor::excel_numeric_to_date(as.numeric(.data$LayingDate))),
                observedHatchDate = suppressWarnings(janitor::excel_numeric_to_date(as.numeric(.data$HatchingDate))),
                observedFledgeDate = suppressWarnings(janitor::excel_numeric_to_date(as.numeric(.data$FledglingDate))),

                ## TODO: Check on meaning of parentheses around numbers
                observedClutchSize = suppressWarnings(as.integer(.data$ClutchSize)),
                observedNumberFledged = as.integer(.data$NumberFledglings),

                ## TODO: Check on cross fostering
                trtID = dplyr::case_when(stringr::str_detect(.data$Crossfostering, "ous") & stringr::str_detect(.data$Crossfostering, "poll") ~ "trt_3",
                                                stringr::str_detect(.data$Crossfostering, "ous") ~ "trt_1",
                                                stringr::str_detect(.data$Crossfostering, "poll") ~ "trt_2",
                                                is.na(.data$Crossfostering) ~ NA_character_,
                                                TRUE ~ "trt_U"),

                ## TODO: Check on one species that is labeled PM+PC <== likely mixed-brood of Great tits and Blue tits
                speciesID = dplyr::case_when(stringr::str_squish(toupper(.data$Sp)) == "PM"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10001)],
                                      TRUE ~ NA_character_), #remove potential specific case (unfamiliar species, mixed-brood, errors)

                ## TODO: Check on brood classifications
                observedClucthType = dplyr::case_when(.data$Brood == "1" ~ "first",
                                         .data$Brood == "2" ~ "second",
                                         .data$Brood == "R" ~ "replacement"),

                ## TODO: Check if lowercase and uppercase are all the same nestboxes
                locationID = toupper(.data$NestBox)) %>%

  ## Rename
  dplyr::rename(femaleID = Female,
                maleID = Male) %>%


  ## Arrange
  dplyr::arrange(.data$Year,
                 .data$locationID,
                 as.Date(.data$observedLayDate, format = "%Y-%m-%d")) %>%

  ## Remove columns
  dplyr::select(-LayingDate,
                -HatchingDate,
                -FledglingDate,
                -PvcF,
                -PvcM,
                -ClutchSize,
                -NumberFledglings,
                -NestBox) %>%

  ## Create droodID based on siteID and row number
  dplyr::ungroup() %>%
  dplyr::mutate(broodID = dplyr::case_when(!is.na(.data$speciesID) ~ paste(.data$siteID, dplyr::row_number(), sep ="-")))



#### BROOD DATA
message("Compiling brood information...")
Brood_data_temp <- create_brood_CAC(cac_data,
                                    optional_variables = optional_variables)

#### CAPTURE DATA
message("Compiling capture information...")
Capture_data_temp <- create_capture_CAC(cac_data,
                                        optional_variables = optional_variables)

#### INDIVIDUAL DATA
message("Compiling individual information...")
Individual_data_temp <- create_individual_CAC(Capture_data_temp,
                                              optional_variables = optional_variables)

# #### MEASUREMENT DATA
# message("Compiling measurement information...")
# Measurement_data_temp <- create_measurement_CAC(cac_data) #new in v2.0

#### LOCATION DATA
message("Compiling location information...")
Location_data_temp <- create_location_CAC(Capture_data_temp)

#### EXPERIMENT DATA
message("Compiling experiment information...")
Experiment_data_temp <- create_experiment_CAC(Brood_data_temp) #new in v2.0


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



# ## Measurement data
# Measurement_data <- Measurement_data_temp %>%
#
#   ## Keep only necessary columns
#   dplyr::select(dplyr::contains(names(data_templates$v2.0$Measurement_data))) %>%
#
#   ## Add missing columns
#   dplyr::bind_cols(data_templates$v2.0$Measurement_data[0, !(names(data_templates$v2.0$Measurement_data) %in% names(.))] %>%
#                      tibble::add_row()) %>%
#
#   ## Remove any NAs from critical columns
#   dplyr::filter_at(vars(siteID),
#                    all_vars(!is.na(.))) %>%
#
#   # Add row ID
#   dplyr::mutate(row = 1:dplyr::n()) %>%
#
#   ## Reorder columns
#   dplyr::select(names(data_templates$v2.0$Measurement_data)) %>%
#   dplyr::ungroup()
#


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

  # Measurement_data <- Measurement_data %>%
  #   dplyr::filter(.data$siteID %in% pop_filter & !(is.na(.data$siteID)))

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

  utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_CAC.csv"), row.names = F)

  utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_CAC.csv"), row.names = F)

  utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_CAC.csv"), row.names = F)

  # utils::write.csv(x = Measurement_data, file = paste0(path, "\\Measurement_data_CAC.csv"), row.names = F)

  utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_CAC.csv"), row.names = F)

  utils::write.csv(x = Experiment_data, file = paste0(path, "\\Experiment_data_CAC.csv"), row.names = F)

  invisible(NULL)

}

if(output_type == "R"){

  message("Returning R objects...")

  return(list(Brood_data = Brood_data,
              Capture_data = Capture_data,
              Individual_data = Individual_data,
              # Measurement_data = Measurement_data,
              Location_data = Location_data,
              Experiment_data = Experiment_data))

}

}


#### --------------------------------------------------------------------------~
#### FUNCTIONS
#### --------------------------------------------------------------------------~


#' Create brood data table for great tits and blue tits in Can Cata, Spain.
#'
#' @param cac_data Data frame of primary data from Can Cata, Spain.
#'
#' @return A data frame.

create_brood_CAC <- function(cac_data,
                             species_filter,
                             optional_variables) {

  ## Get brood data from nest data
  Brood_data_temp <- cac_data %>%

    dplyr::mutate(broodID = paste(.data$Year, 1:dplyr::n(), sep = "-"),
                  observedLayYear = .data$Year,
                  observedLayMonth = as.integer(lubridate::month(.data$observedLayDate)),
                  observedLayDay = as.integer(lubridate::day(.data$observedLayDate)),
                  observedHatchYear = as.integer(lubridate::year(.data$observedHatchDate)),
                  observedHatchMonth = as.integer(lubridate::month(.data$observedHatchDate)),
                  observedHatchDay = as.integer(lubridate::day(.data$observedHatchDate)),
                  observedFledgeYear = as.integer(lubridate::year(.data$observedFledgeDate)),
                  observedFledgeMonth = as.integer(lubridate::month(.data$observedFledgeDate)),
                  observedFledgeDay = as.integer(lubridate::day(.data$observedFledgeDate)),
                  treatmentID = .data$trtID) %>%

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
    ## Reorder columns
    dplyr::select(dplyr::any_of(names(data_templates$v2.0$Brood_data)), tidyselect::everything())

  return(Brood_data_temp)

}

#' Create capture data table for great tits and blue tits in Can Cata, Spain.
#'
#' @param cac_data Data frame of primary data from Can Cata, Spain.
#'
#' @return A data frame.

create_capture_CAC <- function(cac_data,
                               species_filter,
                               optional_variables) {

  ## Create capture data from primary data
  ## TODO: Look into experimental groups
  ## TODO: Determine Capture times
  Capture_data_temp <- cac_data %>%

    ## Pivot longer to make a row for each individual
    tidyr::pivot_longer(cols = c("femaleID","maleID"),
                        names_to = "observedSex",
                        values_to = "individualID",
                        values_drop_na = TRUE) %>% ## Only keep records with band numbers
    ## Remove non-band numbers
    dplyr::filter(!grepl("Anella|anella|No|no|NO",.data$individualID)) %>%

    ## Recode sexes
    dplyr::mutate(observedSex = dplyr::case_when(grepl("female", .data$observedSex) ~ "F",
                                                 grepl("male", .data$observedSex) ~ "M"),

                  ## TODO: Check about age codes
                  observedAge = suppressWarnings(dplyr::case_when(.data$observedSex == "F" ~ dplyr::case_when(.data$AgeF == "A" ~ "adult",
                                                                                                                         .data$AgeF == "Y" ~ "subadult"),
                                                                             .data$observedSex == "M" ~ dplyr::case_when(.data$AgeM == "A" ~ "adult",
                                                                                                                         .data$AgeM == "Y" ~ "subadult"))),

                  ## TODO: Check if there are no chick banding
                  chickAge = NA_character_,
                  ## TODO: Change capture date approximation by true date from data custodian
                  approximateDate = dplyr::case_when(!is.na(.data$observedHatchDate) ~ .data$observedHatchDate + 10,
                                                     TRUE ~ .data$observedLayDate + 25), #attempt to approximate captureDate before getting the right answer

                  ## Create new columns
                  captureYear = .data$Year,
                  captureMonth = lubridate::month(.data$approximateDate),
                  captureDay = lubridate::day(.data$approximateDate),
                  releaseTagID = .data$individualID,
                  captureSiteID = .data$siteID,
                  releaseSiteID = .data$siteID,
                  capturePlotID = .data$plotID,
                  releasePlotID = .data$plotID,
                  captureLocationID = .data$locationID,
                  releaseLocationID = .data$locationID,
                  captureAlive = TRUE,
                  releaseAlive = TRUE) %>%

    ## Create captureID
    ## Arrange
    dplyr::arrange(.data$Year, .data$individualID, as.Date(.data$observedLayDate, format = "%Y-%m-%d")) %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::mutate(captureID = paste(.data$individualID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(captureTagID = dplyr::case_when(stringr::str_detect(.data$captureID, "_1") ~ NA_character_,
                                                  TRUE ~ .data$individualID)) %>%
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


#' Create individual table for great tits and blue tits in Can Cata, Spain.
#'
#' @param Capture_data_temp Capture data output from Can Cata, Spain
#'
#' @return A data frame.

create_individual_CAC <- function(Capture_data_temp,
                                  species_filter,
                                  optional_variables){

  Individual_data_temp <- Capture_data_temp %>%

    #### Format and create new data columns
    dplyr::group_by(.data$individualID) %>%

    dplyr::mutate(siteID = .data$captureSiteID,
                  tagYear = dplyr::if_else(is.na(.data$captureTagID), .data$captureYear, NA_integer_),
                  tagMonth = dplyr::if_else(is.na(.data$captureTagID), .data$captureMonth, NA_integer_),
                  tagDay = dplyr::if_else(is.na(.data$captureTagID), .data$captureDay, NA_integer_),
                  tagStage = dplyr::if_else(is.na(.data$captureTagID), .data$observedAge, NA_character_),
                  tagSiteID = .data$captureSiteID,
                  geneticSex = NA_character_) %>%

    ## Control speciesID
    dplyr::group_by(.data$individualID) %>%
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


#' Create location data table for great tits and blue tits in Can Cata, Spain.
#'
#' @param cac_data Data frame of primaru data from Can Cata, Spain.
#'
#' @return A data frame.

create_location_CAC <- function(cac_data) {

  ## TODO: Check on meaning of letters associated with nest boxes
  ## TODO: Check whether nest boxes have been removed
  Location_data_temp <- cac_data %>%
    dplyr::select(Year, studyID, siteID, locationID) %>%
    dplyr::filter(!is.na(.data$locationID)) %>%
    dplyr::mutate(locationID = factor(.data$locationID, levels = unique(stringr::str_sort(.data$locationID, numeric = T)))) %>%
    dplyr::arrange(.data$locationID) %>%

    ## Keep distinct records
    dplyr::distinct(.data$Year, .data$locationID, .keep_all = TRUE) %>%

    ## All records should be complete: remove any incomplete cases
    tidyr::drop_na() %>%

    ## Get additional information
    dplyr::group_by(.data$studyID, .data$locationID) %>%
    dplyr::mutate(startYear = min(.data$Year, na.rm = TRUE),
                  endYear = NA_integer_,
                  locationType = "nest",
                  locationDetails = "nesting box", #check nestbox type (I think it is wooden nesting box)
                  decimalLatitude = 45.27,
                  decimalLongitude = 2.8,
                  habitatID = "G4") %>%
    ## Keep distinct records
    dplyr::distinct(.data$siteID, .data$locationID, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(data_templates$v2.0$Location_data)), tidyselect::everything())

  return(Location_data_temp)

}


#' No measurement associated with Can Cata, Spain.
#' No measurement data table for Can Cata, Spain.


# create_measurement_CAC <- function(Capture_data_temp) {
#
#   ## Build measuremtent data based on capture data
#   Measurement_data_temp <- Capture_data_temp %>%
#
#     ## Format and create new data table
#     dplyr::mutate(recordID = .data$captureID,
#                   siteID = .data$captureSiteID,
#                   measurementDeterminedYear = .data$captureYear,
#                   measurementDeterminedMonth = .data$captureMonth,
#                   measurementDeterminedDay = .data$captureDay) %>%
#     ## Arrange
#     dplyr::arrange(.data$measurementDeterminedYear,
#                    .data$measurementDeterminedMonth,
#                    .data$measurementDeterminedDay) %>%
#
#     ## Create measurementID
#     dplyr::mutate(measurementID = 1:dplyr::n()) %>%
#
#
#     ## Reorder columns
#     dplyr::select(dplyr::any_of(names(data_templates$v2.0$Measurement_data)), tidyselect::everything())
#
#   return(Measurement_data_temp)
#
# }

#' Create experiment data table for Can Cata, Spain.
#'
#' @param Brood_data_temp Data frame of nest data from Can Cata, Spain.
#'
#' @return A data frame.

create_experiment_CAC <- function(Brood_data_temp) {

  Experiment_data_temp <- Brood_data_temp %>%

    # Drop broods without treatmentID
    dplyr::filter(!is.na(.data$treatmentID)) %>%
    dplyr::select(experimentID = "treatmentID",
                  treatmentStartYear = "observedLayYear",
                  treatmentEndYear = "observedLayYear",
                  "studyID",
                  "siteID") %>%

    # Create new columnes
    dplyr::mutate(experimentType = "cross-fostering",
                  treatmentDetails = dplyr::case_when(experimentID == "trt_1" ~ "eggs swapping among 2 or 3 nests",
                                                      experimentID == "trt_2" ~ "pulli swapping among 2 or 3 nests",
                                                      experimentID == "trt_3" ~ "eggs and pulli swapping among 2 or 3 nests",
                                                      TRUE ~ "no details provided, contact data custodian")) %>%
    dplyr::distinct(.data$experimentID, .data$treatmentStartYear, .keep_all = TRUE) %>%

    dplyr::group_by(.data$treatmentStartYear) %>%

    dplyr::mutate(treatmentID = paste(treatmentStartYear, 1:dplyr::n(), sep = "-")) %>%

    ## Keep distinct records
    dplyr::ungroup() %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(data_templates$v2.0$Experiment_data)), tidyselect::everything())

  return(Experiment_data_temp)

}
