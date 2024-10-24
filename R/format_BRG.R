'Construct standard format for data from Bergen, Norway
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
#'@inheritParams pipeline_params
#'
#'@return Generates either 6 .csv files or 6 data frames in the standard format.
#'@export

format_BRG <- function(db = choose_directory(),
                       path = ".",
                       species = NULL,
                       pop = NULL,
                       output_type = 'R'){

  #Force choose_directory() if used
  force(db)

  start_time <- Sys.time()

  message("Importing primary data...")

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
  original_options <- options(dplyr.summarise.inform = FALSE)
  on.exit(options(original_options), add = TRUE, after = FALSE)


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
    dplyr::transmute(studyID = dplyr::case_when(.data$Site == "Langeskogen" ~ "LKG-1", #asked data custodian whether this was a new population or a new site
                                                TRUE ~ "BRG-1"),
                     siteID = dplyr::case_when(.data$Site == "Langeskogen" ~ "LKG",
                                               TRUE ~ "BRG"),
                     Year = as.integer(.data$Year),
                     speciesID = dplyr::case_when(.data$Species == "Kj\u00f8ttmeis"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10001)],
                                                  .data$Species == "Bl\u00e5meis"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10002)],
                                                  .data$Species == "Svarthvitfluesnapper"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10003)],
                                                  .data$Species == "Svartmeis"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10005)],
                                                  TRUE ~ NA_character_), #remove very specific case (unfamiliar species, mixed-brood instances, or error)
                     plotID = .data$Location,
                     locationID = paste(.data$Location, .data$Nestbox, "NB", sep = "_"),
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
    dplyr::transmute(studyID = dplyr::case_when(.data$Site == "Langeskogen" ~ "LKG-1", #asked data custodian whether this was a new population or a new site
                                                TRUE ~ "BRG-1"),
                     siteID = dplyr::case_when(.data$Site == "Langeskogen" ~ "LKG",
                                               TRUE ~ "BRG"),
                     Year = as.integer(.data$Year), #keep this to facilitate merging
                     speciesID = dplyr::case_when(.data$Species == "Kj\u00f8ttmeis"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10001)],
                                                  .data$Species == "Bl\u00e5meis"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10002)],
                                                  .data$Species == "Svarthvitfluesnapper"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10003)],
                                                  .data$Species == "Svartmeis"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10005)],
                                                  TRUE ~ NA_character_), #remove very specific case (unfamiliar species, mixed-brood instances, or error)
                     plotID = .data$Location,
                     locationID = paste(.data$Location, .data$Nestbox, sep = "_"),
                     captureDate = suppressWarnings(as.Date(paste(.data$Year, .data$Month, .data$Day, sep = "-"))), #keep this to sort data
                     captureYear = .data$Year,
                     captureMonth = as.integer(.data$Month),
                     captureDay = as.integer(.data$Day),
                     individualID = .data$Ring,
                     Age = tolower(.data$Age),
                     chickAge = as.integer(.data$ChickAge),
                     captureTime = gsub("--", ":00", .data$Time),
                     recordedBy = .data$ObservedId,
                     mass = round(suppressWarnings(as.numeric(.data$Weight)), 1),
                     tarsus = round(suppressWarnings(as.numeric(.data$Tarsus)), 2),
                     capturePhysical = TRUE) # chicks are always handled thus captured


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
    dplyr::transmute(studyID = dplyr::case_when(.data$Site == "Langeskogen" ~ "LKG-1", #asked data custodian whether this was a new population or a new site
                                                TRUE ~ "BRG-1"),
                     siteID = dplyr::case_when(.data$Site == "Langeskogen" ~ "LKG",
                                               TRUE ~ "BRG"),
                     Year = as.integer(.data$Year), #keep this to facilitate merging
                     speciesID = dplyr::case_when(.data$Species == "Kj\u00f8ttmeis"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10001)],
                                                  .data$Species == "Bl\u00e5meis"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10002)],
                                                  .data$Species == "Svarthvitfluesnapper"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10003)],
                                                  .data$Species == "Svartmeis"  ~ species_codes$speciesID[which(species_codes$speciesCode == 10005)],
                                                  TRUE ~ NA_character_), #anticipate instances of processing adults from other species
                     plotID = .data$Location,
                     locationID = paste(.data$locationID, .data$Nestbox, "NB", sep = "_"),
                     captureDate = suppressWarnings(as.Date(paste(.data$Year, .data$Month, .data$Day, sep = "-"))), #keep this to sort data
                     captureYear = .data$Year,
                     captureMonth = as.integer(.data$Month),
                     captureDay = as.integer(.data$Day),
                     individualID = .data$Ring,
                     TagID = dplyr::case_when(.data$Control == "new" ~ NA_character_,
                                              TRUE ~ .data$Ring),
                     observedSex = .data$Sex,
                     recordedBy = .data$observerId,
                     Age = dplyr::case_when(.data$ObsAge == "juv" ~ "subadult",
                                            .data$ObsAge == "ad" ~ "adult"),
                     captureTime = dplyr::case_when(stringr::str_detect(.data$Time, "--") ~ gsub("--", ":00", .data$Time), #account for cases when minutes are lacking
                                                    TRUE ~ paste0(substr(.data$Time,1,2), ":", substr(.data$Time,3,4))),
                     capturePhysical = dplyr::if_else(stringr::str_detect(.data$Comment, "ID from color"), FALSE, TRUE),
                     mass = round(suppressWarnings(as.numeric(.data$Weight)), 1),
                     wingLength = as.numeric(.data$WingLength),
                     tarsus = round(suppressWarnings(as.numeric(.data$Tarsus)), 2),
                     beak = round(suppressWarnings(as.numeric(.data$Beak)), 2)) #anticipate future column "beak" as discussed with data custodian

  #### BROOD DATA
  message("Compiling brood information...")
  Brood_data_temp <- create_brood_BRG(nest_data, chick_data, adult_data)

  #### CAPTURE DATA
  message("Compiling capture information...")
  Capture_data_temp <- create_capture_BRG(chick_data, adult_data, Brood_data_temp)

  #### INDIVIDUAL DATA
  message("Compiling individual information...")
  Individual_data_temp <- create_individual_BRG(Capture_data_temp)

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

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(data_templates$v2.0$Brood_data))) %>%

    ## Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Brood_data[0, !(names(data_templates$v2.0$Brood_data) %in% names(.))] %>%
                       tibble::add_row()) %>%

    ## Remove any NAs from critical columns
    dplyr::filter_at(vars(.data$BroodID,
                          .data$PopID,
                          .data$BreedingSeason,
                          .data$Species), dplyr::all_vars(!is.na(.))) %>%
    ## Add rowID
    dplyr::mutate(row = 1:dplyr::n()) %>%

    ## Reorder columns
    dplyr::select(names(data_templates$v2.0$Brood_data)) %>%
    dplyr::ungroup()



  ## Capture data
  Capture_data <- Capture_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(data_templates$v2.0$Capture_data))) %>%

    ## Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Capture_data[0, !(names(data_templates$v2.0$Capture_data) %in% names(.))] %>%
                       tibble::add_row()) %>%

     ## Remove any NAs from critical columns
    dplyr::filter_at(vars(.data$CaptureID,
                          .data$CapturePopID,
                          .data$BreedingSeason,
                          .data$IndvID,
                          .data$Species,
                          .data$CaptureDate), dplyr::all_vars(!is.na(.))) %>%

    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%

    ## Reorder columns
    dplyr::select(names(data_templates$v2.0$Capture_data)) %>%
    dplyr::ungroup()




  ## Individual data
  Individual_data <- Individual_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(data_templates$v2.0$Individual_data))) %>%

    ## Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Individual_data[0, !(names(data_templates$v2.0$Individual_data) %in% names(.))] %>%
                       tibble::add_row()) %>%

    ## Remove any NAs from critical columns
    dplyr::filter_at(vars(.data$PopID,
                          .data$IndvID,
                          .data$Species,
                          .data$RingSeason), dplyr::all_vars(!is.na(.))) %>%

    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%

    ## Reorder columns
    dplyr::select(names(data_templates$v2.0$Individual_data))  %>%
    dplyr::ungroup()




  ## Measurement data
  Measurement_data <- Measurement_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(data_templates$v2.0$Measurement_data))) %>%

    ## Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Measurement_data[0, !(names(data_templates$v2.0$Measurement_data) %in% names(.))] %>%
                       tibble::add_row()) %>%

    ## Remove any NAs from critical columns
    dplyr::filter_at(vars(.data$siteID),
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
    dplyr::filter_at(vars(.data$LocationID,
                          .data$PopID),
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
    dplyr::filter_at(vars(.data$siteID),
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

  ## Filter to keep only desired Pops if specified for Brood, Capture, Individual, Measurement and Location tables
  if(!is.null(pop_filter)){

    Brood_data <- Brood_data %>%
      dplyr::filter(.data$siteID %in% species_filter & !(is.na(.data$siteID)))

    Capture_data <- Capture_data %>%
      dplyr::filter(.data$captureSiteID %in% pop_filter & !(is.na(.data$captureSiteID)))

    Individual_data <- Individual_data %>%
      dplyr::filter(.data$siteID %in% pop_filter & !(is.na(.data$siteID)))

    Measurement_data <- Measurement_data %>%
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

create_brood_BRG <- function(nest_data, chick_data, adult_data) {

  ## Combine primary data to create brood data
  Brood_data_temp <- nest_data %>%

    ## Join summarized chick data
    dplyr::left_join(chick_data %>%
                       dplyr::filter(dplyr::between(.data$ChickAge, 14, 16)) %>%
                       dplyr::group_by(.data$BreedingSeason, .data$Plot, .data$LocationID) %>%
                       dplyr::summarise(AvgChickMass = mean(.data$Mass, na.rm = T),
                                        NumberChicksMass = sum(!is.na(.data$Mass))),
                     by = c("BreedingSeason", "Plot", "LocationID")) %>%

    ## Join adult data to get info on parents
    dplyr::left_join(adult_data %>%
                       dplyr::select(.data$BreedingSeason,
                                     .data$Plot,
                                     .data$LocationID,
                                     .data$IndvID,
                                     .data$Sex_observed) %>%
                       tidyr::pivot_wider(id_cols = c(.data$BreedingSeason,
                                                      .data$Plot,
                                                      .data$LocationID),
                                          values_from = .data$IndvID,
                                          names_from = .data$Sex_observed) %>%
                       dplyr::rename(FemaleID = "F",
                                     MaleID = "M"),
                     by = c("BreedingSeason", "Plot", "LocationID")) %>%

    dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$Plot, .data$LocationID) %>%

    ## Create BroodID
    dplyr::mutate(BroodID = paste(.data$BreedingSeason, 1:dplyr::n(), sep = "-")) %>%

    ## Calculate clutch type
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data =. , protocol_version = "1.1", na.rm = FALSE)) %>%

    ## Set improperly formatted IDs to NA
    dplyr::mutate(dplyr::across(c(.data$FemaleID,
                                  .data$MaleID),
                                ~dplyr::case_when(stringr::str_detect(., "^[:alpha:]{2}[:digit:]{5}$") ~ .,
                                                  TRUE ~ NA_character_))) %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(brood_data_template)), tidyselect::everything())

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

create_capture_BRG <- function(chick_data, adult_data, Brood_data_temp) {


  ## Combine primary data to create capture data
  Capture_data_temp <- adult_data %>%

    ## Bind chick data
    dplyr::bind_rows(chick_data %>%
                       dplyr::mutate(Age_observed = 1L) %>%
                       dplyr::left_join(Brood_data_temp %>%
                                          dplyr::select(.data$BreedingSeason,
                                                        .data$Plot,
                                                        .data$LocationID,
                                                        .data$BroodID),
                                        by = c("BreedingSeason", "Plot", "LocationID"))) %>%

    ## Create new columns
    dplyr::mutate(CapturePopID = .data$PopID,
                  ReleasePopID = .data$PopID,
                  CapturePlot  = .data$Plot,
                  ReleasePlot  = .data$Plot) %>%

    ## Arrange
    dplyr::arrange(.data$IndvID, .data$CaptureDate) %>%

    ## Calculate age
    dplyr::group_by(.data$IndvID) %>%
    calc_age(ID = .data$IndvID,
             Age = .data$Age_observed,
             Date = .data$CaptureDate,
             Year = .data$BreedingSeason) %>%

    ## Set improperly formatted IDs to NA and filter
    dplyr::mutate(IndvID = dplyr::case_when(stringr::str_detect(.data$IndvID,  "^[:alpha:]{2}[:digit:]{5}$") ~ .data$IndvID,
                                            TRUE ~ NA_character_)) %>%
    dplyr::filter(!is.na(.data$IndvID)) %>%

    ## Create CaptureID
    ## Arrange
    dplyr::arrange(.data$BreedingSeason, .data$IndvID, .data$CaptureDate) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, dplyr::row_number(), sep = "_")) %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(capture_data_template)), tidyselect::everything())



  return(Capture_data_temp)

}

#' Create individual table for Bergen, Norway.
#'
#' @param Capture_data_temp Capture data output from Bergen, Norway
#'
#' @return A data frame.

create_individual_BRG <- function(Capture_data_temp){

  ## Create individual data from capture data
  Individual_data_temp <- Capture_data_temp %>%

    #### Format and create new data columns
    dplyr::group_by(.data$IndvID, .data$CapturePopID) %>%
    dplyr::mutate(PopID = .data$CapturePopID) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(RingSeason = min(.data$BreedingSeason, na.rm = T)) %>%

    ## Arrange
    dplyr::arrange(.data$IndvID, .data$CaptureDate) %>%

    ## Determine individual info
    dplyr::mutate(Sex_calculated = purrr::map_chr(.x = list(unique(stats::na.omit(.data$Sex_observed))),
                                                  .f = ~{
                                                    if(length(..1) == 0){
                                                      return(NA_character_)
                                                    } else if(length(..1) == 1){
                                                      return(..1)
                                                    } else {
                                                      return("C")
                                                    }
                                                  }),
                  Sex_genetic = NA_character_,
                  Species = purrr::map_chr(.x = list(unique(stats::na.omit(.data$Species))),
                                           .f = ~{
                                             if(length(..1) == 0){
                                               return(NA_character_)
                                             } else if(length(..1) == 1){
                                               return(..1)
                                             } else {
                                               return("CCCCCC")
                                             }
                                           }),

                  RingAge = purrr::pmap_chr(.l = list(dplyr::first(.data$Age_observed)),
                                            .f = ~{
                                              if(is.na(..1)){
                                                return("adult")
                                              } else if(..1 <= 3L){
                                                return("chick")
                                              } else if(..1 > 3L){
                                                return("adult")
                                              }
                                            }))  %>%


    ## Add BroodID information
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(BroodIDLaid = purrr::map_chr(.x = list(unique(stats::na.omit(.data$BroodID))),
                                               .f = ~{
                                                 if(length(..1) != 1){
                                                   return(NA_character_)
                                                 } else if(length(..1) == 1){
                                                   return(..1)
                                                 }
                                               }),
                  BroodIDFledged = .data$BroodIDLaid) %>%

    ## Keep distinct records by PopID and InvdID
    dplyr::distinct(.data$PopID, .data$IndvID, .keep_all = TRUE) %>%

    ## Arrange
    dplyr::arrange(.data$CaptureID) %>%
    dplyr::ungroup() %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(individual_data_template)), tidyselect::everything())

  return(Individual_data_temp)

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
    dplyr::group_by(.data$PopID, .data$LocationID) %>%
    dplyr::mutate(NestboxID = .data$LocationID,
                  LocationType = "NB",
                  StartSeason = min(.data$BreedingSeason, na.rm = TRUE),
                  EndSeason = NA_integer_,
                  Latitude = 60.25,
                  Longitude = 5.26) %>%

    ## Keep distinct records
    dplyr::distinct(.data$PopID, .data$LocationID, .keep_all = TRUE) %>%
    dplyr::ungroup()

  return(Location_data_temp)

}
