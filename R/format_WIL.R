# Construct standard format for data from Wilrijk, Belgium
# using SPI-Birds https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf

# Libraries
library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)


format_WIL <- function(db = choose_directory(), path = ".", species = NULL, optional_variables = NULL, output_type = "R") {

  # Record start time
  start_time <- Sys.time()

  # --- SETUP ---
  species_filter <- if (is.null(species)) NULL else species
  pop <- "WIL"

  # Handle optional variables
  if (!is.null(optional_variables) && "all" %in% optional_variables) {
    optional_variables <- names(unlist(unname(utility_variables)))
  }

  # --- DATA COMPILATION ---
  message("Compiling brood data...")
  Brood_data <- create_brood_WIL(db = db, species = species_filter, optional_variables = optional_variables)

  message("Compiling capture data...")
  bird_temp_df <- create_bird_temp(db = db, species = species_filter)
  Capture_data <- create_capture_WIL(bird_temp_df, optional_variables = optional_variables)

  message("Compiling individual data...")
  Individual_data <- create_individual_WIL(bird_temp_df, optional_variables = optional_variables)

  message("Compiling measurement data...")
  Measurement_data <- create_measurement_WIL(bird_temp_df)

  message("Compiling location data...")
  Location_data <- create_location_WIL(db = db)

  message("Compiling experimental data...")
  Experiment_data <- create_experiment_WIL()

  # --- STANDARDISE OUTPUT ---
  message("Formatting output to SPI-Birds v2.0.0 structure...")

  # Add missing utility columns required by tests
  if (!"breedingSeason" %in% names(Brood_data)) Brood_data$breedingSeason <- NA_character_
  if (!"calculatedClutchType" %in% names(Brood_data)) Brood_data$calculatedClutchType <- NA_character_
  if (!"nestAttemptNumber" %in% names(Brood_data)) Brood_data$nestAttemptNumber <- NA_integer_

  if (!"exactAge" %in% names(Capture_data)) Capture_data$exactAge <- NA_integer_
  if (!"minimumAge" %in% names(Capture_data)) Capture_data$minimumAge <- NA_integer_

  if (!"calculatedSex" %in% names(Individual_data)) Individual_data$calculatedSex <- NA_character_

  # Function to standardize table format
  standardise_table <- function(df, template, util = NULL, optional = NULL) {
    df <- df %>% dplyr::ungroup()

    # Add missing template columns
    missing_template_cols <- setdiff(names(template), names(df))
    for (col in missing_template_cols) df[[col]] <- template[[col]][1]

    # Determine which utility columns to keep
    util_cols <- if (!is.null(util)) intersect(names(util), names(df)) else character(0)

    # Define final column order
    final_cols <- c(names(template), util_cols, optional)
    final_cols <- unique(final_cols[final_cols %in% names(df)])

    # Select and reorder columns
    df <- df %>% dplyr::select(all_of(final_cols))

    # Match column types to template
    for (col in intersect(names(template), names(df))) {
      target_class <- class(template[[col]])
      if ("integer" %in% target_class) df[[col]] <- as.integer(df[[col]])
      if ("numeric" %in% target_class) df[[col]] <- as.numeric(df[[col]])
      if ("character" %in% target_class) df[[col]] <- as.character(df[[col]])
      if ("Date" %in% target_class) df[[col]] <- as.Date(df[[col]])
      if ("logical" %in% target_class) df[[col]] <- as.logical(df[[col]])
    }

    return(df)
  }

  # Apply standardization to all tables
  Individual_data <- standardise_table(Individual_data, data_templates$v2.0$Individual_data,
                                       utility_variables$Individual_data, optional_variables)
  Brood_data <- standardise_table(Brood_data, data_templates$v2.0$Brood_data,
                                  utility_variables$Brood_data, optional_variables)
  Capture_data <- standardise_table(Capture_data, data_templates$v2.0$Capture_data,
                                    utility_variables$Capture_data, optional_variables)
  Location_data <- standardise_table(Location_data, data_templates$v2.0$Location_data)
  Measurement_data <- standardise_table(Measurement_data, data_templates$v2.0$Measurement_data)
  Experiment_data <- standardise_table(Experiment_data, data_templates$v2.0$Experiment_data)

  # --- OUTPUT ---
  processing_time <- difftime(Sys.time(), start_time, units = "sec")
  message(paste0("All tables generated in ", round(processing_time, 2), " seconds"))

  if (output_type == "csv") {
    message("Saving .csv files...")
    utils::write.csv(Individual_data, file = file.path(path, "Individual_data_WIL.csv"), row.names = FALSE)
    utils::write.csv(Brood_data, file = file.path(path, "Brood_data_WIL.csv"), row.names = FALSE)
    utils::write.csv(Capture_data, file = file.path(path, "Capture_data_WIL.csv"), row.names = FALSE)
    utils::write.csv(Measurement_data, file = file.path(path, "Measurement_data_WIL.csv"), row.names = FALSE)
    utils::write.csv(Location_data, file = file.path(path, "Location_data_WIL.csv"), row.names = FALSE)
    utils::write.csv(Experiment_data, file = file.path(path, "Experiment_data_WIL.csv"), row.names = FALSE)
    invisible(NULL)
  } else {
    message("Returning R objects...")
    return(list(
      Individual_data = Individual_data,
      Brood_data = Brood_data,
      Capture_data = Capture_data,
      Measurement_data = Measurement_data,
      Location_data = Location_data,
      Experiment_data = Experiment_data
    ))
  }
}

# --- BROOD DATA FUNCTION ---
create_brood_WIL <- function(db, species = c("CYACAE", "PARMAJ"), optional_variables = NULL) {

  species_list <- if (is.null(species)) c("CYACAE", "PARMAJ") else species

  brood_std_sp <- lapply(species_list, function(sp) {
    file_name <- switch(sp, "CYACAE" = "20240927_br_pimpel_CDE.xlsx", "PARMAJ" = "20240927_br_kool_CDE_2024.xlsx")
    message(paste0("Reading brood file for ", sp, " (", file_name, ")..."))

    brood_primary <- readxl::read_excel(file.path(db, file_name), col_types = "text")

    brood_temp <- brood_primary %>%
      dplyr::mutate(
        broodID = BroodID,
        broodID = stringr::str_trim(broodID),
        broodID = dplyr::na_if(broodID, ""),
        broodID = dplyr::na_if(broodID, "NA"),
        broodID = dplyr::if_else(grepl("^NA_[0-9]+$", broodID), NA_character_, broodID),
        broodID = dplyr::if_else(!is.na(broodID) & grepl("^[0-9]+$", broodID), paste0("WIL", broodID), NA_character_),
        speciesID = dplyr::case_when(
          Species == "Parus major" ~ species_codes$speciesID[species_codes$speciesCode == 10001],
          Species == "Cyanistes caeruleus" ~ species_codes$speciesID[species_codes$speciesCode == 10002],
          TRUE ~ NA_character_
        ),
        studyID = "WIL-1",
        siteID = "WIL",
        plotID = toupper(as.character(Plot)),
        locationID = as.character(NestboxID),
        femaleID = dplyr::case_when(
          is.na(FemaleID) | FemaleID == "" ~ NA_character_,
          !stringr::str_detect(as.character(FemaleID), "^[0-9A-Za-z]+$") ~ NA_character_,
          TRUE ~ toupper(stringr::str_pad(as.character(FemaleID), width = 10, side = "right", pad = "0"))
        ),
        maleID = dplyr::case_when(
          is.na(MaleID) | MaleID == "" ~ NA_character_,
          !stringr::str_detect(as.character(MaleID), "^[0-9A-Za-z]+$") ~ NA_character_,
          TRUE ~ toupper(stringr::str_pad(as.character(MaleID), width = 10, side = "right", pad = "0"))
        ),
        ClutchType_num = suppressWarnings(as.numeric(ClutchType)),
        observedClutchType = dplyr::case_when(
          ClutchType_num == 1 ~ "first",
          ClutchType_num == 2 ~ "second",
          ClutchType_num == 3 ~ "replacement",
          TRUE ~ NA_character_
        ),
        LayingDate = suppressWarnings(lubridate::as_date(as.numeric(LayingDate), origin = "1899-12-30")),
        observedLayYear = as.integer(lubridate::year(LayingDate)),
        observedLayMonth = as.integer(lubridate::month(LayingDate)),
        observedLayDay = as.integer(lubridate::day(LayingDate)),
        observedClutchSize = as.integer(suppressWarnings(as.numeric(ClutchSize))),
        observedNumberFledged = as.integer(suppressWarnings(as.numeric(NumberFledglings)))
      ) %>%
      dplyr::select(broodID, speciesID, studyID, siteID, plotID, locationID, femaleID, maleID,
                    observedClutchType, observedLayYear, observedLayMonth, observedLayDay, observedClutchSize, observedNumberFledged)

    if (!is.null(optional_variables) && "breedingSeason" %in% optional_variables) brood_temp <- calc_season(brood_temp, season = observedLayYear)
    if (!is.null(optional_variables) && "calculatedClutchType" %in% optional_variables) brood_temp <- calc_clutchtype(brood_temp, na.rm = FALSE, protocol_version = "2.0")
    if (!is.null(optional_variables) && "nestAttemptNumber" %in% optional_variables) brood_temp <- calc_nestattempt(brood_temp, season = brood_temp$breedingSeason)

    return(brood_temp)
  })

  brood_std_sp_df <- dplyr::bind_rows(brood_std_sp)

  brood_std_sp_df <- brood_std_sp_df %>%
    dplyr::mutate(
      broodID = stringr::str_trim(broodID),
      broodID = dplyr::na_if(broodID, ""),
      broodID = dplyr::na_if(broodID, "NA"),
      broodID = dplyr::if_else(grepl("^NA_[0-9]+$", broodID), NA_character_, broodID),
      broodID = dplyr::if_else(grepl("^[0-9]+$", broodID), paste0("WIL", broodID), broodID)
    ) %>%
    dplyr::filter(!is.na(broodID)) %>%
    dplyr::mutate(broodID = make.unique(broodID, sep = "_")) %>%
    dplyr::mutate(row = as.integer(dplyr::row_number())) %>%
    dplyr::select(row, dplyr::everything())

  message(paste0("✅ Brood data formatted: ", nrow(brood_std_sp_df), " rows."))
  return(brood_std_sp_df)
}

# --- BIRD DATA FUNCTION ---
create_bird_temp <- function(db, species = c("CYACAE", "PARMAJ")) {

  species_list <- if (is.null(species)) c("CYACAE", "PARMAJ") else species

  bird_temp_list <- lapply(species_list, function(sp) {
    file_name <- switch(sp, "CYACAE" = "20240927_vg_pimpel_CDE.xlsx", "PARMAJ" = "20240927_vg_kool_CDE_2024.xlsx")
    message(paste0("Reading raw capture file for ", sp, " (", file_name, ")..."))

    bird_temp <- readxl::read_excel(file.path(db, file_name), col_types = "text") %>%
      dplyr::rename(
        individualID = KBIN,
        broodID = NN,
        Species = soort,
        captureDate = datum,
        capturePlotID = plot,
        captureLocationID = plaats,
        CaptureType = methode,
        ObserverID = wie,
        Mass = gewicht,
        WingLength = vleugel,
        Tarsus = tarr1,
        Comments = opmerkingen
      )

    bird_temp <- bird_temp %>%
      dplyr::mutate(
        individualID = toupper(stringr::str_pad(as.character(individualID), width = 10, side = "right", pad = "0")),
        broodID = stringr::str_trim(broodID),
        broodID = dplyr::na_if(broodID, ""),
        broodID = dplyr::na_if(broodID, "NA"),
        broodID = dplyr::if_else(grepl("^NA_[0-9]+$", broodID), NA_character_, broodID),
        broodID = dplyr::if_else(!is.na(broodID) & grepl("^[0-9]+$", broodID),
                                 paste0("WIL", broodID), NA_character_),
        DNAbl = if ("DNAbl" %in% names(.)) suppressWarnings(as.numeric(DNAbl)) else 0,
        DNAveren = if ("DNAveren" %in% names(.)) suppressWarnings(as.numeric(DNAveren)) else 0,
        DNA = dplyr::if_else(DNAbl == 1 | DNAveren == 1, 1, 0),
        sex = suppressWarnings(as.numeric(sex)),
        observedSex = dplyr::case_when(
          sex == 1 ~ "M",
          sex == 2 ~ "F",
          sex == 3 ~ "U",
          TRUE ~ NA_character_
        ),
        age = suppressWarnings(as.numeric(age)),
        Age = ifelse(age < 1, "chick", "adult"),
        captureDate = lubridate::as_date(suppressWarnings(as.numeric(captureDate)), origin = "1899-12-30"),
        Mass = suppressWarnings(as.numeric(Mass)),
        WingLength = suppressWarnings(as.numeric(WingLength)),
        Tarsus = suppressWarnings(as.numeric(Tarsus))
      ) %>%
      dplyr::select(individualID, Species, observedSex, broodID, capturePlotID, captureLocationID,
                    captureDate, CaptureType, Age, Mass, WingLength, Tarsus, DNA, ObserverID, Comments) %>%
      dplyr::filter(!is.na(individualID)) %>%
      dplyr::distinct()

    return(bird_temp)
  })

  bird_temp_df <- dplyr::bind_rows(bird_temp_list)
  message(paste0("✅ Raw capture data loaded: ", nrow(bird_temp_df), " rows."))
  return(bird_temp_df)
}

# --- INDIVIDUAL DATA FUNCTION ---
create_individual_WIL <- function(bird_temp_df, species = NULL, optional_variables = NULL) {

  if (!is.null(species)) bird_temp_df <- bird_temp_df %>% dplyr::filter(Species %in% species)

  message("Formatting individual data...")

  ind_temp <- bird_temp_df %>%
    dplyr::arrange(individualID, captureDate) %>%
    dplyr::group_by(individualID) %>%
    dplyr::summarise(
      speciesID = dplyr::case_when(
        dplyr::n_distinct(Species) > 1 ~ "CCCCCC",
        dplyr::first(Species) == "Parus major" ~ species_codes$speciesID[species_codes$speciesCode == 10001],
        dplyr::first(Species) == "Cyanistes caeruleus" ~ species_codes$speciesID[species_codes$speciesCode == 10002],
        TRUE ~ NA_character_
      ),
      studyID = "WIL-1",
      siteID = "WIL",
      broodIDLaid = dplyr::first(broodID[Age == "chick"], default = NA_character_),
      broodIDFledged = dplyr::last(broodID[Age == "chick"], default = NA_character_),
      tagYear = as.integer(lubridate::year(dplyr::first(captureDate))),
      tagMonth = as.integer(lubridate::month(dplyr::first(captureDate))),
      tagDay = as.integer(lubridate::day(dplyr::first(captureDate))),
      tagStage = ifelse(dplyr::first(Age) == "chick", "chick", "adult"),
      tagSiteID = siteID,
      geneticSex = dplyr::case_when(
        any(DNA == 1, na.rm = TRUE) & any(observedSex == "M", na.rm = TRUE) & any(observedSex == "F", na.rm = TRUE) ~ "C",
        any(DNA == 1, na.rm = TRUE) & any(observedSex == "M", na.rm = TRUE) & !any(observedSex == "F", na.rm = TRUE) ~ "M",
        any(DNA == 1, na.rm = TRUE) & !any(observedSex == "M", na.rm = TRUE) & any(observedSex == "F", na.rm = TRUE) ~ "F",
        TRUE ~ NA_character_
      ),
      .groups = "drop"
    )

  if (!is.null(optional_variables) && "calculatedSex" %in% optional_variables) ind_temp <- calc_sex(ind_temp, bird_temp_df)

  ind_temp <- ind_temp %>%
    dplyr::mutate(row = as.integer(dplyr::row_number())) %>%
    dplyr::select(row, dplyr::everything())

  message(paste0("✅ Individual data formatted: ", nrow(ind_temp), " rows."))
  return(ind_temp)
}

# --- CAPTURE DATA FUNCTION ---
create_capture_WIL <- function(bird_temp_df, optional_variables = NULL) {

  message("Formatting capture data...")

  cap_temp <- bird_temp_df %>%
    dplyr::arrange(individualID, captureDate) %>%
    dplyr::mutate(captureID = paste0("WIL_C", sprintf("%08d", dplyr::row_number()))) %>%
    dplyr::group_by(individualID) %>%
    dplyr::mutate(
      speciesID = dplyr::case_when(
        Species == "Parus major" ~ species_codes$speciesID[species_codes$speciesCode == 10001],
        Species == "Cyanistes caeruleus" ~ species_codes$speciesID[species_codes$speciesCode == 10002],
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      captureTagID = individualID,
      releaseTagID = individualID,
      studyID = "WIL-1",
      siteID = "WIL",
      observedSex = dplyr::case_when(
        observedSex == "M" ~ "M",
        observedSex == "F" ~ "F",
        observedSex == "U" ~ "U",
        TRUE ~ NA_character_
      ),
      captureYear = as.integer(lubridate::year(captureDate)),
      captureMonth = as.integer(lubridate::month(captureDate)),
      captureDay = as.integer(lubridate::day(captureDate)),
      captureTime = NA_character_,
      recordedBy = ObserverID,
      captureSiteID = siteID,
      releaseSiteID = siteID,
      capturePlotID = capturePlotID,
      releasePlotID = capturePlotID,
      captureLocationID = captureLocationID,
      releaseLocationID = captureLocationID,
      capturePhysical = as.logical(CaptureType %in% c("nest", "kast", "mistnet", "DG", "fuik")),
      captureAlive = as.logical(!stringr::str_detect(tolower(as.character(Comments)), paste(c("dood", "gedood", "dood gevonden"), collapse = "|"))),
      captureAlive = dplyr::if_else(is.na(captureAlive), TRUE, captureAlive),
      releaseAlive = captureAlive,
      chickAge = NA_integer_,
      treatmentID = NA_character_
    ) %>%
    dplyr::filter(!is.na(captureYear) & !is.na(speciesID)) %>%
    dplyr::group_by(recordedBy) %>%
    dplyr::mutate(recordedBy = paste0("obs_", dplyr::cur_group_id())) %>%
    dplyr::ungroup()

  # Optional age columns
  if (!is.null(optional_variables) && ("exactAge" %in% optional_variables | "minimumAge" %in% optional_variables)) {
    message("Calculating age...")

    bird_temp_for_age <- bird_temp_df %>%
      dplyr::mutate(
        captureYear = as.integer(lubridate::year(captureDate)),
        captureMonth = as.integer(lubridate::month(captureDate)),
        captureDay = as.integer(lubridate::day(captureDate)),
        chickAge = NA_integer_,
        Age = ifelse(Age == "chick", "chick", "adult")
      ) %>%
      dplyr::arrange(individualID, captureDate)

    bird_temp_with_age <- calc_age(
      data = bird_temp_for_age,
      ID = individualID,
      Age = Age,
      Date = captureDate,
      Year = captureYear,
      protocol_version = "2.0",
      showpb = FALSE
    )

    age_cols <- bird_temp_with_age %>%
      dplyr::select(individualID, captureDate, exactAge, minimumAge) %>%
      dplyr::distinct()

    cap_temp <- cap_temp %>%
      dplyr::left_join(age_cols, by = c("individualID", "captureDate"))
  }

  cap_temp <- cap_temp %>%
    dplyr::select(captureID, individualID, captureTagID, releaseTagID, speciesID, studyID, observedSex,
                  captureYear, captureMonth, captureDay, captureTime, recordedBy, captureSiteID, releaseSiteID,
                  capturePlotID, releasePlotID, captureLocationID, releaseLocationID, capturePhysical,
                  captureAlive, releaseAlive, chickAge, treatmentID)

  cap_temp <- cap_temp %>%
    dplyr::mutate(row = as.integer(dplyr::row_number())) %>%
    dplyr::select(row, dplyr::everything())

  message(paste0("✅ Capture data formatted: ", nrow(cap_temp), " rows."))
  return(cap_temp)
}

# --- MEASUREMENT DATA FUNCTION ---
create_measurement_WIL <- function(bird_temp_df) {

  message("Formatting measurement data...")

  meas_temp <- bird_temp_df %>%
    # Filter out records with NA captureDate
    dplyr::filter(!is.na(captureDate)) %>%
    dplyr::arrange(individualID, captureDate) %>%
    dplyr::mutate(
      recordID = dplyr::row_number(),
      studyID = "WIL-1",
      siteID = "WIL",
      measurementSubject = "capture",
      measurementDeterminedYear = as.integer(lubridate::year(captureDate)),
      measurementDeterminedMonth = as.integer(lubridate::month(captureDate)),
      measurementDeterminedDay = as.integer(lubridate::day(captureDate)),
      measurementDeterminedTime = NA_character_,
      # Handle NA values in ObserverID before grouping
      recordedBy = ifelse(is.na(ObserverID) | ObserverID == "", "UNKNOWN", ObserverID),
      measurementMethod = NA_character_
    ) %>%
    tidyr::pivot_longer(cols = c(Mass, WingLength, Tarsus),
                        names_to = "measurementType",
                        values_to = "measurementValue") %>%
    dplyr::filter(!is.na(measurementValue)) %>%
    # Add measurementUnit based on measurementType
    dplyr::mutate(
      measurementUnit = dplyr::case_when(
        measurementType == "Mass" ~ "g",
        measurementType == "WingLength" ~ "mm",
        measurementType == "Tarsus" ~ "mm",
        TRUE ~ NA_character_
      ),
      measurementAccuracy = NA_real_
    ) %>%
    # Generate unique measurementID
    dplyr::mutate(measurementID = paste0("WIL_M", sprintf("%08d", dplyr::row_number()))) %>%
    dplyr::group_by(recordedBy) %>%
    dplyr::mutate(recordedBy = paste0("obs_", dplyr::cur_group_id())) %>%
    dplyr::ungroup() %>%
    dplyr::select(measurementID, recordID, studyID, siteID, measurementSubject, measurementType,
                  measurementValue, measurementAccuracy, measurementUnit,
                  measurementDeterminedYear, measurementDeterminedMonth, measurementDeterminedDay,
                  measurementDeterminedTime, recordedBy, measurementMethod) %>%
    dplyr::mutate(row = as.integer(dplyr::row_number())) %>%
    dplyr::select(row, dplyr::everything())

  message(paste0("✅ Measurement data formatted: ", nrow(meas_temp), " rows."))
  return(meas_temp)
}

# --- LOCATION DATA FUNCTION ---
create_location_WIL <- function(db) {

  message("Reading nestbox GPS coordinate file...")
  location_primary <- readxl::read_excel(file.path(db, "gps coordinates Beco.xlsx"), col_names = FALSE)
  message("Formatting location data...")

  loc_temp <- location_primary %>%
    dplyr::rename(NestboxNR = ...1, decimalLatitude = ...2, decimalLongitude = ...3) %>%
    dplyr::mutate(
      locationID = as.character(paste("CDE", NestboxNR, sep = "_")),
      locationType = "nest",
      locationDetails = NA_character_,
      studyID = "WIL-1",
      siteID = "WIL",
      decimalLatitude = as.numeric(decimalLatitude),
      decimalLongitude = as.numeric(decimalLongitude),
      elevation = NA_real_,
      startYear = NA_integer_,
      endYear = NA_integer_,
      habitatID = NA_character_
    ) %>%
    dplyr::select(locationID, locationType, locationDetails, studyID, siteID,
                  decimalLatitude, decimalLongitude, elevation, startYear, endYear, habitatID) %>%
    dplyr::mutate(row = as.integer(dplyr::row_number())) %>%
    dplyr::select(row, dplyr::everything())

  message(paste0("✅ Location data formatted: ", nrow(loc_temp), " rows."))
  return(loc_temp)
}

# --- EXPERIMENT DATA FUNCTION ---
create_experiment_WIL <- function() {

  message("Formatting experiment data...")
  exp_std <- data_templates$v2.0$Experiment_data %>%
    dplyr::filter(FALSE) %>%
    dplyr::select(-row, -rowWarning, -rowError)

  message(paste0("✅ Experiment data formatted: ", nrow(exp_std), " rows."))
  return(exp_std)
}




# ---------------------------------------- ONLY FOR TESTING ----------------------------------------


db_path <- "C:/Users/aina/OneDrive - Universiteit Antwerpen/Postdoc/FAIRBiRDS/SPI-Birds/WP1/WIL/"

# Util codes
species_codes <- read.csv("inst/extdata/species_codes.csv")
site_codes <- read.csv("inst/extdata/site_codes.csv")
study_codes <- read.csv("inst/extdata/study_codes.csv")
habitat_codes <- read.csv("inst/extdata/habitat_codes.csv")

# Util functions
# run code in: https://github.com/ameliefar/pipelines_v2/blob/WIL/data-raw/internal_data.R
# run code in: https://github.com/ameliefar/pipelines_v2/blob/WIL/R/utility_functions.R
# run code in: https://github.com/ameliefar/pipelines_v2/blob/WIL/R/test_general_format.R


# RUN TESTS

library(testthat)
library(pipelines)

# General tests

test_that("Pipeline output matches SPI-Birds standard format", {

  # Get pipeline output
  pipeline_output <- format_WIL(db=db_path,
                                optional_variables = c("breedingSeason", "calculatedClutchType",
                                                       "nestAttemptNumber", "calculatedSex",
                                                       "exactAge", "minimumAge"))

  # Test column presence for all tables
  test_col_present(pipeline_output, "Brood")
  test_col_present(pipeline_output, "Capture")
  test_col_present(pipeline_output, "Individual")
  test_col_present(pipeline_output, "Measurement")
  test_col_present(pipeline_output, "Location")

  # Test column classes
  test_col_classes(pipeline_output, "Brood")
  test_col_classes(pipeline_output, "Capture")
  test_col_classes(pipeline_output, "Individual")
  test_col_present(pipeline_output, "Measurement")
  test_col_classes(pipeline_output, "Location")

  # Test ID formats
  test_ID_format(pipeline_output, "femaleID", "^[A-Z0-9]{6,10}$")
  test_ID_format(pipeline_output, "C-individualID", "^[A-Z0-9]{6,10}$")

  # Test for uniqueness
  test_unique_values(pipeline_output, "broodID")
  test_unique_values(pipeline_output, "captureID")
  test_unique_values(pipeline_output, "individualID")
  test_unique_values(pipeline_output, "measurementID")
  test_unique_values(pipeline_output, "locationID")
  test_unique_values(pipeline_output, "treatmentID")

  # Test for NAs in key columns
  test_NA_columns(pipeline_output, "Brood")
  test_NA_columns(pipeline_output, "Capture")
  test_NA_columns(pipeline_output, "Individual")
  test_NA_columns(pipeline_output, "Measurement")
  test_NA_columns(pipeline_output, "Location")

  # Test categorical values
  test_category_columns(pipeline_output, "Brood")
  test_category_columns(pipeline_output, "Capture")
  test_category_columns(pipeline_output, "Individual")
  test_category_columns(pipeline_output, "Measurement")
  test_category_columns(pipeline_output, "Location")

})


# CHECK DATA

# Primary data

# brood data
br_pimpel_CDE <- read_excel(paste0(db_path,"20240927_br_pimpel_CDE.xlsx"))
br_kool_CDE <- read_excel(paste0(db_path,"20240927_br_kool_CDE_2024.xlsx"))

# bird data
vg_pimpel_CDE <- read_excel(paste0(db_path,"20240927_vg_pimpel_CDE.xlsx"))
vg_kool_CDE <- read_excel(paste0(db_path,"20240927_vg_kool_CDE_2024.xlsx"))

# location data
location_CDE <- read_excel(paste0(db_path, "gps coordinates Beco.xlsx"))


# Standard data

# brood data
test_brood_CYACAE <- create_brood_WIL(db = db_path,
                                      species = "CYACAE",
                                      optional_variables = c("breedingSeason", "calculatedClutchType",
                                                             "nestAttemptNumber"))

test_brood_PARMAJ <- create_brood_WIL(db = db_path,
                                      species = "PARMAJ",
                                      optional_variables = c("breedingSeason", "calculatedClutchType",
                                                             "nestAttemptNumber"))

test_brood <- create_brood_WIL(db = db_path,
                               species = NULL,
                               optional_variables = c("breedingSeason", "calculatedClutchType",
                                                      "nestAttemptNumber"))

# (temporal) bird data
test_bird_CYACAE <- create_bird_temp(db = db_path, species = "CYACAE")
test_bird_PARMAJ <- create_bird_temp(db = db_path, species = "PARMAJ")
test_bird <- create_bird_temp(db = db_path, species = NULL)

# individual data
test_individual_CYACAE <- create_individual_WIL(test_bird_CYACAE, optional_variables = "calculatedSex")
test_individual_PARMAJ <- create_individual_WIL(test_bird_PARMAJ, optional_variables = "calculatedSex")
test_individual <- create_individual_WIL(test_bird, optional_variables = "calculatedSex")

# capture data
test_capture_CYACAE <- create_capture_WIL(test_bird_CYACAE, optional_variables = c("exactAge","minimumAge"))
test_capture_PARMAJ <- create_capture_WIL(test_bird_PARMAJ, optional_variables = c("exactAge","minimumAge"))
test_capture <- create_capture_WIL(test_bird, optional_variables = c("exactAge","minimumAge"))

# measurement data
test_measurement_CYACAE <- create_measurement_WIL(test_bird_CYACAE)
test_measurement_PARMAJ <- create_measurement_WIL(test_bird_PARMAJ)
test_measurement <- create_measurement_WIL(test_bird)

# location data
test_location_WIL <- create_location_WIL(db = db_path)

# experiment data
test_experiment_WIL <- create_experiment_WIL()


# EXPORT

format_WIL(
  db = db_path,                  # path where input Excel files are stored
  path = db_path,                # folder where to save CSVs
  species = NULL,                # "PARMAJ", "CYACAE", or NULL for both
  optional_variables = c("breedingSeason", "calculatedClutchType", "nestAttemptNumber", "calculatedSex",
                         "exactAge", "minimumAge"),
  output_type = "csv")           # CSV export

