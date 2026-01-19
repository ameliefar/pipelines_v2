# Construct standard format for data from Wilrijk, Belgium
# using SPI-Birds https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf

# Libraries
library(readxl)
library(dplyr)
library(lubridate)
library(stringr)

format_WIL <- function(db = choose_directory(),
                       path = ".",
                       species = NULL,
                       optional_variables = NULL,
                       output_type = "R") {

  # Determine species and population codes for filtering
  species_filter <- if (is.null(species)) NULL else species
  pop <- "WIL"

  # Handle optional variables
  if (!is.null(optional_variables) && "all" %in% optional_variables)
    optional_variables <- names(unlist(unname(utility_variables)))

  # Record start time
  start_time <- Sys.time()

  # --- BROOD DATA ---
  message("Compiling brood data...")
  Brood_data <- create_brood_WIL(db = db,
                                 species = species_filter,
                                 optional_variables = optional_variables)

  # --- CAPTURE DATA ---
  message("Compiling capture data...")
  bird_temp_df <- create_bird_temp(db = db, species = species_filter)
  Capture_data <- create_capture_WIL(bird_temp_df,
                                     optional_variables = optional_variables)

  # --- INDIVIDUAL DATA ---
  message("Compiling individual data...")
  Individual_data <- create_individual_WIL(bird_temp_df,
                                           optional_variables = optional_variables)

  # --- MEASUREMENT DATA ---
  message("Compiling measurement data...")
  Measurement_data <- create_measurement_WIL(bird_temp_df)

  # --- LOCATION DATA ---
  message("Compiling location data...")
  Location_data <- create_location_WIL(db = db)

  # --- EXPERIMENTAL DATA ---
  message("Compiling experimental data...")
  Experiment_data <- create_experiment_WIL()

  # --- STANDARDISE OUTPUT ---
  message("Formatting output to SPI-Birds v2.0.0 structure...")

  # Utility function for consistent formatting
  add_template <- function(df, template, util = NULL, optional = NULL) {

    df <- df %>% dplyr::ungroup()

    # Get columns to add from template
    missing_cols <- setdiff(names(template), names(df))
    for(col in missing_cols) {
      df[[col]] <- template[[col]][1]
    }

    # Determine utility columns to keep
    util_cols <- if (!is.null(util)) intersect(names(util), names(df)) else character(0)

    # Get final column list and ensure all exist
    final_cols <- c(names(template), util_cols, optional)
    final_cols <- unique(final_cols[final_cols %in% names(df)])

    # Select and reorder columns
    df <- df %>% dplyr::select(all_of(final_cols))

    # Match column types
    for(col in intersect(names(template), names(df))){
      target_class <- class(template[[col]])
      if("integer" %in% target_class) df[[col]] <- as.integer(df[[col]])
      if("numeric" %in% target_class) df[[col]] <- as.numeric(df[[col]])
      if("character" %in% target_class) df[[col]] <- as.character(df[[col]])
      if("Date" %in% target_class) df[[col]] <- as.Date(df[[col]])
      if("logical" %in% target_class) df[[col]] <- as.logical(df[[col]])
    }

    return(df)
  }

  Individual_data <- add_template(Individual_data, data_templates$v2.0$Individual_data,
                                  utility_variables$Individual_data, optional_variables)
  Brood_data      <- add_template(Brood_data, data_templates$v2.0$Brood_data,
                                  utility_variables$Brood_data, optional_variables)
  Capture_data    <- add_template(Capture_data, data_templates$v2.0$Capture_data,
                                  utility_variables$Capture_data, optional_variables)
  Location_data   <- add_template(Location_data, data_templates$v2.0$Location_data)
  Measurement_data<- add_template(Measurement_data, data_templates$v2.0$Measurement_data)
  Experiment_data <- add_template(Experiment_data, data_templates$v2.0$Experiment_data)

  # --- OUTPUT ---
  time <- difftime(Sys.time(), start_time, units = "sec")
  message(paste0("All tables generated in ", round(time, 2), " seconds"))

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
    file_name <- switch(sp,
                        "CYACAE" = "20240927_br_pimpel_CDE.xlsx",
                        "PARMAJ" = "20240927_br_kool_CDE_2024.xlsx")

    message(paste0("Reading brood file for ", sp, " (", file_name, ")..."))

    brood_primary <- readxl::read_excel(file.path(db, file_name), col_types = "text")

    brood_temp <- brood_primary %>%
      dplyr::mutate(
        # Create unique broodID
        broodID = paste0("WIL", sprintf("%06d", as.numeric(BroodID))),
        plotID = toupper(as.character(Plot)),
        locationID = as.character(NestboxID),
        observedClutchSize = as.integer(suppressWarnings(as.numeric(ClutchSize))),
        observedNumberFledged = as.integer(suppressWarnings(as.numeric(NumberFledglings))),
        speciesID = dplyr::case_when(
          Species == "Parus major" ~ species_codes$speciesID[species_codes$speciesCode == 10001],
          Species == "Cyanistes caeruleus" ~ species_codes$speciesID[species_codes$speciesCode == 10002],
          TRUE ~ NA_character_
        ),
        studyID = "WIL-1",
        siteID = "WIL",
        # Fix femaleID and maleID to match regex pattern
        femaleID = dplyr::case_when(
          !is.na(FemaleID) & stringr::str_detect(as.character(FemaleID), "^[0-9A-Za-z]+$") ~
            toupper(stringr::str_pad(as.character(FemaleID), width = 10, side = "right", pad = "0")),
          TRUE ~ NA_character_
        ),
        maleID = dplyr::case_when(
          !is.na(MaleID) & stringr::str_detect(as.character(MaleID), "^[0-9A-Za-z]+$") ~
            toupper(stringr::str_pad(as.character(MaleID), width = 10, side = "right", pad = "0")),
          TRUE ~ NA_character_
        ),
        # Parse dates properly from text
        LayingDate = suppressWarnings(lubridate::as_date(as.numeric(LayingDate), origin = "1899-12-30")),
        observedLayYear = as.integer(lubridate::year(LayingDate)),
        observedLayMonth = as.integer(lubridate::month(LayingDate)),
        observedLayDay = as.integer(lubridate::day(LayingDate)),
        ClutchType_num = suppressWarnings(as.numeric(ClutchType)),
        observedClutchType = case_when(
          ClutchType_num == 1 ~ "first",
          ClutchType_num == 2 ~ "second",
          ClutchType_num == 3 ~ "replacement",
          TRUE ~ NA_character_
        )
      ) %>%
      dplyr::select(-ClutchType_num)

    # Optional variables
    if(!is.null(optional_variables) && "breedingSeason" %in% optional_variables) {
      brood_temp <- calc_season(brood_temp, season = observedLayYear)
    }
    if(!is.null(optional_variables) && "calculatedClutchType" %in% optional_variables) {
      brood_temp <- calc_clutchtype(brood_temp, na.rm = FALSE, protocol_version = "2.0")
    }
    if(!is.null(optional_variables) && "nestAttemptNumber" %in% optional_variables) {
      brood_temp <- calc_nestattempt(brood_temp, season = brood_temp$breedingSeason)
    }

    return(brood_temp)
  })

  # Combine all species
  brood_std_sp_df <- dplyr::bind_rows(brood_std_sp)

  # Ensure unique broodIDs across species
  brood_std_sp_df <- brood_std_sp_df %>%
    dplyr::group_by(broodID) %>%
    dplyr::mutate(
      broodID = if(dplyr::n() > 1) {
        paste0(broodID, "_", dplyr::row_number())
      } else {
        broodID
      }
    ) %>%
    dplyr::ungroup() %>%
    # Add row number as first column
    dplyr::mutate(row = as.integer(dplyr::row_number())) %>%
    dplyr::select(row, dplyr::everything())

  message(paste0("✅ Brood data formatted successfully: ", nrow(brood_std_sp_df), " rows, ", ncol(brood_std_sp_df), " columns."))
  return(brood_std_sp_df)
}

# --- INDIVIDUAL DATA ---
create_individual_WIL <- function(bird_temp_df, species = NULL, optional_variables = NULL) {

  if (!is.null(species)) {
    bird_temp_df <- bird_temp_df %>% dplyr::filter(Species %in% species)
  }

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

  # Optional variables
  if (!is.null(optional_variables) && "calculatedSex" %in% optional_variables) {
    ind_temp <- calc_sex(ind_temp, bird_temp_df)
  }

  # Add row number as first column
  ind_temp <- ind_temp %>%
    dplyr::mutate(row = as.integer(dplyr::row_number())) %>%
    dplyr::select(row, dplyr::everything())

  message(paste0("✅ Individual data formatted: ", nrow(ind_temp), " rows, ", ncol(ind_temp), " columns."))
  return(ind_temp)
}

# --- CAPTURE DATA ---
create_capture_WIL <- function(bird_temp_df, optional_variables = NULL) {

  message("Formatting capture data...")

  cap_temp <- bird_temp_df %>%
    dplyr::arrange(individualID, captureDate) %>%
    dplyr::mutate(
      captureID = paste0("WIL_C", sprintf("%08d", dplyr::row_number()))
    ) %>%
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
      capturePhysical = as.logical(CaptureType %in% c("nest","kast","mistnet","DG","fuik")),
      captureAlive = as.logical(!stringr::str_detect(tolower(as.character(Comments)),
                                                     paste(c("dood","gedood","dood gevonden"), collapse = "|"))),
      captureAlive = if_else(is.na(captureAlive), TRUE, captureAlive),
      releaseAlive = captureAlive,
      chickAge = NA_integer_,
      treatmentID = NA_character_
    ) %>%
    dplyr::filter(!is.na(captureYear)) %>%
    # Anonymise observers
    dplyr::group_by(recordedBy) %>%
    dplyr::mutate(recordedBy = paste0("obs_", dplyr::cur_group_id())) %>%
    dplyr::ungroup()

  # Optional columns
  if("exactAge" %in% optional_variables | "minimumAge" %in% optional_variables) {
    cap_temp <- calc_age(data = cap_temp,
                         Age = cap_temp$Age,
                         protocol_version = "2.0")
  }

  # Add row number as first column
  cap_temp <- cap_temp %>%
    dplyr::mutate(row = as.integer(dplyr::row_number())) %>%
    dplyr::select(row, dplyr::everything())

  message(paste0("✅ Capture data formatted: ", nrow(cap_temp), " rows, ", ncol(cap_temp), " columns."))
  return(cap_temp)
}

# --- MEASUREMENT DATA ---
create_measurement_WIL <- function(bird_temp_df) {

  message("Formatting measurement data...")

  meas_temp <- bird_temp_df %>%
    dplyr::arrange(individualID, captureDate) %>%
    dplyr::mutate(
      recordID = dplyr::row_number(),
      measurementID = paste0("WIL_M", sprintf("%08d", dplyr::row_number())),
      studyID = "WIL-1",
      siteID = "WIL",
      measurementSubject = "capture",
      measurementDeterminedYear = as.integer(lubridate::year(captureDate)),
      measurementDeterminedMonth = as.integer(lubridate::month(captureDate)),
      measurementDeterminedDay = as.integer(lubridate::day(captureDate)),
      measurementDeterminedTime = NA_character_,
      recordedBy = ObserverID,
      measurementMethod = NA_character_
    ) %>%
    tidyr::pivot_longer(
      cols = c(Mass, WingLength, Tarsus),
      names_to = "measurementType",
      values_to = "measurementValue"
    ) %>%
    dplyr::filter(!is.na(measurementValue)) %>%
    dplyr::mutate(measurementAccuracy = NA_real_) %>%
    # Anonymise observers
    dplyr::group_by(recordedBy) %>%
    dplyr::mutate(recordedBy = paste0("obs_", dplyr::cur_group_id())) %>%
    dplyr::ungroup() %>%
    # Add row number as first column
    dplyr::mutate(row = as.integer(dplyr::row_number())) %>%
    dplyr::select(row, dplyr::everything())

  message(paste0("✅ Measurement data formatted: ", nrow(meas_temp), " rows, ", ncol(meas_temp), " columns."))
  return(meas_temp)
}

# --- LOCATION DATA ---
create_location_WIL <- function(db) {

  message("Reading nestbox GPS coordinate file...")

  location_primary <- readxl::read_excel(file.path(db, "gps coordinates Beco.xlsx"), col_names = FALSE)

  message("Formatting location data...")

  loc_temp <- location_primary %>%
    dplyr::rename(
      NestboxNR = ...1,
      decimalLatitude = ...2,
      decimalLongitude = ...3
    ) %>%
    dplyr::mutate(
      locationID = as.character(paste("CDE",NestboxNR,sep="_")),
      studyID = "WIL-1",
      siteID = "WIL",
      locationType = "nest",
      locationDetails = NA_character_,
      elevation = NA_real_,
      startYear = NA_integer_,
      endYear = NA_integer_,
      habitatID = NA_character_,
      decimalLatitude = as.numeric(decimalLatitude),
      decimalLongitude = as.numeric(decimalLongitude)
    ) %>%
    # Add row number as first column
    dplyr::mutate(row = as.integer(dplyr::row_number())) %>%
    dplyr::select(row, dplyr::everything())

  message(paste0("✅ Location data formatted successfully: ", nrow(loc_temp), " rows, ", ncol(loc_temp), " columns."))
  return(loc_temp)
}

# --- EXPERIMENT DATA ---
create_experiment_WIL <- function() {

  message("Formatting experiment data...")

  # Create empty experiment data with correct structure
  exp_std <- data_templates$v2.0$Experiment_data %>%
    dplyr::filter(FALSE)

  message(paste0("✅ Experiment data formatted: ", nrow(exp_std), " rows (no experiments)."))

  return(exp_std)
}







# ONLY FOR TESTING #################################################################################


db_path <- "C:/Users/aina/OneDrive - Universiteit Antwerpen/Postdoc/FAIRBiRDS/SPI-Birds/WP1/WIL/"


# Util codes
species_codes <- read.csv("inst/extdata/species_codes.csv")
site_codes <- read.csv("inst/extdata/site_codes.csv")
study_codes <- read.csv("inst/extdata/study_codes.csv")
habitat_codes <- read.csv("inst/extdata/habitat_codes.csv")

# Util functions
# run code in: https://github.com/SPI-Birds/pipelines/blob/master/data-raw/internal_data.R
# run code in: https://github.com/SPI-Birds/pipelines/blob/master/R/calc_clutchtype.R -> not run it!
# run code in: https://github.com/ameliefar/pipelines_v2/blob/WRS/R/utility_functions.R
# test_general_format


# RUN TESTS

library(testthat)
library(pipelines)



test_that("Pipeline output matches SPI-Birds standard format", {

  # Get pipeline output
  pipeline_output <- format_WIL(db=db_path)

  # Test column presence for all tables
  test_col_present(pipeline_output, "Brood")
  test_col_present(pipeline_output, "Capture")
  test_col_present(pipeline_output, "Individual")

  # Test column classes
  test_col_classes(pipeline_output, "Brood")
  test_col_classes(pipeline_output, "Capture")

  # Test ID formats
  test_ID_format(pipeline_output, "femaleID", "^[A-Z0-9]{6,10}$")
  test_ID_format(pipeline_output, "C-individualID", "^[A-Z0-9]{6,10}$")

  # Test for uniqueness
  test_unique_values(pipeline_output, "broodID")
  test_unique_values(pipeline_output, "captureID")

  # Test for NAs in key columns
  test_NA_columns(pipeline_output, "Brood")
  test_NA_columns(pipeline_output, "Capture")

  # Test categorical values
  test_category_columns(pipeline_output, "Brood")
  test_category_columns(pipeline_output, "Capture")

})


# Check errors

errors <- test_col_present(pipeline_output, "Brood", verbose=TRUE) # NO ERRORS!
errors <- test_col_present(pipeline_output, "Capture", verbose=TRUE) # NO ERRORS!
errors <- test_col_present(pipeline_output, "Individual", verbose=TRUE) # NO ERRORS!

errors <- test_col_classes(pipeline_output, "Capture", verbose=TRUE) # NO ERRORS!
errors <- test_col_classes(pipeline_output, "Brood", verbose=TRUE) # NO ERRORS!

errors <- test_unique_values(pipeline_output, "broodID", verbose=TRUE) # NO ERRORS!
errors <- test_unique_values(pipeline_output, "captureID", verbose=TRUE) # NO ERRORS!
errors <- test_NA_columns(pipeline_output, "Brood", verbose=TRUE) # NO ERRORS!
errors <- test_NA_columns(pipeline_output, "Capture", verbose=TRUE) # NO ERRORS!
errors <- test_category_columns(pipeline_output, "Brood", verbose=TRUE) # NO ERRORS!
errors <- test_category_columns(pipeline_output, "Capture", verbose=TRUE) # NO ERRORS!
errors <- test_category_columns(pipeline_output, "Individual", verbose=TRUE) # NO ERRORS!






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
                                      optional_variables = c("breedingSeason", "calculatedClutchType", "nestAttemptNumber"))

test_brood_PARMAJ <- create_brood_WIL(db = db_path,
                                      species = "PARMAJ",
                                      optional_variables = c("breedingSeason", "calculatedClutchType", "nestAttemptNumber"))

test_brood <- create_brood_WIL(db = db_path,
                               species = NULL,
                               optional_variables = c("breedingSeason", "calculatedClutchType", "nestAttemptNumber"))


# (temporal) bird data
test_bird_CYACAE <- create_bird_temp(db = db_path,
                                     species = "CYACAE")

test_bird_PARMAJ <- create_bird_temp(db = db_path,
                                     species = "PARMAJ")

test_bird <- create_bird_temp(db = db_path,
                              species = NULL)

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
  optional_variables = c("breedingSeason", "calculatedClutchType", "nestAttemptNumber", "calculatedSex", "exactAge", "minimumAge"),
  output_type = "csv")            # this triggers CSV export

