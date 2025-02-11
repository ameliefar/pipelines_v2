#' Construct standard format for data from Moulis, France
#'
#' A pipeline to produce the standard format for the nest box population in Moulis, France, administered by Alexis Chaine.
#'
#' This section provides details on data management choices that are unique to this data.
#' For a general description of the standard format,
#' please see \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf}{here}.
#'
#'\strong{speciesID}: Primarily Great tits and Blue tits.
#'
#'\strong{individualID}: a character string of length 7 where the first character is either a number or the letter "V" and
#'the last six characters are all numbers. Starting 2025, there could also be a character string composed of 8 numbers.
#'
#'\strong{studyID}: one population "MOU-1"
#'
#'\strong{plotID}: six main sites which are divided into woodlot. After discussing with data custodian, plotID refers to woodlot
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

  }
