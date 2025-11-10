#'Construct standard format for data from Warsaw, Poland
#'
#'A pipeline to produce the standard format for the nest box population in Warsaw, Poland, administered by Marta Szulkin.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.pdf}{here}.
#'
#'\strong{Species}: Only PARMAJ and CYACAE are entered in the Capture and Individual tables.
#'
#'\strong{IndvID}: IndvID codes of the form '19XX' (two numbers + XX) indicate a chick that died before fledging.
#' The first two numbers give the year in which the chick died. There is one adult band that also includes 'XX' so this should not
#' be used to filter out these records. The regular expression "^[:digit:]{2}XX" along with stringr::str_detect can be used to identify
#' and filter out these records.
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 6 .csv files or 6 data frames in the standard format.
#'@export

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
