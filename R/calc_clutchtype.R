#' Calculate clutch type (when laying date is in date format)
#'
#' Use info on laying date and fledge data to determine the clutch type of a
#' given brood
#'
#' @param data Data frame. Brood data to use for conversion
#' @param na.rm Logical. Should NA's be removed and treated as 0s. If FALSE, NAs
#'   are treated as true unknowns. If TRUE, NAs are treated as 0s.
#' @param protocol_version Character string. The protocol version of the SPI Birds
#' standard data being used to process the primary data. Either "1.0" (default) or "1.1".
#'
#' @return A character vector with either 'first', 'replacement', 'second', or
#'   NA
#' @export
#'
#' @examples
#' #Create fake dataset
#' set.seed(666)
#' dat <- tibble::tibble(PopID = "TEST", Species = "PARMAJ",
#' FemaleID = sample(LETTERS[1:7], size = 100, replace = TRUE),
#' NumberFledged = rpois(n = 100, lambda = 1),
#' #Create 100 fake broods
#' BreedingSeason = sample(c(seq(2000, 2012, 1)), size = 100, replace = TRUE),
#' LayDate = as.Date(paste(BreedingSeason,
#'                            sample(c(4, 5, 6), size = 100, replace = TRUE),
#'                            sample(seq(1, 31, 1), size = 100, replace = TRUE), sep = "-"),
#'                            format = "%Y-%m-%d"),
#' LayDate_observed = LayDate)
#' calc_clutchtype(data = dat, na.rm = FALSE)
calc_clutchtype <- function(data,
                            protocol_version = "1.0",
                            na.rm = FALSE) {

  ## Version 1.0
  if (protocol_version == "1.0"){
    cutoff_dat <- data %>%
      dplyr::group_by(.data$PopID, .data$BreedingSeason, .data$Species) %>%
      dplyr::mutate(cutoff = tryCatch(expr = min(.data$LayDate, na.rm = TRUE) + lubridate::days(30),
                                      warning = function(...) return(NA))) %>%
      # Determine brood type for each nest based on female ID
      dplyr::group_by(.data$BreedingSeason, .data$Species, .data$FemaleID)

    #Depending on whether NAs should be treated as 0s or NAs we have different paths
    if(na.rm == TRUE){

      clutchtype_calculated <- cutoff_dat %>%
        dplyr::mutate(total_fledge = calc_cumfledge(x = .data$NumberFledged, na.rm = TRUE),
                      row = 1:dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ClutchType_calculated = purrr::pmap_chr(.l = list(rows = .data$row,
                                                                        femID = .data$FemaleID,
                                                                        cutoff_date = .data$cutoff,
                                                                        nr_fledge_before = .data$total_fledge,
                                                                        LD = .data$LayDate),
                                                              .f = function(rows, femID, cutoff_date,
                                                                            nr_fledge_before,
                                                                            LD){

                                                                # clutchtype$tick()$print()

                                                                #Firstly, check if the nest has a LD
                                                                #If not, we cannot calculate BroodType

                                                                if(is.na(LD)){

                                                                  return(NA)

                                                                }

                                                                #Next, check if the female is banded
                                                                #If a female is unbanded we assume the nest can NEVER be secondary
                                                                #If she had had a successful clutch before she would have been caught and banded
                                                                #Therefore it can only be first or replacement (based on 30d rule)
                                                                if(is.na(femID)){

                                                                  if(LD > cutoff_date){

                                                                    return("replacement")

                                                                  } else {

                                                                    return("first")

                                                                  }

                                                                }

                                                                #If she is banded, then we need to apply all rules
                                                                #If it's the first nest recorded for this female in this year...
                                                                if(rows == 1){

                                                                  #If it doesn't meet the 30 day rule, then name it as replacement
                                                                  if(LD > cutoff_date){

                                                                    return("replacement")

                                                                  } else {

                                                                    #Otherwise, we assume it was the first clutch
                                                                    return("first")

                                                                  }

                                                                  #If it's NOT the first nest of the season for this female
                                                                } else {

                                                                  #If there have been no fledglings before this point..
                                                                  if(nr_fledge_before == 0){

                                                                    return("replacement")

                                                                  } else {

                                                                    #If there has been atleast one clutch
                                                                    #that previously produced fledgligns
                                                                    #then this nest is 'second'
                                                                    #N.B. This is the case even if one of the previous nests
                                                                    #was an NA. We just need to know if it's >0, not the exact number
                                                                    return("second")

                                                                  }

                                                                }

                                                              })) %>%
        dplyr::pull("ClutchType_calculated")

    } else {

      clutchtype_calculated <- cutoff_dat %>%
        dplyr::mutate(total_fledge = calc_cumfledge(x = .data$NumberFledged, na.rm = TRUE),
                      total_fledge_na = calc_cumfledge(x = .data$NumberFledged, na.rm = FALSE),
                      row = 1:dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ClutchType_calculated = purrr::pmap_chr(.l = list(rows = .data$row,
                                                                        femID = .data$FemaleID,
                                                                        cutoff_date = .data$cutoff,
                                                                        nr_fledge_before = .data$total_fledge,
                                                                        na_fledge_before = .data$total_fledge_na,
                                                                        LD = .data$LayDate),
                                                              .f = function(rows, femID, cutoff_date,
                                                                            nr_fledge_before, na_fledge_before,
                                                                            LD){

                                                                #Firstly, check if the nest has a LD
                                                                #If not, we cannot calculate BroodType

                                                                if(is.na(LD)){

                                                                  return(NA)

                                                                }

                                                                #Next, check if the female is banded
                                                                #If a female is unbanded we assume the nest can NEVER be secondary
                                                                #If she had had a successful clutch before she would have been caught and banded
                                                                #Therefore it can only be first or replacement (based on 30d rule)
                                                                if(is.na(femID)){

                                                                  if(LD > cutoff_date){

                                                                    return("replacement")

                                                                  } else {

                                                                    return("first")

                                                                  }

                                                                }

                                                                #If she is banded, then we need to apply all rules
                                                                #If it's the first nest recorded for this female in this year...
                                                                if(rows == 1){

                                                                  #If it doesn't meet the 30 day rule, then name it as replacement
                                                                  if(LD > cutoff_date){

                                                                    return("replacement")

                                                                  } else {

                                                                    #Otherwise, we assume it was the first clutch
                                                                    return("first")

                                                                  }

                                                                  #If it's NOT the first nest of the season for this female
                                                                } else {

                                                                  #If there have been no fledglings before this point..
                                                                  if(nr_fledge_before == 0){

                                                                    #If there was atleast one NA record before this one
                                                                    #then we don't know if number of fledged before is
                                                                    #0 or >0. Therefore, we have to say NA.
                                                                    if(na_fledge_before > 0){

                                                                      return(NA)

                                                                    } else {

                                                                      #Otherwise, we can be confident that
                                                                      #number of fledge before is 0
                                                                      #and it must be a replacement
                                                                      return("replacement")

                                                                    }

                                                                  } else {

                                                                    #If there has been atleast one clutch
                                                                    #that previously produced fledgligns
                                                                    #then this nest is 'second'
                                                                    #N.B. This is the case even if one of the previous nests
                                                                    #was an NA. We just need to know if it's >0, not the exact number
                                                                    return("second")

                                                                  }

                                                                }

                                                              })) %>%
        dplyr::pull("ClutchType_calculated")

    }

  }

  ## Version 1.1
  if (protocol_version == "1.1"){

    cutoff_dat <- data %>%
      dplyr::group_by(.data$PopID, .data$BreedingSeason, .data$Species) %>%
      dplyr::mutate(cutoff = tryCatch(expr = min(.data$LayDate_observed, na.rm = TRUE) + lubridate::days(30),
                                      warning = function(...) return(NA))) %>%
      # Determine brood type for each nest based on female ID
      dplyr::group_by(.data$BreedingSeason, .data$Species, .data$FemaleID)

    #Depending on whether NAs should be treated as 0s or NAs we have different paths
    if(na.rm == TRUE){

      clutchtype_calculated <- cutoff_dat %>%
        dplyr::mutate(total_fledge = calc_cumfledge(x = .data$NumberFledged_observed, na.rm = TRUE),
                      row = 1:dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ClutchType_calculated = purrr::pmap_chr(.l = list(rows = .data$row,
                                                                        femID = .data$FemaleID,
                                                                        cutoff_date = .data$cutoff,
                                                                        nr_fledge_before = .data$total_fledge,
                                                                        LD = .data$LayDate_observed),
                                                              .f = function(rows, femID, cutoff_date,
                                                                            nr_fledge_before,
                                                                            LD){

                                                                # clutchtype$tick()$print()

                                                                #Firstly, check if the nest has a LD
                                                                #If not, we cannot calculate BroodType

                                                                if(is.na(LD)){

                                                                  return(NA)

                                                                }

                                                                #Next, check if the female is banded
                                                                #If a female is unbanded we assume the nest can NEVER be secondary
                                                                #If she had had a successful clutch before she would have been caught and banded
                                                                #Therefore it can only be first or replacement (based on 30d rule)
                                                                if(is.na(femID)){

                                                                  if(LD > cutoff_date){

                                                                    return("replacement")

                                                                  } else {

                                                                    return("first")

                                                                  }

                                                                }

                                                                #If she is banded, then we need to apply all rules
                                                                #If it's the first nest recorded for this female in this year...
                                                                if(rows == 1){

                                                                  #If it doesn't meet the 30 day rule, then name it as replacement
                                                                  if(LD > cutoff_date){

                                                                    return("replacement")

                                                                  } else {

                                                                    #Otherwise, we assume it was the first clutch
                                                                    return("first")

                                                                  }

                                                                  #If it's NOT the first nest of the season for this female
                                                                } else {

                                                                  #If there have been no fledglings before this point..
                                                                  if(nr_fledge_before == 0){

                                                                    return("replacement")

                                                                  } else {

                                                                    #If there has been atleast one clutch
                                                                    #that previously produced fledgligns
                                                                    #then this nest is 'second'
                                                                    #N.B. This is the case even if one of the previous nests
                                                                    #was an NA. We just need to know if it's >0, not the exact number
                                                                    return("second")

                                                                  }

                                                                }

                                                              })) %>%
        dplyr::pull("ClutchType_calculated")

    } else {

      clutchtype_calculated <- cutoff_dat %>%
        dplyr::mutate(total_fledge = calc_cumfledge(x = .data$NumberFledged_observed, na.rm = TRUE),
                      total_fledge_na = calc_cumfledge(x = .data$NumberFledged_observed, na.rm = FALSE),
                      row = 1:dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ClutchType_calculated = purrr::pmap_chr(.l = list(rows = .data$row,
                                                                        femID = .data$FemaleID,
                                                                        cutoff_date = .data$cutoff,
                                                                        nr_fledge_before = .data$total_fledge,
                                                                        na_fledge_before = .data$total_fledge_na,
                                                                        LD = .data$LayDate_observed),
                                                              .f = function(rows, femID, cutoff_date,
                                                                            nr_fledge_before, na_fledge_before,
                                                                            LD){

                                                                #Firstly, check if the nest has a LD
                                                                #If not, we cannot calculate BroodType

                                                                if(is.na(LD)){

                                                                  return(NA)

                                                                }

                                                                #Next, check if the female is banded
                                                                #If a female is unbanded we assume the nest can NEVER be secondary
                                                                #If she had had a successful clutch before she would have been caught and banded
                                                                #Therefore it can only be first or replacement (based on 30d rule)
                                                                if(is.na(femID)){

                                                                  if(LD > cutoff_date){

                                                                    return("replacement")

                                                                  } else {

                                                                    return("first")

                                                                  }

                                                                }

                                                                #If she is banded, then we need to apply all rules
                                                                #If it's the first nest recorded for this female in this year...
                                                                if(rows == 1){

                                                                  #If it doesn't meet the 30 day rule, then name it as replacement
                                                                  if(LD > cutoff_date){

                                                                    return("replacement")

                                                                  } else {

                                                                    #Otherwise, we assume it was the first clutch
                                                                    return("first")

                                                                  }

                                                                  #If it's NOT the first nest of the season for this female
                                                                } else {

                                                                  #If there have been no fledglings before this point..
                                                                  if(nr_fledge_before == 0){

                                                                    #If there was atleast one NA record before this one
                                                                    #then we don't know if number of fledged before is
                                                                    #0 or >0. Therefore, we have to say NA.
                                                                    if(na_fledge_before > 0){

                                                                      return(NA)

                                                                    } else {

                                                                      #Otherwise, we can be confident that
                                                                      #number of fledge before is 0
                                                                      #and it must be a replacement
                                                                      return("replacement")

                                                                    }

                                                                  } else {

                                                                    #If there has been atleast one clutch
                                                                    #that previously produced fledgligns
                                                                    #then this nest is 'second'
                                                                    #N.B. This is the case even if one of the previous nests
                                                                    #was an NA. We just need to know if it's >0, not the exact number
                                                                    return("second")

                                                                  }

                                                                }

                                                              })) %>%
        dplyr::pull("ClutchType_calculated")

    }

  }

  return(clutchtype_calculated)

}
