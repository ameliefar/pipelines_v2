## Define templates for each of the standard data tables
## Protocol version 1.1.0
## Dummy values are entered for columns that cannot contain NAs

## Brood data template
brood_data_template <- tibble::tibble(BroodID = as.character(paste0("POP-", 1:7)), ## Missing data not allowed
                                      PopID = as.character("POP"), ## Missing data not allowed
                                      BreedingSeason = as.integer(2021), ## Missing data not allowed
                                      Species = as.character("SPECIES"), ## Missing data not allowed
                                      Plot = NA_character_,
                                      LocationID = NA_character_,
                                      FemaleID = NA_character_,
                                      MaleID = NA_character_,
                                      ClutchType_observed = c("first","second","replacement",NA,NA,NA,NA),
                                      ClutchType_calculated = c("first","second","replacement",NA,NA,NA,NA),
                                      LayDate_observed = as.Date(NA),
                                      LayDate_min = as.Date(NA),
                                      LayDate_max = as.Date(NA),
                                      ClutchSize_observed = NA_integer_,
                                      ClutchSize_min = NA_integer_,
                                      ClutchSize_max = NA_integer_,
                                      HatchDate_observed = as.Date(NA),
                                      HatchDate_min = as.Date(NA),
                                      HatchDate_max = as.Date(NA),
                                      BroodSize_observed = NA_integer_,
                                      BroodSize_min = NA_integer_,
                                      BroodSize_max = NA_integer_,
                                      FledgeDate_observed = as.Date(NA),
                                      FledgeDate_min = as.Date(NA),
                                      FledgeDate_max = as.Date(NA),
                                      NumberFledged_observed= NA_integer_,
                                      NumberFledged_min = NA_integer_,
                                      NumberFledged_max = NA_integer_,
                                      AvgEggMass = NA_real_,
                                      NumberEggs = NA_integer_,
                                      AvgChickMass = NA_real_,
                                      NumberChicksMass = NA_integer_,
                                      AvgTarsus = NA_real_,
                                      NumberChicksTarsus = NA_integer_,
                                      OriginalTarsusMethod = c("Alternative", "Standard", "Oxford", NA, NA, NA,NA),
                                      ExperimentID = c("PHENOLOGY","COHORT", "PARENTAGE", "SURVIVAL", "OTHER", "SURVIVAL; OTHER", NA))

## Save
save(brood_data_template, file = "./data/Brood_data_template.RData")

## Capture data template
capture_data_template <- tibble::tibble(CaptureID = as.character(paste0("POP-", 1:6)), ## Missing data not allowed
                                        IndvID = as.character("INDV00"), ## Missing data not allowed
                                        Species = as.character("SPECIES"), ## Missing data not allowed
                                        Sex_observed = NA_character_,
                                        BreedingSeason = as.integer(2021), ## Missing data not allowed
                                        CaptureDate = as.Date("2021-04-01"), ## Missing data not allowed
                                        CaptureTime = NA_character_,
                                        ObserverID = NA_character_,
                                        LocationID = NA_character_,
                                        CaptureAlive = NA,
                                        ReleaseAlive = NA,
                                        CapturePopID = as.character("POP"), ## Missing data not allowed
                                        CapturePlot = NA_character_,
                                        ReleasePopID = as.character(ifelse(ReleaseAlive == FALSE, NA_character_, CapturePopID)), ## Missing data only allowed if ReleaseAlive is False
                                        ReleasePlot = NA_character_, ## Missing data only allowed if ReleaseAlive is False
                                        Mass = NA_real_,
                                        Tarsus = NA_real_,
                                        OriginalTarsusMethod = NA_character_,
                                        WingLength = NA_real_,
                                        Age_observed = NA_integer_,
                                        Age_calculated = NA_integer_,
                                        ChickAge = NA_integer_,
                                        ExperimentID = c("PHENOLOGY","COHORT", "PARENTAGE", "SURVIVAL", "OTHER", NA))

## Save
save(capture_data_template, file = "./data/Capture_data_template.RData")


## Individual data template
individual_data_template <- tibble::tibble(IndvID = as.character(paste0("INDV0", 1:4)), ## Missing data not allowed
                                           Species = as.character("SPECIES"), ## Missing data not allowed
                                           PopID = as.character("POP"), ## Missing data not allowed
                                           BroodIDLaid = NA_character_,
                                           BroodIDFledged = NA_character_,
                                           RingSeason = as.integer(2021), ## Missing data not allowed
                                           RingAge = c("chick", "adult", NA, NA),
                                           Sex_calculated = c("M","F","C", NA),
                                           Sex_genetic = c("M","F","C", NA))

## Save
save(individual_data_template, file = "./data/Individual_data_template.RData")

## Location
location_data_template <- tibble::tibble(LocationID = as.character("NEST"), ## Missing data not allowed
                                         NestboxID = NA_character_, ## Missing data allowed  for some species
                                         LocationType = c("NB", "MN", NA, NA, NA), ## Missing data not allowed
                                         PopID = as.character("POP"), ## Missing data not allowed
                                         Latitude = NA_real_,
                                         Longitude = NA_real_,
                                         StartSeason = NA_integer_,
                                         EndSeason = NA_integer_,
                                         HabitatType = c("deciduous","evergreen","mixed", "urban", NA))

## Save
save(location_data_template, file = "./data/Location_data_template.RData")
