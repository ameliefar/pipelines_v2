testthat::skip_if(!exists("data_path"))
pipeline_output <- format_STR(db = paste0(data_path, "/STR_Strasbourg_France"),
                              optional_variables = "all")


testthat::test_that("STR outputs all files...", {

  testthat::expect_true(all(c("WAN", "STR", "ROB") %in% pipeline_output$Brood_data$siteID))
  testthat::expect_true(all(c("WAN", "STR", "ROB") %in% pipeline_output$Capture_data$captureSiteID))
  testthat::expect_true(all(c("WAN", "STR", "ROB") %in% pipeline_output$Individual_data$siteID))
  testthat::expect_true(all(c("WAN", "STR", "ROB") %in% pipeline_output$Measurement_data$siteID))
  testthat::expect_true(all(c("WAN", "STR", "ROB") %in% pipeline_output$Location_data$siteID))
  testthat::expect_true(all(c("WAN", "STR", "ROB") %in% pipeline_output$Experiment_data$siteID))

})



testthat::test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for all possible outcomes of calculatedClutchType
  #Take a subset of only STR data
  STR_data <- dplyr::filter(pipeline_output$Brood_data, siteID %in% c("STR", "ROB", "WAN"))

  #Test 1: Brood where clutch type = first
  testthat::expect_equal(subset(STR_data, broodID == "2019-351")$speciesID, "PARMAJ")
  testthat::expect_equal(subset(STR_data, broodID == "2019-351")$calculatedClutchType, "first")
  testthat::expect_equal(subset(STR_data, broodID == "2019-351")$observedLayYear, 2019)
  testthat::expect_equal(subset(STR_data, broodID == "2019-351")$observedClutchSize, 10)
  testthat::expect_equal(subset(STR_data, broodID == "2019-351")$observedBroodSize, 10)
  testthat::expect_equal(subset(STR_data, broodID == "2019-351")$observedNumberFledged, 3)
  testthat::expect_equal(subset(STR_data, broodID == "2019-351")$breedingSeason, "2019")
  testthat::expect_equal(subset(STR_data, broodID == "2019-351")$nestAttemptNumber, 1)

  #Test 2: Brood where clutch type = replacement (because first is known to have failed)
  testthat::expect_equal(subset(STR_data, broodID == "2022-148")$speciesID, "PARMAJ")
  testthat::expect_equal(subset(STR_data, broodID == "2022-148")$calculatedClutchType, "replacement")
  testthat::expect_equal(subset(STR_data, broodID == "2022-148")$observedLayYear, 2022)
  testthat::expect_equal(subset(STR_data, broodID == "2022-148")$observedClutchSize, 10)
  testthat::expect_equal(subset(STR_data, broodID == "2022-148")$observedBroodSize, 10)
  testthat::expect_equal(subset(STR_data, broodID == "2022-148")$observedNumberFledged, 5)
  testthat::expect_equal(subset(STR_data, broodID == "2022-148")$breedingSeason, "2022")
  testthat::expect_equal(subset(STR_data, broodID == "2022-148")$nestAttemptNumber, 1)

  #Test 3: Brood where clutch type = replacement (past the cutoff)
  testthat::expect_equal(subset(STR_data, broodID == "2017-913")$speciesID, "PARMAJ")
  testthat::expect_equal(subset(STR_data, broodID == "2017-913")$calculatedClutchType, "replacement")
  testthat::expect_equal(subset(STR_data, broodID == "2017-913")$observedLayYear, 2017)
  testthat::expect_equal(subset(STR_data, broodID == "2017-913")$observedClutchSize, NA_integer_)
  testthat::expect_equal(subset(STR_data, broodID == "2017-913")$observedBroodSize, 7)
  testthat::expect_equal(subset(STR_data, broodID == "2017-913")$observedNumberFledged, 6)
  testthat::expect_equal(subset(STR_data, broodID == "2017-913")$breedingSeason, "2017")
  testthat::expect_equal(subset(STR_data, broodID == "2017-913")$nestAttemptNumber, 1)

  #Test 4: Brood where clutch type = second
  testthat::expect_equal(subset(STR_data, broodID == "2021-441")$speciesID, "PARMAJ")
  testthat::expect_equal(subset(STR_data, broodID == "2021-441")$calculatedClutchType, "second")
  testthat::expect_equal(subset(STR_data, broodID == "2021-441")$observedLayYear, 2021)
  testthat::expect_equal(subset(STR_data, broodID == "2021-441")$observedClutchSize, 6)
  testthat::expect_equal(subset(STR_data, broodID == "2021-441")$observedBroodSize, NA_integer_)
  testthat::expect_equal(subset(STR_data, broodID == "2021-441")$observedNumberFledged, NA_integer_)
  testthat::expect_equal(subset(STR_data, broodID == "2021-441")$breedingSeason, "2021")
  testthat::expect_equal(subset(STR_data, broodID == "2021-441")$nestAttemptNumber, 2)

  #Test 5: FemaleIDs only contain numbers (or V/O at the first character) and all are 6-8 characters long (until 2022, only chicks with V+6digits ring number, but males
  #and females breeders recruited can have one in 2023 onwards)
  testthat::expect_true(all(nchar(STR_data$femaleID[!is.na(STR_data$femaleID)]) %in% c(6,7,8) &
                    stringr::str_detect(STR_data$femaleID[!is.na(STR_data$femaleID)], "^(V|[0-9])+[:digit:]+$")))

  #Test 8: MaleIDs only contain numbers (or V/O at the first character) and all are 6-8 characters long
  testthat::expect_true(all(nchar(STR_data$maleID[!is.na(STR_data$maleID)]) %in% c(6,7,8)
                  & stringr::str_detect(STR_data$maleID[!is.na(STR_data$maleID)], "^(V|O|[0-9])+[:digit:]+$")))

})


testthat::test_that("Individual data returns an expected outcome...", {

  #We want to run a test for each sex for individuals caught as adults and chicks

  #Take a subset of only STR data
  STR_data <- dplyr::filter(pipeline_output$Individual_data, siteID %in% c("WAN", "STR", "ROB"))

  #Test 1: Male caught first as breeder (unidentified age)
  #Individual 7971223 should be listed as a male great tit
  testthat::expect_equal(subset(STR_data, individualID == "7971223")$calculatedSex, "M")
  testthat::expect_equal(subset(STR_data, individualID == "7971223")$speciesID, "PARMAJ")
  #They should have no broodIDLaid or Fledged because she was never caught as a chick
  testthat::expect_equal(subset(STR_data, individualID == "7971223")$broodIDLaid, NA_character_)
  testthat::expect_equal(subset(STR_data, individualID == "7971223")$broodIDFledged, NA_character_)
  #His tag year should be 2016 with a tagStage of 'NA' (age was not identified when first tagged)
  testthat::expect_equal(subset(STR_data, individualID == "7971223")$tagYear, 2016)
  testthat::expect_equal(subset(STR_data, individualID == "7971223")$tagStage, NA_character_)

  #Test 2: Female caught first as breeder (unidentified age)
  #Individual 7971215 should be listed as a female great tit
  testthat::expect_equal(subset(STR_data, individualID == "7971215")$calculatedSex, "F")
  testthat::expect_equal(subset(STR_data, individualID == "7971215")$speciesID, "PARMAJ")
  #She should have no broodIDLaid or Fledged because this individual was first caught as a breeder
  testthat::expect_equal(subset(STR_data, individualID == "7971215")$broodIDLaid, NA_character_)
  testthat::expect_equal(subset(STR_data, individualID == "7971215")$broodIDFledged, NA_character_)
  #Her tag year should be 2016 with a tagStage of 'NA' (unidentified age when captured)
  testthat::expect_equal(subset(STR_data, individualID == "7971215")$tagYear, 2016)
  testthat::expect_equal(subset(STR_data, individualID == "7971215")$tagStage, NA_character_)

  #Test 3: Caught as chick
  #Individual 7971346 should be listed as a [conflicted sex] great tit
  testthat::expect_equal(subset(STR_data, individualID == "7971346")$calculatedSex, "C")
  testthat::expect_equal(subset(STR_data, individualID == "7971346")$speciesID, "PARMAJ")
  #Check that broodIDLaid/Fledged are as expected
  #This individual was not cross-fostered, so they should be the same
  testthat::expect_equal(subset(STR_data, individualID == "7971346")$broodIDLaid, "2017-53")
  testthat::expect_equal(subset(STR_data, individualID == "7971346")$broodIDFledged, "2017-53")
  #tag Year should be 2017 with a tagstage of 'chick'
  testthat::expect_equal(subset(STR_data, individualID == "7971346")$tagYear, 2017)
  testthat::expect_equal(subset(STR_data, individualID == "7971346")$tagStage, "chick")

  #Test 4: individualIDs only contain numbers (or V as the first character) and all are 6-8 characters long
  testthat::expect_true(all(nchar(STR_data$individualID[!is.na(STR_data$individualID)]) %in% c(6,7,8) &
                    stringr::str_detect(STR_data$individualID[!is.na(STR_data$individualID)], "^(V|[0-9])+[:digit:]+$")))

})



testthat::test_that("Capture data returns an expected outcome...", {

  #We want to run tests for captures as both chicks, males, and females
  #Currently we have no chick data, so we can only test adults

  #Take a subset of only STR data
  STR_data <- dplyr::filter(pipeline_output$Capture_data, captureSiteID %in% c("STR", "WAN", "ROB"))

  #Test 1: Individual ringed as a chick
  #Test the male has the correct number of capture records
  testthat::expect_equal(nrow(subset(STR_data, individualID == "8211502")), 7)
  #Test that the first capture is as expected (2017-06-09)
  testthat::expect_equal(subset(STR_data, individualID == "8211502")$captureYear[1], 2017)
  testthat::expect_equal(subset(STR_data, individualID == "8211502")$captureMonth[1], 6)
  testthat::expect_equal(subset(STR_data, individualID == "8211502")$captureDay[1], 9)

  #Test that the 6th capture of the male is as expected (2022-05-06)
  testthat::expect_equal(subset(STR_data, individualID == "8211502")$captureYear[6], 2022)
  testthat::expect_equal(subset(STR_data, individualID == "8211502")$captureMonth[6], 5)
  testthat::expect_equal(subset(STR_data, individualID == "8211502")$captureDay[6], 6)

  #Test that exact and minimum age calculated are correct on first capture and last capture
  testthat::expect_equal(subset(STR_data, individualID == "8211502")$minimumAge[1], 0L)
  testthat::expect_equal(subset(STR_data, individualID == "8211502")$minimumAge[6], 4L)
  testthat::expect_equal(subset(STR_data, individualID == "8211502")$exactAge[1], 0L)
  testthat::expect_equal(subset(STR_data, individualID == "8211502")$exactAge[6], 4L)

  #Test 2: Female caught only as adult
  #Test it has the correct number of capture records
  testthat::expect_equal(nrow(subset(STR_data, individualID == "8211824")), 5)
  #Test that the first capture is as expected (2018-05-08)
  testthat::expect_equal(subset(STR_data, individualID == "8211824")$captureYear[1], 2018)
  testthat::expect_equal(subset(STR_data, individualID == "8211824")$captureMonth[1], 5)
  testthat::expect_equal(subset(STR_data, individualID == "8211824")$captureDay[1], 8)
  #Test that on first capture, captureTagID is NA and releaseTagID is 8811824
  testthat::expect_equal(subset(STR_data, individualID == "8211824")$captureTagID[1], NA_character_)
  testthat::expect_equal(subset(STR_data, individualID == "8211824")$releaseTagID[1], "8211824")
  #Test that the 5th capture is as expected (2021-06-11)
  testthat::expect_equal(subset(STR_data, individualID == "8211824")$captureYear[5], 2021)
  testthat::expect_equal(subset(STR_data, individualID == "8211824")$captureMonth[5], 6)
  testthat::expect_equal(subset(STR_data, individualID == "8211824")$captureDay[5], 11)
  #Test that on 5th capture, captureTagID and releaseTagID are 8211824
  testthat::expect_equal(subset(STR_data, individualID == "8211824")$captureTagID[5],"8211824")
  testthat::expect_equal(subset(STR_data, individualID == "8211824")$releaseTagID[5], "8211824")

  #Test that first and last minmum age calculated is as expected/test that first and last exact age is NA (not tagged as chick)
  testthat::expect_equal(subset(STR_data, individualID == "8211824")$minimumAge[1], 1L)
  testthat::expect_equal(subset(STR_data, individualID == "8211824")$minimumAge[5], 4L)
  testthat::expect_equal(subset(STR_data, individualID == "8211824")$exactAge[1], NA_integer_)
  testthat::expect_equal(subset(STR_data, individualID == "8211824")$exactAge[5], NA_integer_)

  #Test 3: Male caught only as adult
  #Test it has the correct number of capture records
  testthat::expect_equal(nrow(subset(STR_data, individualID == "7971525")), 6)
  #Test that the first capture date is as expected (2018-05-18)
  testthat::expect_equal(subset(STR_data, individualID == "7971525")$captureYear[1], 2018)
  testthat::expect_equal(subset(STR_data, individualID == "7971525")$captureMonth[1], 5)
  testthat::expect_equal(subset(STR_data, individualID == "7971525")$captureDay[1], 18)
  #Test that the first capture is not associated with a captureTagID but with a releaseTagID
  testthat::expect_equal(subset(STR_data, individualID == "7971525")$captureTagID[1], NA_character_)
  testthat::expect_equal(subset(STR_data, individualID == "7971525")$releaseTagID[1], "7971525")
  #Test that the 4th capture is as expected (2022-05-24)
  testthat::expect_equal(subset(STR_data, individualID == "7971525")$captureYear[4], 2022)
  testthat::expect_equal(subset(STR_data, individualID == "7971525")$captureMonth[4], 5)
  testthat::expect_equal(subset(STR_data, individualID == "7971525")$captureDay[4], 24)
  #Test that the last capture is associated with a captureTagID and a releaseTagID
  testthat::expect_equal(subset(STR_data, individualID == "7971525")$captureTagID[4], "7971525")
  testthat::expect_equal(subset(STR_data, individualID == "7971525")$releaseTagID[4], "7971525")

  #Test that first and last exact age  is set to NA and minimul age is as expected
  testthat::expect_equal(subset(STR_data, individualID == "7971525")$minimumAge[1], 1L)
  testthat::expect_equal(subset(STR_data, individualID == "7971525")$minimumAge[4], 5L)
  testthat::expect_equal(subset(STR_data, individualID == "7971525")$exactAge[4], NA_integer_)
  testthat::expect_equal(subset(STR_data, individualID == "7971525")$exactAge[4], NA_integer_)


  #Test 4: individualIDs are all properly formatted
  testthat::expect_true(all(nchar(STR_data$individualID) %in% c(6,7,8) & stringr::str_detect(STR_data$individualID, "^(V|[0-9])+[:digit:]+$")))

})


testthat::test_that("Location_data returns an expected outcome...", {

  #We want to run tests for nest boxes (there are no mistnets)

  #Take a subset of only STR data
  STR_data <- dplyr::filter(pipeline_output$Location_data, siteID %in% c("STR", "WAN", "ROB"))

  #Test 1: Nestbox check
  #Location listed as a nest box that has lat/long from separate file
  #Record has expected LocationType
  testthat::expect_true(subset(STR_data, locationID == "CITA_69_NB_1")$locationType == "nest")
  #habitatID as expected
  testthat::expect_true(subset(STR_data, locationID == "CITA_69_NB_1")$habitatID == "J1")
  #Expect Start and EndYear is as expected
  testthat::expect_equal(subset(STR_data, locationID == "CITA_69_NB_1")$startYear, 2018L)
  testthat::expect_equal(subset(STR_data, locationID == "CITA_69_NB_1")$endYear, NA_integer_)
  #Check that LocationID is in the expected siteID
  testthat::expect_equal(subset(STR_data, locationID == "CITA_69_NB_1")$siteID, "STR")
  #Check that latitude and longitude are as expected
  testthat::expect_equal(round(subset(STR_data, locationID == "CITA_69_NB_1")$decimalLatitude, 2) %>% setNames(nm = NULL), 48.58)
  testthat::expect_equal(round(subset(STR_data, locationID == "CITA_69_NB_1")$decimalLongitude, 2) %>% setNames(nm = NULL), 7.78)

  #Test 2: Nestbox check
  #Location with no lat/long info
  #Record has expected LocationType
  testthat::expect_true(subset(STR_data, locationID == "OBSE_11a_NB_1")$locationType == "nest")
  #habitatID is set to J1 (zone corresponds to "downtown", so urban habitat)
  testthat::expect_equal(subset(STR_data, locationID == "OBSE_11a_NB_1")$habitatID, "J1")
  #Expect Start and End Year set respectively to 2014 and 2020
  testthat::expect_equal(subset(STR_data, locationID == "OBSE_11a_NB_1")$startYear, 2014L)
  testthat::expect_equal(subset(STR_data, locationID == "OBSE_11a_NB_1")$endYear, 2020L)
  #Check that LocationID is in the expected siteID
  testthat::expect_equal(subset(STR_data, locationID == "OBSE_11a_NB_1")$siteID, "STR")
  #Check that latitude and longitude are as expected
  testthat::expect_equal(subset(STR_data, locationID == "OBSE_11a_NB_1")$decimalLatitude %>% setNames(nm = NULL), NA_real_)
  testthat::expect_equal(subset(STR_data, locationID == "OBSE_11a_NB_1")$decimalLongitude %>% setNames(nm = NULL), NA_real_)

  #Test 3: Mistnet check (location outside main study area)
  #LocationType is as expected
  testthat::expect_true(subset(STR_data, locationID == "WAN_WANT_MN")$locationType == "capture")
  #LocationDetails is as expected (specifiy "Mist net")
  testthat::expect_true(subset(STR_data, locationID == "WAN_WANT_MN")$locationDetails == "mist-net")
  #habitatID should correspond to forest habitat (G1) since "want" site corresponds to a forest site (within WAN siteID)
  testthat::expect_equal(subset(STR_data, locationID == "WAN_WANT_MN")$habitatID, "G1")
  #Expect Start and EndYear is as expected
  testthat::expect_equal(subset(STR_data, locationID == "WAN_WANT_MN")$startYear, 2019L)
  testthat::expect_equal(subset(STR_data, locationID == "WAN_WANT_MN")$endYear, 2019L)
  #Check that LocationID is in the expected siteID
  testthat::expect_equal(subset(STR_data, locationID == "WAN_WANT_MN")$siteID, "WAN")

  #Test 4: Nestbox check: case when nest box type was changed
  #LocationType is as expected for both occurrences
  testthat::expect_true(subset(STR_data, locationID == "ESPL_21_NB_1")$locationType == "nest")
  testthat::expect_true(subset(STR_data, locationID == "ESPL_21_NB_2")$locationType == "nest")
  #LocationDetails is as expected for both occurrences
  testthat::expect_true(subset(STR_data, locationID == "ESPL_21_NB_1")$locationDetails == "Wooden nesting box")
  testthat::expect_equal(subset(STR_data, locationID == "ESPL_21_NB_2")$locationDetails, NA_character_)
  #habitatID as expected for both occurrences
  testthat::expect_equal(subset(STR_data, locationID == "ESPL_21_NB_1")$habitatID, "J1")
  testthat::expect_equal(subset(STR_data, locationID == "ESPL_21_NB_2")$habitatID, "J1")
  #Expect Start and EndYear is as expected for both occurrences
  testthat::expect_equal(subset(STR_data, locationID == "ESPL_21_NB_1")$startYear, 2014L)
  testthat::expect_equal(subset(STR_data, locationID == "ESPL_21_NB_1")$endYear, 2019L)
  testthat::expect_equal(subset(STR_data, locationID == "ESPL_21_NB_2")$startYear, 2021L)
  testthat::expect_equal(subset(STR_data, locationID == "ESPL_21_NB_2")$endYear, NA_integer_)
  #Check that LocationID is in the expected siteID
  testthat::expect_equal(subset(STR_data, locationID == "ESPL_21_NB_1")$siteID, "STR")
  testthat::expect_equal(subset(STR_data, locationID == "ESPL_21_NB_2")$siteID, "STR")
  #Check that latitude and longitude are as expected
  testthat::expect_equal(round(subset(STR_data, locationID == "ESPL_21_NB_1")$decimalLatitude, 2) %>% setNames(nm = NULL), 48.58)
  testthat::expect_equal(round(subset(STR_data, locationID == "ESPL_21_NB_1")$decimalLongitude, 2) %>% setNames(nm = NULL), 7.77)
  testthat::expect_equal(round(subset(STR_data, locationID == "ESPL_21_NB_2")$decimalLatitude, 2) %>% setNames(nm = NULL), 48.58)
  testthat::expect_equal(round(subset(STR_data, locationID == "ESPL_21_NB_2")$decimalLongitude, 2) %>% setNames(nm = NULL), 7.77)

  #Test 5: Nestbox check: case when there was a gap in monitoring the nest box
  #LocationType is as expected for both occurrences
  testthat::expect_true(subset(STR_data, locationID == "CITA_32_NB_1")$locationType == "nest")
  testthat::expect_true(subset(STR_data, locationID == "CITA_32_NB_2")$locationType == "nest")
  #LocationDetails is as expected for both occurrences
  testthat::expect_true(subset(STR_data, locationID == "CITA_32_NB_1")$locationDetails == "Schwegler nesting box")
  testthat::expect_true(subset(STR_data, locationID == "CITA_32_NB_2")$locationDetails == "Schwegler nesting box")
  #habitatID as expected for both occurrences
  testthat::expect_equal(subset(STR_data, locationID == "CITA_32_NB_1")$habitatID, "J1")
  testthat::expect_equal(subset(STR_data, locationID == "CITA_32_NB_2")$habitatID, "J1")
  #Expect Start and EndYear is as expected for both occurrences
  testthat::expect_equal(subset(STR_data, locationID == "CITA_32_NB_1")$startYear, 2017L)
  testthat::expect_equal(subset(STR_data, locationID == "CITA_32_NB_1")$endYear, 2018L)
  testthat::expect_equal(subset(STR_data, locationID == "CITA_32_NB_2")$startYear, 2022L)
  testthat::expect_equal(subset(STR_data, locationID == "CITA_32_NB_2")$endYear, NA_integer_)
  #Check that LocationID is in the expected siteID
  testthat::expect_equal(subset(STR_data, locationID == "CITA_32_NB_1")$siteID, "STR")
  testthat::expect_equal(subset(STR_data, locationID == "CITA_32_NB_2")$siteID, "STR")
  #Check that latitude and longitude are as expected
  testthat::expect_equal(round(subset(STR_data, locationID == "CITA_32_NB_1")$decimalLatitude, 2) %>% setNames(nm = NULL), 48.58)
  testthat::expect_equal(round(subset(STR_data, locationID == "CITA_32_NB_1")$decimalLongitude, 2) %>% setNames(nm = NULL), 7.77)
  testthat::expect_equal(round(subset(STR_data, locationID == "CITA_32_NB_2")$decimalLatitude, 2) %>% setNames(nm = NULL), 48.58)
  testthat::expect_equal(round(subset(STR_data, locationID == "CITA_32_NB_2")$decimalLongitude, 2) %>% setNames(nm = NULL), 7.77)

})

testthat::test_that("Measurement_data returns an expected outcome...", {
  #We want to run test for measurements
  #Take a subset of only STR data
  STR_data <- dplyr::filter(pipeline_output$Measurement_data, siteID %in% c("STR", "WAN", "ROB"))

  #Test 1: several measurements for individual 7971665
  #tarsus should be 19.30 mm measured with the alternative method recorded by obs_1
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementValue[1], 19.30)
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementUnit[1], "mm")
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementMethod[1], "alternative")
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$recordedBy[1], "obs_2")
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementType[1], "tarsus")
  #wing length should be 76.0 mm recorded by obs_1
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementValue[2], 76.0)
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementUnit[2], "mm")
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementMethod[2], "flattened, maximum chord from ESF guidelines")
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$recordedBy[2], "obs_2")
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementType[2], "wing length")

  #mass should be 16.40 g recorded by obs_1
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementValue[5], 16.4)
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementUnit[5], "g")
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementMethod[5], NA_character_)
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$recordedBy[5], "obs_2")
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementType[5], "mass")

  #head-beak length should be 30 mm recorded by obs_1
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementValue[3], 30)
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementUnit[3], "mm")
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementMethod[3], "length from the back of the head to the tip of the beak")
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$recordedBy[3], "obs_2")
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementType[3], "head beak length")

  #fat score should be 1 recorded by obs_1
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementValue[4], 1)
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementUnit[4], "no unit")
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementMethod[4], "fat score from ESF guidelines")
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$recordedBy[4], "obs_2")
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementType[4], "fat score")

  #handling docility should be 0 recorded by obs_1
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementValue[6], 0)
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementUnit[6], "no unit")
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementMethod[6], "behavioral score (0 to 3) of docility in hand")
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$recordedBy[6], "obs_2")
  testthat::expect_equal(subset(STR_data, recordID == "7971665_1")$measurementType[6], "handling docility")


  #Test 2: several measurements for individual (chick) V017121
  #tarsus should be 19.7 mm measured with the alternative method recorded by obs_9
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$measurementValue[1], 19.7)
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$measurementUnit[1], "mm")
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$measurementMethod[1], "alternative")
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$recordedBy[1], "obs_12")
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$measurementType[1], "tarsus")
  #wing length should be 49.0 mm recorded by obs_9
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$measurementValue[2], 49.0)
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$measurementUnit[2], "mm")
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$measurementMethod[2], "flattened, maximum chord from ESF guidelines")
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$recordedBy[2], "obs_12")
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$measurementType[2], "wing length")
  #mass should be 16.2g recorded by obs_9
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$measurementValue[5], 16.2)
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$measurementUnit[5], "g")
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$measurementMethod[5], NA_character_)
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$recordedBy[5], "obs_12")
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$measurementType[5], "mass")

  #head-beak length should be 25.5 mm recorded by obs_9
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$measurementValue[3], 25.5)
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$measurementUnit[3], "mm")
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$measurementMethod[3], "length from the back of the head to the tip of the beak")
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$recordedBy[3], "obs_12")
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$measurementType[3], "head beak length")

  #fat score should be 3.0 recorded by obs_9
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$measurementValue[4], 3.0)
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$measurementUnit[4], "no unit")
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$measurementMethod[4], "fat score from ESF guidelines")
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$recordedBy[4], "obs_12")
  testthat::expect_equal(subset(STR_data, recordID == "V017121_1")$measurementType[4], "fat score")

})

testthat::test_that("Experiment_data returns an expected outcome...", {
  #We want to run test for measurements
  #Take a subset of only STR data
  STR_data <- dplyr::filter(pipeline_output$Experiment_data, siteID %in% c("STR", "WAN", "ROB"))

  #Test 1: thermic stress experiment
  #In 2024,  a thermic stess experiment on nestlings
  testthat::expect_equal(subset(STR_data, treatmentID == "2024_ROB_br_5")$siteID, "ROB")
  testthat::expect_equal(subset(STR_data, treatmentID == "2024_ROB_br_5")$experimentID, "br_5")
  testthat::expect_equal(subset(STR_data, treatmentID == "2024_ROB_br_5")$experimentType, "thermic_stress")
  testthat::expect_equal(subset(STR_data, treatmentID == "2024_ROB_br_5")$treatmentStartYear, 2024)
  testthat::expect_equal(subset(STR_data, treatmentID == "2024_ROB_br_5")$treatmentEndYear, 2024)
  testthat::expect_equal(subset(STR_data, treatmentID == "2024_ROB_br_5")$treatmentStage, "nestlings")


})


## General tests (for pipelines formatted to standard protocol version 2.0)


testthat::test_that("Expected columns are present", {
  ## Will fail if not all the expected columns are present

  ## Brood data: Test that all columns are present
  test_col_present(pipeline_output, "Brood")

  ## Capture data: Test that all columns are present
  test_col_present(pipeline_output, "Capture")

  ## Individual data: Test that all columns are present
  test_col_present(pipeline_output, "Individual")

  ## Measurement data: Test that all columns are present
  test_col_present(pipeline_output, "Measurement")

  ## Location data: Test that all columns are present
  test_col_present(pipeline_output, "Location")

  ## Experiment data: Test that all columns are present
  test_col_present(pipeline_output, "Experiment")

}) #test passed


testthat::test_that("Key columns in each table do not have NAs", {

  ## Brood
  test_NA_columns(pipeline_output, "Brood")

  ## Capture
  test_NA_columns(pipeline_output, "Capture")

  ## Individual
  test_NA_columns(pipeline_output, "Individual")

  ## Measurement
  test_NA_columns(pipeline_output, "Measurement")

  ## Location
  test_NA_columns(pipeline_output, "Location")

  ## Experiment
  test_NA_columns(pipeline_output, "Experiment")

}) #test passed


testthat::test_that("Column classes are as expected", {

  ## Will fail if columns that are shared by the output and the templates have different classes.

  ## Brood data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Brood")

  ## Capture data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Capture")

  ## Individual data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Individual")

  ## Measurement data: Test that all column classes are expected
  test_col_present(pipeline_output, "Measurement")

  ## Location data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Location")

  ## Experiment data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Experiment")

}) #test passed

testthat::test_that("Key columns only contain unique values", {

  ## broodID has only unique values
  test_unique_values(pipeline_output, "broodID")

  ## captureID has only unique values
  test_unique_values(pipeline_output, "captureID")

  ## individualID has only unique values
  test_unique_values(pipeline_output, "individualID")

  ## measurementID has only unique values
  test_unique_values(pipeline_output, "measurementID")

  ## locationID has only unique values
  test_unique_values(pipeline_output, "locationID")

  ## treatmentID has only unique values
  test_unique_values(pipeline_output, "treatmentID")

}) #Test passed




testthat::test_that("Categorical columns do not have unexpected values", {

  ## Brood
  test_category_columns(pipeline_output, "Brood")

  ## Capture
  test_category_columns(pipeline_output, "Capture")

  ## Individual
  test_category_columns(pipeline_output, "Individual")

  ## Measurement
  test_category_columns(pipeline_output, "Measurement")

  ## Location
  test_category_columns(pipeline_output, "Location")

  ## Experiment
  test_category_columns(pipeline_output, "Experiment")

}) #Test passed

