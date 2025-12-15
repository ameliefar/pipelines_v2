testthat::skip_if(!exists("data_path"))

pipeline_output <- format_WRS(db,
                              optional_variables = "all")


testthat::test_that("WRS outputs all files...", {

  testthat::expect_true(all("WRS" %in% pipeline_output$Brood_data$siteID))
  testthat::expect_true(all("WRS" %in% pipeline_output$Capture_data$captureSiteID))
  testthat::expect_true(all("WRS" %in% pipeline_output$Individual_data$siteID))
  testthat::expect_true(all("WRS" %in% pipeline_output$Measurement_data$siteID))
  testthat::expect_true(all("WRS" %in% pipeline_output$Location_data$siteID))
  # testthat::expect_true(all("WRS" %in% pipeline_output$Experiment_data$siteID)) #no Experiment table

}) #test passed



## Tests relative to brood table

testthat::test_that("Brood_data returns an expected outcome...", {

  ## Take a subset of only WRS data
  WRS_data <- dplyr::filter(pipeline_output$Brood_data, siteID %in% c("WRS"))


  ## General brood data
  loc96_2017 <- subset(WRS_data,
                       observedLayYear == 2017 &
                         siteID == "WRS" &
                         locationID == "POL96")

  testthat::expect_equal(!is.na(loc96_2017$broodID), TRUE)
  testthat::expect_equal(loc96_2017$breedingSeason, "2017")
  testthat::expect_equal(loc96_2017$plotID, "WRS_POL")
  testthat::expect_equal(loc96_2017$speciesID, "CYACAE")
  testthat::expect_equal(loc96_2017$femaleID, "K7V6029")
  testthat::expect_equal(loc96_2017$maleID, "K7V3117")
  testthat::expect_equal(loc96_2017$observedLayMonth, 4)
  testthat::expect_equal(loc96_2017$observedLayDay, 8)
  testthat::expect_equal(loc96_2017$observedClutchSize, 13)
  testthat::expect_equal(loc96_2017$observedHatchMonth, 5)
  testthat::expect_equal(loc96_2017$observedHatchDay, 7)
  testthat::expect_equal(loc96_2017$observedBroodSize, 12)
  testthat::expect_equal(loc96_2017$observedNumberFledged, 5)
  testthat::expect_equal(loc96_2017$observedFledgeMonth, NA_integer_)
  testthat::expect_equal(loc96_2017$observedFledgeDay, NA_integer_)
  testthat::expect_equal(loc96_2017$calculatedClutchType, "first")
  testthat::expect_equal(loc96_2017$nestAttemptNumber, 1)


  # Test that brood calculated as "replacement" is correct
  mur4_2019 <- subset(WRS_data,
                    observedLayYear == 2019 &
                      siteID == "WRS" &
                      locationID == "MUR4")

  testthat::expect_equal(!is.na(mur4_2019$broodID), TRUE)
  testthat::expect_equal(mur4_2019$breedingSeason, "2019")
  testthat::expect_equal(mur4_2019$plotID, "WRS_MUR")
  testthat::expect_equal(mur4_2019$speciesID, "PARMAJ")
  testthat::expect_equal(mur4_2019$femaleID, "K4Z3227")
  testthat::expect_equal(mur4_2019$maleID, NA_character_)
  testthat::expect_equal(mur4_2019$observedLayMonth, 5)
  testthat::expect_equal(mur4_2019$observedLayDay, 27)
  testthat::expect_equal(mur4_2019$observedClutchSize, 6)
  testthat::expect_equal(mur4_2019$observedHatchMonth, 6)
  testthat::expect_equal(mur4_2019$observedHatchDay, 13)
  testthat::expect_equal(mur4_2019$observedBroodSize, 5)
  testthat::expect_equal(mur4_2019$observedNumberFledged, 4)
  testthat::expect_equal(mur4_2019$observedFledgeMonth, NA_integer_)
  testthat::expect_equal(mur4_2019$observedFledgeDay, NA_integer_)
  testthat::expect_equal(mur4_2019$calculatedClutchType, "replacement") #defined as "replacement" because late in the season
  testthat::expect_equal(mur4_2019$nestAttemptNumber, 1) #set as one because only clutch reported at that nestbox that year with no associated female ID

  # Test that brood calculated as "second" is correct
  kpn38_2017 <- subset(WRS_data,
                    observedLayYear == 2017 &
                      siteID == "WRS" &
                      locationID == "KPN38" &
                      broodID == "KPN38-150")

  testthat::expect_equal(!is.na(kpn38_2017$broodID), TRUE)
  testthat::expect_equal(kpn38_2017$breedingSeason, "2017")
  testthat::expect_equal(kpn38_2017$plotID, "WRS_KPN")
  testthat::expect_equal(kpn38_2017$speciesID, "PARMAJ")
  testthat::expect_equal(kpn38_2017$femaleID, "K7V4224")
  testthat::expect_equal(kpn38_2017$maleID, "K7V4839")
  testthat::expect_equal(kpn38_2017$observedLayMonth, 5)
  testthat::expect_equal(kpn38_2017$observedLayDay, 26)
  testthat::expect_equal(kpn38_2017$observedClutchSize, 9)
  testthat::expect_equal(kpn38_2017$observedHatchMonth, 6)
  testthat::expect_equal(kpn38_2017$observedHatchDay, 13)
  testthat::expect_equal(kpn38_2017$observedBroodSize, 9)
  testthat::expect_equal(kpn38_2017$observedNumberFledged, 9)
  testthat::expect_equal(kpn38_2017$observedFledgeMonth, NA_integer_)
  testthat::expect_equal(kpn38_2017$observedFledgeDay, NA_integer_)
  testthat::expect_equal(kpn38_2017$calculatedClutchType, "second")
  testthat::expect_equal(kpn38_2017$nestAttemptNumber, 1) #although second breeding attempt from the identified female, no identified male so nestAttempt is set to 1

})
 #test passed

## Tests relative to capture table
testthat::test_that("Capture_data returns an expected outcome...", {


  #Take a subset of only WRS data
  WRS_data <- dplyr::filter(pipeline_output$Capture_data, captureSiteID %in% c("WRS"))

  ## K090259, banded as chick, recruited
  cap1_K090259 <- subset(WRS_data,
                         individualID == "K090259"
                         & captureYear == 2021
                         & captureMonth == 5
                         & captureDay == 28)

  testthat::expect_equal(cap1_K090259$speciesID, "CYACAE")
  testthat::expect_equal(cap1_K090259$captureTagID, NA_character_)
  testthat::expect_equal(cap1_K090259$releaseTagID, "K090259")
  testthat::expect_equal(cap1_K090259$captureID, "K090259_1")
  testthat::expect_equal(cap1_K090259$observedSex, NA_character_)
  testthat::expect_equal(cap1_K090259$captureTime, NA_character_)
  testthat::expect_equal(cap1_K090259$captureLocationID, "POL82")
  testthat::expect_equal(cap1_K090259$releaseLocationID, "POL82")
  testthat::expect_equal(cap1_K090259$capturePlotID, "WRS_POL")
  testthat::expect_equal(cap1_K090259$releasePlotID, "WRS_POL")
  testthat::expect_equal(cap1_K090259$releaseSiteID, "WRS")
  testthat::expect_equal(cap1_K090259$captureAlive, TRUE)
  testthat::expect_equal(cap1_K090259$releaseAlive, TRUE)
  testthat::expect_equal(cap1_K090259$capturePhysical, TRUE)
  testthat::expect_equal(cap1_K090259$minimumAge, 0)
  testthat::expect_equal(cap1_K090259$exactAge, 0)
  testthat::expect_equal(cap1_K090259$chickAge, 15)
  testthat::expect_equal(cap1_K090259$treatmentID, NA_character_)


  ## K090259, captured as a recruit in 2025 (fourth time captured)
  cap2_K090259 <- subset(WRS_data,
                         individualID == "K090259"
                         & captureYear == 2025
                         & captureMonth == 5
                         & captureDay == 19)

  testthat::expect_equal(cap2_K090259$speciesID, "CYACAE")
  testthat::expect_equal(cap2_K090259$captureTagID, "K090259")
  testthat::expect_equal(cap2_K090259$releaseTagID, "K090259")
  testthat::expect_equal(cap2_K090259$captureID, "K090259_4")
  testthat::expect_equal(cap2_K090259$observedSex, "M")
  testthat::expect_equal(cap2_K090259$captureTime, "12:13")
  testthat::expect_equal(cap2_K090259$captureLocationID, "POL62")
  testthat::expect_equal(cap2_K090259$releaseLocationID, "POL62")
  testthat::expect_equal(cap2_K090259$capturePlotID, "WRS_POL")
  testthat::expect_equal(cap2_K090259$releasePlotID, "WRS_POL")
  testthat::expect_equal(cap2_K090259$releaseSiteID, "WRS")
  testthat::expect_equal(cap2_K090259$captureAlive, TRUE)
  testthat::expect_equal(cap2_K090259$releaseAlive, TRUE)
  testthat::expect_equal(cap2_K090259$capturePhysical, TRUE)
  testthat::expect_equal(cap2_K090259$minimumAge, 3)
  testthat::expect_equal(cap2_K090259$exactAge, 3)
  testthat::expect_equal(cap2_K090259$chickAge, NA_integer_)
  testthat::expect_equal(cap2_K090259$treatmentID, NA_character_)


  ## K8Y6389 adult female
  cap_K8Y6389 <- subset(WRS_data,
                        individualID == "K8Y6389"
                        & captureYear == 2022
                        & captureMonth == 5
                        & captureDay == 19)

  testthat::expect_equal(cap_K8Y6389$speciesID, "CYACAE")
  testthat::expect_equal(cap_K8Y6389$captureTagID, NA_character_)
  testthat::expect_equal(cap_K8Y6389$releaseTagID, "K8Y6389")
  testthat::expect_equal(cap_K8Y6389$captureID, "K8Y6389_1")
  testthat::expect_equal(cap_K8Y6389$observedSex, "F")
  testthat::expect_equal(cap_K8Y6389$captureTime, "08:15")
  testthat::expect_equal(cap_K8Y6389$captureLocationID, "PAL12")
  testthat::expect_equal(cap_K8Y6389$releaseLocationID, "PAL12")
  testthat::expect_equal(cap_K8Y6389$capturePlotID, "WRS_PAL")
  testthat::expect_equal(cap_K8Y6389$releasePlotID, "WRS_PAL")
  testthat::expect_equal(cap_K8Y6389$captureSiteID, "WRS")
  testthat::expect_equal(cap_K8Y6389$releaseSiteID, "WRS")
  testthat::expect_equal(cap_K8Y6389$captureAlive, TRUE)
  testthat::expect_equal(cap_K8Y6389$releaseAlive, TRUE)
  testthat::expect_equal(cap_K8Y6389$capturePhysical, TRUE)
  testthat::expect_equal(cap_K8Y6389$minimumAge, 1)
  testthat::expect_equal(cap_K8Y6389$exactAge, NA_integer_)
  testthat::expect_equal(cap_K8Y6389$chickAge, NA_integer_)
  testthat::expect_equal(cap_K8Y6389$treatmentID, NA_character_)

  ## K248610 male first-year  breeder

  cap_K248610 <- subset(WRS_data,
                        individualID == "K248610"
                        & captureYear == 2023
                        & captureMonth == 5
                        & captureDay == 19)

  testthat::expect_equal(cap_K248610$speciesID, "CYACAE")
  testthat::expect_equal(cap_K248610$captureTagID, NA_character_)
  testthat::expect_equal(cap_K248610$releaseTagID, "K248610")
  testthat::expect_equal(cap_K248610$captureID, "K248610_1")
  testthat::expect_equal(cap_K248610$observedSex, "M")
  testthat::expect_equal(cap_K248610$captureTime, "09:35")
  testthat::expect_equal(cap_K248610$captureLocationID, "KPN51")
  testthat::expect_equal(cap_K248610$releaseLocationID, "KPN51")
  testthat::expect_equal(cap_K248610$capturePlotID, "WRS_KPN")
  testthat::expect_equal(cap_K248610$releasePlotID, "WRS_KPN")
  testthat::expect_equal(cap_K248610$captureSiteID, "WRS")
  testthat::expect_equal(cap_K248610$releaseSiteID, "WRS")
  testthat::expect_equal(cap_K248610$captureAlive, TRUE)
  testthat::expect_equal(cap_K248610$releaseAlive, TRUE)
  testthat::expect_equal(cap_K248610$capturePhysical, TRUE)
  testthat::expect_equal(cap_K248610$minimumAge, 1)
  testthat::expect_equal(cap_K248610$exactAge, NA_integer_)
  testthat::expect_equal(cap_K248610$chickAge, NA_integer_)
  testthat::expect_equal(cap_K248610$treatmentID, NA_character_)

}) #Test passed

## Tests relative to individual table

testthat::test_that("Individual data returns an expected outcome...", {

  #Take a subset of only WRS data
  WRS_data <- dplyr::filter(pipeline_output$Individual_data, siteID %in% c("WRS"))

  #Individual K174254, banded as 1st year breeder
  ind_K174254 <- subset(WRS_data, individualID == "K174254")
  testthat::expect_equal(ind_K174254$speciesID, "PARMAJ")
  testthat::expect_equal(ind_K174254$tagSiteID, "WRS")
  testthat::expect_equal(is.na(ind_K174254$broodIDLaid), TRUE)
  testthat::expect_equal(is.na(ind_K174254$broodIDFledged), TRUE)
  testthat::expect_equal(ind_K174254$tagYear, 2022)
  testthat::expect_equal(ind_K174254$tagStage, "subadult")
  testthat::expect_equal(ind_K174254$calculatedSex, "M")
  testthat::expect_equal(ind_K174254$geneticSex, NA_character_)

  #Individual K248192, banded as chick, recruited
  ind_K248192 <- subset(WRS_data, individualID == "K248192")
  testthat::expect_equal(ind_K248192$speciesID, "CYACAE")
  testthat::expect_equal(ind_K248192$tagSiteID, "WRS")
  testthat::expect_equal(!is.na(ind_K248192$broodIDLaid), TRUE)
  testthat::expect_equal(!is.na(ind_K248192$broodIDFledged), TRUE)
  testthat::expect_equal(ind_K248192$tagYear, 2022)
  testthat::expect_equal(ind_K248192$tagStage, "chick")
  testthat::expect_equal(ind_K248192$calculatedSex, "F")
  testthat::expect_equal(ind_K248192$geneticSex, NA_character_)


  #Individual K174080, adult captured twice
  ind_K174080 <- subset(WRS_data, individualID == "K174080")
  testthat::expect_equal(ind_K174080$speciesID, "PARMAJ")
  testthat::expect_equal(ind_K174080$tagSiteID, "WRS")
  testthat::expect_equal(is.na(ind_K174080$broodIDLaid), TRUE)
  testthat::expect_equal(is.na(ind_K174080$broodIDFledged), TRUE)
  testthat::expect_equal(ind_K174080$tagYear, 2023)
  testthat::expect_equal(ind_K174080$tagStage, "adult")
  testthat::expect_equal(ind_K174080$calculatedSex, "M")
  testthat::expect_equal(ind_K174080$geneticSex, NA_character_)


}) #test passed

## Tests relative to measurement table
testthat::test_that("Measurement data returns an expected outcome...", {

  #Take a subset of only WRS data
  WRS_data <- dplyr::filter(pipeline_output$Measurement_data, siteID %in% c("WRS"))

  ## K090800, measured as a chick, weight
  cap1_K090800 <- subset(WRS_data,
                         recordID == "K090800_1" &
                           measurementType == "mass")

  testthat::expect_equal(cap1_K090800$measurementDeterminedYear, 2020)
  testthat::expect_equal(cap1_K090800$measurementDeterminedMonth, 6)
  testthat::expect_equal(cap1_K090800$measurementDeterminedDay, 4)
  testthat::expect_equal(cap1_K090800$siteID, "WRS")
  testthat::expect_equal(cap1_K090800$measurementSubject, "capture")
  testthat::expect_equal(cap1_K090800$measurementValue, 11.9)
  testthat::expect_equal(cap1_K090800$measurementUnit, "g")
  testthat::expect_equal(cap1_K090800$recordedBy, NA_character_)

  ## K090800, measured as an adult (second capture), wing length
  cap2_K090800 <- subset(WRS_data,
                         recordID == "K090800_2" &
                           measurementType == "winglength")

  testthat::expect_equal(cap2_K090800$measurementDeterminedYear, 2021)
  testthat::expect_equal(cap2_K090800$measurementDeterminedMonth, 5)
  testthat::expect_equal(cap2_K090800$measurementDeterminedDay, 31)
  testthat::expect_equal(cap2_K090800$siteID, "WRS")
  testthat::expect_equal(cap2_K090800$measurementSubject, "capture")
  testthat::expect_equal(cap2_K090800$measurementValue, 64)
  testthat::expect_equal(cap2_K090800$measurementUnit, "mm")
  testthat::expect_equal(cap2_K090800$recordedBy, NA_character_)

  ## K174477 adult male 4th capture, tarsus length
  cap_K174477 <- subset(WRS_data,
                        recordID == "K174477_4"
                        & measurementType == "tarsus")

  testthat::expect_equal(cap_K174477$measurementDeterminedYear, 2025)
  testthat::expect_equal(cap_K174477$measurementDeterminedMonth, 5)
  testthat::expect_equal(cap_K174477$measurementDeterminedDay, 16)
  testthat::expect_equal(cap_K174477$siteID, "WRS")
  testthat::expect_equal(cap_K174477$measurementSubject, "capture")
  testthat::expect_equal(cap_K174477$measurementValue, 19.00)
  testthat::expect_equal(cap_K174477$measurementUnit, "mm")
  testthat::expect_equal(cap_K174477$recordedBy, NA_character_)

}) #test passed

## Tests relative location table

testthat::test_that("Location_data returns an expected outcome...", {

  #Take a subset of only WRS data
  WRS_data <- dplyr::filter(pipeline_output$Location_data, siteID %in% c("WRS"))

  ## Nestbox 42 in CMZ
  loc_42 <- subset(WRS_data, locationID == "CMZ42")
  testthat::expect_equal(loc_42$locationType, "nest")
  testthat::expect_equal(loc_42$siteID, "WRS")
  testthat::expect_equal(loc_42$decimalLatitude, 52.2477)
  testthat::expect_equal(loc_42$decimalLongitude, 20.97498)
  testthat::expect_equal(loc_42$startYear, 2025)
  testthat::expect_equal(loc_42$endYear, 2025)
  testthat::expect_equal(loc_42$habitatID, "J1")


  ## Nestbox 9 in KPN
  loc_9 <- subset(WRS_data, locationID == "KPN9")
  testthat::expect_equal(loc_9$locationType, "nest")
  testthat::expect_equal(loc_9$siteID, "WRS")
  testthat::expect_equal(loc_9$decimalLatitude, 52.35828)
  testthat::expect_equal(loc_9$decimalLongitude, 20.78438)
  testthat::expect_equal(loc_9$startYear, 2016)
  testthat::expect_equal(loc_9$endYear, 2025)
  testthat::expect_equal(loc_9$habitatID, "G3")


}) #test passed



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

  ## Experiment data: Test that all columns are present (not run as there is no experimental table)
  #testthat::test_col_present(pipeline_output, "Experiment")

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

  ## Experiment (not run as there is no experimental table)
  #test_NA_columns(pipeline_output, "Experiment")

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

  ## Experiment data: Test that all column classes are expected (not run as there is no experimental table)
  #test_col_classes(pipeline_output, "Experiment")

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

}) #test passed


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

  ## Experiment  (not run as there is no experimental table)
  #test_category_columns(pipeline_output, "Experiment")

}) #Test passed
