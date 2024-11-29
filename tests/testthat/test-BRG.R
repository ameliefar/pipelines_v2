testthat::skip_if(!exists("data_path"))

pipeline_output <- format_BRG(db = paste0(data_path, "/BRG_Bergen_Norway"),
                              optional_variables = "all")


test_that("BRG outputs all files...", {

  expect_true(all("BRG" %in% pipeline_output$Brood_data$siteID))
  expect_true(all("BRG" %in% pipeline_output$Capture_data$captureSiteID))
  expect_true(all("BRG" %in% pipeline_output$Individual_data$siteID))
  expect_true(all("BRG" %in% pipeline_output$Measurement_data$siteID))
  expect_true(all("BRG" %in% pipeline_output$Location_data$siteID))
#'  expect_true(all("BRG" %in% pipeline_output$Experiment_data$siteID)) #no Experiment table

}) #test passed



test_that("Brood_data returns an expected outcome...", {

  ## Take a subset of only BRG data
  BRG_data <- dplyr::filter(pipeline_output$Brood_data, siteID %in% c("BRG"))


  ## General brood data
  loc10_2017 <- subset(BRG_data,
                       observedLayYear == 2017 &
                         siteID == "BRG" &
                         locationID == "Milde_10_NB")

  expect_equal(!is.na(loc10_2017$broodID), TRUE)
  expect_equal(loc10_2017$breedingSeason, "2017")
  expect_equal(loc10_2017$plotID, "Milde")
  expect_equal(loc10_2017$speciesID, "PARMAJ")
  expect_equal(loc10_2017$femaleID, NA_character_)
  expect_equal(loc10_2017$maleID, NA_character_)
  expect_equal(loc10_2017$observedLayMonth, 5)
  expect_equal(loc10_2017$observedLayDay, 7)
  expect_equal(loc10_2017$observedClutchSize, 7)
  expect_equal(loc10_2017$observedHatchMonth, 5)
  expect_equal(loc10_2017$observedHatchDay, 24)
  expect_equal(loc10_2017$observedBroodSize, 6)
  expect_equal(loc10_2017$observedNumberFledged, 5)
  expect_equal(loc10_2017$observedFledgeMonth, NA_integer_)
  expect_equal(loc10_2017$observedFledgeDay, NA_integer_)
  expect_equal(loc10_2017$calculatedClutchType, "first")
  expect_equal(loc10_2017$nestAttemptNumber, 1)

  # Test that brood calculated as "replacement" is correct
  loc29_2022 <- subset(BRG_data,
                       observedLayYear == 2022 &
                         siteID == "BRG" &
                         locationID == "Milde_29_NB")

  expect_equal(!is.na(loc29_2022$broodID), TRUE)
  expect_equal(loc29_2022$breedingSeason, "2022")
  expect_equal(loc29_2022$plotID, "Milde")
  expect_equal(loc29_2022$speciesID, "PARMAJ")
  expect_equal(loc29_2022$femaleID, NA_character_)
  expect_equal(loc29_2022$maleID, NA_character_)
  expect_equal(loc29_2022$observedLayMonth, 5)
  expect_equal(loc29_2022$observedLayDay, 20)
  expect_equal(loc29_2022$observedClutchSize, 8)
  expect_equal(loc29_2022$observedHatchMonth, 6)
  expect_equal(loc29_2022$observedHatchDay, 7)
  expect_equal(loc29_2022$observedBroodSize, NA_integer_)
  expect_equal(loc29_2022$observedNumberFledged, 3)
  expect_equal(loc29_2022$observedFledgeMonth, NA_integer_)
  expect_equal(loc29_2022$observedFledgeDay, NA_integer_)
  expect_equal(loc29_2022$calculatedClutchType, "replacement") #defined as "replacement" because late in the season
  expect_equal(loc29_2022$nestAttemptNumber, 1) #set as one because only clutch reported at that nestbox that year with no associated female ID

}) #test passed


test_that("Capture_data returns an expected outcome...", {


  #Take a subset of only BRG data
  BRG_data <- dplyr::filter(pipeline_output$Capture_data, captureSiteID %in% c("BRG"))

  ## EK06579, banded as chick, recruited
  cap1_EK06579 <- subset(BRG_data,
                        individualID == "EK06579"
                        & captureYear == 2018
                        & captureMonth == 5
                        & captureDay == 29)

  expect_equal(cap1_EK06579$speciesID, "PARMAJ")
  expect_equal(cap1_EK06579$captureTagID, NA_character_)
  expect_equal(cap1_EK06579$releaseTagID, "EK06579")
  expect_equal(cap1_EK06579$captureID, "EK06579_1")
  expect_equal(cap1_EK06579$observedSex, NA_character_)
  expect_equal(cap1_EK06579$captureTime, "10:00")
  expect_equal(cap1_EK06579$captureLocationID, "Milde_33_NB")
  expect_equal(cap1_EK06579$releaseLocationID, "Milde_33_NB")
  expect_equal(cap1_EK06579$capturePlotID, "Milde")
  expect_equal(cap1_EK06579$releasePlotID, "Milde")
  expect_equal(cap1_EK06579$releaseSiteID, "BRG")
  expect_equal(cap1_EK06579$captureAlive, TRUE)
  expect_equal(cap1_EK06579$releaseAlive, TRUE)
  expect_equal(cap1_EK06579$capturePhysical, TRUE)
  expect_equal(cap1_EK06579$minimumAge, 0)
  expect_equal(cap1_EK06579$exactAge, 0)
  expect_equal(cap1_EK06579$chickAge, 15)
  expect_equal(cap1_EK06579$treatmentID, NA_character_)

  ## EK06579, captured as a recruit in 2021 (fourth time captured)
  cap2_EK06579 <- subset(BRG_data,
                         individualID == "EK06579"
                         & captureYear == 2021
                         & captureMonth == 5
                         & captureDay == 30)

  expect_equal(cap2_EK06579$speciesID, "PARMAJ")
  expect_equal(cap2_EK06579$captureTagID, "EK06579")
  expect_equal(cap2_EK06579$releaseTagID, "EK06579")
  expect_equal(cap2_EK06579$captureID, "EK06579_4")
  expect_equal(cap2_EK06579$observedSex, "M")
  expect_equal(cap2_EK06579$captureTime, "18:30")
  expect_equal(cap2_EK06579$captureLocationID, "Milde_19_NB")
  expect_equal(cap2_EK06579$releaseLocationID, "Milde_19_NB")
  expect_equal(cap2_EK06579$capturePlotID, "Milde")
  expect_equal(cap2_EK06579$releasePlotID, "Milde")
  expect_equal(cap2_EK06579$releaseSiteID, "BRG")
  expect_equal(cap2_EK06579$captureAlive, TRUE)
  expect_equal(cap2_EK06579$releaseAlive, TRUE)
  expect_equal(cap2_EK06579$capturePhysical, TRUE)
  expect_equal(cap2_EK06579$minimumAge, 3)
  expect_equal(cap2_EK06579$exactAge, 3)
  expect_equal(cap2_EK06579$chickAge, NA_integer_)
  expect_equal(cap2_EK06579$treatmentID, NA_character_)


  ## HD86461 adult male
  cap_HD86461 <- subset(BRG_data,
                        individualID == "HD86461"
                        & captureYear == 2019
                        & captureMonth == 6
                        & captureDay == 2)

  expect_equal(cap_HD86461$speciesID, "CYACAE")
  expect_equal(cap_HD86461$captureTagID, NA_character_)
  expect_equal(cap_HD86461$releaseTagID, "HD86461")
  expect_equal(cap_HD86461$captureID, "HD86461_1")
  expect_equal(cap_HD86461$observedSex, "M")
  expect_equal(cap_HD86461$captureTime, "09:27")
  expect_equal(cap_HD86461$captureLocationID, "Milde_37_NB")
  expect_equal(cap_HD86461$releaseLocationID, "Milde_37_NB")
  expect_equal(cap_HD86461$capturePlotID, "Milde")
  expect_equal(cap_HD86461$releasePlotID, "Milde")
  expect_equal(cap_HD86461$captureSiteID, "BRG")
  expect_equal(cap_HD86461$releaseSiteID, "BRG")
  expect_equal(cap_HD86461$captureAlive, TRUE)
  expect_equal(cap_HD86461$releaseAlive, TRUE)
  expect_equal(cap_HD86461$capturePhysical, TRUE)
  expect_equal(cap_HD86461$minimumAge, 1)
  expect_equal(cap_HD86461$exactAge, NA_integer_)
  expect_equal(cap_HD86461$chickAge, NA_integer_)
  expect_equal(cap_HD86461$treatmentID, NA_character_)


  ## HK18934 only identified without a physical capture in 2023
  cap_HK18934 <- subset(BRG_data,
                        individualID == "HK18934"
                        & captureYear == 2023
                        & captureMonth == 6
                        & captureDay == 9)

  expect_equal(cap_HK18934$speciesID, "CYACAE")
  expect_equal(cap_HK18934$captureTagID, "HK18934")
  expect_equal(cap_HK18934$releaseTagID, "HK18934")
  expect_equal(cap_HK18934$captureID, "HK18934_3") #third time seen
  expect_equal(cap_HK18934$observedSex, "M")
  expect_equal(cap_HK18934$captureTime, "13:00")
  expect_equal(cap_HK18934$captureLocationID, "Milde_35_NB")
  expect_equal(cap_HK18934$releaseLocationID, "Milde_35_NB")
  expect_equal(cap_HK18934$capturePlotID, "Milde")
  expect_equal(cap_HK18934$releasePlotID, "Milde")
  expect_equal(cap_HK18934$captureSiteID, "BRG")
  expect_equal(cap_HK18934$releaseSiteID, "BRG")
  expect_equal(cap_HK18934$captureAlive, TRUE)
  expect_equal(cap_HK18934$releaseAlive, TRUE)
  expect_equal(cap_HK18934$capturePhysical, FALSE)
  expect_equal(cap_HK18934$minimumAge, 3)
  expect_equal(cap_HD86461$exactAge, NA_integer_)
  expect_equal(cap_HD86461$chickAge, NA_integer_)
  expect_equal(cap_HD86461$treatmentID, NA_character_)


}) #Test passed



test_that("Individual data returns an expected outcome...", {

  #Take a subset of only BRG data
  BRG_data <- dplyr::filter(pipeline_output$Individual_data, siteID %in% c("BRG"))

  #Individual EK06602, banded as chick, recruited
  ind_EK06602 <- subset(BRG_data, individualID == "EK06602")
  expect_equal(ind_EK06602$speciesID, "PARMAJ")
  expect_equal(ind_EK06602$tagSiteID, "BRG")
  expect_equal(!is.na(ind_EK06602$broodIDLaid), TRUE)
  expect_equal(!is.na(ind_EK06602$broodIDFledged), TRUE)
  expect_equal(ind_EK06602$tagYear, 2018)
  expect_equal(ind_EK06602$tagStage, "chick")
  expect_equal(ind_EK06602$calculatedSex, "F")
  expect_equal(ind_EK06602$geneticSex, NA_character_)

  #Individual HH96002, adult captured twice
  ind_HH96002 <- subset(BRG_data, individualID == "HH96002")
  expect_equal(ind_HH96002$speciesID, "CYACAE")
  expect_equal(ind_HH96002$tagSiteID, "BRG")
  expect_equal(is.na(ind_HH96002$broodIDLaid), TRUE)
  expect_equal(is.na(ind_HH96002$broodIDFledged), TRUE)
  expect_equal(ind_HH96002$tagYear, 2017)
  expect_equal(ind_HH96002$tagStage, "subadult")
  expect_equal(ind_HH96002$calculatedSex, "F")
  expect_equal(ind_HH96002$geneticSex, NA_character_)


  #Individual HD86406, adult captured twice
  ind_HD86406 <- subset(BRG_data, individualID == "HD86406")
  expect_equal(ind_HD86406$speciesID, "PERATE")
  expect_equal(ind_HD86406$tagSiteID, "BRG")
  expect_equal(is.na(ind_HD86406$broodIDLaid), TRUE)
  expect_equal(is.na(ind_HD86406$broodIDFledged), TRUE)
  expect_equal(ind_HD86406$tagYear, 2019)
  expect_equal(ind_HD86406$tagStage, "subadult")
  expect_equal(ind_HD86406$calculatedSex, "M")
  expect_equal(ind_HD86406$geneticSex, NA_character_)


}) #test passed


test_that("Measurement data returns an expected outcome...", {

  #Take a subset of only BRG data
  BRG_data <- dplyr::filter(pipeline_output$Measurement_data, siteID %in% c("BRG"))

  ## EK06579, measured as a chick, weight
  cap1_EK06579 <- subset(BRG_data,
                         recordID == "EK06579_1" &
                          measurementType == "mass")

  expect_equal(cap1_EK06579$measurementDeterminedYear, 2018)
  expect_equal(cap1_EK06579$measurementDeterminedMonth, 5)
  expect_equal(cap1_EK06579$measurementDeterminedDay, 29)
  expect_equal(cap1_EK06579$siteID, "BRG")
  expect_equal(cap1_EK06579$measurementSubject, "capture")
  expect_equal(cap1_EK06579$measurementValue, 17.5)
  expect_equal(cap1_EK06579$measurementUnit, "g")
  expect_equal(cap1_EK06579$recordedBy, "AM")

  ## EK06579, measured as an adult (fourth capture), wing length
  cap2_EK06579 <- subset(BRG_data,
                         recordID == "EK06579_4" &
                           measurementType == "winglength")

  expect_equal(cap2_EK06579$measurementDeterminedYear, 2021)
  expect_equal(cap2_EK06579$measurementDeterminedMonth, 5)
  expect_equal(cap2_EK06579$measurementDeterminedDay, 30)
  expect_equal(cap2_EK06579$siteID, "BRG")
  expect_equal(cap2_EK06579$measurementSubject, "capture")
  expect_equal(cap2_EK06579$measurementValue, 77)
  expect_equal(cap2_EK06579$measurementUnit, "mm")
  expect_equal(cap2_EK06579$recordedBy, "AM")

  ## HD86406 adult male 2d capture, tarsus length
  cap_HD86406 <- subset(BRG_data,
                        recordID == "HD86406_2"
                        & measurementType == "tarsus")

  expect_equal(cap_HD86406$measurementDeterminedYear, 2020)
  expect_equal(cap_HD86406$measurementDeterminedMonth, 5)
  expect_equal(cap_HD86406$measurementDeterminedDay, 30)
  expect_equal(cap_HD86406$siteID, "BRG")
  expect_equal(cap_HD86406$measurementSubject, "capture")
  expect_equal(cap_HD86406$measurementValue, 17.01)
  expect_equal(cap_HD86406$measurementUnit, "mm")
  expect_equal(cap_HD86406$recordedBy, "AM")

}) #test passed

test_that("Location_data returns an expected outcome...", {

  #Take a subset of only BRG data
  BRG_data <- dplyr::filter(pipeline_output$Location_data, siteID %in% c("BRG"))

  ## Nestbox 29 in Milde
  loc_29 <- subset(BRG_data, locationID == "Milde_29_NB")
  expect_equal(loc_29$locationType, "nest")
  expect_equal(loc_29$siteID, "BRG")
  expect_equal(loc_29$decimalLatitude, 60.25)
  expect_equal(loc_29$decimalLongitude, 5.26)
  expect_equal(loc_29$startYear, 2017)
  expect_equal(loc_29$endYear, NA_integer_)
  expect_equal(loc_29$habitatID, "G2")

  ## Nestbox 2 in Langeskogen
  loc_2 <- subset(BRG_data, locationID == "Langeskogen_2_NB")
  expect_equal(loc_2$locationType, "nest")
  expect_equal(loc_2$siteID, "BRG")
  expect_equal(loc_2$decimalLatitude, 60.25)
  expect_equal(loc_2$decimalLongitude, 5.26)
  expect_equal(loc_2$startYear, 2023)
  expect_equal(loc_2$endYear, NA_integer_)
  expect_equal(loc_2$habitatID, "G1")


}) #test passed

## General tests (for pipelines formatted to standard protocol version 2.0)

test_that("Expected columns are present", {
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



test_that("Key columns in each table do not have NAs", {

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


test_that("Column classes are as expected", {

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

test_that("Key columns only contain unique values", {

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




test_that("Categorical columns do not have unexpected values", {

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

