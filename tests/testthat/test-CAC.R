testthat::skip_if(!exists("data_path"))

pipeline_output <- format_CAC(db = paste0(data_path, "/CAC_CanCata_Spain"),
                              optional_variables = "all")

testthat::test_that("CAC outputs all files...", {

  testthat::expect_true(all("CAC" %in% pipeline_output$Brood_data$siteID))
  testthat::expect_true(all("CAC" %in% pipeline_output$Capture_data$captureSiteID))
  testthat::expect_true(all("CAC" %in% pipeline_output$Individual_data$siteID))
  #testthat::expect_true(all("CAC" %in% pipeline_output$Measurement_data$siteID)) #no Measurement table
  testthat::expect_true(all("CAC" %in% pipeline_output$Location_data$siteID))
  testthat::expect_true(all("CAC" %in% pipeline_output$Experiment_data$siteID))

}) #test passed



testthat::test_that("Brood_data returns an expected outcome...", {

  ## General brood data
  loc98_2010 <- subset(pipeline_output$Brood_data,
                       observedLayYear == 2010 &
                         siteID == "CAC" &
                         locationID == "98")

  testthat::expect_equal(!is.na(loc98_2010$broodID), TRUE)
  testthat::expect_equal(loc98_2010$breedingSeason, "2010")
  testthat::expect_equal(loc98_2010$plotID, "CAC")
  testthat::expect_equal(loc98_2010$speciesID, "PARMAJ")
  testthat::expect_equal(loc98_2010$femaleID, "L743243")
  testthat::expect_equal(loc98_2010$maleID, "L362065")
  testthat::expect_equal(loc98_2010$observedLayMonth, 4)
  testthat::expect_equal(loc98_2010$observedLayDay, 12)
  testthat::expect_equal(loc98_2010$observedClutchSize, 8)
  testthat::expect_equal(loc98_2010$observedHatchMonth, NA_integer_)
  testthat::expect_equal(loc98_2010$observedHatchDay, NA_integer_)
  testthat::expect_equal(loc98_2010$observedBroodSize, NA_integer_)
  testthat::expect_equal(loc98_2010$observedNumberFledged, 7)
  testthat::expect_equal(loc98_2010$observedFledgeMonth, NA_integer_)
  testthat::expect_equal(loc98_2010$observedFledgeDay, NA_integer_)
  testthat::expect_equal(loc98_2010$calculatedClutchType, "first")
  testthat::expect_equal(loc98_2010$nestAttemptNumber, 1)

  # Test that brood calculated as "replacement" is correct
  loc164_2009 <- subset(pipeline_output$Brood_data,
                       observedLayYear == 2009 &
                         siteID == "CAC" &
                         locationID == "164")

  testthat::expect_equal(!is.na(loc164_2009$broodID), TRUE)
  testthat::expect_equal(loc164_2009$breedingSeason, "2009")
  testthat::expect_equal(loc164_2009$plotID, "CAC")
  testthat::expect_equal(loc164_2009$speciesID, "PARMAJ")
  testthat::expect_equal(loc164_2009$femaleID, NA_character_)
  testthat::expect_equal(loc164_2009$maleID, NA_character_)
  testthat::expect_equal(loc164_2009$observedLayMonth, 5)
  testthat::expect_equal(loc164_2009$observedLayDay, 20)
  testthat::expect_equal(loc164_2009$observedClutchSize, 6)
  testthat::expect_equal(loc164_2009$observedHatchMonth, 6)
  testthat::expect_equal(loc164_2009$observedHatchDay, 5)
  testthat::expect_equal(loc164_2009$observedBroodSize, NA_integer_)
  testthat::expect_equal(loc164_2009$observedNumberFledged, 2)
  testthat::expect_equal(loc164_2009$observedFledgeMonth, NA_integer_)
  testthat::expect_equal(loc164_2009$observedFledgeDay, NA_integer_)
  testthat::expect_equal(loc164_2009$calculatedClutchType, "replacement") #defined as "replacement" because late in the season
  testthat::expect_equal(loc164_2009$nestAttemptNumber, 1) #set as one because only clutch reported at that nestbox that year with no associated female ID

}) #test passed


testthat::test_that("Capture_data returns an expected outcome...", {

  ## L007721, female subadult first banded in 2009 at 132
  cap1_L007721 <- subset(pipeline_output$Capture_data,
                         individualID == "L007721"
                         & captureYear == 2009
                         & captureMonth == 5
                         & captureDay == 17)

  testthat::expect_equal(cap1_L007721$speciesID, "PARMAJ")
  testthat::expect_equal(cap1_L007721$captureTagID, NA_character_)
  testthat::expect_equal(cap1_L007721$releaseTagID, "L007721")
  testthat::expect_equal(cap1_L007721$captureID, "L007721_1")
  testthat::expect_equal(cap1_L007721$observedSex, "F")
  testthat::expect_equal(cap1_L007721$captureTime, NA_character_)
  testthat::expect_equal(cap1_L007721$captureLocationID, "132")
  testthat::expect_equal(cap1_L007721$releaseLocationID, "132")
  testthat::expect_equal(cap1_L007721$capturePlotID, "CAC")
  testthat::expect_equal(cap1_L007721$releasePlotID, "CAC")
  testthat::expect_equal(cap1_L007721$releaseSiteID, "CAC")
  testthat::expect_equal(cap1_L007721$captureAlive, TRUE)
  testthat::expect_equal(cap1_L007721$releaseAlive, TRUE)
  testthat::expect_equal(cap1_L007721$capturePhysical, TRUE)
  testthat::expect_equal(cap1_L007721$minimumAge, 1)
  testthat::expect_equal(cap1_L007721$exactAge, NA_integer_)
  testthat::expect_equal(cap1_L007721$chickAge, NA_integer_)
  testthat::expect_equal(cap1_L007721$treatmentID, NA_character_)

  ## L007721, female adult recaptured for the third time in 2010 at 131R
  cap2_L007721 <- subset(pipeline_output$Capture_data,
                         individualID == "L007721"
                         & captureYear == 2010
                         & captureMonth == 6
                         & captureDay == 16)

  testthat::expect_equal(cap2_L007721$speciesID, "PARMAJ")
  testthat::expect_equal(cap2_L007721$captureTagID, "L007721")
  testthat::expect_equal(cap2_L007721$releaseTagID, "L007721")
  testthat::expect_equal(cap2_L007721$captureID, "L007721_3")
  testthat::expect_equal(cap2_L007721$observedSex, "F")
  testthat::expect_equal(cap2_L007721$captureTime, NA_character_)
  testthat::expect_equal(cap2_L007721$captureLocationID, "131R")
  testthat::expect_equal(cap2_L007721$releaseLocationID, "131R")
  testthat::expect_equal(cap2_L007721$capturePlotID, "CAC")
  testthat::expect_equal(cap2_L007721$releasePlotID, "CAC")
  testthat::expect_equal(cap2_L007721$releaseSiteID, "CAC")
  testthat::expect_equal(cap2_L007721$captureAlive, TRUE)
  testthat::expect_equal(cap2_L007721$releaseAlive, TRUE)
  testthat::expect_equal(cap2_L007721$capturePhysical, TRUE)
  testthat::expect_equal(cap2_L007721$minimumAge, 2)
  testthat::expect_equal(cap2_L007721$exactAge, NA_integer_)
  testthat::expect_equal(cap2_L007721$chickAge, NA_integer_)
  testthat::expect_equal(cap2_L007721$treatmentID, NA_character_)


  ## L743881 adult male in 2014 (2d capture) in 27
  cap_L743881 <- subset(pipeline_output$Capture_data,
                        individualID == "L743881"
                        & captureYear == 2014
                        & captureMonth == 5
                        & captureDay == 6)

  testthat::expect_equal(cap_L743881$speciesID, "PARMAJ")
  testthat::expect_equal(cap_L743881$captureTagID, "L743881")
  testthat::expect_equal(cap_L743881$releaseTagID, "L743881")
  testthat::expect_equal(cap_L743881$captureID, "L743881_2")
  testthat::expect_equal(cap_L743881$observedSex, "M")
  testthat::expect_equal(cap_L743881$captureTime, NA_character_)
  testthat::expect_equal(cap_L743881$captureLocationID, "27")
  testthat::expect_equal(cap_L743881$releaseLocationID, "27")
  testthat::expect_equal(cap_L743881$capturePlotID, "CAC")
  testthat::expect_equal(cap_L743881$releasePlotID, "CAC")
  testthat::expect_equal(cap_L743881$captureSiteID, "CAC")
  testthat::expect_equal(cap_L743881$releaseSiteID, "CAC")
  testthat::expect_equal(cap_L743881$captureAlive, TRUE)
  testthat::expect_equal(cap_L743881$releaseAlive, TRUE)
  testthat::expect_equal(cap_L743881$capturePhysical, TRUE)
  testthat::expect_equal(cap_L743881$minimumAge, 1) #Should be "2" but I guess it was captured less than 365 days after the first capture
  testthat::expect_equal(cap_L743881$exactAge, NA_integer_)
  testthat::expect_equal(cap_L743881$chickAge, NA_integer_)
  testthat::expect_equal(cap_L743881$treatmentID, NA_character_)


}) #Test passed


testthat::test_that("Individual data returns an expected outcome...", {

  #Individual 2690100, banded as female adult in 1998
  ind_2690100 <- subset(pipeline_output$Individual_data, individualID == "2690100")
  testthat::expect_equal(ind_2690100$speciesID, "PARMAJ")
  testthat::expect_equal(ind_2690100$tagSiteID, "CAC")
  testthat::expect_equal(!is.na(ind_2690100$broodIDLaid), FALSE)
  testthat::expect_equal(!is.na(ind_2690100$broodIDFledged), FALSE)
  testthat::expect_equal(ind_2690100$tagYear, 1998)
  testthat::expect_equal(ind_2690100$tagStage, "adult")
  testthat::expect_equal(ind_2690100$calculatedSex, "F")
  testthat::expect_equal(ind_2690100$geneticSex, NA_character_)

  #Individual 1KA34036, banded as male subadult in 2019
  ind_1KA34036 <- subset(pipeline_output$Individual_data, individualID == "1KA34036")
  testthat::expect_equal(ind_1KA34036$speciesID, "PARMAJ")
  testthat::expect_equal(ind_1KA34036$tagSiteID, "CAC")
  testthat::expect_equal(is.na(ind_1KA34036$broodIDLaid), TRUE)
  testthat::expect_equal(is.na(ind_1KA34036$broodIDFledged), TRUE)
  testthat::expect_equal(ind_1KA34036$tagYear, 2019)
  testthat::expect_equal(ind_1KA34036$tagStage, "subadult")
  testthat::expect_equal(ind_1KA34036$calculatedSex, "M")
  testthat::expect_equal(ind_1KA34036$geneticSex, NA_character_)

}) #test passed



testthat::test_that("Location_data returns an expected outcome...", {

  ## Nestbox 29 first occupied in 2000
  loc_29 <- subset(pipeline_output$Location_data, locationID == "29")
  testthat::expect_equal(loc_29$locationType, "nest")
  testthat::expect_equal(loc_29$siteID, "CAC")
  testthat::expect_equal(loc_29$decimalLatitude, 45.27)
  testthat::expect_equal(loc_29$decimalLongitude, 2.8)
  testthat::expect_equal(loc_29$startYear, 2000)
  testthat::expect_equal(loc_29$endYear, NA_integer_)
  testthat::expect_equal(loc_29$habitatID, "G4")

  ## Nestbox 2 first occupied in 1998
  loc_2 <- subset(pipeline_output$Location_data, locationID == "2")
  testthat::expect_equal(loc_2$locationType, "nest")
  testthat::expect_equal(loc_2$siteID, "CAC")
  testthat::expect_equal(loc_2$decimalLatitude, 45.27)
  testthat::expect_equal(loc_2$decimalLongitude, 2.8)
  testthat::expect_equal(loc_2$startYear, 1998)
  testthat::expect_equal(loc_2$endYear, NA_integer_)
  testthat::expect_equal(loc_2$habitatID, "G4")


}) #test passed



testthat::test_that("Experiment_data returns an expected outcome...", {

  ## egg swapping in 1999
  trt_1_1999 <- subset(pipeline_output$Experiment_data, experimentID == "trt_1" & treatmentStartYear == 1999)
  testthat::expect_equal(trt_1_1999$treatmentID, "1999-1")
  testthat::expect_equal(trt_1_1999$siteID, "CAC")
  testthat::expect_equal(trt_1_1999$experimentType, "cross-fostering")
  testthat::expect_equal(trt_1_1999$treatmentDetails, "eggs swapping among 2 or 3 nests")
  testthat::expect_equal(trt_1_1999$treatmentStartMonth, NA_integer_)
  testthat::expect_equal(trt_1_1999$treatmentEndYear, 1999)
  testthat::expect_equal(trt_1_1999$recordedBy, NA_character_)

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

  # ## Measurement data: Test that all columns are present
  # test_col_present(pipeline_output, "Measurement")

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

  # ## Measurement
  # test_NA_columns(pipeline_output, "Measurement")

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

  # ## Measurement data: Test that all column classes are expected
  # test_col_present(pipeline_output, "Measurement")

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

  # ## measurementID has only unique values
  # test_unique_values(pipeline_output, "measurementID")

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

  # ## Measurement
  # test_category_columns(pipeline_output, "Measurement")

  ## Location
  test_category_columns(pipeline_output, "Location")

  ## Experiment
  test_category_columns(pipeline_output, "Experiment")

}) #Test passed
