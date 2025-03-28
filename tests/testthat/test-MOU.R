testthat::skip_if(!exists("data_path"))

pipeline_output <- format_MOU(db = paste0(data_path, "/MOU_Moulis_France"),
                              optional_variables = "all")


testthat::test_that("MOU outputs all files...", {

  testthat::expect_true(all("MOU" %in% pipeline_output$Brood_data$siteID))
  testthat::expect_true(all("MOU" %in% pipeline_output$Capture_data$captureSiteID))
  testthat::expect_true(all("MOU" %in% pipeline_output$Individual_data$siteID))
  testthat::expect_true(all("MOU" %in% pipeline_output$Measurement_data$siteID))
  testthat::expect_true(all("MOU" %in% pipeline_output$Location_data$siteID))
  testthat::expect_true(all("MOU" %in% pipeline_output$Experiment_data$siteID))

}) #test passed


testthat::test_that("Brood_data returns an expected outcome...", {

  ## Take a subset of only MOU data
  MOU_data <- dplyr::filter(pipeline_output$Brood_data, siteID %in% c("MOU"))


  ## General brood data
  G5_2016 <- subset(MOU_data,
                    observedLayYear == 2016 &
                      siteID == "MOU" &
                      locationID == "G5_G008_NB")

  testthat::expect_equal(!is.na(G5_2016$broodID), TRUE)
  testthat::expect_equal(G5_2016$breedingSeason, "2016")
  testthat::expect_equal(G5_2016$plotID, "Galey")
  testthat::expect_equal(G5_2016$speciesID, "CYACAE")
  testthat::expect_equal(G5_2016$femaleID, "7720938")
  testthat::expect_equal(G5_2016$maleID, "7969646")
  testthat::expect_equal(G5_2016$observedLayMonth, 4)
  testthat::expect_equal(G5_2016$observedLayDay, 22)
  testthat::expect_equal(G5_2016$observedClutchSize, 8)
  testthat::expect_equal(G5_2016$observedHatchMonth, 5)
  testthat::expect_equal(G5_2016$observedHatchDay, 15)
  testthat::expect_equal(G5_2016$observedBroodSize, 7)
  testthat::expect_equal(G5_2016$observedNumberFledged, 7)
  testthat::expect_equal(G5_2016$observedFledgeMonth, 6)
  testthat::expect_equal(G5_2016$observedFledgeDay, 3)
  testthat::expect_equal(G5_2016$calculatedClutchType, "first")
  testthat::expect_equal(G5_2016$nestAttemptNumber, 1)

  # Test that brood calculated as "replacement" is correct
  c4_2022 <- subset(MOU_data,
                    observedLayYear == 2022 &
                      siteID == "MOU" &
                      locationID == "C4_C146_NB")

  testthat::expect_equal(!is.na(c4_2022$broodID), TRUE)
  testthat::expect_equal(c4_2022$breedingSeason, "2022")
  testthat::expect_equal(c4_2022$plotID, "Cescau")
  testthat::expect_equal(c4_2022$speciesID, "PARMAJ")
  testthat::expect_equal(c4_2022$femaleID, NA_character_)
  testthat::expect_equal(c4_2022$maleID, NA_character_)
  testthat::expect_equal(c4_2022$observedLayMonth, 5)
  testthat::expect_equal(c4_2022$observedLayDay, 22)
  testthat::expect_equal(c4_2022$observedClutchSize, 8)
  testthat::expect_equal(c4_2022$observedHatchMonth, 6)
  testthat::expect_equal(c4_2022$observedHatchDay, 10)
  testthat::expect_equal(c4_2022$observedBroodSize, 7)
  testthat::expect_equal(c4_2022$observedNumberFledged, 6)
  testthat::expect_equal(c4_2022$observedFledgeMonth, 6)
  testthat::expect_equal(c4_2022$observedFledgeDay, 30)
  testthat::expect_equal(c4_2022$calculatedClutchType, "replacement") #defined as "replacement" because late in the season
  testthat::expect_equal(c4_2022$nestAttemptNumber, 1) #set as one because only clutch reported at that nestbox that year with no associated female ID


  # Test that brood calculated as "second" is correct
  c1_2019 <- subset(MOU_data,
                    observedLayYear == 2019 &
                      siteID == "MOU" &
                      locationID == "C1_C005_NB")

  testthat::expect_equal(!is.na(c1_2019$broodID), TRUE)
  testthat::expect_equal(c1_2019$breedingSeason, "2019")
  testthat::expect_equal(c1_2019$plotID, "Cescau")
  testthat::expect_equal(c1_2019$speciesID, "PARMAJ")
  testthat::expect_equal(c1_2019$femaleID, "8383220")
  testthat::expect_equal(c1_2019$maleID, "8383573")
  testthat::expect_equal(c1_2019$observedLayMonth, 6)
  testthat::expect_equal(c1_2019$observedLayDay, 6)
  testthat::expect_equal(c1_2019$observedClutchSize, 3)
  testthat::expect_equal(c1_2019$observedHatchMonth, 6)
  testthat::expect_equal(c1_2019$observedHatchDay, 23)
  testthat::expect_equal(c1_2019$observedBroodSize, 1)
  testthat::expect_equal(c1_2019$observedNumberFledged, 1)
  testthat::expect_equal(c1_2019$observedFledgeMonth, 7)
  testthat::expect_equal(c1_2019$observedFledgeDay, 15)
  testthat::expect_equal(c1_2019$calculatedClutchType, "second")
  testthat::expect_equal(c1_2019$nestAttemptNumber, 2) #set as one because only clutch reported at that nestbox that year with no associated female ID
}) #test passed


testthat::test_that("Capture_data returns an expected outcome...", {


  #Take a subset of only MOU data
  MOU_data <- dplyr::filter(pipeline_output$Capture_data, captureSiteID %in% c("MOU"))

  ## 6956336, blue tit banded as chick, recruited
  cap1_6956336 <- subset(MOU_data,
                         individualID == "6956336"
                         & captureYear == 2014
                         & captureMonth == 5
                         & captureDay == 10)

  testthat::expect_equal(cap1_6956336$speciesID, "CYACAE")
  testthat::expect_equal(cap1_6956336$captureTagID, NA_character_)
  testthat::expect_equal(cap1_6956336$releaseTagID, "6956336")
  testthat::expect_equal(cap1_6956336$captureID, "6956336_1")
  testthat::expect_equal(cap1_6956336$observedSex, "U")
  testthat::expect_equal(cap1_6956336$captureTime, "11:21")
  testthat::expect_equal(cap1_6956336$captureLocationID, "M4_M029_NB")
  testthat::expect_equal(cap1_6956336$releaseLocationID, "M4_M029_NB")
  testthat::expect_equal(cap1_6956336$capturePlotID, "Moulis")
  testthat::expect_equal(cap1_6956336$releasePlotID, "Moulis")
  testthat::expect_equal(cap1_6956336$releaseSiteID, "MOU")
  testthat::expect_equal(cap1_6956336$captureAlive, TRUE)
  testthat::expect_equal(cap1_6956336$releaseAlive, TRUE)
  testthat::expect_equal(cap1_6956336$capturePhysical, TRUE)
  testthat::expect_equal(cap1_6956336$minimumAge, 0)
  testthat::expect_equal(cap1_6956336$exactAge, 0)
  testthat::expect_equal(cap1_6956336$chickAge, NA_integer_)
  testthat::expect_equal(cap1_6956336$treatmentID, NA_character_)

  ## 6956336, female blue tit captured as a recruit in 2018, 4th capture
  cap2_6956336 <- subset(MOU_data,
                         individualID == "6956336"
                         & captureYear == 2018
                         & captureMonth == 5
                         & captureDay == 15)

  testthat::expect_equal(cap2_6956336$speciesID, "CYACAE")
  testthat::expect_equal(cap2_6956336$captureTagID, "6956336")
  testthat::expect_equal(cap2_6956336$releaseTagID, "6956336")
  testthat::expect_equal(cap2_6956336$captureID, "6956336_4")
  testthat::expect_equal(cap2_6956336$observedSex, "F")
  testthat::expect_equal(cap2_6956336$captureTime, "14:53")
  testthat::expect_equal(cap2_6956336$captureLocationID, "M4_M014_NB")
  testthat::expect_equal(cap2_6956336$releaseLocationID, "M4_M014_NB")
  testthat::expect_equal(cap2_6956336$capturePlotID, "Moulis")
  testthat::expect_equal(cap2_6956336$releasePlotID, "Moulis")
  testthat::expect_equal(cap2_6956336$releaseSiteID, "MOU")
  testthat::expect_equal(cap2_6956336$captureAlive, TRUE)
  testthat::expect_equal(cap2_6956336$releaseAlive, TRUE)
  testthat::expect_equal(cap2_6956336$capturePhysical, TRUE)
  testthat::expect_equal(cap2_6956336$minimumAge, 4)
  testthat::expect_equal(cap2_6956336$exactAge, 4)
  testthat::expect_equal(cap2_6956336$chickAge, NA_integer_)
  testthat::expect_equal(cap2_6956336$treatmentID, NA_character_)

  ## 6956336, female blue tit recorded through RFID door in 2019, 5th time seen
  cap3_6956336 <- subset(MOU_data,
                         individualID == "6956336"
                         & captureYear == 2019
                         & captureMonth == 5
                         & captureDay == 5)

  testthat::expect_equal(cap3_6956336$speciesID, "CYACAE")
  testthat::expect_equal(cap3_6956336$captureTagID, "6956336")
  testthat::expect_equal(cap3_6956336$releaseTagID, "6956336")
  testthat::expect_equal(cap3_6956336$captureID, "6956336_5")
  testthat::expect_equal(cap3_6956336$observedSex, "U")
  testthat::expect_equal(cap3_6956336$captureTime, "08:50")
  testthat::expect_equal(cap3_6956336$captureLocationID, "M5_M017_NB")
  testthat::expect_equal(cap3_6956336$releaseLocationID, "M5_M017_NB")
  testthat::expect_equal(cap3_6956336$capturePlotID, "Moulis")
  testthat::expect_equal(cap3_6956336$releasePlotID, "Moulis")
  testthat::expect_equal(cap3_6956336$releaseSiteID, "MOU")
  testthat::expect_equal(cap3_6956336$captureAlive, TRUE)
  testthat::expect_equal(cap3_6956336$releaseAlive, TRUE)
  testthat::expect_equal(cap3_6956336$capturePhysical, FALSE)
  testthat::expect_equal(cap3_6956336$minimumAge, 4)
  testthat::expect_equal(cap3_6956336$exactAge, 4)
  testthat::expect_equal(cap3_6956336$chickAge, NA_integer_)
  testthat::expect_equal(cap3_6956336$treatmentID, NA_character_)


  ## 9763360 great tit adult male captured as a breeder
  cap_9763360 <- subset(MOU_data,
                        individualID == "9763360"
                        & captureYear == 2022
                        & captureMonth == 5
                        & captureDay == 13)

  testthat::expect_equal(cap_9763360$speciesID, "PARMAJ")
  testthat::expect_equal(cap_9763360$captureTagID, NA_character_)
  testthat::expect_equal(cap_9763360$releaseTagID, "9763360")
  testthat::expect_equal(cap_9763360$captureID, "9763360_1")
  testthat::expect_equal(cap_9763360$observedSex, "M")
  testthat::expect_equal(cap_9763360$captureTime, "10:10")
  testthat::expect_equal(cap_9763360$captureLocationID, "C2_C090_NB")
  testthat::expect_equal(cap_9763360$releaseLocationID, "C2_C090_NB")
  testthat::expect_equal(cap_9763360$capturePlotID, "Cescau")
  testthat::expect_equal(cap_9763360$releasePlotID, "Cescau")
  testthat::expect_equal(cap_9763360$captureSiteID, "MOU")
  testthat::expect_equal(cap_9763360$releaseSiteID, "MOU")
  testthat::expect_equal(cap_9763360$captureAlive, TRUE)
  testthat::expect_equal(cap_9763360$releaseAlive, TRUE)
  testthat::expect_equal(cap_9763360$capturePhysical, TRUE)
  testthat::expect_equal(cap_9763360$minimumAge, 1)
  testthat::expect_equal(cap_9763360$exactAge, NA_integer_)
  testthat::expect_equal(cap_9763360$chickAge, NA_integer_)
  testthat::expect_equal(cap_9763360$treatmentID, NA_character_)


  ## 7476354 a Marsh tit, with undetermined sex or age, recaptured with mist net in 2017
  cap_7476354 <- subset(MOU_data,
                        individualID == "7476354"
                        & captureYear == 2017
                        & captureMonth == 11
                        & captureDay == 30)

  testthat::expect_equal(cap_7476354$speciesID, "POEPAL")
  testthat::expect_equal(cap_7476354$captureTagID, "7476354")
  testthat::expect_equal(cap_7476354$releaseTagID, "7476354")
  testthat::expect_equal(cap_7476354$captureID, "7476354_4") #fourth time captured
  testthat::expect_equal(cap_7476354$observedSex, "U")
  testthat::expect_equal(cap_7476354$captureTime, "10:57")
  testthat::expect_equal(cap_7476354$captureLocationID, "M1_MN")
  testthat::expect_equal(cap_7476354$releaseLocationID, "M1_MN")
  testthat::expect_equal(cap_7476354$capturePlotID, "Moulis")
  testthat::expect_equal(cap_7476354$releasePlotID, "Moulis")
  testthat::expect_equal(cap_7476354$captureSiteID, "MOU")
  testthat::expect_equal(cap_7476354$releaseSiteID, "MOU")
  testthat::expect_equal(cap_7476354$captureAlive, TRUE)
  testthat::expect_equal(cap_7476354$releaseAlive, TRUE)
  testthat::expect_equal(cap_7476354$capturePhysical, TRUE)
  testthat::expect_equal(cap_7476354$minimumAge, 4) #not sure
  testthat::expect_equal(cap_7476354$exactAge, NA_integer_)
  testthat::expect_equal(cap_7476354$chickAge, NA_integer_)
  testthat::expect_equal(cap_7476354$treatmentID, NA_character_)


}) #Test passed





testthat::test_that("Individual data returns an expected outcome...", {

  #Take a subset of only MOU data
  MOU_data <- dplyr::filter(pipeline_output$Individual_data, siteID %in% c("MOU"))

  #Individual 9763230, banded as chick, recruited
  ind_9763230 <- subset(MOU_data, individualID == "9763230")
  testthat::expect_equal(ind_9763230$speciesID, "PARMAJ")
  testthat::expect_equal(ind_9763230$tagSiteID, "MOU")
  testthat::expect_equal(!is.na(ind_9763230$broodIDLaid), TRUE)
  testthat::expect_equal(!is.na(ind_9763230$broodIDFledged), TRUE)
  testthat::expect_equal(ind_9763230$tagYear, 2021)
  testthat::expect_equal(ind_9763230$tagStage, "chick")
  testthat::expect_equal(ind_9763230$calculatedSex, "M")
  testthat::expect_equal(ind_9763230$geneticSex, NA_character_)

  #Individual 8383298, adult seen five times
  ind_8383298 <- subset(MOU_data, individualID == "8383298")
  testthat::expect_equal(ind_8383298$speciesID, "CYACAE")
  testthat::expect_equal(ind_8383298$tagSiteID, "MOU")
  testthat::expect_equal(is.na(ind_8383298$broodIDLaid), TRUE)
  testthat::expect_equal(is.na(ind_8383298$broodIDFledged), TRUE)
  testthat::expect_equal(ind_8383298$tagYear, 2018)
  testthat::expect_equal(ind_8383298$tagStage, "subadult")
  testthat::expect_equal(ind_8383298$calculatedSex, "C")
  testthat::expect_equal(ind_8383298$geneticSex, NA_character_)


  #Individual 7720900, adult captured four times
  ind_7720900 <- subset(MOU_data, individualID == "7720900")
  testthat::expect_equal(ind_7720900$speciesID, "PERATE")
  testthat::expect_equal(ind_7720900$tagSiteID, "MOU")
  testthat::expect_equal(is.na(ind_7720900$broodIDLaid), TRUE)
  testthat::expect_equal(is.na(ind_7720900$broodIDFledged), TRUE)
  testthat::expect_equal(ind_7720900$tagYear, 2015)
  testthat::expect_equal(ind_7720900$tagStage, NA_character_)
  testthat::expect_equal(ind_7720900$calculatedSex, NA_character_)
  testthat::expect_equal(ind_7720900$geneticSex, NA_character_)


}) #test passed


testthat::test_that("Measurement data returns an expected outcome...", {

  #Take a subset of only MOU data
  MOU_data <- dplyr::filter(pipeline_output$Measurement_data, siteID %in% c("MOU"))

  ## 7720679, measured when tagged, tarsus length
  cap1_7720679 <- subset(MOU_data,
                         recordID == "7720679_1" &
                           measurementType == "tarsus")

  testthat::expect_equal(cap1_7720679$measurementDeterminedYear, 2015)
  testthat::expect_equal(cap1_7720679$measurementDeterminedMonth, 6)
  testthat::expect_equal(cap1_7720679$measurementDeterminedDay, 5)
  testthat::expect_equal(cap1_7720679$siteID, "MOU")
  testthat::expect_equal(cap1_7720679$measurementSubject, "capture")
  testthat::expect_equal(cap1_7720679$measurementValue, 22.2)
  testthat::expect_equal(cap1_7720679$measurementUnit, "mm")
  testthat::expect_equal(cap1_7720679$recordedBy, "ASC")

  ## 7720679, measured as an adult (fourth capture), wing length
  cap2_7720679 <- subset(MOU_data,
                         recordID == "7720679_4" &
                           measurementType == "winglength")

  testthat::expect_equal(cap2_7720679$measurementDeterminedYear, 2017)
  testthat::expect_equal(cap2_7720679$measurementDeterminedMonth, 5)
  testthat::expect_equal(cap2_7720679$measurementDeterminedDay, 21)
  testthat::expect_equal(cap2_7720679$siteID, "MOU")
  testthat::expect_equal(cap2_7720679$measurementSubject, "capture")
  testthat::expect_equal(cap2_7720679$measurementValue, 77)
  testthat::expect_equal(cap2_7720679$measurementUnit, "mm")
  testthat::expect_equal(cap2_7720679$recordedBy, "ASC")

  ## 7720717 subadult undetermined sex, first capture, head length
  cap_7720717 <- subset(MOU_data,
                        recordID == "7720717_1"
                        & measurementType == "headlength")

  testthat::expect_equal(cap_7720717$measurementDeterminedYear, 2016)
  testthat::expect_equal(cap_7720717$measurementDeterminedMonth, 1)
  testthat::expect_equal(cap_7720717$measurementDeterminedDay, 27)
  testthat::expect_equal(cap_7720717$siteID, "MOU")
  testthat::expect_equal(cap_7720717$measurementSubject, "capture")
  testthat::expect_equal(cap_7720717$measurementValue, 24.4)
  testthat::expect_equal(cap_7720717$measurementUnit, "mm")
  testthat::expect_equal(cap_7720717$recordedBy, "AT")

  ## 8994787 subadult male, first capture, fat score
  cap_8994787 <- subset(MOU_data,
                        recordID == "8994787_1"
                        & measurementType == "fatscore")

  testthat::expect_equal(cap_8994787$measurementDeterminedYear, 2021)
  testthat::expect_equal(cap_8994787$measurementDeterminedMonth, 1)
  testthat::expect_equal(cap_8994787$measurementDeterminedDay, 6)
  testthat::expect_equal(cap_8994787$siteID, "MOU")
  testthat::expect_equal(cap_8994787$measurementSubject, "capture")
  testthat::expect_equal(cap_8994787$measurementValue, 0)
  testthat::expect_equal(cap_8994787$measurementUnit, "no unit")
  testthat::expect_equal(cap_8994787$recordedBy, "ASC")


  ## 7720310 chick, second capture, mass
  cap_7720310 <- subset(MOU_data,
                        recordID == "7720310_2"
                        & measurementType == "mass")

  testthat::expect_equal(cap_7720310$measurementDeterminedYear, 2016)
  testthat::expect_equal(cap_7720310$measurementDeterminedMonth, 5)
  testthat::expect_equal(cap_7720310$measurementDeterminedDay, 19)
  testthat::expect_equal(cap_7720310$siteID, "MOU")
  testthat::expect_equal(cap_7720310$measurementSubject, "capture")
  testthat::expect_equal(cap_7720310$measurementValue, 10.0)
  testthat::expect_equal(cap_7720310$measurementUnit, "g")
  testthat::expect_equal(cap_7720310$recordedBy, "AT")


  ## 7476223 subadult female, first capture, tail length
  cap_7476223 <- subset(MOU_data,
                        recordID == "7476223_1"
                        & measurementType == "taillength")

  testthat::expect_equal(cap_7476223$measurementDeterminedYear, 2014)
  testthat::expect_equal(cap_7476223$measurementDeterminedMonth, 10)
  testthat::expect_equal(cap_7476223$measurementDeterminedDay, 15)
  testthat::expect_equal(cap_7476223$siteID, "MOU")
  testthat::expect_equal(cap_7476223$measurementSubject, "capture")
  testthat::expect_equal(cap_7476223$measurementValue, 51)
  testthat::expect_equal(cap_7476223$measurementUnit, "mm")
  testthat::expect_equal(cap_7476223$recordedBy, "ES")

}) #test passed

testthat::test_that("Location_data returns an expected outcome...", {

  #Take a subset of only MOU data
  MOU_data <- dplyr::filter(pipeline_output$Location_data, siteID %in% c("MOU"))

  ## Nestbox H415 in Castera
  loc_h415 <- subset(MOU_data, locationID == "H8_H415_NB")
  testthat::expect_equal(loc_h415$locationType, "nest")
  testthat::expect_equal(loc_h415$siteID, "MOU")
  testthat::expect_equal(loc_h415$elevation, 1359.0000)
  testthat::expect_equal(loc_h415$decimalLatitude, 42.8975)
  testthat::expect_equal(loc_h415$decimalLongitude, 1.0728)
  testthat::expect_equal(loc_h415$startYear, 2015)
  testthat::expect_equal(loc_h415$endYear, 2019)
  testthat::expect_equal(loc_h415$habitatID, NA_character_)

  ## Nestbox M104 in Moulis
  loc_M104 <- subset(MOU_data, locationID == "M1_M104_NB")
  testthat::expect_equal(loc_M104$locationType, "nest")
  testthat::expect_equal(loc_M104$siteID, "MOU")
  testthat::expect_equal(loc_M104$elevation, 486.0000)
  testthat::expect_equal(loc_M104$decimalLatitude, 42.9660)
  testthat::expect_equal(loc_M104$decimalLongitude, 1.0915)
  testthat::expect_equal(loc_M104$startYear, 2011)
  testthat::expect_equal(loc_M104$endYear, NA_integer_)
  testthat::expect_equal(loc_M104$habitatID, NA_character_)

  ## Mistnest G1 in Galey
  loc_G1 <- subset(MOU_data, locationID == "G1_MN")
  testthat::expect_equal(loc_G1$locationType, "capture")
  testthat::expect_equal(loc_G1$siteID, "MOU")
  testthat::expect_equal(loc_G1$elevation, NA_real_)
  testthat::expect_equal(loc_G1$decimalLatitude, NA_real_)
  testthat::expect_equal(loc_G1$decimalLongitude, NA_real_)
  testthat::expect_equal(loc_G1$startYear, 2017)
  testthat::expect_equal(loc_G1$endYear, 2018)
  testthat::expect_equal(loc_G1$habitatID, NA_character_)


}) #test passed


testthat::test_that("Experiment_data returns an expected outcome...", {

  #Take a subset of only MOU data
  MOU_data <- dplyr::filter(pipeline_output$Experiment_data, siteID %in% c("MOU"))

  ## cross-fostering experiment in 2014
  exp_cf <- subset(MOU_data, treatmentID == "cross-fostering_2014")
  testthat::expect_equal(exp_cf$experimentID, "cross-fostering")
  testthat::expect_equal(exp_cf$siteID, "MOU")
  testthat::expect_equal(exp_cf$experimentType, "behavioural experiment")
  testthat::expect_equal(exp_cf$treatmentDetails, "Contact data custodian for details")
  testthat::expect_equal(exp_cf$treatmentStartYear, 2014)
  testthat::expect_equal(exp_cf$treatmentEndYear, 2014)
  testthat::expect_equal(exp_cf$recordedBy, NA_character_)
  testthat::expect_equal(exp_cf$reference, NA_character_)

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

  ## Experiment
  test_category_columns(pipeline_output, "Experiment")

}) #Test passed
