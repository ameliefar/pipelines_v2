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


  #Take a subset of only BRG data
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
