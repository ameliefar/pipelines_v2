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

  ## Take a subset of only CAC data
  CAC_data <- dplyr::filter(pipeline_output$Brood_data, siteID %in% c("CAC"))


  ## General brood data
  loc98_2010 <- subset(CAC_data,
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
  loc164_2009 <- subset(CAC_data,
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


  #Take a subset of only CAC data
  CAC_data <- dplyr::filter(pipeline_output$Capture_data, captureSiteID %in% c("CAC"))

  ## L007721, female subadult first banded in 2009 at 132
  captured in 131R in 2010
  cap1_EK06579 <- subset(CAC_data,
                         individualID == "EK06579"
                         & captureYear == 2018
                         & captureMonth == 5
                         & captureDay == 29)

  testthat::expect_equal(cap1_EK06579$speciesID, "PARMAJ")
  testthat::expect_equal(cap1_EK06579$captureTagID, NA_character_)
  testthat::expect_equal(cap1_EK06579$releaseTagID, "EK06579")
  testthat::expect_equal(cap1_EK06579$captureID, "EK06579_1")
  testthat::expect_equal(cap1_EK06579$observedSex, NA_character_)
  testthat::expect_equal(cap1_EK06579$captureTime, "10:00")
  testthat::expect_equal(cap1_EK06579$captureLocationID, "Milde_33_NB")
  testthat::expect_equal(cap1_EK06579$releaseLocationID, "Milde_33_NB")
  testthat::expect_equal(cap1_EK06579$capturePlotID, "Milde")
  testthat::expect_equal(cap1_EK06579$releasePlotID, "Milde")
  testthat::expect_equal(cap1_EK06579$releaseSiteID, "BRG")
  testthat::expect_equal(cap1_EK06579$captureAlive, TRUE)
  testthat::expect_equal(cap1_EK06579$releaseAlive, TRUE)
  testthat::expect_equal(cap1_EK06579$capturePhysical, TRUE)
  testthat::expect_equal(cap1_EK06579$minimumAge, 0)
  testthat::expect_equal(cap1_EK06579$exactAge, 0)
  testthat::expect_equal(cap1_EK06579$chickAge, 15)
  testthat::expect_equal(cap1_EK06579$treatmentID, NA_character_)

  ## L007721, female adult recaptured for the third time in 2010 at 131R
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
