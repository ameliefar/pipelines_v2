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
  testthat::expect_equal(kpn38_2017$nestAttemptNumber, 1) #SHOULD BE "2" as this is a true second attempt
}
)
