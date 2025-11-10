# Script to convert primary data from blue tit BECO studies to SPI-Birds standard format
# following https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf

setwd("../WIL/primarydata/")
path <- "~/GitHub/WIL/primarydata/"


# primary data
primary_brood <- readxl::read_excel(
  paste0(path, "20231024_br_pimpel_CDE.xlsx"),
  col_types = c("numeric", "text", "text","text",
                "text", "text", "text", "date",
                "text", "date", "numeric", "date",
                "numeric", "numeric", "numeric", "numeric",
                "numeric", "numeric", "numeric", "text")) %>%
  rename(PopID=Plot,
         NumberFledged=NumberFledglings,
         BreedingSeason=Sample_year,
         LayDate=LayingDate) %>%
  mutate(LayDate_observed = LayDate,
         calculatedClutchType = calc_clutchtype(., na.rm = FALSE))

primary_capture <- readxl::read_excel(
  paste0(path, "/blue_tit_beco\\", "20240216_vg_pimpel_CDE.xlsx"),
  col_types = c("text", "numeric", "text","text",
                "text", "text", "numeric", "numeric",
                "date", "text", "text", "text",
                "text", "text", "numeric", "numeric",
                "numeric", "text", "text", "text",
                "text", "text", "text")) %>%
  dplyr::rename("Species" = "soort",
                "RecordNumber" = "recnum",
                "BroodID" = "NN",
                "IndividualID" = "KBIN",
                "ColourRing" = "kleur",
                "Sex" = "sex",
                "Age" = "age",
                "CaptureNumber" = "van",
                "CaptureDate" = "datum",
                "plotID" = "plot",
                "NestBoxNumber1" = "nestkast",
                "NestBoxNumber2" = "plaats",
                "CaptureType" = "methode",
                "ObserverID" = "wie",
                "Mass" = "gewicht",
                "WingLength" = "vleugel",
                "Tarsus" = "tarr1",
                "DNAblood" = "DNAbl",
                "DNAfeather" = "DNAveren",
                "BloodSample" = "bloedstaal",
                "SampleNumber" = "staalnr",
                "TagID" = "tagnr",
                "Comments" = "opmerkingen") %>%
  filter(!is.na(IndividualID)) %>%
  distinct()

# Individual data
Individual_data <- primary_capture %>%
  dplyr::arrange(.data$IndividualID, .data$CaptureDate) %>%
  dplyr::group_by(.data$IndividualID) %>%
  dplyr::summarise(speciesID = dplyr::case_when(length(unique(.data$Species)) == 2 ~ "CCCCCC",
                                                length(unique(.data$Species)) == 1 &
                                                  Species=="Cyanistes caeruleus"~ "CYACAE",
                                                TRUE ~ NA_character_),
                   studyID=dplyr::first(.data$plotID),
                   siteID=dplyr::first(.data$plotID),
                   broodIDLaid = dplyr::first(.data$BroodID[Age == 0]),
                   broodIDFledged = dplyr::last(.data$BroodID[Age == 0]),
                   tagYear = lubridate::year(dplyr::first(.data$CaptureDate)),
                   tagMonth = lubridate::month(dplyr::first(.data$CaptureDate)),
                   tagDay = lubridate::day(dplyr::first(.data$CaptureDate)),
                   tagStage = dplyr::case_when(dplyr::first(.data$Age) < 1 ~ "chick",
                                               dplyr::first(.data$Age) >= 1 ~ "adult"),
                   tagSiteID = dplyr::first(.data$siteID),
                   geneticSex = dplyr::case_when(
                     any(DNAblood == 1 | DNAfeather == 1) & 1 %in% Sex & 2 %in% Sex ~ "C",  # Male (1) and Female (2) -> Conflictive (3)
                     any(DNAblood == 1 | DNAfeather == 1) & 1 %in% Sex & 3 %in% Sex ~ "M",  # Male (1) and Unknown (3) -> Male (1)
                     any(DNAblood == 1 | DNAfeather == 1) & 2 %in% Sex & 3 %in% Sex ~ "F",  # Female (2) and Unknown (3) -> Female (2)
                     any(DNAblood == 1 | DNAfeather == 1) ~ first(Sex),                    # Else take the first sex value
                     TRUE ~ NA_character_),  # If neither DNAblood nor DNAfeather is 1
                   calculatedSex = dplyr::case_when(
                     any(DNAblood == 0 | DNAfeather == 0) & 1 %in% Sex & 2 %in% Sex ~ "C",  # Male (1) and Female (2) -> Conflictive (3)
                     any(DNAblood == 0 | DNAfeather == 0) & 1 %in% Sex & 3 %in% Sex ~ "M",  # Male (1) and Unknown (3) -> Male (1)
                     any(DNAblood == 0 | DNAfeather == 0) & 2 %in% Sex & 3 %in% Sex ~ "F",  # Female (2) and Unknown (3) -> Female (2)
                     any(DNAblood == 0 | DNAfeather == 0) ~ first(Sex),                    # Else take the first sex value
                     TRUE ~ NA_character_),  # If neither DNAblood nor DNAfeather is 0
                   .groups = "drop") %>%
  distinct() %>%
  dplyr::mutate(row=row_number()) %>%
  dplyr::select(row, everything()) # reorder columns

write.csv(x = Individual_data, file = paste0(path, "/standard_output\\Individual_data_BECO.csv"), row.names = F)

# Brood data
Brood_data1 <- primary_brood %>%
  dplyr::group_by(.data$BroodID) %>%
  dplyr::summarise(speciesID = dplyr::case_when(length(unique(.data$Species)) == 2 ~ "CCCCCC",
                                                length(unique(.data$Species)) == 1 &
                                                  Species=="Cyanistes caeruleus"~ "CYACAE",
                                                TRUE ~ NA_character_),
                   studyID=PopID,
                   siteID=dplyr::first(.data$PopID),
                   plotID=dplyr::first(.data$PopID),
                   locationID=dplyr::first(.data$PopID),
                   femaleID=FemaleID,
                   maleID=MaleID,
                   observedClutchType=dplyr::case_when(
                     ClutchType==1 ~ "first",
                     ClutchType==2 ~ "second",
                     ClutchType==3 ~ "replacement",
                     TRUE ~ NA_character_),
                   observedLayYear=lubridate::year(LayDate),
                   observedLayMonth=lubridate::month(LayDate),
                   observedLayDay=lubridate::day(LayDate),
                   minimumLayYear=dplyr::first(lubridate::year(LayDate)),
                   minimumLayMonth=dplyr::first(lubridate::month(LayDate)),
                   minimumLayDay=dplyr::first(lubridate::day(LayDate)),
                   maximumLayYear=dplyr::last(lubridate::year(LayDate)),
                   maximumLayMonth=dplyr::last(lubridate::month(LayDate)),
                   maximumLayDay=dplyr::last(lubridate::day(LayDate)),
                   observedClutchSize=ClutchSize,
                   minimumClutchSize=min(ClutchSize),
                   maximumClutchSize=max(ClutchSize),
                   observedHatchYear=dplyr::case_when(
                     NumberFledged>0 ~ BreedingSeason,
                     TRUE ~ NA_integer_),
                   observedHatchMonth=NA_integer_,
                   observedHatchDay=NA_integer_,
                   minimumHatchYear=dplyr::case_when(
                     NumberFledged>0 ~ dplyr::last(lubridate::year(LayDate)),
                     TRUE ~ NA_integer_),
                   minimumHatchMonth=dplyr::case_when(
                     NumberFledged>0 ~ dplyr::last(lubridate::month(LayDate)),
                     TRUE ~ NA_integer_),
                   minimumHatchDay=dplyr::case_when(
                     NumberFledged>0 ~ dplyr::last(lubridate::day(LayDate)),
                     TRUE ~ NA_integer_),
                   observedFledgeYear=dplyr::case_when(
                     NumberFledged>0 ~ BreedingSeason,
                     TRUE ~ NA_integer_),
                   observedFledgeMonth=NA_integer_,
                   observedFledgeDay=NA_integer_,
                   maximumFledgeYear=dplyr::case_when(
                     NumberFledged>0 ~ max(BreedingSeason),
                     TRUE ~ NA_integer_),
                   maximumFledgeMonth=NA_integer_,
                   maximumFledgeDay=NA_integer_,
                   observedNumberFledged=NumberFledged,
                   minimumNumberFledged=min(NumberFledged),
                   maximumNumberFledged=max(NumberFledged),
                   treatmentID=NA_character_,
                   breedingSeason=BreedingSeason,
                   calculatedClutchType = calculatedClutchType,
                  .groups = "drop") %>%
  distinct()


Brood_data2 <- primary_capture %>% # need hatching and fledgling data to create the remaining variables
  dplyr::group_by(.data$BroodID) %>%
  dplyr::summarise(maximumHatchYear=max(lubridate::year(CaptureDate)),
                   maximumHatchMonth=max(lubridate::month(CaptureDate)),
                   maximumHatchDay=max(lubridate::day(CaptureDate)),
                   observedBroodSize=n_distinct(IndividualID),
                   minimumBroodSize=min(n_distinct(IndividualID)),
                   maximumBroodSize=max(n_distinct(IndividualID)),
                   minimumFledgeYear=min(lubridate::year(CaptureDate)),
                   minimumFledgeMonth=min(lubridate::month(CaptureDate)),
                   minimumFledgeDay=min(lubridate::day(CaptureDate)),
                   .groups = "drop") %>%
  distinct()


Brood_data <- left_join(Brood_data1,Brood_data2,by="BroodID") %>%
  dplyr::mutate(row=row_number()) %>%
  dplyr::select(row, BroodID, speciesID, studyID, siteID, plotID, locationID, femaleID, maleID,
                observedClutchType, observedLayYear, observedLayMonth, observedLayDay, minimumLayYear,
                minimumLayMonth, minimumLayDay, maximumLayYear, maximumLayMonth, maximumLayDay,
                observedClutchSize, minimumClutchSize, maximumClutchSize, observedHatchYear,
                observedHatchMonth, observedHatchDay, minimumHatchYear, minimumHatchMonth,
                minimumHatchDay, maximumHatchYear, maximumHatchMonth, maximumHatchDay,
                observedBroodSize, minimumBroodSize, maximumBroodSize, observedFledgeYear,
                observedFledgeMonth, observedFledgeDay, minimumFledgeYear, minimumFledgeMonth,
                minimumFledgeDay, maximumFledgeYear, maximumFledgeMonth, maximumFledgeDay,
                observedNumberFledged, minimumNumberFledged, maximumNumberFledged, treatmentID,
                breedingSeason, calculatedClutchType)

write.csv(x = Brood_data, file = paste0(path, "/standard_output\\Brood_data_BECO.csv"), row.names = F)


# Capture data
Capture_data <- primary_capture %>%
  dplyr::arrange(.data$CaptureDate) %>%
  dplyr::mutate(row=row_number(),
                CaptureID=row_number()) %>%
  dplyr::rename(individualID=IndividualID) %>%
  dplyr::group_by(individualID) %>%
  dplyr::mutate(captureTagID = if_else(row_number() == 1, NA_character_, as.character(individualID)),
                speciesID = dplyr::case_when(length(unique(.data$Species)) == 2 ~ "CCCCCC",
                                             length(unique(.data$Species)) == 1 &
                                               Species=="Cyanistes caeruleus"~ "CYACAE",
                                             TRUE ~ NA_character_)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(releaseTagID=individualID,
                studyID=plotID,
                observedSex = dplyr::case_when(
                  Sex==1 ~ "M",
                  Sex==2 ~ "F",
                  Sex==3 ~ "U",
                  TRUE ~ NA_character_),
                captureYear=lubridate::year(CaptureDate),
                captureMonth=lubridate::month(CaptureDate),
                captureDay=lubridate::day(CaptureDate),
                captureTime=NA_character_,
                recordedBy=ObserverID,
                captureSiteID=plotID,
                releaseSiteID=plotID,
                capturePlotID=plotID,
                releasePlotID=plotID,
                captureLocationID=plotID,
                releaseLocationID=plotID,
                capturePhysical = case_when(
                  CaptureType %in% c("nest", "kast", "mistnet", "DG", "fuik") ~ TRUE,
                  CaptureType %in% c("nc", "gezien", "scan", NA, "other") ~ FALSE,
                  TRUE ~ NA_real_),
                captureAlive = !stringr::str_detect(Comments,
                                           paste(c("dood", "gedood", "dood gevonden", "lag dood",
                                                   "dood in nest", "dood in kast", "dood tijdens",
                                                   "dood bij contrôle", "dood op"), collapse = "|")),
                releaseAlive = case_when(
                  !captureAlive ~ FALSE,  # Already dead when discovered
                  stringr::str_detect(Comments,
                             paste(c("dood", "gedood", "dood gevonden", "lag dood",
                                     "dood in nest", "dood in kast", "dood tijdens",
                                     "dood bij contrôle", "dood op", "gedood door"), collapse = "|")) ~ FALSE, # Died during or after capture
                  TRUE ~ TRUE),  # Otherwise, it was alive at the end of capture
                chickAge=NA_character_, # no information of the age of chicks in days since hatching
               treatmentID=NA_character_,
               exactAge=Age,
               minimumAge=min(Age)) %>%
  dplyr::select(CaptureID, individualID, captureTagID, releaseTagID, speciesID, studyID,
                observedSex, captureYear, captureMonth, captureDay, captureTime,
                recordedBy, captureSiteID, releaseSiteID, capturePlotID, releasePlotID,
                captureLocationID, releaseLocationID, capturePhysical, captureAlive,
                releaseAlive, chickAge, treatmentID, exactAge, minimumAge) %>%
  distinct()

write.csv(x = Capture_data, file = paste0(path, "/standard_output\\Capture_data_BECO.csv"), row.names = F)


# Measurement data
Measurement_data1 <- primary_capture %>%
  distinct() %>%
  dplyr::arrange(.data$CaptureDate) %>%
  dplyr::transmute(
                recordID=row_number(), # recordID should be the same as CaptureID
                studyID=plotID,
                siteID=plotID,
                measurementSubject="capture",
                mass=Mass,
                winglength=WingLength,
                tarsus=Tarsus,
                measurementAccuracy=NA_character_,
                measurementDeterminedYear=year(CaptureDate),
                measurementDeterminedMonth=month(CaptureDate),
                measurementDeterminedDay=day(CaptureDate),
                measurementDeterminedTime=NA_character_,
                recordedBy=ObserverID,
                measurementMethod=NA_character_)

Measurement_data <- pivot_longer(Measurement_data1, cols=c("mass","winglength","tarsus"),
                                  names_to="measurementType",
                                  values_to="measurementValue") %>%
  filter(!is.na(measurementValue)) %>%
  mutate(row=row_number(),
         measurementID=row_number())%>%
  dplyr::select(row, measurementID, recordID, studyID, siteID, measurementSubject, measurementType,
                measurementValue, measurementAccuracy, measurementDeterminedYear, measurementDeterminedMonth,
                measurementDeterminedDay, measurementDeterminedTime, recordedBy, measurementMethod)

write.csv(x = Measurement_data, file = paste0(path, "/standard_output\\Measurement_data_BECO.csv"), row.names = F)


