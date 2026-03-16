# header ####
## Author: Ashley Marranzino
## Started on: 20251216
## Purpose: Transforming DARC annotation .tsv file into format for submission to OBIS


## SET WORKING DIRECTORY ####

#' clean working environment
rm(list = ls())

#' Working Directory should call location where DARC .tsv files are stored on computer
#' All .tsv files need to conform to the following naming convention: Expedition number + "_DARC_Annotations_Full.tsv"

dir <- "C:/Users/ashley.marranzino/Documents/Annotations"

setwd(dir)

## LOAD REQUIRED PACKAGES ####

if(!require('tidyverse'))install.packages('tidyverse'); library('tidyverse') 
if(!require('worrms'))install.packages('worrms'); library('worrms') 
if(!require("googlesheets4")) install.packages("googlesheets4"); library("googlesheets4")
if(!require("xlsx"))install.packages("xlsx"); library(xlsx)

## DEFINE TERMINOLOGY ####

#' Set standard name to refer to your data (e.g., the expedition number)

data_name <- "EX2306"

#' Fill in the expedition attribution
#' For EX : "This research used data collected during NOAA Ship Okeanos Explorer expedition [insert EX2XXX - expedition name], led by NOAA Ocean Exploration, [insert if applicable: , and executed under ((permit info))]. Taxonomic identifications provided by the Deep-sea Animal Research Center at University of Hawai'i at Manoa."
#' For NA : refer to the OET Publication Policy (https://docs.google.com/document/d/116PiJxLJFnE_pnaVDxmRyW7Ggg9zB7nksQ70n5j5tvQ/edit?tab=t.0#heading=h.ahu5icvlvp1) 

### ~~ FOR OKEANOS ~~ ####
attribution <- paste0("This research used data collected during NOAA Ship Okeanos Explorer expedition ", data_name, ", led by NOAA Ocean Exploration. Taxonomic identifications provided by the Deep-sea Animal Research Center at University of Hawai'i at Manoa.")

### ~~ FOR NAUTILUS ~~ ####
#' For NA : refer to the OET Publication Policy (https://docs.google.com/document/d/116PiJxLJFnE_pnaVDxmRyW7Ggg9zB7nksQ70n5j5tvQ/edit?tab=t.0#heading=h.ahu5icvlvp1) 
# attribution <- 
 
                                             
## READ IN DATA ####

### READ IN AND FORMAT DEPLOYMENT LOCATION DATA ####

#### ~~ FOR OKEANOS DATA 2203 AND AFTER ~~ ####
#' Use the Okeanos Station Log to determine dive locations. 
#' This sheet https://docs.google.com/spreadsheets/d/16UtbBeOUPCodbXdgmlzndIST5b5B9f7A4ygg73BAfFQ/edit?gid=1155543597#gid=1155543597 is the NCEI backend for the Okeonos tracker
#' Use googlesheets4 package to read data directly from the sheet
#' If this is the first time using the package, you will get a prompt to grant authorization to access the sheet
#' This will open a pop up in another window to ask which account you would like to grant access to
#' once authorization has been granted, the data will be read in 
#' 
#' NOTE: the station log only goes back to EX2203. If working with older cruise data, you will need to determine a different way to identify ROV deployment locations

station_log <- read_sheet('https://docs.google.com/spreadsheets/d/16UtbBeOUPCodbXdgmlzndIST5b5B9f7A4ygg73BAfFQ/edit?gid=1155543597#gid=1155543597')

str(station_log)

#' Format the dive_location dataframe so data can be pulled from it to build the Event Table

#' The tibble reads in the Deployment_UTC_DateTime and Recovery_UTC_DateTime as lists
#' But there are errors in the dataframe where some rows are entered in the incorrect format and read in as chr columns
#' These chr entries need to be reformatted into POSIXct format
#' Because these columns are lists, you need to use the map function to drill down into the list and check for character string list entries 

station_log_clean <- station_log |> 
  mutate(Deployment_UTC_DateTime = map(Deployment_UTC_DateTime, \(sub_list){
    map(sub_list, \(x){
      if(is.character(x)){
        return(as.POSIXct (x, tz = "UTC", format = "%m/%d/%Y %H:%M:%S"))
      } 
      return(x)
    }) |> 
      list_simplify() #removes the list of lists created by the map function
  })) |> 
  mutate(Recovery_UTC_DateTime = map(Recovery_UTC_DateTime, \(sub_list){
    map(sub_list, \(x){
      if(is.character(x)){
        return(as.POSIXct (x, tz = "UTC", format = "%m/%d/%Y %H:%M:%S"))
      }
      return(x)
    }) |> 
      list_simplify()
  })) 

str(station_log_clean)

#'now that the columns are in the proper format, unnest them and turn them into a dataframe
station_log_clean <- station_log_clean |> 
  unnest(cols = c(Deployment_UTC_DateTime, Recovery_UTC_DateTime)) |> 
  as.data.frame() 
str(station_log_clean) #check to make sure the dataframe looks okay before proceeding


#' Create a dataframe with the station_log entries of interest for your data_name
cruise_log <- station_log_clean |> 
  filter(CruiseID == data_name) |> 
  mutate (eventID = paste0 (CruiseID, "_", PlatformID)) |>  #use the CruiseID and the PlatformID to create the eventID
  mutate (eventID = case_when(PlatformType == "ROV" ~str_replace(eventID, "ROV", "DIVE"), #modify the eventId to match with the terminology used by AOML so these events align with the eDNA sequence data being submitted
                             PlatformType == "Rosette"  ~str_replace(eventID, "ROS", "CTD"),
                             TRUE ~ eventID)) 
str(cruise_log)

#' the cruise_log dataframe will serve as the backbone for the Event Table

#### ~~ FOR NAUTILUS DATA [INCOMPLETE] ~~ ####

### READ IN SAMPLE LOCATION DATA [INCOMPLETE] ####
#' Sample data will be combined with Annotation data to form the Occurrence Table

### READ IN ANNOTATION DATASET ####
#' Read in the DARC TSV containing All annotations from expedition. 
#' Make sure the file name follows the convention Expedition number + "_DARC_Annotations_Full.tsv"
#' Datasets are the same for EX and Nautilus cruises

annotation_import <- read_tsv(paste0(dir, "/DARC Annotations/", data_name, "_DARC_Annotations_Full.tsv"), na = "-999") 

#' Review annotation import to make sure it looks okay
str(annotation_import)


## VERIFY IDENTIFICATIONS ARE VALID ####

#' Check that the species names present in the annotation_import dataframe are valid in the most current WoRMS taxonomy
#' Create a dataframe of the  unique species names and then query that against the WoRMS API using the WORRMS package

species <- annotation_import %>%
  dplyr::select(c(ScientificName, AphiaID)) %>% 
  dplyr::distinct() %>%
  filter (!is.na(ScientificName))%>% 
  filter (!is.na(AphiaID))

#species_list <- species |> 
#  dplyr::pull (ScientificName)

#worms <- worrms::wm_records_names(name = species_list)

df <- data.frame()

#' This step may take a while since it is running through the entire species list and searching against the WoRMS API
for (i in 1:nrow(species)){
    row <- species[i,]
    worms_record <- worrms::wm_record(species$AphiaID[i])
    
    df <- rbind(df,worms_record)
}

#' check which AphiaIDs are missing 
#' should have 0 missing IDs 
missing_aphias <- which(!species$AphiaID %in% df$AphiaID) #returns a list of the rows that are missing from the df
length(missing_aphias)

#' Check which AphiaIDs are unaccepted
(unaccepted <- df %>%
  filter(status != "accepted"))


length(unaccepted)

#' If there are unaccepted species names, determine if they are identification errors that need to be resolved in the original dataset
#' If not, it is recommended that unaccepted names are left as is so that data submission reflects the original identification provided by DARC.
#' OBIS system has a procedure for updating unaccepted AphiaIDs to currently accepted terminology, so the data will get cleaned on their end if needed. 

#' create a dataframe with the WoRMS details that are wanted in the occurrence table
#' use the AphiaID to repopulate taxonomic ranks and lsid in the occurrence table
taxa_data <- df |> 
  distinct() |> 
  select(c(AphiaID, scientificname, rank, kingdom, phylum, class, order, family, genus, lsid))
  

## BUILD OCCURRENCE TABLE ####

### BUILD OCCURRENCE TABLE FOR ANNOTATIONS ####
occurrence_annotations <- annotation_import |> 
  #mutate(across(where(is.numeric),~na_if(., -999.0))) |>  #add a decimal point to avoid error by passing a double instead of an integer
  #' Create a new column for dive number that will be used to populate eventID
  mutate(diveNumber = case_when(Vessel == "Okeanos Explorer" ~ str_sub(Station,-2,-1),
                                Vessel == "Nautilus" ~ str_sub(Station,-3,-1)))|>
  mutate(eventID = paste0(data_name, "_DIVE", diveNumber)) |> 
  rename (occurrenceID = TrackingID, 
          vernacularName = VernacularName, 
          taxonID = AphiaID, 
          specificEpithet = Species, 
          infraspecificEpithet = Subspecies, 
          verbatimIdentification = CombinedNameID, 
          identificationRemarks = IdentificationComments, 
          identifiedBy = IdentifiedBy, 
          subgenus = Subgenus,
          dateIdentified = IdentificationDate, 
          identificationQualifier = IdentificationQualifier, 
          identificationVerificationStatus = IdentificationVerificationStatus,
          waterBody = Ocean, 
          country = Country, 
          locality = Locality, 
          decimalLatitude = Latitude, 
          decimalLongitude = Longitude, 
          verbatimDepth = DepthInMeters, 
          minimumDepthInMeters = MinimumDepthInMeters, 
          maximumDepthInMeters = MaximumDepthInMeters,
          locationRemarks = LocationComments, 
          parentEventID = SurveyID,
          vitality = Condition, 
          associatedTaxa = AssociatedTaxa, 
          eventRemarks = OccurrenceComments, 
          coordinateUncertaintyInMeters = LocationAccuracy,
          habitat = Habitat, 
          associatedMedia = HighlightImageFilePath, 
          modified = Modified, 
          scientificName = ScientificName, 
          scientificNameAuthorship = ScientificNameAuthorship) |> 
  #remove annotations not associated with an identified taxa
  filter(!is.na(taxonID)) |> 
  #fix organismQuantity so it is populated with IndividualCount if available and CategoricalAbundance if that is populated instead. 
  mutate(organismQuantity = case_when(is.na((IndividualCount)) & is.na(CategoricalAbundance) ~ NA,
                                      is.na(IndividualCount) & !is.na(CategoricalAbundance) ~ as.character(CategoricalAbundance), #need to change both columns to character for code to work
                                      !is.na(IndividualCount) ~ as.character(IndividualCount))) |> 
  #set the organismQuantityType based on if values are populated by IndividualCount or CategoricalAbundance
  mutate(oranismQuantityType = case_when(is.na((IndividualCount)) & is.na(CategoricalAbundance) ~ NA,
                                         is.na(IndividualCount) & !is.na(CategoricalAbundance) ~ "approximate number of individuals", 
                                         !is.na(IndividualCount) ~ "number of individuals")) |> 
  #populate eventDate by concatenating ObservationDate and ObservationTime
  mutate(eventDate = ymd(ObservationDate) +hms(ObservationTime))|>
  mutate(eventDate = as.POSIXlt(eventDate, tz = "UTC",format= " %Y-%m-%d %H:%M:%OS")) |>  # final format needs to be  ISO 8601: YYYY-MM-DDTHH:MM:SSZ 
  mutate (occurrenceStatus = "present", 
          basisOfRecord = "MachineObservation",
          eventType = "Observation", 
          samplingProtocol = "ROV dive",
          fundingAttribution = attribution,
          scientificName = (str_remove(scientificName, " sp."))) |> 
  #remove unnecessary columns
  select (-c(SampleID, 
             Citation, 
             Repository, 
             VernacularNameCategory, 
             Phylum, 
             Class, 
             Subclass, 
             Order, 
             Suborder, 
             Family, 
             Subfamily,
             Genus, 
             Morphospecies, 
             Synonyms, 
             LargeMarineEcosystem, 
             FishCouncilRegion, 
             DepthMethod, 
             ObservationDate, 
             ObservationTime,
             Vessel,
             PI, 
             PIAffiliation, 
             Purpose, 
             SurveyComments, 
             Station, 
             SamplingEquipment, 
             VehicleName, 
             SampleAreaInSquareMeters, 
             Density, 
             Cover, 
             VerbatimSize,
             MinimumSize,
             MaximumSize,
             WeightInKg,
             NavType,
             OtherData,
             Substrate,
             CMECSGeoForm,
             Temperature,
             Salinity, 
             Oxygen,
             RecordType, 
             ImageFilePath, 
             WebSite,
             EntryDate, 
             Reporter,
             ReporterEmail, 
             ReporterComments, 
             EventID, 
             diveNumber, 
             IndividualCount,
             CategoricalAbundance, 
             TaxonRank, 
             LifeScienceIdentifier)) |> 
   #add in the taxonomic details from WoRMS by joining species dataframe (taxon_data) to this one
  left_join(y = taxa_data, by = join_by(taxonID == AphiaID), relationship = "many-to-one") |>  #do not define x - it will keep the pipe from working unless the occurrence df is created in advance
  #rename new columns as need
  rename(taxonRank = rank, scientificNameID = lsid) |> 
  #reorder columns 
  relocate (eventID, parentEventID, .after = occurrenceID) |> 
  relocate (subgenus, infraspecificEpithet, specificEpithet, .after = genus) |> 
  relocate (scientificName : scientificNameID, .before = vernacularName) |> 
  relocate (scientificNameID, .after = scientificName) 

#' Build a list of the rows that contain samples and their corresponding sample and occurrenceIDs
sample_ids <- occurrence_annotations |>
  filter(str_detect(occurrenceID, data_name)) |> 
  separate_wider_delim(cols = occurrenceID, delim = " | ", names = c("occurrenceID", "otherCatalogNumbers")) |> 
  select(occurrenceID, otherCatalogNumbers) |> 
  mutate(otherCatalogNumbers = gsub(otherCatalogNumbers, pattern = "; ", replacement = " | ")) 

#' Remove the pipes from occurrenceID column
#' First check if there are any records that are NOT samples
test<- occurrence_annotations |> 
  filter(str_detect(occurrenceID, " | ")) |> 
  filter(!str_detect(occurrenceID, data_name))
#'If any of the occurrenceIDs have additional information that is not a sample name, 
#'print those and inspect them to determine if they contain information that needs to be kept, or if the text can be deleted
if(nrow(test)>0){
  print(test$occurrenceID)
} else {
  print("No pipes in occurrenceIDs")
}

#' Once you determine you can delete text, remove data after the | in the occurrence record 
occurrence_annotations <- occurrence_annotations |> 
  mutate(occurrenceID = str_remove(occurrenceID, " \\|.*")) #remove anything after the | for annotations that have more than once entry


#' Now we add more details about other catalog numbers for physical samples
#' Can add in the short specimen ID from the SODA catalog
#' First need to read in the sample data - also included in the event table portion below
samples <- xlsx::read.xlsx(paste0(dir, "/Sample Data/", data_name, "_Cruise_Specimens.xlsx"), sheetIndex = 1) 

#'Now add the Shortened.Specimen.ID and Accession.Number columns to the sample_ids dataframe
sample_ids <- sample_ids |> 
  left_join(samples, by = join_by (otherCatalogNumbers == Specimen.ID)) |> 
  select(c(occurrenceID, otherCatalogNumbers, Shortened.Specimen.ID, Accession.Number, Smithsonian.Public.Link)) |> 
 #if there is a shortened specimen ID, add that to the otherCatalogNumbers - this is what is used in the eventTable and used in eDNA submissions
   mutate (otherCatalogNumbers = case_when(!is.na(Shortened.Specimen.ID) ~ paste0 (otherCatalogNumbers, " | ", Shortened.Specimen.ID),
                                          TRUE ~ otherCatalogNumbers),
          #if there are any smithsonian catalog numbers and links, add those into the relatedResourceID column (this may not be populated for recent expeditions)
           relatedResourceID = case_when(!is.na(Accession.Number) & !is.na (Smithsonian.Public.Link) ~ paste0("SMNMH ", Accession.Number, " | ", Smithsonian.Public.Link),
                                        !is.na(Accession.Number) & is.na(Smithsonian.Public.Link) ~ paste0("SMNMH ", Accession.Number),
                                        is.na(Accession.Number) & !is.na(Smithsonian.Public.Link) ~ Smithsonian.Public.Link,
                                        TRUE ~ NA),
          relatedResourceType = case_when(!is.na(relatedResourceID) ~"PreservedSpecimen",
                                          TRUE ~ NA)) |> 
  select(-c(Shortened.Specimen.ID, Accession.Number, Smithsonian.Public.Link))

#'Now add the sample_ids into the occurrence_annotations dataframe
occurrence_annotations <- occurrence_annotations |> 
  left_join(sample_ids)
#'Check to make sure the join worked - resulting data frame should have long and short EX specimen IDs at the very least         
test <- occurrence_annotations |> 
  filter(!is.na(otherCatalogNumbers))

#' Now check if the scientific names in the occurrence_annotations dataframe match WoRMS database
scientificnames <- occurrence_annotations |> 
  mutate(WoRMS = scientificName %in% scientificname) |> 
  filter (WoRMS = FALSE)
nrow(scientificnames)

if  (nrow(scientificnames)>0){
  print(scientificnames)
  } else {
    print("Scintific names match")
    }

#' remove the redundant scientific name column once you verify all names match
occurrence_annotations <- occurrence_annotations |> 
  select(-scientificname)


 

### BUILD OCCURRENCE TABLE FOR SAMPLES [incomplete] ####
#' Determine how to add in catalog Numbers and smithsonian ID for samples collected - 
#' may need to create a separate dataframe for samples and bind to the occurrence table here
#' may make the most sense to build a separate sample dataframe from SODA exports and then merge with this table
#' ~ISSUE WITH SAMPLES IN ANNOTATION DATA
#' DARC annotations have multiple annotations for a single sample event. To avoid redundancy, for EX data it will be better to use SODA export. Could use SODA time and ID?
#' duplicated annotations do not have the sampe ID (some are IDed to finer resolution - need to be able to account for that)
#' there are also some errors of incorrectly named samples - prefer to use SODA export since that has more extensive QA/QC
#' 
### FOR OKEANOS #####

#DO NOt NEED THIS - just put samples in event table and add in data with EMOF


#may need to filter out geo and water and only keep bio BUT ADD ALL to the event table
#make a dataframe of the sample data from the original annotation import for more accurate species IDs


#replicate the format of the occurrence_annotations dataframe 
# occurrence_samples <- samples |> 
#   #only keep biological samples
#   filter(str_ends(Shortened.Specimen.ID, "B")) |> 
#   #match the columns of the occurrence_annotations dataframe
#   mutate(eventID = paste0(CruiseData_ID, "_", Dive.ID),
#          verbatimIdentification = case_when (!is.na(Field.ID) & !is.na(Specimen.Comments) & !is.na(Lab.Identification) ~ paste0(Field.ID " | ", Specimen.Comments, " | ", Lab.Identification),
#                                              !is.na(Field.ID) & !is.na(Lab.Identification) & is.na(Specimen.Comments) ~ paste0(Field.ID, " | ", Lab.Identification),
#                                              !is.na(Field.ID) & !is.na(Specimen.Comments) & is.na(Lab.Identification) ~ paste0(Field.ID, " | ", Specimen.Comments),
#                                              !is.na(Field.ID) & is.na(Specimen.Comments) & is.na(Lab.Identification) ~ Field.ID,
#                                              is.na(Field.ID) & !is.na(Specimen.Comments) & !is.na(Lab.Identification) ~ paste0(Specimen.Comments, " | ", Lab.Identification),
#                                              is.na(Field.ID) & !is.na(Specimen.Comments) & is.na(Lab.Identification) ~ Specimen.Comments, 
#                                              is.na(Field.ID) & is.na(Specimen.Comments) & !is.na(Lab.Identification) ~ Lab.Identification),
#          identificationRemarks = NA,
#          identifiedBy = case_when(!is.na(Field.ID.By) & is.na(Lab.Identification.by) ~ Field.ID.By,
#                                   !is.na(Field.ID.By) & !is.na(Lab.Identification.by) ~ paste0(Field.ID.By, " | ", Lab.Identification.by),
#                                   is.na(Field.ID.By) & !is.na(Lab.Identification.by) ~ Lab.Identification.by, 
#                                   is.na(Field.ID.By) & is.na(Lab.Identification.by) ~ NA),
#          dateIdentified = NA,
#          identificationQulifier = case_when(!is.na(Field.ID) & !is.na(Specimen.Comments) & !is.na(Lab.Identification) ~ "Field ID | Lab ID",
#                                             !is.na(Field.ID) & !is.na(Lab.Identification) & is.na(Specimen.Comments) ~ "Field ID | Lab ID",
#                                             !is.na(Field.ID) & !is.na(Specimen.Comments) & is.na(Lab.Identification) ~ "Field ID",
#                                             !is.na(Field.ID) & is.na(Specimen.Comments) & is.na(Lab.Identification) ~ "Field ID",
#                                             is.na(Field.ID) & !is.na(Specimen.Comments) & !is.na(Lab.Identification) ~ "Lab ID",
#                                             is.na(Field.ID) & !is.na(Specimen.Comments) & is.na(Lab.Identification) ~ "Field ID", 
#                                             is.na(Field.ID) & is.na(Specimen.Comments) & !is.na(Lab.Identification) ~ "Lab ID"),
#          identificationVerificationStatus = 0, 
#          waterBody = NA,
#          country =
                                             )|> 
# rename(occurrenceID = Shortened.Specimen.ID, 
#        parentEventID = CruiseData_ID
#        verbatimIdentification = 


#' The following code is to create a sample log from DARC annotations - but would prefer to use SODA log instead
# samples <- annotation_import |>
#   #pull only annotations for samples into a dataframe
#   filter(str_detect(OccurrenceComments, regex("sample", ignore_case = TRUE))) |>
#   #replicate the dataframe format of the occurrence dataframe
#   mutate(across(where(is.numeric),~na_if(., -999.0))) |>  #add a decimal point to avoid error by passing a double instead of an integer
#   #Create a new column for dive number that will be used to populate eventID
#   mutate(diveNumber = case_when(Vessel == "Okeanos Explorer" ~ str_sub(Station,-2,-1),
#                                 Vessel == "Nautilus" ~ str_sub(Station,-3,-1)))|>
#   mutate(eventID = paste0(data_name, "_DIVE", diveNumber)) |>
#   rename (occurrenceID = TrackingID, vernacularName = VernacularName, taxonID = AphiaID, specificEpithet = Species, infraspecificEpithet = Subspecies,
#           verbatimIdentification = CombinedNameID, identificationRemarks = IdentificationComments, identifiedBy = IdentifiedBy, subgenus = Subgenus,
#           dateIdentified = IdentificationDate, identificationQualifier = IdentificationQualifier, identificationVerificationStatus = IdentificationVerificationStatus,
#           waterBody = Ocean, country = Country, locality = Locality, decimalLatitude = Latitude, decimalLongitude = Longitude, verbatimDepth = DepthInMeters,
#           minimumDepthInMeters = MinimumDepthInMeters, maximumDepthInMeters = MaximumDepthInMeters, locationRemarks = LocationComments, parentEventID = SurveyID,
#           vitality = Condition, associatedTaxa = AssociatedTaxa, eventRemarks = OccurrenceComments, coordinateUncertaintyInMeters = LocationAccuracy,
#           habitat = Habitat, associatedMedia = HighlightImageFilePath, modified = Modified) |>
#   #fix organismQuantity so it is populated with IndividualCount if available and CategoricalAbundance if that is populated instead.
#   mutate(organismQuantity = case_when(is.na((IndividualCount)) & is.na(CategoricalAbundance) ~ NA,
#                                       is.na(IndividualCount) & !is.na(CategoricalAbundance) ~ as.character(CategoricalAbundance), #need to change both columns to character for code to work
#                                       !is.na(IndividualCount) ~ as.character(IndividualCount))) |>
#   #set the organismQuantityType based on if values are populated by IndividualCount or CategoricalAbundance
#   mutate(oranismQuantityType = case_when(is.na((IndividualCount)) & is.na(CategoricalAbundance) ~ NA,
#                                          is.na(IndividualCount) & !is.na(CategoricalAbundance) ~ "approximate number of individuals",
#                                          !is.na(IndividualCount) ~ "number of individuals")) |>
#   #populate eventDate by concatenating ObservationDate and ObservationTime
#   mutate(eventDate = ymd(ObservationDate) +hms(ObservationTime))|>
#   mutate(eventDate = as.POSIXlt(eventDate, tz = "UTC",format= " %Y-%m-%d %H:%M:%OS")) |>  # final format needs to be  ISO 8601: YYYY-MM-DDTHH:MM:SSZ
#   mutate (occurrenceStatus = "present", basisOfRecord = "MachineObservation", eventType = "Observation", samplingProtocol = "ROV dive",
#           fundingAttribution = attribution,
#           scientificName = paste0(str_remove(ScientificName, "sp."), " ", ScientificNameAuthorship)) |>
#   #remove unnecessary columns
#   select (-c(SampleID, Citation, Repository, VernacularNameCategory, Phylum, Class, Subclass, Order, Suborder, Family, Subfamily, Genus, ScientificNameAuthorship, Morphospecies, Synonyms,
#              LargeMarineEcosystem, FishCouncilRegion, DepthMethod, ObservationDate, ObservationTime, Vessel, PI, PIAffiliation, Purpose, SurveyComments, Station, SamplingEquipment,
#              VehicleName, SampleAreaInSquareMeters, Density, Cover, VerbatimSize, MinimumSize, MaximumSize, WeightInKg, NavType, OtherData, Substrate, CMECSGeoForm, Temperature,
#              Salinity, Oxygen, RecordType, ImageFilePath, WebSite, EntryDate, Reporter, ReporterEmail, ReporterComments, ScientificName, EventID, diveNumber, IndividualCount,
#              CategoricalAbundance, TaxonRank, LifeScienceIdentifier)) |>
#   #add in the taxonomic details from WoRMS by joining species dataframe (taxon_data) to this one
#   left_join(y = taxa_data, by = join_by(taxonID == AphiaID), relationship = "many-to-one") |>  #do not define x - it will keep the pipe from working unless the occurrence df is created in advance
#   #rename new columns as need
#   rename(taxonRank = rank, scientificNameID = lsid) |>
#   #reorder columns
#   relocate (eventID, parentEventID, .after = occurrenceID) |>
#   relocate (subgenus, infraspecificEpithet, specificEpithet, .after = genus) |>
#   relocate (scientificName : scientificNameID, .before = vernacularName) |>
#   relocate (scientificNameID, .after = scientificName) |>
#   #keep only the sample number as the occurrence ID
#   mutate(occurrenceID = str_remove(occurrenceID, ".*\\|\\s*")) |> #.* - matches any character (.) zero or more times (*); \\| tells R that the pipe is a character, not an "OR"; \\s removes white space after pipe
#   mutate(occurrenceID = str_replace(string = occurrenceID, pattern = "^.*_D2", replacement = as.character(data_name))) |>
#   mutate(occurrenceID = str_replace(occurrenceID, "DIVE", "D")) |>
#   mutate(occurrenceID = str_remove(occurrenceID, "SPEC")) |>
#   mutate(occurrenceID = str_replace(occurrenceID, "BIO", "B")) |>
#   mutate(occurrenceID = str_replace(occurrenceID, "GEO", "G")) |>
#   mutate(occurrenceID = str_replace(occurrenceID, "WAT", "W")) |> 
#   filter(!str_detect(occurrenceID, "BLW")) #remove any "blank" water samples since those are not actual collections
# 

#' MAYBE WILL MAKE MOST SENSE TO PULL OUT SODA RECORDS, CREATE A DATAFRAME FOR SODA, 
#' THEN CHECK FOR SMITHSONIAN IDS AND ADD IN VIA A JOIN - 
#' THIS WILL REQUIRE ADDING COL(S) TO THE SAMPLE AND occurrence DFS
# 
# test <- samples %>%
#   #filter(!str_detect(occurrenceID, "EX2306"))
#   #filter(duplicated(.))
#   group_by(occurrenceID) |> 
#   filter(n()>1) |>
#   ungroup()
# 
# print(samples[240,"eventRemarks"])

### FOR NAUTILUS (INCOMPLETE) #####
#' ~NOTE: 
#' we do not have a list of Nautilus samples like we do for SODA, so we will need to rely on DARC annotations for those - 
#' do we want to have standardized for all datasets? Or use SODA and merge based on "ShortSpec_ID"
#' For Nautilus - do we just want to not include? Ask them for sample data log to be included later?
#' 
#'



## BUILD EVENT TABLE ####

## EVENT TABLE FOR DEPLOYMENTS #####
#' Event Table will be built off of the cruise_log dataframe, which contains details about each platform deployment on a cruise
#' Event Table should include all information about the sampling event, such as date, location, depth, sampling protocol, etc.
#' #' Event Core tables must have eventID, eventDate, decimalLatitude, decimalLongitude, countryCode (required for GBIF), and geodeticDatum (should be in WGS84)
#' 
#' For more details: https://ioos.github.io/bio_mobilization_workshop/04-create-schema.html#event-core-with-extended-measurement-or-fact-extension, https://dwc.tdwg.org/list/ 
#'
#' NOTE: The following code will only work for EX Cruises
#' For Nautilus cruises, some values will need to be changed 

  event <- cruise_log |>
  filter(PlatformType %in% c("ROV", "Rosette")) |> #only keep ROV dive and Rosette categories for now - determine if additional deployment types are desired and include those at a later date
  rename(parentEventID = CruiseID, 
          eventDate = Deployment_UTC_DateTime,
          decimalLatitude = Deployment_Latitude_dd,
          decimalLongitude = Deployment_Longitude_dd,
          eventRemarks = Comments,
          minimumElevationInMeters = MinDepth_m,
          maximumElevationInMeters = MaxDepth_m) |> 
  mutate(institutionID = "https://ror.org/05xqpda80",
         institutionCode = "NOAA Ocean Exploration",
         eventType = "Survey",
         geodeticDatum = "WGS84",
         fundingAttribution = "NOAA Ocean Exploration",  ##NOTE: May need to modify this for non-EX cruises if additional partners funded the expedition
         fundingAttributionID = "https://ror.org/05xqpda80", ##NOTE: May need to modify this for non-EX cruises if additional partners funded the expedition
         samplingProtocol = case_when(PlatformType == "ROV" ~ "Remotely Operated Vehicle (ROV) dive, https://doi.org/10.25923/n605-za83", #provides link to the ROV manual in the IR
                                      PlatformType == "Rosette" ~ "CTD Rosette deployment")) |>  
  select(c(eventID, 
         parentEventID,
         eventType,
         eventDate,
         decimalLatitude,
         decimalLongitude,
         minimumElevationInMeters,
         maximumElevationInMeters,
         samplingProtocol,
         eventRemarks,
         institutionID,
         institutionCode,
         fundingAttribution,
         fundingAttributionID,
         geodeticDatum))
 
## EVENT TABLE FOR SAMPLES #####
 # Create another table with sampling events to add to the event table
       
 #' Ingest sample data from last SODA export (note: this will only work for expeditions pre 2026)
 #' Download the SODA sample export "EX2XXXX_Cruise_Specimens.xls" from the NCEI archive at the following site:
 #' https://www.ncei.noaa.gov/data/oceans/archive/arc0241/0311845/1.1/data/0-data/EX_SODA_Archive_September_2025/exports/
 #' 
  
 samples <- xlsx::read.xlsx(paste0(dir, "/Sample Data/", data_name, "_Cruise_Specimens.xlsx"), sheetIndex = 1)
 str(samples)
       
       
event_samples <- samples |> 
  mutate(parentEventID =  paste0(CruiseData_ID, "_", Dive.ID),
         eventType = "Sample",
         minimumElevationInMeters = -Depth..m.,
         maximumElevationInMeters = -Depth..m., 
         samplingProtocol = "Remotely Operated Vehicle (ROV) dive, https://doi.org/10.25923/n605-za83",
         eventRemarks = case_when(Collection.Reason != "Not Applicable" & !is.na(Field.ID) ~ paste0(Collection.Reason, " - ", Field.ID), 
                                  Collection.Reason != "Not Applicable" & is.na(Field.ID) ~ Collection.Reason,
                                  TRUE ~ Field.ID),
         institutionID = "https://ror.org/05xqpda80",
         institutionCode = "NOAA Ocean Exploration",
         fundingAttribution = "NOAA Ocean Exploration",  ##NOTE: May need to modify this for non-EX cruises if additional partners funded the expedition
         fundingAttributionID = "https://ror.org/05xqpda80", ##NOTE: May need to modify this for non-EX cruises if additional partners funded the expedition)
         geodeticDatum = "WGS84", 
         eventDate = lubridate::ymd_hms(paste0(CollectionDate, " ", CollectionTime..UTC.), tz = "UTC")) |> 
  rename(eventID = Shortened.Specimen.ID,
         decimalLatitude = Latitude..Dec.Deg.,
         decimalLongitude = Longitude..Dec.Deg.) |> 
  select(c(eventID, 
           parentEventID,
           eventType,
           eventDate,
           decimalLatitude,
           decimalLongitude,
           minimumElevationInMeters,
           maximumElevationInMeters,
           samplingProtocol,
           eventRemarks,
           institutionID,
           institutionCode,
           fundingAttribution,
           fundingAttributionID,
           geodeticDatum))

#Combine the sample events with the full event dataframe
event<- rbind(event, event_samples)
      
## BUILD  EMOF [incomplete] ####
#' The EMOF (extended measurement or fact) table is an extension within the Event Core. EMOFs provide additional information about an occurrence
#' Within the EMOF, measurmentValue, measurementType, and measurmentUnit are condensed into their own columns (respectively)
#' The EMOF can contain extended measurments of fact for both events AND occurrences


### BUILD EMOF FOR ANNOTATIONS ####
emof_annotation <- annotation_import |> 
  #mutate(across(where(is.numeric),~na_if(., -999.0))) |>  #add a decimal point to avoid error by passing a double instead of an integer
  #Create a new column for dive number that will be used to populate eventID
  mutate(diveNumber = case_when(Vessel == "Okeanos Explorer" ~ str_sub(Station,-2,-1),
                                Vessel == "Nautilus" ~ str_sub(Station,-3,-1)))|>
  mutate(eventID = paste0(data_name, "_DIVE", diveNumber)) |> 
  filter(!is.na(AphiaID)) |> 
  mutate(eventID = paste0(data_name, "_DIVE", diveNumber)) |> 
  rename (occurrenceID = TrackingID, parentEventID = SurveyID) |> 
  #remove annotations for samples (add those in later)
  filter(!str_detect(occurrenceID, data_name)) |> 
  #remove text after | if present to make consistent with occurenceIDs in occurrence table
  select (occurrenceID, eventID, parentEventID, Vessel, SamplingEquipment, VehicleName, Temperature, Salinity, Oxygen) |> 
  mutate(across(everything(), as.character)) |> 
  pivot_longer(Vessel:Oxygen, names_to = "measurementType", values_to = "measurementValue") |> 
  mutate(measurementType = str_replace(measurementType, "SamplingEquipment", "Platform type")) |> 
  mutate(measurementType = str_replace(measurementType, "VehicleName", "Platform Name")) |> 
  mutate(measurementType = str_replace(measurementType, "Temperature", "temperature")) |> 
  mutate(measurementType = str_replace(measurementType, "Salinity", "salinity")) |> 
  mutate(measurementType = str_replace(measurementType, "Oxygen", "oxygenConcentration")) |> 
  mutate(measurementUnits = case_when(measurementType == "temperature" ~ "degrees celcius",
                                      measurementType == "salinity" ~ "PSU",
                                      measurementType == "oxygenConcentration" ~ "ml/L"),
         measurementTypeID = case_when(measurementType == "Vessel" ~ "https://vocab.nerc.ac.uk/collection/C17/current/334A/",
                                       measurementType == "Platform type" ~ "https://vocab.nerc.ac.uk/collection/L06/current/20/ | https://mmisw.org/ont/ioos/platform/submersible",
                                       measurementType == "temperature" ~ "https://vocab.nerc.ac.uk/collection/P01/current/TEMPCU01/ | https://vocab.nerc.ac.uk/collection/P06/current/UPAA/",
                                       measurementType == "salinity" ~ "https://vocab.nerc.ac.uk/collection/P01/current/ODSDM021/ | https://vocab.nerc.ac.uk/collection/P06/current/",
                                       measurementType == "oxygenConcentration" ~ "https://vocab.nerc.ac.uk/collection/P01/current/DOXYUCKG/ | https://vocab.nerc.ac.uk/collection/P06/current/UMLL/"),
         measurementValue = str_replace(measurementValue, "Okeanos Explorer", "NOAA Ship Okeanos Explorer"),
         measurementValue = str_replace(measurementValue, "Nautilus", "Exploration Vessel Nautilus"),
         measurementValue = str_replace(measurementValue, "ROV", "submersible"),
         measurementValue = str_replace(measurementValue, "Deep Discoverer", "ROV Deep Discoverer"),
         measurementValue = str_replace(measurementValue, "Hercules", "ROV Hercules"))

### BUILD EMOF FOR SAMPLES ####
emof_samples <- samples |> 
  mutate(parentEventID =  paste0(CruiseData_ID, "_", Dive.ID)) |> 
  rename(eventID = Shortened.Specimen.ID,
         ) |> 
  select(eventID, parentEventID, Vessel, SamplingEquipment, VehicleName, Temperature..Deg.C., Salinity..psu., Dissolved.Oxygen..mg.l.) |> 
  mutate(across(everything(), as.character)) |> 
  pivot_longer(Vessel:Dissolved.Oxygen..mg.l., names_to = "measurementType", values_to = "measurementValue") |> 
  mutate(measurementType = str_replace(measurementType, "SamplingEquipment", "Platform type")) |> 
  mutate(measurementType = str_replace(measurementType, "VehicleName", "Platform Name")) |> 
  mutate(measurementType = str_replace(measurementType, "Temperature..Deg.C.", "temperature")) |> 
  mutate(measurementType = str_replace(measurementType, "Salinity..psu", "salinity")) |> 
  mutate(measurementType = str_replace(measurementType, "Dissolved.Oxygen..mg.l.", "oxygenConcentration")) |> 
  mutate(measurementUnits = case_when(measurementType == "temperature" ~ "degrees celcius",
                                      measurementType == "salinity" ~ "PSU",
                                      measurementType == "oxygenConcentration" ~ "mg/L"),
         measurementTypeID = case_when(measurementType == "Vessel" ~ "https://vocab.nerc.ac.uk/collection/C17/current/334A/",
                                       measurementType == "Platform type" ~ "https://vocab.nerc.ac.uk/collection/L06/current/20/ | https://mmisw.org/ont/ioos/platform/submersible",
                                       measurementType == "temperature" ~ "https://vocab.nerc.ac.uk/collection/P01/current/TEMPCU01/ | https://vocab.nerc.ac.uk/collection/P06/current/UPAA/",
                                       measurementType == "salinity" ~ "https://vocab.nerc.ac.uk/collection/P01/current/ODSDM021/ | https://vocab.nerc.ac.uk/collection/P06/current/",
                                       measurementType == "oxygenConcentration" ~ "https://vocab.nerc.ac.uk/collection/P01/current/DOXYUCKG/ | https://vocab.nerc.ac.uk/collection/P06/current/UMGL/"),
         measurementValue = str_replace(measurementValue, "Okeanos Explorer", "NOAA Ship Okeanos Explorer"),
         measurementValue = str_replace(measurementValue, "ROV", "submersible"),
         measurementValue = str_replace(measurementValue, "Deep Discoverer", "ROV Deep Discoverer")) |> 
  mutate(occurrenceID = NA) |> 
  relocate(occurrenceID, .before = eventID)


### BUILD EMOF FOR EVENTS [incomplete] ####
#'Use data in the cruise log to build the emof_event dataframe and provide additional details about events (e.g., platform type, vessel name)

emof_event <- cruise_log |> 
  filter(PlatformType %in% c("ROV", "Rosette")) |> #only keep ROV dive and Rosette categories for now - determine if additional deployment types are desired and include those at a later date
  rename(parentEventID = CruiseID) |> 
  select (eventID, parentEventID, Vessel, PlatformType, PlatformName) |> 
  pivot_longer(Vessel:PlatformName, names_to = "measurementType", values_to = "measurementValue") |> 
  mutate(measurementType = str_replace(measurementType, "PlatformType", "Platform type")) |> 
  mutate(measurementType = str_replace(measurementType, "PlatformName", "Platform name")) |> 
  mutate(measurementTypeID = case_when(measurementType == "Vessel" ~ "https://vocab.nerc.ac.uk/collection/C17/current/334A/",
                                measurementValue == "ROV Deep Discoverer" ~ "https://vocab.nerc.ac.uk/collection/L06/current/20/ | https://mmisw.org/ont/ioos/platform/submersible",
                                measurementType == "Platform type" & is.na(measurementValue) ~ "https://vocab.nerc.ac.uk/collection/L05/current/130/ | https://vocab.nerc.ac.uk/collection/L05/current/30/")) |> 
  mutate(measurementValue = case_when (measurementValue == "ROV" ~ str_replace(measurementValue, "ROV", "submersible"),
                                       TRUE ~ measurementValue),
         measurementUnits = NA,
         occurrenceID = NA) |> 
  relocate(measurementTypeID, .after = last_col()) |> 
  relocate(occurrenceID, .before = eventID)
  
         
  
#### COMBINE ANNOTATION, SAMPLE AND EVENT EMOFS ####

emof <- rbind(emof_annotation, emof_event, emof_samples)

### EXPORT DATAFRAMES ####

#Create a directory 
dir.create(paste0(dir,"/Exports/OBIS"))
dir.create(paste0(dir, "/Exports/OBIS/", data_name))

write.csv(x = occurrence_annotations, file = file.path(paste0(dir, "/Exports/OBIS/", data_name), paste0(data_name, "_occurrenceTable.csv")), row.names = FALSE)
write.csv(x = emof, file = file.path(paste0(dir, "/Exports/OBIS/", data_name), paste0(data_name, "_emofTable.csv")), row.names = FALSE) 
write.csv(x = event, file = file.path(paste0(dir, "/Exports/OBIS/", data_name), paste0(data_name, "_eventTable.csv")), row.names = FALSE)   

#

