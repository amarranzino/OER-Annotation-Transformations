#### header ####
## Author: Ashley Marranzino
## Started on: 20250818
## Purpose: Transforming DARC annotation .txt file into format for ingestion for the Benthic Animal Guide

#### Set Working Directory ####
## Working Directory should call location where DARC .txt files are stored on computer
## All .txt files need to conform to the following naming convention: Expedition number + "_DARC_Annotations_BAG.txt"

wd <- "C:/Users/ashley.marranzino/Documents/Annotations/DARC Annotations"

setwd(wd)

#### Required Packages ####

if(!require('tidyverse'))install.packages('tidyverse'); library('tidyverse') 
if(!require('worrms'))install.packages('worrms'); library('worrms') 



#FILL IN EXPEDITION DETAILS ----------------------------------------------------------
#NOTE:  THESE VALUES ARE EXPEDITION SPECIFIC AND MUST BE CHANGED FOR EACH DATASET


#set standard name to refer to your data (e.g., the expedition number)

data_name <- "EX2301"


#change this based on where expedition occurred. 
#Use either "Pacific", "Atlantic", or "Alaska". If new regions are used, work with NCEI to deterimine new names 
locality <- "Pacific"  

#Set SeaTube URLs for each dive. Refer to Seatube (https://data.oceannetworks.ca/ExpeditionManagement) and check the URL for each video playback

#create a dataframe containing Seatube links
#If videos are not on SeaTube, fill DIVE_VIDEO_URL with NA
seatube <- data.frame(DiveNumber = c("01", "02", "03", "04", "05", "06", "07", "08", "09"), #create a list of dive numbers for the expedition
                      DIVE_VIDEO_URL = c("https://data.oceannetworks.ca/app/dive-logs/2753", #Dive01 URL
                               "https://data.oceannetworks.ca/app/dive-logs/2763", #Dive02 URL
                               "https://data.oceannetworks.ca/app/dive-logs/2773", #Dive03 URL
                               "https://data.oceannetworks.ca/app/dive-logs/2783", #Dive04 URL
                               "https://data.oceannetworks.ca/app/dive-logs/2793", #Dive05 URL
                               "https://data.oceannetworks.ca/app/dive-logs/2803", #Dive06 URL
                               "https://data.oceannetworks.ca/app/dive-logs/2813", #Dive07 URL
                               "https://data.oceannetworks.ca/app/dive-logs/2823", #Dive08 URL
                               "https://data.oceannetworks.ca/app/dive-logs/2833" ))  #Dive09 URL

#Set the attribution statement. For Okeanos data, use the following statement. For Nautilus data use the expedition specific statement. 
attribution <- paste0("This research used data collected during NOAA Ship Okeanos Explorer expedition ", data_name, ", led by NOAA Ocean Exploration. Taxonomic identifications provided by the Deep-sea Animal Research Center at the University of Hawai'i at Manoa.")

head(seatube)

#READ IN DATA ---------------------------------------------------------------------------------

#Read in the DARC Txt containing All annotations from expedition. Make sure the file name follows the convention Expedition number + "_DARC_Annotations_BAG.txt"

annotation_import <- read.delim(paste0(wd, "/", data_name, "_DARC_Annotations_BAG.txt", na.strings = -999))

#for the EX23 data that was submitted for entire season instead of for each expedition, use the following code:
annotation_import_23 <- read.delim("EX23--_Animal_Guide_Photos_20250312.txt", head = TRUE, na.strings = -999)

EX2301 <- annotation_import_23 |> 
  filter(SurveyID == "EX2301")

EX2304 <- annotation_import_23 |> 
  filter(SurveyID == "EX2304")

EX2306 <- annotation_import_23 |> 
  filter(SurveyID == "EX2306")

annotation_import <- EX2301

#Review annotation import to make sure it looks okay
str(annotation_import)



#cLEAN DATAFRAME AND PUT IN BAG FORMAT ----------------------------------------------------------

#Ensure only the expedition of interest is included in dataset
annotation_import <- annotation_import |> 
  filter(SurveyID == data_name)

str(annotation_import$SurveyID) #this should only return expedition of interest that you named as your data_name

#Create a column that has dive number 
#This is different for Okeanos vs Nautilus dives

annotation_clean <- annotation_import |>   
  mutate(DiveNumber = case_when(Vessel == "Okeanos Explorer" ~ str_sub(Station,-2,-1),
                                Vessel == "Nautilus" ~ str_sub(Station,-3,-1)))|> 
  #remove the 2nd entry for rows that have multiple tracking IDs
  mutate(Date = as.Date(ObservationDate, origin = "1899-12-30"),   #Format date column into date format. Origin based on the issue with importing Excel data. 
         Time = ObservationTime*86400) |>  #Format the time column into number of seconds
  #create a combined date time column in format used by SeaTube "YYYY-MM-DDTHH:MM:SSZ"
  # mutate(Date = as.POSIXct(Date, tz= "UTC", format = "%Y-%m-%d")) |> 
  # mutate(DATE_TIME = paste0(Date, "T", Time,))
  mutate(DATE_TIME = as.POSIXct(Date) + Time) |>
  mutate(DATE_TIME = gsub(pattern = " UTC", "", DATE_TIME)) |> 
  mutate(DATE_TIME = paste0(gsub(" ", "T", DATE_TIME), "Z"))|>
  left_join(seatube, by = NULL) |> #adds in the seatube URL for each dive from the seatube dataframe created above
  mutate(ScientificName = str_remove(ScientificName, "sp\\.")) |> #remove the "sp." from the scientific name where present
  mutate(TRACKING_ID = str_remove(TrackingID, pattern = "\\|.*"),  #finds the first | in the string, removes the | (\\|) and character after it (.) any number of times (*)
         GENUS_SUBGENUS = case_when(!is.na(Genus) & !is.na(Species) ~ coalesce(Genus, Species, sep = " '"), #concatenate genus and species are both available, o
                                    !is.na(Genus) & is.na(Species) ~ Genus,
                                    TRUE ~ NA),
         SPECIES_SUBSPECIES = coalesce(str_remove(Species, ".* "), " ",Subspecies), #Remove the genus name in front of the species and pastes the subspecies on to end. Use coalesce to ignore NAs. Paste and Paste0 will include any NAs as text
         IMAGE_PATH = paste0("https://www.ncei.noaa.gov/waf/okeanos-animal-guide/images/", locality, "/"),
         #Populate DIVE_URL with NCEI Oceanographic Package URL for Okeanos Explorer dives; leave blank for other vessels
         DIVE_URL = if_else(Vessel == "Okeanos Explorer", 
                            paste0("https://www.ncei.noaa.gov/waf/okeanos-rov-cruises/",tolower(SurveyID), "/#tab-", as.numeric(DiveNumber)), NA),
         #If Videos are on SeaTube, populate with the link to the timestamped Seatube video; leave blank if video not in seatube
         VIDEO_SEGMENT_URL = if_else(!is.na(DIVE_VIDEO_URL), 
                                     paste0(DIVE_VIDEO_URL, "?&time=", DATE_TIME), NA), #link to the annotation timestamp in SeaTube
        # CF_NR_SPECIES = NA,
         #DESCRIPTION = NA,
         #FAMILY_OR_HIGHER = NA,
         #ICONIC_IMAGE = NA, 
         SUPER_GROUP = NA,
         GROUP_ = NA,
         SUBGROUP = NA,
         CATEGORY = NA,
         SUBCATEGORY = NA,
         SUBPHYLUM = NA,
         SUPERCLASS = NA,
         INFRACLASS = NA,
         SUPERORDER = NA,
         INFRAORDER = NA,
         SUPERFAMILY = NA,
         SUBFAMILY = NA,
         ATTRIBUTIONS = attribution,
         LOCALITY = trimws(str_extract(Locality, pattern = "[^;]+$"), which = "left"), #extracts the string after the last ; in the string ("[^;]+$) and removes the space (trimws) at the start of the string
         #create a unique string to assign as the filename. This must be unique for each annotation. 
         #IMAGE_FILENAME = paste0(gsub(" .*$", "", CombinedNameID), "_", str_extract(GuidePhoto, pattern = "[/;]+$"))) |> 
         IMAGE_FILENAME = str_extract(GuidePhoto,  pattern = "[^/]+$"), which = "left" ) |> #keep only the file name from the GuidePhoto image path
  mutate(IMAGE_FILENAME = gsub("\\.(jpg|png)$", "", IMAGE_FILENAME)) |> #reomove the file extension from the filename
  mutate(DIVE_LAT = round(Latitude, 2),
         DIVE_LONG = round (Longitude, 2),
         IMAGE_LAT = round(Latitude, 5),
         IMAGE_LONG = round (Longitude, 5),
         DEPTH_M = round(DepthInMeters, 0)) |> 
  rename(c(IMAGE_ALT_TEXT = CombinedNameID, OCEAN = Ocean, REGION = LargeMarineEcosystem, CRUISEID = SurveyID, DIVE_NUM = DiveNumber, TEMPERATURE = Temperature, 
           OXYGEN = Oxygen, SALINITY = Salinity, PHYLUM = Phylum, CLASS = Class, SUBCLASS = Subclass, ORDER = Order, SUBORDER = Suborder, 
           FAMILY = Family, COMMON_NAME = VernacularName, OTU = ScientificName, TAXON_RANK = TaxonRank, MORPHOSPECIES = Morphospecies, VESSEL = Vessel, 
           APHIA_ID = AphiaID, ICONIC_IMAGE = PhotoQuality, PLATFORM = VehicleName, DESCRIPTION = IdentificationComments, DARC_URL = GuidePhoto)) |> 
  select(c(IMAGE_FILENAME, IMAGE_ALT_TEXT, IMAGE_PATH, SUPER_GROUP, GROUP_, SUBGROUP, CATEGORY, SUBCATEGORY, OCEAN, REGION, LOCALITY, CRUISEID, DIVE_NUM,
           DIVE_URL, DATE_TIME, DIVE_LAT, DIVE_LONG, IMAGE_LAT, IMAGE_LONG, VIDEO_SEGMENT_URL, DEPTH_M, TEMPERATURE, OXYGEN, SALINITY, PHYLUM, SUBPHYLUM, SUPERCLASS, CLASS,
           SUBCLASS, INFRACLASS, SUPERORDER, ORDER, SUBORDER, INFRAORDER, SUPERFAMILY, FAMILY, GENUS_SUBGENUS, SPECIES_SUBSPECIES, DESCRIPTION, 
           COMMON_NAME, OTU, ICONIC_IMAGE, TRACKING_ID, TAXON_RANK, SUBFAMILY, MORPHOSPECIES, VESSEL, APHIA_ID, PLATFORM, ATTRIBUTIONS, DARC_URL)) |> 
  mutate(VESSEL =  case_when(VESSEL == "Okeanos Explorer" ~ "NOAA Ship Okeanos Explorer",
                             VESSEL == "Nautilus" ~ "E/V Nautilus")) |> #rename ships to full name of vessel
  
  mutate(SUBCATEGORY = case_when(ORDER == "Gadiformes" ~ "Gadiform Fishes",
                                 ORDER == "Ophidiiformes" ~ "Ophidiiform Fishes", 
                                 FAMILY == "Synaphobranchidae" ~ "Cutthroat Eels",
                                 ORDER == "Anguilliformes" & !FAMILY == "Synaphobranchidae" ~ "Eels Other",
                                 SUPERFAMILY %in% c("Actinostoloidea",
                                                    "Metridioidea") ~ SUPERFAMILY,
                                 ORDER == "Actinaria" & !SUPERFAMILY %in% c("Actinostoloidea","Metridioidea") ~ "Anemones Other",
                                 FAMILY %in% c("Cladopathidae", 
                                               "Schizopathidae") ~ FAMILY,
                                 ORDER == "Antipatharia" & !FAMILY %in% c("Cladopathidae", "Schizopathidae") ~ "Black Corals Other",
                                 FAMILY %in% c("Acanthogorgiidae", 
                                               "Chrysogorgiidae",
                                               "Coralliidae",
                                               "Paragorgiidae",
                                               "Plexauridae") ~ FAMILY,
                                 
                                 TRUE ~ NA
                                 )
         )|>
  mutate(CATEGORY = case_when(ORDER == "Lophiiformes" ~ "Anglerfishes",
                              SUBCATEGORY %in% c("Gadiform Fishes", "Ophidiiform Fishes", "Eel-like Fishes Other") ~ "Eel-like Fishes",
                              SUBCATEGORY %in% c("Cutthroat Eels", "Eels Other") ~ "Eels",
                              ORDER == "Pleuronectiformes" ~ "Flatfishes",
                              ORDER == "Aulopiformes" ~ "Lizardfishes & Allies",
                              ORDER == "Perciformes" ~ "Perch-like Fishes",
                              ORDER == "Scorpaeniformes" ~ "Scorpaeniform Fishes",
                              INFRACLASS == "Batoidea" ~ "Rays",
                              INFRACLASS == "Selachii" ~ "Sharks",
                              ORDER == "Actinaria" ~ "Anemones",
                              ORDER == "Antipatharia" ~ "Black Corals",
                              ORDER == "Corallimorpharia" ~ "Corallimorpharians",
                              ORDER == "Scleractinia" ~ "Hard Corals",
                              ORDER == "Zoantharia" ~ "Zoantharians",
                              FAMILY %in% c("Acanthogorgiidae", 
                                            "Chrysogorgiidae", 
                                            "Corallidae", 
                                            "Keratoisididae", 
                                            "Paragorgiidae") ~ "Gorgonians Other",
                              SUPERFAMILY == "Pennatuloidea" ~ "Sea Pens",
                              ORDER %in% c("Keratoisididae", 
                                           "Primnoidae") ~ "Gorgonians Other", #for now, putting all of these in "other" since it is impossible to parse branched and unbranched forms based on the taxonomy alone
                              SUPERFAMILY == "Atedonoidea" ~ SUPERFAMILY,
                              ORDER == "Comatulida" & !SUPERFAMILY == "Atedonoidea" ~ "Feather Stars Other",
                              FAMILY == "Goniasteridae" ~ FAMILY,
                              ORDER == "Valvatida" & !FAMILY == "Goniasteridae" ~ "Valvatida Other",
                              FAMILY %in% c("Euplectellidae", 
                                            "Rossellidae") ~ FAMILY,
                              ORDER == "Lyssacinosida" & !FAMILY %in% c("Euplectellidae", 
                                                                        "Rossellidae") ~"Lyssacinosida Other",
                              FAMILY %in% c("Euretidae,
                                            Farreidae") ~ FAMILY,
                              ORDER == "Sceptrulophora" & !FAMILY %in% c("Euretidae, Farreidae") ~ "Sceptrulophora Other",
                              TRUE ~ NA
                              )
         ) |> 
  mutate(CATEGORY = case_when (CLASS == "Teleostei" & is.na(CATEGORY) ~ "Ray-finned Fishes Other",
                               CLASS == "Octocorallia" & is.na(CATEGORY) ~ "Soft Corals",
                               TRUE ~ CATEGORY
                               )
         ) |> 
  mutate(SUBGROUP = case_when(SUPERFAMILY == "Paguroidea" ~ "Hermit Crabs",
                              FAMILY == "Lithodidae" ~ "King Crabs", 
                              INFRAORDER == "Brachyura" ~ "True Crabs",
                              CLASS == "Holocephali" ~ "Chimaeras",
                              ORDER == "Myxiniformes" ~ "Hagfishes",
                              CLASS == "Teleostei" ~ "Ray-finned Fishes",
                              CLASS == "Elasmobranchii" ~ "Sharks and Rays",
                              CLASS == "Hexacorallia" ~ "Hexacorals",
                              CLASS == "Octocorallia" ~ "Octocorals", 
                              ORDER == "Ceriantharia" ~ "Tube Anemones",
                              SUBCLASS == "Hydroidolina" ~ "Hydrozoans Coral-like",
                              CLASS == "Hydrozoa" & !SUBCLASS == "Hydroidolina" ~ "Hydrozoans Other",
                              CLASS == "Ophiuroidea" & !ORDER == "Euryalae" ~ "Brittlestars",
                              ORDER == "Euryalae" ~ "Basketstars",
                              ORDER %in% c("Elasipodida", 
                                           "Synallactida") ~ ORDER,
                              CLASS == "Holothuroidea" & !ORDER %in% c("Elasipodida", "Synallactida") ~ "Sea Cucumbers Other",
                              ORDER == "Comatulida" ~ "Feather Stars",
                              CLASS == "Crinoidea" & !ORDER == "Comatulida" ~ "Sea Lillies", 
                              ORDER %in% c("Valvatida",
                                           "Velatida") ~ ORDER,
                              CLASS == "Asteroidea" & !ORDER %in% c("Valvatida", "Velatida") ~ "Sea Stars Other",
                              ORDER %in% c("Cidaroida",
                                           "Echinothuroida") ~ ORDER,
                              INFRACLASS == "Irregularia" ~ INFRACLASS,
                              CLASS == "Echinoidea" & !INFRACLASS == "Irregularia" & !ORDER  %in% c("Cidaroida", "Echinothuroida") ~ "Sea Urchins Other",
                              ORDER %in% c("Poecilosclerida", 
                                           "Tetractinellida") ~ ORDER,
                              CLASS == "Demospongiae" & !ORDER %in% c("Poecilosclerida", "Tetractinellida") ~ "Demosponges Other",
                              ORDER %in% c("Amphidiscosida",
                                           "Hexasterophora", 
                                           "Lyssacinosida",
                                           "Sceptrulophora") ~ ORDER,
                              CLASS == "Hexactinellida" & !ORDER %in% c("Amphidiscosida",
                                                                        "Hexasterophora", 
                                                                        "Lyssacinosida",
                                                                        "Sceptrulophora") ~ "Glass Sponges Other"
                              )
         ) |>
  mutate(GROUP_ = case_when(SUPER_GROUP == "Arthropoda" & SUBCLASS == "Cirripedia" ~ "Barnacles",
                            SUBGROUP %in% c("Hermit Crabs", "King Crabs", "True Crabs") ~ "Crabs",
                            SUBGROUP %in% c("Caridean Shrimp", "Dendrobranchiata Shrimp") ~ "Shrimp",
                            SUBGROUP %in% c("Chirostyloidea", "Galatheoidea") ~ "Squat Lobsters",
                            PHYLUM == "Chordata" & CLASS %in% c("Appendicularia", 
                                                                "Ascidiacea", 
                                                                "Thaliacea") ~ "Tunicates",
                            PHYLUM == "Chordata" & !CLASS %in% c("Appendicularia", 
                                                                 "Ascidiacea", 
                                                                 "Thaliacea") ~ "Vertebrates",
                            SUBGROUP %in% c("Hexacorals",
                                            "Octocorals",
                                            "Tube Anemones") ~ "Corals and Anemones",
                            CLASS == "Hydrozoa" ~ "Hydrozoans",
                            CLASS == "Ophiuroidea" ~ "Brittlestars and Basketstars",
                            CLASS == "Holothuroidea" ~ "Sea Cucumbers",
                            CLASS == "Crinoidea" ~ "Sea Lillies and Feather Stars", 
                            CLASS == "Asteroidea" ~ "Sea Stars",
                            CLASS == "Echinoidea" ~ "Sea Urchins",
                            CLASS %in% c("Cephalopoda",
                                         "Gastropoda") ~ CLASS,
                            PHYLUM == "Mollusca" & !CLASS %in% c("Cephalopoda", "Gastrapoda") ~ "Mollusca Other", 
                            CLASS == "Demospongiae" ~ "Demosponges", 
                            CLASS == "Hexactinellida" ~ "Glass Sponges",
                            TRUE ~ NA
                            )
  ) |> 
  mutate(GROUP_ = case_when(SUPER_GROUP == "Arthropoda" & is.na(GROUP_) ~ "Arthropoda Other",
                            SUPER_GROUP == "Porifera" & is.na(GROUP_) ~ "Sponges Other",
                            TRUE ~ GROUP_
                            )
  ) |>
  mutate(SUPER_GROUP = case_when(PHYLUM == "Annelida" ~ "Annelida (worms)",
                                 PHYLUM == "Arthropoda" ~ "Arthropoda", 
                                 PHYLUM == "Chordata" ~ "Chordata",
                                 PHYLUM == "Cnidaria" ~ "Cnidaria",
                                 PHYLUM == "Ctenophora" ~ "Ctenophora (compb jellies)", 
                                 PHYLUM == "Echinodermata" ~ "Echinodermata",
                                 PHYLUM == "Mollusca" ~ "Mollusca", 
                                 PHYLUM == "Porifera" ~ "Porifera", 
                                 TRUE ~ "Animalia Other"
                                 )
         ) |> 
  mutate(across(where(is.character), ~(na_if(.x,"NA"))))

  
#verify that all the levels populated okay
test <- annotation_clean |> 
    filter(!is.na(GROUP_) & is.na(SUPER_GROUP))

  
  #check if there are any redundant strings in IMAGE_FILENAME 
duplicate_filenames <- annotation_clean |> 
 group_by(IMAGE_FILENAME) |> 
  filter(n() >1) |> 
  ungroup()

nrow(duplicate_filenames)

### Export file to send to BAG

#Create a directory to store exports in
dir.create(paste0(wd,"/Exports/BAG")) #create a folder in your working directory called "Exports" if it does not already exist
path <- paste0(wd, "/Exports/BAG")

filename = paste0(data_name,"_BAG_Export.csv")

write.csv(x = annotation_clean, file = file.path(path, filename), row.names = FALSE, na = "")



