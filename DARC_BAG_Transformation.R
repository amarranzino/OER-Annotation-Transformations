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

#set standard name to refer to your data (e.g., the expedition number)

data_name <- "NA167"

#Read in the DARC Txt containing All annotations from expedition. Make sure the file name follows the convention Expedition number + "_DARC_Annotations_BAG.txt"

annotation_import <- read.delim(paste0(wd, "/", data_name, "_DARC_Annotations_BAG.txt"))

#Review annotation import to make sure it looks okay
str(annotation_import)

#Create a column that has dive number 
#This is different for Okeanos vs Nautilus dives

annotation_clean <- annotation_import |>   
  mutate(DiveNumber = case_when(Vessel == "Okeanos Explorer" ~ str_sub(Station,-2,-1),
                                Vessel == "Nautilus" ~ str_sub(Station,-3,-1)))|> 
  #remove the 2nd entry for rows that have multiple tracking IDs
  mutate(Date = as.Date(ObservationDate, origin = "1899-12-30"),   #Format date column into date format. Origin based on the issue with importing Excel data. 
         Time = ObservationTime*86400) |>  #Format the time column into number of seconds
  mutate (DATE_TIME = as.POSIXct(Date) + Time) |>  #create a combined date time column in YYYY-MM-DD HH:MM:SS format
  mutate(TRACKING_ID = str_remove(TrackingID, pattern = "\\|.*"),  #finds the first | in the string, removes the | (\\|) and character after it (.) any number of times (*)
         GENUS_SUBGENUS = paste (Genus, Species, sep = " '"),
         SPECIES_SUBSPECIES = coalesce(str_remove(Species, ".* "), " ",Subspecies), #Remove the genus name in front of the species and pastes the subspecies on to end. Use coalesce to ignore NAs. Paste and Paste0 will include any NAs as text
         IMAGE_PATH = NA,
         DIVE_URL = NA,
         VIDEO_SEGMENT_URL = NA,
         CF_NR_SPECIES = NA,
         DESCRIPTION = NA,
         FAMILY_OR_HIGHER = NA,
         ICONIC_IMAGE = NA, 
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
         LOCALITY = trimws(str_extract(Locality, pattern = "[^;]+$"), which = "left"), #extracts the string after the last ; in the string ("[^;]+$) and removes the space (trimws) at the start of the string
         IMAGE_FILENAME = str_remove(ImageFilePath, pattern = "\\|.*")) |>  #removes anything after the first | in the string
  mutate(IMAGE_FILENAME = str_extract(IMAGE_FILENAME, pattern = "[^/]+$")) |>  #extracts only the text following the last / in the IMAGE_FILENAME (removes the filepath in front of the filename). "[^/]" matches any character that is not a /. "+" denotes that the [^/] should be matched at least once. "$" starts the search from the end of the string. 
  mutate(IMAGE_FILENAME = str_remove (IMAGE_FILENAME, pattern = "\\.(png|jpg)")) |> #removes the file extension from the filename. the "\\." searches for the last . and removes anything following that. the (png|jpg) removes text that is either "png" or (use "|" for or) "jpg". Cann add more file extensins if needed
  mutate(DIVE_LAT = round(Latitude, 2),
         DIVE_LONG = round (Longitude, 2),
         IMAGE_LAT = round(Latitude, 2),
         IMAGE_LONG = round (Longitude, 2)) |> 
  rename(c(IMAGE_ALT_TEXT = CombinedNameID, OCEAN = Ocean, REGION = LargeMarineEcosystem, CRUISEID = SurveyID, DIVE_NUM = DiveNumber, DEPTH_M = DepthInMeters,
           TEMPERATURE = Temperature, OXYGEN = Oxygen, SALINITY = Salinity, PHYLUM = Phylum, CLASS = Class, SUBCLASS = Subclass, ORDER = Order, SUBORDER = Suborder, 
           FAMILY = Family, COMMON_NAME = VernacularName, OTU = ScientificName, TAXON_RANK = TaxonRank, MORPHOSPECIES = Morphospecies, VESSEL = Vessel, 
           APHIA_ID = AphiaID, PHOTO_QUALITY = PhotoQuality, PLATFORM = VehicleName)) |> 
  select(c(IMAGE_FILENAME, IMAGE_ALT_TEXT, IMAGE_PATH, SUPER_GROUP, GROUP_, SUBGROUP, CATEGORY, SUBCATEGORY, OCEAN, REGION, LOCALITY, CRUISEID, DIVE_NUM,
           DIVE_URL, DATE_TIME, DIVE_LAT, DIVE_LONG, IMAGE_LAT, IMAGE_LONG, VIDEO_SEGMENT_URL, DEPTH_M, TEMPERATURE, OXYGEN, SALINITY, PHYLUM, SUBPHYLUM, SUPERCLASS, CLASS,
           SUBCLASS, INFRACLASS, SUPERORDER, ORDER, SUBORDER, INFRAORDER, SUPERFAMILY, FAMILY, GENUS_SUBGENUS, CF_NR_SPECIES, SPECIES_SUBSPECIES, DESCRIPTION, FAMILY_OR_HIGHER,
           COMMON_NAME, OTU, ICONIC_IMAGE, TRACKING_ID, TAXON_RANK, SUBFAMILY, MORPHOSPECIES, VESSEL, APHIA_ID, PHOTO_QUALITY, PLATFORM))
  


