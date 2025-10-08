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

data_name <- "EX2304"

#change this based on where expedition occurred. 
#Use either "Pacific", "Atlantic", or "Alaska". If new regions are used, work with NCEI to deterimine new names 
location <- "Alaska"  

#Set SeaTube URLs for each dive. Refer to Seatube (https://data.oceannetworks.ca/ExpeditionManagement) and check the URL for each video playback

#create a dataframe containing Seatube links
#If videos are not on SeaTube, fill DIVE_VIDEO_URL with NA
seatube <- data.frame(DiveNumber = c("01", "02", "03", "04", "05", "06", "07", "08"), #create a list of dive numbers for the expedition
                      DIVE_VIDEO_URL = c("https://data.oceannetworks.ca/app/dive-logs/2853", #Dive01 URL
                               "https://data.oceannetworks.ca/app/dive-logs/2863", #Dive02 URL
                               "https://data.oceannetworks.ca/app/dive-logs/6570", #Dive03 URL
                               "https://data.oceannetworks.ca/app/dive-logs/6580", #Dive04 URL
                               "https://data.oceannetworks.ca/app/dive-logs/6590", #Dive05 URL
                               "https://data.oceannetworks.ca/app/dive-logs/6600", #Dive06 URL
                               "https://data.oceannetworks.ca/app/dive-logs/6610", #Dive07 URL
                               "https://data.oceannetworks.ca/app/dive-logs/6620"))  #Dive08 URL

head(seatube)

#READ IN DATA ---------------------------------------------------------------------------------

#Read in the DARC Txt containing All annotations from expedition. Make sure the file name follows the convention Expedition number + "_DARC_Annotations_BAG.txt"

annotation_import <- read.delim(paste0(wd, "/", data_name, "_DARC_Annotations_BAG.txt"))


#Review annotation import to make sure it looks okay
str(annotation_import)


#cLEAN DATAFRAME AND PUT IN BAG FORMAT ----------------------------------------------------------

#Create a column that has dive number 
#This is different for Okeanos vs Nautilus dives

annotation_clean <- annotation_import |>   
  mutate(DiveNumber = case_when(Vessel == "Okeanos Explorer" ~ str_sub(Station,-2,-1),
                                Vessel == "Nautilus" ~ str_sub(Station,-3,-1)))|> 
  #remove the 2nd entry for rows that have multiple tracking IDs
  mutate(Date = as.Date(ObservationDate, origin = "1899-12-30"),   #Format date column into date format. Origin based on the issue with importing Excel data. 
         Time = ObservationTime*86400) |>  #Format the time column into number of seconds
  #create a combined date time column in format used by SeaTube "YYYY-MM-DDTHH:MM:SSZ"
  mutate (DATE_TIME = paste0(as.POSIXct(Date, tz= "UTC", format = "%Y-%m-%d"), "T", as.POSIXct(Time, tz="UTC", format = "%H:%M:%OS"), "Z")) |>  
  left_join(seatube, by = NULL) |> #adds in the seatube URL for each dive from the seatube dataframe created above
  mutate(TRACKING_ID = str_remove(TrackingID, pattern = "\\|.*"),  #finds the first | in the string, removes the | (\\|) and character after it (.) any number of times (*)
         GENUS_SUBGENUS = paste (Genus, Species, sep = " '"),
         SPECIES_SUBSPECIES = coalesce(str_remove(Species, ".* "), " ",Subspecies), #Remove the genus name in front of the species and pastes the subspecies on to end. Use coalesce to ignore NAs. Paste and Paste0 will include any NAs as text
         IMAGE_PATH = paste0("https://www.ncei.noaa.gov/waf/okeanos-animal-guide/images/", location, "/"),
         #Populate DIVE_URL with NCEI Oceanographic Package URL for Okeanos Explorer dives; leave blank for other vessels
         DIVE_URL = if_else(Vessel == "Okeanos Explorer", 
                            paste0("https://www.ncei.noaa.gov/waf/okeanos-rov-cruises/",data_name, "/#tab-", as.numeric(DiveNumber)), NA),
         #If Videos are on SeaTube, populate with the link to the timestamped Seatube video; leave blank if video not in seatube
         VIDEO_SEGMENT_URL = if_else(!is.na(DIVE_VIDEO_URL), 
                                     paste0(DIVE_VIDEO_URL, "?&time=", DATE_TIME), NA), #link to the annotation timestamp in SeaTube
         CF_NR_SPECIES = NA,
         #DESCRIPTION = NA,
         FAMILY_OR_HIGHER = NA,
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
         ATTRIBUTIONS = NA,
         LOCALITY = trimws(str_extract(Locality, pattern = "[^;]+$"), which = "left"), #extracts the string after the last ; in the string ("[^;]+$) and removes the space (trimws) at the start of the string
         #create a unique string to assign as the filename. This must be unique for each annotation. 
         IMAGE_FILENAME = paste0(data_name, "_", gsub(" .*$", "", CombinedNameID), "_", TrackingID)) |> 
   mutate(DIVE_LAT = round(Latitude, 2),
         DIVE_LONG = round (Longitude, 2),
         IMAGE_LAT = round(Latitude, 5),
         IMAGE_LONG = round (Longitude, 5)) |> 
  rename(c(IMAGE_ALT_TEXT = CombinedNameID, OCEAN = Ocean, REGION = LargeMarineEcosystem, CRUISEID = SurveyID, DIVE_NUM = DiveNumber, DEPTH_M = DepthInMeters,
           TEMPERATURE = Temperature, OXYGEN = Oxygen, SALINITY = Salinity, PHYLUM = Phylum, CLASS = Class, SUBCLASS = Subclass, ORDER = Order, SUBORDER = Suborder, 
           FAMILY = Family, COMMON_NAME = VernacularName, OTU = ScientificName, TAXON_RANK = TaxonRank, MORPHOSPECIES = Morphospecies, VESSEL = Vessel, 
           APHIA_ID = AphiaID, ICONIC_IMAGE = PhotoQuality, PLATFORM = VehicleName, DESCRIPTION = IdentificationComments)) |> 
  select(c(IMAGE_FILENAME, IMAGE_ALT_TEXT, IMAGE_PATH, SUPER_GROUP, GROUP_, SUBGROUP, CATEGORY, SUBCATEGORY, OCEAN, REGION, LOCALITY, CRUISEID, DIVE_NUM,
           DIVE_URL, DATE_TIME, DIVE_LAT, DIVE_LONG, IMAGE_LAT, IMAGE_LONG, VIDEO_SEGMENT_URL, DEPTH_M, TEMPERATURE, OXYGEN, SALINITY, PHYLUM, SUBPHYLUM, SUPERCLASS, CLASS,
           SUBCLASS, INFRACLASS, SUPERORDER, ORDER, SUBORDER, INFRAORDER, SUPERFAMILY, FAMILY, GENUS_SUBGENUS, CF_NR_SPECIES, SPECIES_SUBSPECIES, DESCRIPTION, FAMILY_OR_HIGHER,
           COMMON_NAME, OTU, ICONIC_IMAGE, TRACKING_ID, TAXON_RANK, SUBFAMILY, MORPHOSPECIES, VESSEL, APHIA_ID, PLATFORM, ATTRIBUTIONS)) |> 
  mutate(VESSEL =  case_when(VESSEL == "Okeanos Explorer" ~ "NOAA Ship Okeanos Explorer",
                             VESSEL == "Nautilus" ~ "E/V Nautilus"))  #rename ships to full name of vessel


### Export file to send to BAG

#Create a directory to store exports in
dir.create(paste0(wd,"/Exports/BAG")) #create a folder in your working directory called "Exports" if it does not already exist
path <- paste0(wd, "/Exports/BAG")

filename = paste0(data_name,"_BAG_Export.csv")

write.csv(x = annotation_clean, file = file.path(path, filename), row.names = FALSE)



