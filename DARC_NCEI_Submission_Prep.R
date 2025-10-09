#### header ####
## Author: Ashley Marranzino
## Started on: 20250919
## Purpose: Transforming DARC annotation .tsv file into format for archival at NCEI


#### Set Working Directory ####
## Working Directory should call location where DARC .tsv files are stored on computer
## All .tsv files need to conform to the following naming convention: Expedition number + "_DARC_Annotations_Full.tsv"

wd <- "C:/Users/ashley.marranzino/Documents/Annotations/DARC Annotations"

setwd(wd)

#### Required Packages ####

if(!require('tidyverse'))install.packages('tidyverse'); library('tidyverse') 


#set standard name to refer to your data (e.g., the expedition number)

data_name <- "EX2304"

#Read in the DARC TSV containing All annotations from expedition. Make sure the file name follows the convention Expedition number + "_DARC_Annotations_Full.tsv"

annotation_import <- read_tsv(paste0(wd, "/", data_name, "_DARC_Annotations_Full.tsv"))

#Review annotation import to make sure it looks okay
str(annotation_import)

annotation_clean <- annotation_import |>   
  mutate(DiveNumber = case_when(Vessel == "Okeanos Explorer" ~ str_sub(Station,-2,-1),
                                Vessel == "Nautilus" ~ str_sub(Station,-3,-1)))|> 
  #(DiveNumber = as.factor(DiveNumber)) |> 
 # mutate(Annotation_timestamp = ymd(ObservationDate) +hms(ObservationTime))|>
  #mutate(Annotation_timestamp = as.POSIXlt(Annotation_timestamp, tz = "UTC",format= " %Y-%m-%d %H:%M:%OS")) |> 
  mutate(across(where(is.numeric),~na_if(., -999))) 


#DARC TSV includes all dives for an expedition. To prepare for archival with NCEI, we need to submit a single file for each dive.
#Break up the full expedition into single dives and save each dive as a new file. 
#Since this is the QA/QCed version, NCEI will archive as a V2

#Create a directory to store exports in
dir.create(paste0(wd,"/Exports/NCEI"))
dir.create(paste0(wd,"/Exports/NCEI/", data_name))

path <- paste0(wd,"/Exports/NCEI/", data_name)

#create a new dataframe for each dive and export as a .csv
for (i in 1:n_distinct(annotation_clean$DiveNumber)){
  dat <- annotation_clean |>  
    filter(DiveNumber == unique(annotation_clean$DiveNumber)[i])|>
    select(-DiveNumber)
  
  filename = paste0(data_name,"_DIVE",unique(annotation_clean$DiveNumber)[i],"_ANNOTATIONS_V2.csv")
  
  #names(dat)
  
  write.csv(x = dat, file = file.path(path, filename), row.names = FALSE)
}
