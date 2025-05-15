#Set Working Directory to location where the .tsv from DARC is

wd <- "C:/Users/ashley.marranzino/Documents/Annotations/DARC Annotations"

setwd(wd)

#Load packages if needed

if(!require('tidyverse'))install.packages('tidyverse'); library('tidyverse') 




#set standard name to refer to your data (e.g., the expedition number)

data_name <- "EX2301"

#Read in the DARC TSV containing All annotations from expedition. Make sure the file name follows the convention Expedition number + "_DARC_Annotations_Full.tsv"

annotation_import <- read_tsv(paste0(wd, "/", data_name, "_DARC_Annotations_Full.tsv"))

#Review annotation import to make sure it looks okay
str(annotation_import)
view(annotation_import)

#Create a dataframe with annotaor email addresses to reference later
#NEED TO: figure out how to check that these are the only names in the CreatorName col
emails <- data.frame (name = c("Putts, Meagan", "Hcarlson", "Bingo, Sarah", "Cunanan, Nikki", "Judah, Aaron"),
                      email = c( "puttsmr@hawaii.edu", "hcarlson@hawaii.edu", "sarahr6@hawaii.edu", "tngutlay@hawaii.edu", "ajudah@hawaii.edu" ))

#Create a column that has dive number 
#This is different for Okeanos vs Nautilus dives

annotation_clean <- as.data.frame(annotation_import) 

if (annotation_clean$Vessel[1] == "Okeanos Explorer"){
   annotation_clean <- annotation_clean |> 
   mutate(DiveNumber = str_sub(Station, -2, -1))
} else if (annotation_clean$Vessel[1] == "Nautilus"){
   annotation_clean <- annotation_clean |> 
     mutate(DiveNumber = str_sub(Station, -3, -1))
}
  

