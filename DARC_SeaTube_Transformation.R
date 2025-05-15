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
