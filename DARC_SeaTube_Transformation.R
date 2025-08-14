#### header ####
## Author: Ashley Marranzino
## Started on: 20250516
## Purpose: Transforming DARC annotation .tsv file into format for ingestion for SeaTube


#### Set Working Directory ####
## Working Directory should call location where DARC .tsv files are stored on computer
## All .tsv files need to conform to the following naming convention: Expedition number + "_DARC_Annotations_Full.tsv"

wd <- "C:/Users/ashley.marranzino/Documents/Annotations/DARC Annotations"

setwd(wd)

#### Required Packages ####

if(!require('tidyverse'))install.packages('tidyverse'); library('tidyverse') 
if(!require('worrms'))install.packages('worrms'); library('worrms') 


#set standard name to refer to your data (e.g., the expedition number)

data_name <- "EX2306"

#Read in the DARC TSV containing All annotations from expedition. Make sure the file name follows the convention Expedition number + "_DARC_Annotations_Full.tsv"

annotation_import <- read_tsv(paste0(wd, "/", data_name, "_DARC_Annotations_Full.tsv"))

#Review annotation import to make sure it looks okay
str(annotation_import)


#Create a dataframe with annotaor email addresses to reference later
#NEED TO: figure out how to check that these are the only names in the CreatorName col
emails <- data.frame (name = c("Putts, Meagan", "hcarlson", "Bingo, Sarah", "Cunanan, Nikki", "Judah, Aaron"),
                      email = c( "puttsmr@hawaii.edu", "hcarlson@hawaii.edu", "sarahr6@hawaii.edu", "tngutlay@hawaii.edu", "ajudah@hawaii.edu" ))

#Create a column that has dive number 
#This is different for Okeanos vs Nautilus dives

annotation_clean <- annotation_import |>   
  mutate(DiveNumber = case_when(Vessel == "Okeanos Explorer" ~ str_sub(Station,-2,-1),
                                       Vessel == "Nautilus" ~ str_sub(Station,-3,-1)))|> 
  mutate(Annotation_timestamp = ymd(ObservationDate) +hms(ObservationTime))|>
  mutate(Annotation_timestamp = as.POSIXlt(Annotation_timestamp, tz = "UTC",format= " %Y-%m-%d %H:%M:%OS")) |> 
  mutate(`To Be Reviewed` = TRUE, Taxonomy = NA)|> 
  left_join(emails, join_by(IdentifiedBy == name)) |>  #add emails for annotators based on referencing the email dataframe
  mutate(across(where(is.numeric),~na_if(., -999))) |>  #removes the -999 present for no data in numeric columns
  mutate(Comments = case_when(!is.na (IdentificationComments) & !is.na(OccurrenceComments) ~ paste0(IdentificationComments, OccurrenceComments, sep = "; "),
                              !is.na (IdentificationComments) & is.na(OccurrenceComments) ~ IdentificationComments,
                              is.na(IdentificationComments) & !is.na (OccurrenceComments) ~ OccurrenceComments),
         ScientificName = str_remove(ScientificName, "sp."),
         Modified = (as.POSIXlt(paste0 (Modified, " 00:00:01"),tz = "UTC", format = " %Y-%m-%d %H:%M:%OS"))) #adds a time to the "Modified" column



#### Determine Parent taxon for each annotation ####

#Create a dataframe with the unique AphiaIDs from this set of annotations
Aphia <- annotation_clean |> 
  filter (!is.na(AphiaID)) |> 
  select (AphiaID) |> 
  distinct(AphiaID)
  

#Loop through each Aphia ID to determine the full classification level of the ID using the worrms Package

#Create a dataframe to hold output from the loop
df <- data.frame(
  AphiaID = numeric (),
  ParentTaxon = character(),
  stringsAsFactors=FALSE)


for (i in 1:length(Aphia$AphiaID)){
  row <- Aphia[i,]
  classification <- worrms::wm_classification (Aphia$AphiaID[i])
  
  parent <- (length(classification$rank)-1) #find the rank above lowest taxonomic rank and populate as Parent Taxon
  
  if (parent >= 1){  
    Classification_wide <- classification |> 
      select(rank, scientificname) |> 
      pivot_wider(names_from = rank, values_from = scientificname) |> 
      mutate(AphiaID = Aphia$AphiaID[i], 
             ParentTaxon = classification$scientificname[parent])|> 
      select(AphiaID, ParentTaxon)
  }
  else{ next
    
  }
  
  df <-bind_rows(df,Classification_wide)
}

df <- df |> 
  distinct(AphiaID, .keep_all = T)

#Add the full taxonomic classification system in df to annotation_clean and remove unnecessary columns

annotation_classification<-annotation_clean |> 
  left_join(df, join_by(AphiaID == AphiaID))|>  
  select(c(DiveNumber, Annotation_timestamp, email, `To Be Reviewed`, Taxonomy, ParentTaxon, Comments,ScientificName, CMECSGeoForm, 
           Substrate, Habitat, Morphospecies, IdentificationQualifier, IndividualCount, CategoricalAbundance, VerbatimSize, 
           MinimumSize, MaximumSize, Condition, AssociatedTaxa, LocationAccuracy, RecordType, Modified))|>
  rename(`Annotation timestamp` = Annotation_timestamp, `Creator email` = email, Geoform = CMECSGeoForm, Taxon = ScientificName, , `NOAA Biology/Morphospecies` = Morphospecies, 
         `NOAA Biology/Identification Qualifier` = IdentificationQualifier, `NOAA Biology/Individual Count` = IndividualCount, `Parent Taxon` = ParentTaxon,
         `NOAA Biology/Categorical Abundance` = CategoricalAbundance, `NOAA Biology/Verbatim Size` = VerbatimSize, `NOAA Biology/Minimum Size` = MinimumSize,
         `NOAA Biology/Maximum Size` = MaximumSize, `NOAA Biology/Condition` = Condition, `NOAA Biology/Associated Taxa` = AssociatedTaxa, 
         `NOAA Biology/Location Accuracy` = LocationAccuracy, `NOAA Biology/Record Type` = RecordType, `Creation timestamp` = Modified) |> 
  relocate(c('Creation timestamp', Comments), .before = `To Be Reviewed`)



#Create a biological annotation dataset that is a subset of annotation_clean with just biological data
annotation_bio <- annotation_classification |> 
  filter(!is.na(Taxon)) |> 
  mutate(Taxonomy = "WoRMS", Geoform = NA, Substrate = NA, Habitat = NA)

#Create a geological annotation dataset that is a subset of annotation_clean with just the geological data
annotation_geo <- annotation_classification |> 
  filter(!is.na(Geoform)) |> 
  mutate(Taxonomy = "CMECS", `Parent Taxon` = NA, Taxon = NA, Comments = NA, across(starts_with("NOAA"), ~NA))

#create an even annotation dataset that is a subset of annotation_clean with just comments associated
annotation_event <- annotation_classification |> 
  filter(is.na(Taxon)) |> 
  mutate(Taxonomy = NA, Geoform = NA, Substrate = NA, Habitat = NA, across(starts_with("NOAA"), ~NA))

#merge bio, geo, and even datasets for a complete dataset
annotation_full <- rbind(annotation_bio, annotation_geo, annotation_event) |> 
  mutate(across(starts_with("NOAA"), as.character)) |> 
  mutate(across(everything(), ~replace_na(as.character(.), ""))) #Remove all "NA"s from dataframe



#As of 4 August 2025 - SeaTube is only updating Biological observations. For the time being, skip creation and merger of the geo and event dataframes and just write the bio dataframe to annotation_full
annotation_full <- rbind(annotation_bio, annotation_event) |> 
 select(-c(Geoform, Substrate, Habitat)) |>
 mutate(across(starts_with("NOAA"), as.character)) |> 
 mutate(across(where(is.character), ~replace_na(as.character(.), ""))) #Remove all "NA"s from dataframe



#Create a directory to store exports in
dir.create(paste0(wd,"/Exports/"))

#create a new dataframe for each dive and export as a .csv
for (i in 1:n_distinct(annotation_full$DiveNumber)){
  dat <- annotation_full |>  
    filter(DiveNumber == unique (annotation_full$DiveNumber)[i])|>
    select(-DiveNumber)
  
  filename = paste0(data_name,"_DIVE",unique(annotation_full$DiveNumber)[i],".csv")
  
  names(dat)
  
  write.csv(dat,paste0(wd,"/Exports/SeaTube_", filename), row.names = FALSE)
}
