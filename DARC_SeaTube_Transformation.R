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
emails <- data.frame (name = c("Putts, Meagan", "hcarlson", "Bingo, Sarah", "Cunanan, Nikki", "Judah, Aaron"),
                      email = c( "puttsmr@hawaii.edu", "hcarlson@hawaii.edu", "sarahr6@hawaii.edu", "tngutlay@hawaii.edu", "ajudah@hawaii.edu" ))

#Create a column that has dive number 
#This is different for Okeanos vs Nautilus dives

annotation_clean <- as.data.frame(annotation_import) 
annotation_clean <- annotation_clean|>
  mutate(DiveNumber = case_when(Vessel == "Okeanos Explorer" ~ str_sub(Station,-2,-1)),
                      case_when(Vessel == "Nautilus" ~ str_sub(Station,-3,-1)))|> 
  mutate(Annotation_timestamp = with(annotation_clean, ymd(ObservationDate) +hms(ObservationTime))|>
           as.POSIXlt(tz = "UTC",format= " %Y-%m-%d %H:%M:%OS")) |> 
  mutate(`Creation timestamp` = NA, `To Be Reviewed` = TRUE, Taxonomy = NA, `Parent taxon` = NA)|> 
  left_join(emails, join_by(IdentifiedBy == name)) |> 
  mutate(across(where(is.numeric),~na_if(., -999))) |> 
  mutate(`Parent taxon` = if_else(!is.na (Class), Phylum, NA))(!is.na(Subspecies), Species, NA),
         `Parent taxon` = if_else(!is.na(Species), Genus, NA),
         `Parent taxon` = if_else(!is.na (Genus), Subfamily, NA),
         `Parent taxon` = if_else(!is.na (Subfamily), Family, NA), 
         `Parent taxon` = if_else(!is.na (Family),Suborder, NA),
         `Parent taxon` = if_else(!is.na (Suborder), Order, NA), 
         `Parent taxon` = if_else(!is.na (Order), Subclass, NA),
         `Parent taxon` = if_else(!is.na (Subclass), Class, NA),
         `Parent taxon` = if_else |> 
   mutate(Comments = case_when(!is.na (IdentificationComments) & !is.na(OccurrenceComments) ~ paste0(IdentificationComments, OccurrenceComments, sep = "; "),
                              !is.na (IdentificationComments) & is.na(OccurrenceComments) ~ IdentificationComments,
                              is.na(IdentificationComments) & !is.na (OccurrenceComments) ~ OccurrenceComments))|>
  select(c(DiveNumber, Annotation_timestamp, email, `Creation timestamp`, `To Be Reviewed`, Taxonomy, `Parent taxon`, Comments,ScientificName, CMECSGeoForm, 
           Substrate, Habitat, Morphospecies, IdentificationQualifier, IndividualCount, CategoricalAbundance, VerbatimSize, 
           MinimumSize, MaximumSize, Condition, AssociatedTaxa, LocationAccuracy, RecordType))|>
  rename(`Annotation timestamp` = Annotation_timestamp, `Creator email` = email, Geoform = CMECSGeoForm, Taxon = ScientificName, , `NOAA Biology/Morphospecies` = Morphospecies, 
         `NOAA Biology/Identification Qualifier` = IdentificationQualifier, `NOAA Biology/Individual Count` = IndividualCount, 
         `NOAA Biology/Categorical_Abundace` = CategoricalAbundance, `NOAA Biology/Verbatim Size` = VerbatimSize, `NOAA Biology/Minimum Size` = MinimumSize,
         `NOAA Biology/Maximum Size` = MaximumSize, `NOAA Biology/Condition` = Condition, `NOAA Biology/Associated Taxa` = AssociatedTaxa, 
         `NOAA Biology/Location Accuracy` = LocationAccuracy, `NOAA Biology/Record Type` = RecordType) |> 
  relocate(Comments, .before = `To Be Reviewed`)
 
  

#Create a biological annotation dataset that is a subset of annotation_clean with just biological data
annotation_bio <- annotation_clean |> 
  filter(!is.na(Taxon)) |> 
  mutate(Taxonomy = "WoRMS", Geoform = NA, Substrate = NA, Habitat = NA)

#Create a geological annotation dataset that is a subset of annotation_clean with just the geological data
annotation_geo <- annotation_clean |> 
  filter(!is.na(Geoform)) |> 
  mutate(Taxonomy = "CMECS", `Parent taxon` = NA, Taxon = NA, Comments = NA, across(starts_with("NOAA"), ~NA))

#create an even annotation dataset that is a subset of annotation_clean with just comments associated
annotation_event <- annotation_clean |> 
  filter(is.na(Taxon)) |> 
  mutate(Taxonomy = NA, Geoform = NA, Substrate = NA, Habitat = NA, across(starts_with("NOAA"), ~NA))

#merge bio, geo, and even datasets for a complete dataset

annotation_full <- rbind(annotation_bio, annotation_geo, annotation_event)

#Remove all "NA"s from dataframe
annotation_full[is.na(annotation_full)] <- ""

#Create a directory to store exports in
dir.create(paste0(wd,"/Exports/"))
 
#create a new dataframe for each dive and export as a .csv
for (i in 1:n_distinct(annotation_full$DiveNumber)){
  dat <- annotation_full |>  
    filter(DiveNumber == unique(annotation_full$DiveNumber[i])) |> 
    select(-DiveNumber)
  
  filename = paste0(data_name,"_DIVE",unique(annotation_full$DiveNumber)[i],".csv")
  
  names(dat)
 
  write.csv(dat,paste0(wd,"/Exports/SeaTube_", filename), row.names = FALSE)
}
