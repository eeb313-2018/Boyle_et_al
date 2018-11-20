library(tidyverse)

setwd("/Users/Dunc/Documents/GitHub/Boyle_et_al")

## Always use this code to start so that we have the same column names and cleaned data
predprey <- read_csv("predprey.csv",col_names = TRUE)
#View(predprey)

# Fix the spaces in column headers
names(predprey) <- gsub(" ", "_", names(predprey)) #gsub replaces all cases
names(predprey) <- sub("-", "_", names(predprey)) #sub only replaces the first case in each character string
names(predprey) <- sub("__", "_", names(predprey)) #predator taxon started with two spaces
names(predprey) <- gsub("/", ".", names(predprey)) #fix / in some column names
names(predprey) <- sub("_._", ".", names(predprey)) #fix / in some column names
#View(predprey)


######predatorlength 
unique(predprey$Predator_length_unit)#needs fixing to all mm, \xb5m is unicode for µm
predprey$Predator_length_unit <- gsub("\xb5m", "um",predprey$Predator_length_unit)

predprey <- predprey %>% mutate(multiplierpredlengthunit = case_when(
  Predator_length_unit == 'mm'~ 1,
  Predator_length_unit == 'cm' ~ 10,
  Predator_length_unit == 'um' ~ 0.001))
predprey <- predprey %>% mutate(Predator_length = Predator_length*multiplierpredlengthunit) 

predprey$Predator_length_unit <- gsub("cm", "mm",predprey$Predator_length_unit)
predprey$Predator_length_unit <- gsub("um", "mm",predprey$Predator_length_unit)

######prey length 
unique(predprey$Prey_length_unit) # needs fixing to all mm, \xb5m is unicode for µm
predprey$Prey_length_unit <- gsub("\xb5m", "um",predprey$Prey_length_unit)

predprey <- predprey %>% mutate(multiplierpreylengthunit = case_when(
  Prey_length_unit == 'mm'~ 1,
  Prey_length_unit == 'cm' ~ 10,
  Prey_length_unit == 'um' ~ 0.001))
predprey <- predprey %>% mutate(Prey_length = Prey_length*multiplierpreylengthunit) 

predprey$Prey_length_unit <- gsub("cm", "mm",predprey$Prey_length_unit)
predprey$Prey_length_unit <- gsub("um", "mm",predprey$Prey_length_unit)

######pred mass
unique(predprey$Predator_mass_unit) 

predprey <- predprey %>% mutate(multiplierpreylengthunit = case_when(
  Predator_mass_unit == 'g'~ 1000))
predprey <- predprey %>% mutate(Predator_mass = Predator_mass*multiplierpreylengthunit) 

predprey$Predator_mass_unit <- gsub("g", "mg",predprey$Predator_mass_unit)

######prey mass
unique(predprey$Prey_mass_unit) # must convert to all mg

predprey <- predprey %>% mutate(multiplierpreymassunit = case_when(
  Prey_mass_unit == 'mg'~ 1,
  Prey_mass_unit == 'g'~ 1000))
predprey <- predprey %>% mutate(Prey_mass = Prey_mass*multiplierpreymassunit) 

predprey$Prey_mass_unit <- gsub("g", "mg",predprey$Prey_mass_unit)


######Other corrections
unique(predprey$Prey_conversion_to_mass_reference) # sub 'n/a' with blank
predprey$Prey_conversion_to_mass_reference <- gsub("n/a", " ",predprey$Prey_conversion_to_mass_reference)
unique(predprey$Prey_conversion_to_mass_reference)

unique(predprey$Specific_habitat) #Correct for lower and uppercase shelf
predprey$Specific_habitat <- gsub("shelf","Shelf",predprey$Specific_habitat)
unique(predprey$Specific_habitat)


#Convert latitude and longitude to decimal values
unique(predprey$Latitude)

predprey$Latitude <- gsub("\xbc"," ", predprey$Latitude)
predprey$Latitude <- gsub("'N","", predprey$Latitude)
predprey$Latitude <- gsub("'S","", predprey$Latitude)

predprey$Latitude = measurements::conv_unit(predprey$Latitude, from = 'deg_dec_min',to = 'dec_deg')



#write the CSV
write.csv(predprey, file = 'predpreyaltered.csv')
