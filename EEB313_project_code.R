library(tidyverse)

setwd("C:/Users/coleb/Documents/GitHub/Boyle_et_al-master/Boyle_et_al")

## Always use this code to start so that we have the same column names and cleaned data
predprey <- read_csv("predprey.csv",col_names = TRUE)
View(predprey)

# Fix the spaces in column headers
names(predprey) <- gsub(" ", "_", names(predprey)) #gsub replaces all cases
names(predprey) <- sub("-", "_", names(predprey)) #sub only replaces the first case in each character string
names(predprey) <- sub("__", "_", names(predprey)) #predator taxon started with two spaces
names(predprey) <- gsub("/", ".", names(predprey)) #fix / in some column names
names(predprey) <- sub("_._", ".", names(predprey)) #fix / in some column names
View(predprey)

# Time to check for values which don't make sense

######predatorlength 
unique(predprey$Predator_length_unit)#needs fixing to all mm, \xb5m is unicode for µm


predprey <- predprey %>% mutate(multiplierpredlengthunit = case_when(
  Predator_length_unit == 'mm'~ 1,
  Predator_length_unit == 'cm' ~ 0.1,
  Predator_length_unit == 'um' ~ 0.01))
predprey <- predprey %>% mutate(Predator_length = Predator_length/multiplierpredlengthunit) 

######prey length 
unique(predprey$Prey_length_unit) # needs fixing to all mm, \xb5m is unicode for µm
predprey <- predprey %>% mutate(multiplierpreylengthunit = case_when(
  Prey_length_unit == 'mm'~ 1,
  Prey_length_unit == 'cm' ~ 0.1,
  Prey_length_unit == 'um' ~ 0.01))
predprey <- predprey %>% mutate(Prey_length = Predator_length/multiplierpreylengthunit) 

unique(predprey$Predator_mass_unit) 

######prey mass
unique(predprey$Prey_mass_unit) # must convert to all mg
predprey <- predprey %>% mutate(multiplierpreymassunit = case_when(
  Prey_mass_unit == 'mg'~ 0.001,
  Prey_mass_unit == 'g'~ 1))
predprey <- predprey %>% mutate(Prey_mass = Prey_mass/multiplierpreymassunit) 


unique(predprey$Prey_conversion_to_mass_reference) # sub 'n/a' with blank
predprey$Prey_conversion_to_mass_reference <- gsub("n/a", " ",predprey$Prey_conversion_to_mass_reference)
unique(predprey$Prey_conversion_to_mass_reference)

unique(predprey$Specific_habitat) #Correct for lower and uppercase shelf
predprey$Specific_habitat <- gsub("shelf","Shelf",predprey$Specific_habitat)
unique(predprey$Specific_habitat)


#Convert latitude and longitude to decimal values

