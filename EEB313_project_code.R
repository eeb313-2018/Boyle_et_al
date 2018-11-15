library(tidyverse)

# setwd("your local working directory")

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
unique(predprey$Predator_length_unit) #needs fixing to all mm, \xb5m is unicode for µm


unique(predprey$Prey_length_unit) # needs fixing to all mm, \xb5m is unicode for µm


unique(predprey$Prey_mass_unit) # must convert to all mg


unique(predprey$Prey_conversion_to_mass_reference) # sub 'n/a' with blank
gsub("n/a", "",predprey$Prey_conversion_to_mass_reference)
unique(predprey$Prey_conversion_to_mass_reference)

unique(predprey$Specific_habitat) #Correct for lower and uppercase shelf
gsub("shelf","Shelf",predprey$Specific_habitat)
unique(predprey$Specific_habitat)


#Convert latitude and longitude to decimal values