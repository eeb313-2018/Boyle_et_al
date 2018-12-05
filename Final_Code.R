### Load Libraries
library(tidyverse)
library(car)
library(psych)
library(multcomp)
library(measurements)
library(RColorBrewer)
library(MASS)
library(pracma)
library(maps)
library(ggrepel)
library(mapdata)

### Load Data
setwd('C:/Users/coleb/Documents/GitHub/Boyle_et_al-master/Boyle_et_al')
predprey <- read_csv(file = 'predprey.csv',col_names = TRUE)

### Make theme to go on all the plots 
fte_theme <- function(){
  color.background = 'ghostwhite'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill=color.background,color = color.background)) +
    theme(plot.background = element_rect(fill = color.background, color = color.background)) +
    theme(panel.border = element_rect(colour = 'black')) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, size = 15, vjust = 1.25)) +
    theme(axis.text.x = element_text(size = 8, color = color.axis.text, angle = 90)) + 
    theme(axis.text.y = element_text(size = 10, color = color.axis.text)) + 
    theme(axis.title.x = element_text(size = 12, color = color.axis.title, vjust = 0)) +
    theme(axis.title.y = element_text(size = 12, color = color.axis.title, vjust = 1.25)) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(axis.line.x = element_line(color="black", size = 0.15),
          axis.line.y = element_line(color="black", size = 0.15)) 
}


######## Data Fixing


### Fix the spaces in column headers
names(predprey) <- gsub(" ", "_", names(predprey)) #gsub replaces all cases
names(predprey) <- sub("-", "_", names(predprey)) #sub only replaces the first case in each character string
names(predprey) <- sub("__", "_", names(predprey)) #predator taxon started with two spaces
names(predprey) <- gsub("/", ".", names(predprey)) #fix / in some column names
names(predprey) <- sub("_._", ".", names(predprey)) #fix / in some column names

### Fix a naming inconsistency
unique(predprey$Predator_lifestage)
predprey$Predator_lifestage <- gsub("Adult", "adult",predprey$Predator_lifestage)
predprey$Predator_lifestage <- gsub("larva / juvenile","larva/juvenile",predprey$Predator_lifestage)

### Predator Length Inconsistencies
unique(predprey$Predator_length_unit)#needs fixing to all mm, \xb5m is unicode for µm
predprey$Predator_length_unit <- gsub("\xb5m", "um",predprey$Predator_length_unit)

predprey <- predprey %>% mutate(multiplierpredlengthunit = case_when(
  Predator_length_unit == 'mm'~ 1,
  Predator_length_unit == 'cm' ~ 10,
  Predator_length_unit == 'um' ~ 0.001))
predprey <- predprey %>% mutate(Predator_length = Predator_length*multiplierpredlengthunit) 

predprey$Predator_length_unit <- gsub("cm", "mm",predprey$Predator_length_unit)
predprey$Predator_length_unit <- gsub("um", "mm",predprey$Predator_length_unit)

### Prey Length Inconsistencies
unique(predprey$Prey_length_unit) # needs fixing to all mm, \xb5m is unicode for µm
predprey$Prey_length_unit <- gsub("\xb5m", "um",predprey$Prey_length_unit)

predprey <- predprey %>% mutate(multiplierpreylengthunit = case_when(
  Prey_length_unit == 'mm'~ 1,
  Prey_length_unit == 'cm' ~ 10,
  Prey_length_unit == 'um' ~ 0.001))
predprey <- predprey %>% mutate(Prey_length = Prey_length*multiplierpreylengthunit) 

predprey$Prey_length_unit <- gsub("cm", "mm",predprey$Prey_length_unit)
predprey$Prey_length_unit <- gsub("um", "mm",predprey$Prey_length_unit)

### Predator Mass Fixing
unique(predprey$Predator_mass_unit) 

predprey <- predprey %>% mutate(multiplierpreylengthunit = case_when(
  Predator_mass_unit == 'g'~ 1000))
predprey <- predprey %>% mutate(Predator_mass = Predator_mass*multiplierpreylengthunit) 

predprey$Predator_mass_unit <- gsub("g", "mg",predprey$Predator_mass_unit)

### Prey Mass Fixing
unique(predprey$Prey_mass_unit) # must convert to all mg

predprey <- predprey %>% mutate(multiplierpreymassunit = case_when(
  Prey_mass_unit == 'mg'~ 1,
  Prey_mass_unit == 'g'~ 1000))
predprey <- predprey %>% mutate(Prey_mass = Prey_mass*multiplierpreymassunit) 

predprey$Prey_mass_unit <- gsub("g", "mg",predprey$Prey_mass_unit)
predprey$Prey_mass_unit <- gsub("mmg", "mg",predprey$Prey_mass_unit)

### Other Miscenaleous Corrections
unique(predprey$Prey_conversion_to_mass_reference) # sub 'n/a' with blank
predprey$Prey_conversion_to_mass_reference <- gsub("n/a", " ",predprey$Prey_conversion_to_mass_reference)
unique(predprey$Prey_conversion_to_mass_reference)

unique(predprey$Specific_habitat) #Correct for lower and uppercase shelf
predprey$Specific_habitat <- gsub("shelf","Shelf",predprey$Specific_habitat)
unique(predprey$Specific_habitat)

### NOTE TO INSTRUCTORS: our code below (commented out) is the code we used to transform out lat and long variables
### to decimal degrees. Given the ratchet state of the code below, this was obviously a challenge to do, and as such,
### we had to use a method that we suspect is operating system-specific (the first line of code where we use
### '...gsub("\xba"," ", predprey$Latitude) is the issue area), so we're leaving this code here so you can see how 
### we did it, but what we did is just run this code on the computer it worked on and then copied the lat and long (only)
### into the raw data .csv that we loaded in above. We couldn't figure out another way to make this code work in a 
### perfectly reproducible way. Ce la vie!! 

# ### Convert latitude and longitude to decimal values
# predprey$Latitude <- gsub("\xba"," ", predprey$Latitude)
# 
# predprey <- predprey %>%
#   mutate(multiplier = ifelse(grepl("S", Latitude), -1, +1))
# 
# predprey$Latitude <- gsub("'N","", predprey$Latitude)
# predprey$Latitude <- gsub("'S","", predprey$Latitude)
# 
# unique(predprey$Latitude)
# predprey$Latitude = measurements::conv_unit(predprey$Latitude, from = 'deg_dec_min',to = 'dec_deg')
# predprey$Latitude <- as.numeric(predprey$Latitude)
# predprey$Latitude <- predprey$Latitude * predprey$multiplier
# 
# 
# predprey$Longitude <- gsub("\xba"," ", predprey$Longitude)
# 
# predprey <- predprey %>%
#   mutate(multiplier_longitude = ifelse(grepl("W", Longitude), -1, +1))
# 
# predprey$Longitude <- gsub("'W","", predprey$Longitude)
# predprey$Longitude <- gsub("'E","", predprey$Longitude)
# 
# predprey$Longitude = measurements::conv_unit(predprey$Longitude, from = 'deg_dec_min',to = 'dec_deg')
# predprey$Longitude <- as.numeric(predprey$Longitude)
# 
# predprey$Longitude <- predprey$Longitude * predprey$multiplier_longitude

### For when we end up playing with the str of latitude, lets save it now as a numeric vector
latnum <- predprey$Latitude

######## Exploratory plotting

### Predator length (untransformed) vs Latitude
PredLenvLat <- predprey %>% 
  ggplot(aes(x=Latitude, y = Predator_length))+
  geom_smooth(method="lm", se=F)+
  geom_jitter(alpha = 0.3)+
  fte_theme() +
  labs(x = "Latitude", y = "Predator Length")
PredLenvLat

### Prey Length (untransformed) vs Latitude
PreyLenvLat <- predprey %>% 
  ggplot(aes(x=Latitude, y = Prey_length))+
  geom_smooth()+
  geom_jitter(alpha = 0.3)+
  geom_point()+
  labs(x = "Latitude", y = "Prey lenth")+  
  fte_theme()
PreyLenvLat

### Mean Predator Length (untransformed) vs Geographic Location (name of Geographic location tells us something about latitude)
MeanPredLenvGeoLoc <- predprey %>% 
  group_by(Geographic_location) %>% 
  summarize(Mean = mean(Predator_length, na.rm = TRUE)) %>% 
  filter(Mean > 0) %>% 
  ggplot() +
  geom_bar(aes(x = reorder(Geographic_location, -Mean), y = Mean), colour = 'grey68', 
           fill = 'turquoise3', alpha = 0.5, width = 0.8, base = 10, stat = 'identity') +
  fte_theme() +
  theme(legend.position = 'none') +
  labs(x = 'Geographic Location', y = 'Mean Predator Length', title = 'Predator Length by Geographic Location')
MeanPredLenvGeoLoc
#hmm so potentially a problem here in that all the biggest things seem to be concentrated in Maine


### MeanPredator Mass (untransformed) vs Geographic Location (name of Geographic location tells us something about latitude)
MeanPredMassvGeoLoc <- predprey %>% 
  group_by(Geographic_location) %>% 
  summarize(Mean = mean(Predator_mass, na.rm = TRUE)) %>% 
  filter(Mean > 0) %>% 
  ggplot() +
  geom_bar(aes(x = reorder(Geographic_location, -Mean), y = Mean), colour = 'grey68', 
           fill = 'turquoise3', alpha = 0.5, width = 0.8, base = 10, stat = 'identity') +
  fte_theme() +
  theme(legend.position = 'none') +
  labs(x = 'Geographic Location', y = 'Mean Predator Length', title = 'Predator Length by Geographic Location')
MeanPredMassvGeoLoc
#hmm so potentially a problem here in that all the biggest things seem to be concentrated in Maine

### Number of Observations of Predator Species (Common Name)
CountperPredSP <- ggplot(data = predprey) +
  geom_bar(aes(x = Predator_common_name), colour = 'black', fill = 'red2',
           position = position_dodge(width=2))+
  fte_theme() +
  labs(x = 'Predator Species', y = 'Number of Observations')
CountperPredSP

### Number of Observations of Prey Species (Common Name)
CountperPreySP <- ggplot(data = predprey) +
  geom_bar(aes(x = Prey_common_name), colour = 'black', fill = 'red2',
           position = position_dodge(width=2))+
  fte_theme() +
  labs(x = 'Prey Species', y = 'Number of Observations')
CountperPreySP

### So obviously our mass and length data are annoying to deal with, lets log-transform them and see
### what some visualizations look like after that

### Log transform
predprey <- predprey %>% 
  mutate(logPredator_length = log10(Predator_length),
         logPrey_length = log10(Prey_length),
         logPredator_mass = log10(Predator_mass),
         logPrey_mass = log10(Prey_mass))

### Log Predator Length vs. Geographical Location
MeanLogPredLengthvGeoLoc <- predprey %>% 
  group_by(Geographic_location) %>% 
  summarize(Mean = mean(logPredator_length, na.rm = TRUE)) %>% 
  filter(Mean > 0) %>% 
  ggplot() +
  geom_bar(aes(x = reorder(Geographic_location, -Mean), y = Mean), colour = 'grey68', 
           fill = 'turquoise3', alpha = 0.5, width = 0.8, base = 10, stat = 'identity') +
  fte_theme() +
  theme(legend.position = 'none') +
  labs(x = 'Geographic Location', y = 'Mean Predator Length (log10)', title = 'Predator Length by Geographic Location')
MeanLogPredLengthvGeoLoc

MeanLogPredMassvGeoLoc <- predprey %>% 
  group_by(Geographic_location) %>% 
  summarize(Mean = mean(logPredator_mass, na.rm = TRUE)) %>% 
  filter(Mean > 0) %>% 
  ggplot() +
  geom_bar(aes(x = reorder(Geographic_location, -Mean), y = Mean), colour = 'grey68', 
           fill = 'turquoise3', alpha = 0.5, width = 0.8, base = 10, stat = 'identity') +
  fte_theme() +
  theme(legend.position = 'none') +
  labs(x = 'Geographic Location', y = 'Mean Predator Length (log10)', title = 'Predator Length by Geographic Location')
MeanLogPredMassvGeoLoc

### Plot of mean Annual Temperature vs predator length 
predprey$Mean_annual_temp <- as.factor(predprey$Mean_annual_temp)
MeanLogPredLengthvTemp <- predprey %>% 
  group_by(Mean_annual_temp) %>% 
  summarize(Mean = mean(log10(Predator_length), na.rm = TRUE)) %>% 
  filter(Mean > 0) %>% 
  ggplot(aes(x = Mean_annual_temp, y = Mean)) +
  geom_bar(colour = 'grey68', 
           fill = 'turquoise3', alpha = 0.5, width = 0.8, base = 10, stat = 'identity') +
  fte_theme() +
  theme(legend.position = 'none') +
  labs(x = 'Mean Annual Temperature', y = 'Mean Predator Length (log10)', title = 'Predator Length by Temperature')
MeanLogPredLengthvTemp

### So from we can't really see anything sticking out like we thought we were going to see, if Bergmann's Rule was 
### applicable here. So let's try seeing if some models can verify that Bergmann's rule is in fact not really 
### relevant in this scenario

######## Analysis

### Would be nice to use neg. binom regressions here, but as our dependent variables aren't count data, it doesn't work

### Running some models 

### Run model to look at Predator Length being explained by latitude
predprey$Latitude <- as.factor(predprey$Latitude);predprey$Longitude <- as.factor(predprey$Longitude)
Lat_pred_length1<-lm(Predator_length ~ Latitude + Longitude, data = predprey)
plot(Lat_pred_length1)
summary(Lat_pred_length)
# Significant result: We see on average that the predator length increases as we go from Southern lattitudes to more northern ones. Not what we'd expect under bergman's rule. Avg change in length of 1.4257cm/degree of latitude. Rquared value very low so NOT predictive. 
# obviously this violates assumptions underpinning linear regressions 

### Perhaps binning predator size values helps fix the data's skewedness?
summary(predprey$Predator_length)
predprey <- predprey %>% 
  mutate(PredLength = ifelse(Predator_length < 300, '1', 
                           ifelse(Predator_length > 300 & Predator_length < 600 , '2',
                                  ifelse(Predator_length > 600 & Predator_length < 1200, '3',
                                         ifelse(Predator_length > 1200 & Predator_length < 1500, '4',
                          ifelse(Predator_length > 1500 & Predator_length < 1800, '5',
                                 ifelse(Predator_length > 1800 & Predator_length < 2100, '6',
                                        ifelse(Predator_length > 2100 & Predator_length < 2400, '7',
                          ifelse(Predator_length > 2400 & Predator_length < 2700, '8', '9'))))))))) 
BinnedLength <- predprey %>% 
  group_by(Latitude) %>% 
  ggplot(aes(x = PredLength)) +
  geom_histogram(stat = 'count')
# clearly binning doesn't help 

### let's try removing some of our over sampled areas that are also the biggest, and then running 
### a linear model and a plot to see if we can pull anything out
predprey$Latitude <- as.factor(predprey$Latitude)

toremove <- c('49', '50.83333333', '51.86666667', '40.16666667', '40.91666667','41.13333333',
  '42.18333333', '42.66666667', '43.31666667', '43.33333333', '44', '45')
removed40s <- predprey %>% 
  filter(., !(Latitude %in% toremove))
Removedmod <- lm(Predator_mass ~ Latitude+Mean_annual_temp+Longitude+Depth+Geographic_location, data = removed40s)
summary(Removedmod)
plot(Removedmod)
#nothing really jumps out and assumptions are violated 

RemovedTempplot <- removed40s %>% 
  group_by(Mean_annual_temp) %>% 
  summarize(Mean = mean(log10(Predator_mass))) %>% 
  ggplot() +
  geom_bar(aes(x = Mean_annual_temp, y = Mean), colour = 'grey68', fill = 'turquoise3', stat = 'identity')+
  fte_theme() +
  labs(x = 'Mean Annual Temperature', y = 'Mean Predator Mass (log10)')
RemovedTempplot 

### Attempt to run a new model with just the northern hemisphere and the mean of the predator lengths and masses since
### northern one has way more observations
predprey$Latitude <- as.numeric(predprey$Latitude)
predpreymeans <- predprey %>% 
  filter(Latitude > 0) %>% 
  group_by(Latitude) %>% 
  summarize(Mean = (mean(Predator_length)))

### Attempt at plotting the dataset just made
NorthernMeanPredLength <- ggplot(data = predpreymeans, aes(x = Latitude, y = Mean)) +
  geom_point() +
  geom_smooth() +
  fte_theme() +
  labs(x = 'Latitude', y = 'Mean Predator Length in Northern Hemisphere')
NorthernMeanPredLength
# clearly no relationship here, let's confirm with a quick regression

### Quick model to check if the visual relationship holds
lm1 <-lm(nthroot(Mean,3) ~ Latitude, data = predprey2)
par(mfrow = c(2,2))
plot(lm1)
# taking the third root of the mean doesn't fix the non-conformity of the data to the assumptions of a linear model
# thus, this model isn't valid, but out plot above tells us that there is not a pattern here that looks anything like
# we expected to see if Bergmann's rule was true




### Linear model to look at if the prey length shows a pattern across latitude
predprey$Latitude <- as.factor(predprey$Latitude)
Lat_prey_length<-lm(Prey_length~ Latitude , data = predprey)
summary(Lat_prey_length)
plot(Lat_prey_length)
# Prey much smaller than predators but follow same general trend of increasing in size from southern lattitudes to more northern ones. Significant. Not prove Bergman's rule 
# Rsquared value very low so NOT predictive. 

### Linear model looking at a number of predictors for Predator Length
Predict_predator_length<- lm(Predator_length ~ Latitude+Longitude+Predator_taxon+Predator_lifestage+Depth+Prey, data=predprey)
summary(Predict_predator_length)
plot(Predict_predator_length)
# The model is extremely predictive and relatively simple. This additive model with only a few factors is a good predictor of predator length 

### Same model as above but for prey 
Predict_prey_length<- lm(Prey_length ~ Latitude+Longitude+Prey_taxon+Depth+Predator, data=predprey)
summary(Predict_predator_length)
plot(Predict_prey_length)
# Rsquared circa .8 again. Solid model. 




##Comparing North and South of Equator
predprey$Latitude <- latnum
summary(predprey$Latitude)
predpreyN<- predprey %>% 
  filter(Latitude > 0)
  
predpreyS<- predprey %>% 
  filter(Latitude < 0)              
summary(predpreyN$Predator_length)
# Question: how does lattitude predict predator and prey size in each hemisphere? Are the trends different? 

### Linear models to address above question
lmNLat<-lm(Predator_length ~ Latitude, data= predpreyN)
summary(lmNLat)
plot(lmNLat)

lmSlat<- lm(Predator_length ~ Latitude, data= predpreyS)
summary(lmSlat)
plot(lmSlat)

summary(predpreyS$Latitude)
length(unique(predpreyS$Latitude))
length(unique(predpreyN$Latitude))
length(nrow(predpreyS))

# Latitude is much stronger predictor of predator size in the southern hemisphere than in the northern hemisphere. Much more drastic change in the northern hemisphere as well. 

### Same models as above but with Prey 
lmNLatPrey<-lm(Prey_length ~ Latitude, data= predpreyN)
summary(lmNLatPrey)
plot(lmNLatPrey)
lmSlatPrey<- lm(Prey_length ~ Latitude, data= predpreyS)
summary(lmSlatPrey)
plot(lmSlatPrey)

### Linear models testing differnences within hemispheres for mass instead of length 
lmNLatMass<-lm(Predator_mass ~ Latitude, data= predpreyN)
summary(lmNLatMass)
plot(lmNLatMass)

lmSlatMass<- lm(Predator_mass ~ Latitude, data= predpreyS)
summary(lmSlatMass)
plot(lmNLatMass)
# Southern hemisphere is again pretty good at predicting but northern hemisphere has abysmally low Rsquared. 

### Similar models to above looking at predator mass now 
lmNLatMassPy<-lm(Prey_mass ~ Latitude, data= predpreyN)
summary(lmNLatMassPy)
plot(lmNLatMassPy)

lmSlatMassPy<- lm(Prey_mass ~ Latitude, data= predpreyS)
summary(lmSlatMassPy)
plot(lmNLatMassPy)
# Both terrible predictors of prey Mass! 

# Okay so at this point, althought our models are flawed, we can safely assume from our plots etc. that 
# Latitude isn't on its own, necessarily structuring size in this system - so what is?

###### Comparing Sizes at Differing Depths and Lattitudes: 


### Linear models looking at how depth can predict length and mass
PredLengthDepth<- lm(Predator_length ~ Depth, data=predprey)
summary(PredLengthDepth)
plot(PredLengthDepth)

PreyLengthDepth<- lm(Prey_length ~ Depth, data = predprey)
summary(PreyLengthDepth)
plot(PreyLengthDepth)

PredMassDepth<- lm(Predator_mass ~ Depth, data= predprey)
summary(PredMassDepth)
plot(PredMassDepth)

PreyMassDepth<- lm(Prey_mass ~ Depth, data=predprey)
summary(PreyMassDepth)
plot(PreyMassDepth)
# Depth is a much better predictor of Predator length than it is of predator mass. 
# How does this relationship change for Northern and Sourthern Hemisphere? 

### Linear Models examining above trends but separated by hemisphere
NPredLengthDepth<- lm(Predator_length ~ Depth, data=predpreyN)
summary(NPredLengthDepth)
plot(NPredLengthDepth)

NPreyLengthDepth<- lm(Prey_length ~ Depth, data = predpreyN)
summary(NPreyLengthDepth)
plot(NPreyLengthDepth)

NPredMassDepth<- lm(Predator_mass ~ Depth, data= predpreyN)
summary(NPredMassDepth)
plot(NPredMassDepth)

NPreyMassDepth<- lm(Prey_mass ~ Depth, data=predpreyN)
summary(NPreyMassDepth)
plot(NPreyMassDepth)
# Most R squared still garbage. 

PredLengthDepthS<- lm(Predator_length ~ Depth, data=predpreyS)
summary(PredLengthDepthS)
plot(PredLengthDepthS)

PreyLengthDepthS<- lm(Prey_length ~ Depth, data = predpreyS)
summary(PreyLengthDepthS)
plot(PreyLengthDepthS)

PredMassDepthS<- lm(Predator_mass ~ Depth, data= predpreyS)
summary(PredMassDepthS)
plot(PredMassDepthS)

PreyMassDepthS<- lm(Prey_mass ~ Depth, data=predpreyS)
summary(PreyMassDepthS)
plot(PreyMassDepthS)
# Depth is a good predictor of predator length in the southern hemisphere, a moderate predictor of prey lenght, a good predictor of prey length, and a terrible predictor of prey mass. 


###### Does Mean Annual Temperature provide any predictive power?


### Linear model examining mean annual temperature
tempmodel1<- lm(Predator_length~Mean_annual_temp, data=predpreyS)
summary(tempmodel1)
plot(tempmodel1)

tempmodel2<-lm(Prey_length ~ Mean_annual_temp, data=predpreyS) 
summary(tempmodel2)
plot(tempmodel2)

### plot of latitude vs mean annual temp to check our assumption that it's going to get colder closer to the poles
predprey$Latitude <- as.numeric(predprey$Latitude)
MeanTempvLatitude <-predprey %>% 
  ggplot(aes(x=Latitude, y=Mean_annual_temp))+
  geom_point()+
  geom_smooth(se=T, colour = 'red2') +
  fte_theme() +
  labs(x = 'Latitude', y = 'Mean Annual Temperature')
MeanTempvLatitude

#### Let's think about dividing the Ocean into depth classes and seeing if Depth can tell us anything 
# Depth Classes: 
 
# Epipelagic = Depth<200
# Mesopelagic = 200<Depth<1000
# Bathypelagic = 1000<Depth<4000
# Abyssopelagic= 4000<Depth<6000

# Hemisphere Classes:
# Lattitude>0 = Northern_Hemisphere
# Lattitude<0 = Southern_Hemisphere

### Transforming Latitude into a hempishere character named "Hemisphere"
predprey1<-predprey %>% 
mutate(Hemisphere =ifelse(Latitude>0, 'Northern_Hemisphere', 'Southern_Hemisphere'))
summary(predprey1)

### Transfroming Depth into Depth classes: 
# Epipelagic = Depth<200
# Mesopelagic = 200<Depth<1000
# Bathypelagic = 1000<Depth<4000
# Abyssopelagic= 4000<Depth<6000
predprey2<-predprey1 %>% 
mutate(Ocean_Layer = ifelse(Depth<200, 'Epipelagic',
                            ifelse(200<Depth& Depth<1000, "Mesopelagic",
                                   ifelse(1000<Depth &Depth<4000, 'Bathypelagic',
                                          ifelse(4000<Depth &Depth<6000,"Abyssopelagic",'NA')))))

### Let's plot this
OceanLayer1 <- predprey2 %>% 
  group_by(Ocean_Layer, Hemisphere) %>% 
  summarise(Mean=mean(Predator_length,na.rm=T)) %>% 
  ggplot(aes(x=Ocean_Layer, y=Mean, shape=Hemisphere, color=Hemisphere))+
  geom_point(size=4, position=position_dodge(width=0.6))+
  geom_line()+
  scale_x_discrete(limits = c("Epipelagic", "Mesopelagic", "Bathypelagic", "Abyssopelagic"))+
  fte_theme() +
  labs(x = 'Ocean Layer (Depth Class)', y = 'Mean Predator Length')
OceanLayer1


###ANOVA Testing the signifigance of this plot
Dale.anova <- aov(Predator_length ~ Ocean_Layer+Hemisphere + Ocean_Layer:Hemisphere, data=predprey2)
anova(Dale.anova)

hist(resid(Dale.anova))

TukeyHSD(Dale.anova) #To check what's related to each other in detail
plot(Dale.anova)

### Plot mean annual temperature vs mean predator length 
MeanTempvPredLen <- predprey2 %>% 
  group_by(Mean_annual_temp, Hemisphere) %>% 
  summarise(Mean=mean(Predator_length,na.rm=T)) %>% 
  ggplot(aes(x=Mean_annual_temp, y=Mean, shape=Hemisphere, color=Hemisphere))+
  geom_point(size=4, position=position_dodge(width=0.6))+
  geom_line() +
  fte_theme() +
  labs(x = 'Mean Annual Temperature', y = 'Mean Predator Length')
MeanTempvPredLen

###ANOVA Testing the signifigance of this plot as well 
Dale.anova1 <- aov(Predator_length ~ Mean_annual_temp +Hemisphere + Mean_annual_temp:Hemisphere, data=predprey2)
summary(Dale.anova1)

hist(resid(Dale.anova1))

      
plot(Dale.anova1)


###Significant results for both the above models/plots but no homoskedacity of variance. Chosing to ignore this assumption

###### Let's look at longitude and see if that tells us anything?


### Linear Model of predator and prey mass against longitude
predlong<- lm(Predator_mass~Longitude, data = predprey) #significant and negative 
summary(predlong)
plot(predlong)

preylong<-lm(Prey_mass~Longitude, data = predprey) #not significant
summary(preylong)
plot(preylong)

##### What about species richness? Can that be explained for our different groups in our dataset by latitude?

### Add a species richness column to the hemisphere-divided dataset
predprey2 <- predprey2 %>% 
  mutate(Prey_Species_Richness=length(unique(Prey_common_name)), 
         Predator_Species_Richness=length(unique(Predator_common_name)))
predprey2N <- predprey2 %>% 
  filter(Hemisphere=="Northern_Hemisphere")

predprey2S <- predprey2 %>%
  filter(Hemisphere=="Southern_Hemisphere")
### Make different datasets for northern and southern to make models on

### Overall Linear model 
Speciesrichlm <- lm(Predator_Species_Richness ~ Latitude*Hemisphere, data = predprey2)
summary(Speciesrichlm)
plot(Speciesrichlm)
### This model found that latitude was a significant predictor, southern hemisphere showed a significant different trend from northern, and the interaction of southern hemisphere and latitude was also significant.

### plot predator species richness against latitude
PredatorrichlatN <- predprey2N %>% 
  group_by(Latitude) %>% 
  summarize(Predator_Species_Richness=length(unique(Predator_common_name))) %>% #find the unique number of species at each latitude
  ggplot(.,aes(x=Latitude, y=Predator_Species_Richness))+
  geom_point()+
  geom_smooth(method = 'lm')+
  fte_theme()+
  labs(x = 'Latitude', y = 'Predator Species Richness')
PredatorrichlatN

PredatorrichlmN <- lm(Predator_Species_Richness ~ Latitude, data = predprey2N)
summary(PredatorrichlmN) #significant
plot(PredatorrichlmN) #looks pretty normal!

PreyrichlatN <- predprey2N %>% 
  group_by(Latitude) %>% 
  summarize(Prey_Species_Richness=length(unique(Prey_common_name))) %>% 
  ggplot(.,aes(x=Latitude, y=Prey_Species_Richness))+
  geom_point()+
  geom_smooth(method = 'lm')+
  fte_theme()+
  labs(x = 'Latitude', y = 'Prey Species Richness')
PreyrichlatN

PreyrichlmN <- lm(Prey_Species_Richness ~ Latitude, data = predprey2N)
summary(PreyrichlmN) #significant
plot(PreyrichlmN)

PredatorrichlatS <- predprey2S %>% 
  group_by(Latitude) %>% 
  summarize(Predator_Species_Richness=length(unique(Predator_common_name))) %>% #find the unique number of species at each latitude
  ggplot(.,aes(x=Latitude, y=Predator_Species_Richness))+
  geom_point()+
  geom_smooth(method = 'lm')+
  fte_theme()+
  labs(x = 'Latitude', y = 'Predator Species Richness')
PredatorrichlatS

PredatorrichlmS <- lm(Predator_Species_Richness ~ Latitude, data = predprey2S)
summary(PredatorrichlmS) #non-significant
plot(PredatorrichlmS)

PreyrichlatS <- predprey2S %>% 
  group_by(Latitude) %>% 
  summarize(Prey_Species_Richness=length(unique(Prey_common_name))) %>% 
  ggplot(.,aes(x=Latitude, y=Prey_Species_Richness))+
  geom_point()+
  geom_smooth(method = 'lm')+
  fte_theme()+
  labs(x = 'Latitude', y = 'Prey Species Richness')
PreyrichlatS

PreyrichlmS <- lm(Prey_Species_Richness ~ Latitude, data = predprey2S)
summary(PreyrichlmS) #not significant
plot(PreyrichlmS)

### How about running a PCA

#select only the traits we think will have biological meaning
cutpredprey<-predprey %>% 
dplyr::select(Predator, Predator_common_name, Predator_total_length, Predator_mass, Prey_common_name, Prey_taxon, Prey_length, Prey_mass, Geographic_location, Latitude, Longitude, Depth, Mean_annual_temp, Specific_habitat)

#change all traits to be numeric, to do PCA
str(cutpredprey)
cutpredprey$Predator<- as.numeric(cutpredprey$Predator)
cutpredprey$Predator_common_name<- as.numeric(cutpredprey$Predator_common_name)
cutpredprey$Prey_common_name<- as.numeric(cutpredprey$Prey_common_name)
cutpredprey$Prey_taxon<- as.numeric(cutpredprey$Prey_taxon)
cutpredprey$Geographic_location<- as.numeric(cutpredprey$Geographic_location)
cutpredprey$Depth<- as.numeric(cutpredprey$Depth)
cutpredprey$Specific_habitat<- as.numeric(cutpredprey$Specific_habitat)

cutpredprey<- cutpredprey[complete.cases(cutpredprey),] #Only use complete cases to make things easier
predprey_cor <- cor(cutpredprey) #create a correlation matrix
det(predprey_cor) #we want to see that our determinant is greater than the necessary value of 0.00001, and it is

### Running the first principal test:
pc1 <- principal(predprey_cor, nfactors =14, rotate = "none") #Put 14 factors because this is how many traits we left in
pc1
#There are six SS loadings with a value over 1, meaning only 6 factors are probably important
plot(pc1$values, type = "b") #6 components from the scree plot is also a reasonable estimate

### Let's run the function again with only 6 factors:
pc2 <- principal(predprey_cor, nfactors = 6, rotate = "none")
pc2

### Finally, let's run the function using rotate to adjust the values:
pc3 <- principal(predprey_cor, nfactors = 6, rotate = "varimax")
pc3

a<-print.psych(pc3, cut = 0.3, sort = TRUE) #To create a nice read out of our groups
# The important groups here could be the clumping of depth, temperature, and predator. Another could be specific habitat and its correlation with predator mass. Finally, prey common name and latitude are also highly correlated. 


###### Let's see if we can make some conclusions about how depth is structuring the age classes we see in this data

### Determine mean depth for each age class
predprey %>% 
  group_by(Predator_lifestage) %>% 
  summarise(Mean=mean(Depth))


### Run an anova to test for depth differences between the different age classes
life_stage_depth.anova <- aov(predprey$Depth ~ predprey$Predator_lifestage)
summary(life_stage_depth.anova)

# ANOVA assumes normality so we must test the model residuals
hist(residuals(life_stage_depth.anova))
# Output of residuals is approximately normal

# ANOVA assumes independent measures, which the study used

# ANOVA assumes equality of variances
bartlett.test(Depth~Predator_lifestage, data=predprey)
# Output has a p-value of 2.2e-16 indicating that variances are not equal
# Therefore, this ANOVA is invalid, however, equality of variances is sometimes overlooked and this ANOVA can give us useful information

### Anova test showed significance, use a Tukey HSD test to determine which groups differ
TukeyHSD(life_stage_depth.anova)


### Remove these three age classes because they are unclear/small and the graph will be more illistative with just adult, juvenile, larva
pred_depth_plot_data <- predprey %>% 
  group_by(Predator_lifestage) %>% 
  filter(Predator_lifestage!="postlarva/juvenile") %>% 
  filter(Predator_lifestage!="postlarva") %>% 
  filter(Predator_lifestage!="larva/juvenile")

### Make a plot of predator lifestage vs. the lifestage depth to illustrate where they respond
pred_depth_plot <- pred_depth_plot_data %>% 
  ggplot() +
  geom_point(aes(y=-Depth,
                 x=Predator,
                 colour=Predator_lifestage,
                 size=Predator_lifestage)) +
  scale_size_manual(values=c(5,3.5,2)) +    # Adjusts the sizes of the three different Predator Lifestages
  geom_abline(intercept=0,
              slope=0,
              colour="blue",
              size=1) +
  guides(colour=guide_legend(title="Predator Lifestage"),   # Ensures that there are not three duplicate legends
         size=guide_legend(title="Predator Lifestage")) +   # Ensures that there are not three duplicate legends
  labs(y = "Depth (m)") +
  theme(axis.text.x = element_blank(),      # remove axis text labels
        axis.title.x = element_blank(),     # remove axis title label
        axis.ticks.x = element_blank()) +   # remove axis tick marks
  theme(legend.position = "bottom") +       # legend at the bottom instead of side of plot
  fte_theme()
pred_depth_plot

### Map of Study Area
coords <- data.frame(c(unique(predprey$Latitude)), (unique(predprey$Longitude)))
colnames(coords) <- c('lat', 'long')



global <- map_data('world')
map <- ggplot() + geom_polygon(data = global, aes(x = long, y = lat, group = group), 
                               fill = 'grey80', colour = 'grey40') +
  coord_fixed(1.3) +
  geom_point(data = coords, aes(long,lat), color = 'red3', size = 2.5)+
  fte_theme() +
  geom_text_repel(xlim = c(-180,180), ylim = c(-180, 180))+
  labs(x = 'Longitude', y = 'Latitude', title = 'Sampling Sites Across the Globe')+
  scale_x_continuous(limits = c(-180,180))
map

