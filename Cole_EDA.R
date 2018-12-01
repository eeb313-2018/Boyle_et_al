setwd('C:/Users/coleb/Documents/GitHub/Boyle_et_al-master/Boyle_et_al')
predprey <- read.csv(file = 'predpreyaltered.csv')

#packages
library(tidyverse);library(RColorBrewer);library(scales); library(grid); library(extrafont)

#inital subbing/other commands

predprey$Predator_lifestage <- gsub("Adult", "adult",predprey$Predator_lifestage)

predprey <- predprey %>% 
  mutate(logPredator_length = log10(Predator_length),
         logPrey_length = log10(Prey_length),
         logPredator_mass = log10(Predator_mass),
         logPrey_mass = log10(Prey_mass))
 
#make theme for all plots
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

#lets look at how mean predator and prey sizes differ between our regions and between specific habitats
meanpredlength_geoloc <- predprey %>% 
  group_by(Geographic_location) %>% 
  summarize(Mean = mean(logPredator_length, na.rm = TRUE)) %>% 
  filter(Mean > 0) %>% 
  ggplot() +
  geom_bar(aes(x = reorder(Geographic_location, -Mean), y = Mean), colour = 'grey68', 
           fill = 'turquoise3', alpha = 0.5, width = 0.8, base = 10, stat = 'identity') +
  fte_theme() +
  theme(legend.position = 'none') +
  labs(x = 'Geographic Location', y = 'Mean Predator Length (log10)', title = 'Predator Length by Geographic Location')
meanpredlength_geoloc

predprey$Mean_annual_temp <- as.factor(predprey$Mean_annual_temp) #do this so the next plot will work

meanpredmass_geoloc <- predprey %>% 
  group_by(Mean_annual_temp) %>% 
  summarize(Mean = mean(logPredator_mass, na.rm = TRUE)) %>% 
  filter(Mean > 0) %>% 
  ggplot(.) +
  geom_bar(aes(x = Mean_annual_temp, y = Mean), colour = 'black', 
           fill = 'red1', alpha = 0.7, width = 0.7, stat = 'identity') +
  fte_theme() +
  theme(legend.position = 'none') +
  labs(x = 'Mean Annual Temperature', y = 'Mean Predator Mass (log10)', title = 'Predator Mass by Mean Temperature')
meanpredmass_geoloc

predprey$Mean_annual_temp <- as.numeric(predprey$Mean_annual_temp) #change it back just for whoever else uses it

#### Visually easy to see here that Bergman's rule does not really fit, judging only from the names 
#### of the locations





meanpredmass_geoloc

meanpreylength_geoloc <- predprey %>% 
  group_by(Geographic_location) %>% 
  summarize(Mean = mean(logPrey_length, na.rm = TRUE)) %>% 
  filter(Mean > 0) %>% 
  ggplot(.) +
  geom_bar(aes(x = reorder(Geographic_location, -Mean), y = Mean), colour = 'grey68',
           fill = 'turquoise3', alpha = 0.5, stat = 'identity') +
  theme_classic() +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 90, size = 6))



# how well do the traits here (fork length, mass) predict the predator in a) specific habitats, b) latitudes
summary(predprey$Predator_length)
predprey <- predprey %>% 
  mutate(PredSize = ifelse(Predator_length < 300, 'Small', 
         ifelse(Predator_length > 300 & Predator_length < 1200 , 'Medium', 'Large'))) 
predprey$PredSize <- as.factor(predprey$PredSize)


predprey %>% 
  group_by(Latitude)
  summarize(Mean = mean(logPrey_mass))
  ggplot(aes(x = Latitude, y = Mean)) +
  geom_point()

predprey$Depth <- as.factor(predprey$Depth)
# do species themselves show larger averages within species as they move across latitudes 
predprey$Latitude <- as.factor(predprey$Latitude)
summary(predprey$Latitude)
p1 <- predprey %>% 
  filter(Latitude == '-12') %>% 
  summarize(Mean = mean(Predator_length)) %>% 
  ggplot(., aes(x = Depth, y = Mean)) +
  geom_bar()

pred1 <- predprey %>% 
  filter(Latitude == '-12')
summary(pred1$Depth)  
# do species in the same functional groups (or something like that) show these latitudinal gradients

tuna <- predprey %>% 
  filter(Predator_common_name %in% c('Yellowfin tuna', 'Bigeye tuna', 'Atlantic bluefin tuna', 'Albacore'))
sharks <- predprey %>% 
  filter(Predator_common_name %in% c('Atlantic sharpnose shark', 'Blacktip shark', 'Finetooth shark'))

summary(tuna$Latitude)

sharks %>% 
  ggplot() +
  geom_bar(aes(x = Latitude, y = logPredator_length), stat = 'identity')


### make a plot separating the ocean into it's depth layers (x axis) and then the symbol is the north and south hemisphere
### and then have the 






