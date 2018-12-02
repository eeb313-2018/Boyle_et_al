

predprey <- read_csv("predpreyaltered.csv",col_names=TRUE)

# Remove unnecessary columns to make the table easier to view
predprey <- predprey %>% 
  select(-c(X1,In_ref_ID,
            Predator_dimension_measured,
            Predator_standard_length,
            Predator_fork_length,
            Predator_total_length,
            Predator_TL.FL.SL_conversion_reference,
            Standardised_predator_length,
            Predator_measurement_type,
            Predator_length_mass_conversion_method,
            Predator_length_mass_conversion_reference,
            Predator_quality_of_length_mass_conversion,
            Predator_mass_check,
            Predator_mass_check_diff,
            Predator_ratio_mass.mass,
            SI_predator_mass,
            Prey_conversion_to_length_method,
            Prey_quality_of_conversion_to_length,
            Prey_conversion_to_length_reference,
            SI_prey_length,
            Prey_dimension_measured,
            Prey_width,
            Prey_width_unit,
            Prey_measurement_type,
            Prey_mass_check,
            Prey_mass_check_diff,
            Prey_ratio_mass.mass,
            SI_prey_mass,
            Prey_conversion_to_mass_method,
            Prey_conversion_to_mass_reference,
            Prey_quality_of_conversion_to_mass,
            Notes.assumptions,
            multiplierpredlengthunit,
            multiplierpreylengthunit,
            multiplierpreymassunit,
            multiplier,multiplier_longitude))
View(head(predprey))

# Fix Duplicates of Adult and adult in Predator_lifestage --> make only adult 
predprey$Predator_lifestage <- gsub("Adult","adult",predprey$Predator_lifestage)
# Fix spaces areound the / in "larva / juvenile"
predprey$Predator_lifestage <- gsub("larva / juvenile","larva/juvenile",predprey$Predator_lifestage)

unique(predprey$Predator_lifestage) # Ensure correct fixes



#### Life Stage vs. Depth Analyses ####

# Determine mean depth for each age class
predprey %>% 
  group_by(Predator_lifestage) %>% 
  summarise(Mean=mean(Depth))

# Run an anova to test for depth differences between the different age classes
life_stage_depth.anova <- aov(predprey$Depth ~ predprey$Predator_lifestage)
summary(life_stage_depth.anova)

# Anova test showed significance, use a Tukey HSD test to determine which groups differ
TukeyHSD(life_stage_depth.anova)

# Remove these three age classes because they are unclear/small and the graph will be more illistative with just adult, juvenile, larva
pred_depth_plot_data <- predprey %>% 
  group_by(Predator_lifestage) %>% 
  filter(Predator_lifestage!="postlarva/juvenile") %>% 
  filter(Predator_lifestage!="postlarva") %>% 
  filter(Predator_lifestage!="larva/juvenile")

# Make a plot of predator lifestage vs. the lifestage depth to illustrate where they respond
pred_depth_plot_data %>% 
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
