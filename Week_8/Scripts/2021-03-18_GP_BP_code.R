
#Plotting penguin data from palmerpenguin package for GP/ BP
#Created by: Claudia Rea
#Created on 2021-03-18
#Updated on 2021-
#######################################################################
### Load libraries #######
library(palmerpenguins)
library(tidyverse)
library(here)
library(praise)
library(PNWColors)
library(beyonce)
library(ggthemes)


### Load data #########################################################
#The data is part of the package and is called penguins
#Look at data using glimpse
glimpse(penguins)

#### Create boxplot ##################################################
penguins <- na.omit(penguins) #remove NAs in sex and species

penguins <- penguins %>%
  mutate(sex = recode(sex, 'male' = "Male", 'female' = "Female")) #change to proper capitalization

ggplot(data = penguins, 
       mapping = aes(x = species, #compare species for bill depth (mm)
                     y = body_mass_g,
                     fill = species), #fill color species
       stat = "identity") +  
  geom_boxplot() +
  facet_grid(~sex, switch = "x") + #facet for sex within species
  scale_fill_manual(values = pnw_palette(11)[c(4, 2, 6)]) + #PNW palette fill
  labs(title = "Comparison of Body Mass (g) Across Species Within Sex", #add title
       x = "Species", #change x axis label
       y = "Body Mass (g)", #change y axis label
       fill = "Species", #change legend label
       caption = "Source: Palmer Station LTER / palmerpenguins packages") + #add caption
  theme_classic() + #change theme
  theme(axis.title.x = element_text(color = "deepskyblue4", face = 2), #change axis text color
        axis.title.y = element_text(color = "deepskyblue4", face = 2))+
  ggsave(here("Week_7", "Output", "Good_Plot.png"), #save plot
         width = 7, height = 5)


## bad plot
# did not remove NAs in sex and species

penguins <- penguins %>%
  mutate(sex = recode(sex, 'male' = "Male", 'female' = "Female")) #change to proper capitalization

ggplot(data = penguins, 
       mapping = aes(x = body_mass_g, #compare species for body mass, 
                     y = sex,# changed y to lose context
                     color = year, # added unnecessary year by color legend
                     fill = sex), #fill color sex
       stat = "identity") +  #took out facet grid to lose species categorization
  geom_jitter() + #jitter does not portray a clear picture of the data 
  labs(title = "Body Mass of Boy and Girl Wet Birds", #uninformative title & no caption
       x = "Weight", # switched labels to match data, does not match title (Body Mass)
       y = "Penguins", #change y axis label
       fill = "Type")+ # legend label is too ambiguous and in the wrong place
  theme_economist() + #the theme does not match the figure 
  theme(axis.title.x = element_text(color = "lightyellow", face = 11), #change axis text color, matches with background and is cramped
        axis.title.y = element_text(color = "magenta", face = 5))+ # illegible, wrong language
  ggsave(here("Week_7", "Output", "Bad_Plot.png"), #save plot
         width = 7, height = 5)
