### Created by: Claudia Rea
### Created on: 2021-02-08

#### Libraries ###
install.packages("palmerpenguins")
library("palmerpenguins")
library(tidyverse)
glimpse(penguins)

###Start w/ penguin dataframe ###
ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm,## defines axis labels
                     y = bill_length_mm,
                     color = species,
                     size = body_mass_g,
                     alpha = flipper_length_mm)) + ## color can be categorized
 geom_point()+
  labs(title = "Bill depth and lenght",
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x = "Bill depth (mm)", y = "Bill length (mm)",
       color = "Species",
       shape = "Species",
       caption = "Source: Palmer Station LTER / palmerpenguins package")+
  scale_color_viridis_d()

ggplot(penguins,
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           color = species))+
  geom_point()+
  scale_color_viridis_d()+
  facet_grid(sex~species) ## order of a~b determines which is rows which is columns

       