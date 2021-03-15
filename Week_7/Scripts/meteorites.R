library(tidyverse)
library(here)
library(maps)
library(mapdata)
library(mapproj)
# read in .csv
meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")
# world map
world<-map_data("world")
head(world)
# plot 
ggplot()+
  geom_polygon(data = world, 
               aes(x = long, y = lat, group = group),
               color = "black")+
  geom_point(data = meteorites, # add a point at all my sites
             aes(x = long,
                 y = lat))
