install.packages("here")

         
### This is my first script
### Created by: Claudia Rea
### Created on: 2021-02-03
############################

## Libraries ##
library(tidyverse)
library(here)         

#### Read in Data ###
WeightData <- read.csv(here("Week_2","data","weightdata.csv"))
#### Data Analysis##
## Looks at the top 6 lines of the dataframe
head(WeightData)
## Looks at the bottom 6 lines of the dataframe
tail(WeightData)
## opens  new window to look at the entire dataframe
view(WeightData)
