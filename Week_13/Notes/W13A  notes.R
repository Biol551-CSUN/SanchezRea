##Week 13 A notes
## Created by: Claudia Rea
## created on: 20210426
##  Libbraries 
library(tidyverse)
library(here)
# simple for loop 
print(paste("The year is",2000))
# put it in a real for loop 
years <- c(2015:2021)
years<-c(2015:2021)
for (i in years){ # set up the for loop where i is the index
  print(paste("The year is", i)) # loop over i
}

# how to sAVE
#Pre-allocate space for the for loop, R is too slow if it has to measure the space on its own
# empty matrix
year_data<-data.frame(matrix(ncol = 2, nrow = length(years))) ## this maakes it os that the rows are produced based on how many years there are
# add column names
colnames(year_data)<-c("year", "year_name")
year_data

for (i in 1:length(years)){
  year_data$year_name[i] <- paste("The year is", years[i])
  year_data$year[i] <- years[i]
}
year_data

testdata<-read.csv(here("Week_13", "Data", "cond_data","011521_CT316_1pcal.csv"))
glimpse(testdata)

# point to the location on the computer of the folder
CondPath<-here("Week_13", "Data", "cond_data")
# list all the files in that path with a specific pattern
# In this case we are looking for everything that has a .csv in the filename

# you can use regex to be more specific if you are looking for certain patterns in filenames
files <- dir(path = CondPath,pattern = ".csv") ## will only bring in .csv files
files

# pre-allocate space
# make an empty dataframe that has one row for each file and 3 columns
cond_data<-data.frame(matrix(nrow = length(files), ncol = 3))
# give the dataframe column names
colnames(cond_data)<-c("filename","mean_temp", "mean_sal")
cond_data

raw_data<-read.csv(paste0(CondPath,"/",files[1])) # test by reading in the first file and see if it works # the backslash is important. paste0 = takes space out between sets 
head(raw_data)
mean_temp<-mean(raw_data$Temperature, na.rm = TRUE) # calculate a mean
mean_temp

for (i in 1:length(files)){ # loop over 1:3 the number of files 
  raw_data<-read.csv(paste0(CondPath,"/",files[i]))
  glimpse(raw_data)
}

for (i in 1:length(files)){ # loop over 1:3 the number of files 
  raw_data<-read.csv(paste0(CondPath,"/",files[i]))
  #glimpse(raw_data)
  cond_data$filename[i]<-files[i]
} 
cond_data

for (i in 1:length(files)){ # loop over 1:3 the number of files 
  raw_data<-read.csv(paste0(CondPath,"/",files[i]))
  #glimpse(raw_data)
  cond_data$filename[i]<-files[i]
  cond_data$mean_temp[i]<-mean(raw_data$Temperature, na.rm =TRUE)
  cond_data$mean_sal[i]<-mean(raw_data$Salinity, na.rm =TRUE)
} 
cond_data

######### using purr
1:10 %>% # a vector from 1 to 10 (we are going to do this 10 times) %>% # the vector to iterate over
  map(rnorm, n = 15)  %>% # calculate 15 random numbers based on a normal distribution in a list 
  map_dbl(mean) # calculate the mean. It is now a vector which is type "double"
# doing the same but creatimg our own function as well 
1:10 %>% # list 1:10
  map(function(x) rnorm(15, x)) %>% # make your own function
  map_dbl(mean)
# Use a formula when you want to change the arguments within the function
1:10 %>%
  map(~ rnorm(15, .x)) %>% # changes the arguments inside the function
  map_dbl(mean)

##########Bring files in with purr instead of a for loop 
# point to the location on the computer of the folder
CondPath<-here("Week_13", "Data", "cond_data")
files <- dir(path = CondPath,pattern = ".csv")
files
#Or, we can get the full file names in one less step by doing this...
files <- dir(path = CondPath,pattern = ".csv", full.names = TRUE)
#save the entire path name
files
#### Next, read in the files using map instead of a for loop while retaining the filename as a column.
data<-files %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(read_csv,.id = "filename") # map everything to a dataframe and put the id in a column called filename
data

Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
## You might get a weird warning from this package about converting a warning to error.  This will get rid of that
remotes::install_github("jespermaag/gganatogram")