### Week 5b Lab Assignment 
### Created by: Group 2, Jasmin Abdulla, Jamie Kerlin, Roland Lacap, Claudia Rea
### Created on: 2021-02-24

### Load Libraries
library(tidyverse)
library(here)
library(lubridate)
library(ghibli)
library(PNWColors)

### Load Data
CondData<-read_csv(here("Week_5","Data","CondData.csv"))
DepthData<-read_csv(here("Week_5","Data","DepthData.csv"))

View(CondData)
View(DepthData)

glimpse(CondData)
glimpse(DepthData)

### convert CondData to nearest 10 sec
CondData<- CondData %>% 
  mutate(DateTime = ymd_hms(date)) %>%  #convert date into ISO
  mutate(DateTime = round_date(DateTime, "10 seconds")) %>% #round times into the nearest 10 seconds
  select(2:5) #select only matching columns to Depth data
  

DepthData<- DepthData %>%
  mutate(DateTime = ymd_hms(date)) %>% #convert date into ISO
  select(2:4) #select only matching columns to CondData

CondDepth <- inner_join(CondData,DepthData) %>% #join CondData and DepthData while removing NAs
  mutate(time = format(DateTime, "%H:%M")) %>% #create column for only HH:MM
  group_by(time) %>% #group the time column
  summarise(Mean_depth = mean(Depth), #take means of all variables
            Mean_temp = mean(TempInSitu), 
            Mean_salinity = mean(SalinityInSitu_1pCal)) %>% 
  write_csv(here("Week_5","Output","Means_of_data.csv")) %>% 
  ggplot(aes(y=Mean_depth, #create plot where depth changes upon temp
             x=Mean_temp))+
  scale_y_reverse()+ #reverse y axis
  geom_smooth(aes(color=TRUE, #create a line plot, allow color for the line and fill
                  fill=TRUE))+ 
  guides(color=FALSE, #remove Legends
         fill = FALSE)+
  geom_jitter(color="Purple", #add jitter plot and change color to purple
              alpha =0.5)+ #change transparency of the dots
  scale_x_continuous(position = "top") +#shift the x axis to the top
  labs(x = "AVG Temperature (C)", #change labels for x and y axis
       y = "AVG Depth (m)",
       title = "Average temperature changing with average depth")+ #Add title
  theme_bw()+ #theming for the ggplot
  theme(panel.background = element_rect(fill = "lavender"))+ #color scales
  scale_color_manual(values = ghibli_palette("KikiMedium")[4])+
  scale_fill_manual(values = ghibli_palette("KikiLight")[4])+
  ggsave(here("Week_5","Output","20210224_Week5b_Lab_Assignment.png"))

CondDepth


