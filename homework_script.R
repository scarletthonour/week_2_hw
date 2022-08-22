#Exam
#Each week we will provide a short task to test your knowledge, 
#these should be used to guide your study for the final exam.

#The task is to join some non spatial data to some spatial data 
#and wrangle it.

#You need calculate the average percent of science students 
#(in all) grades per county meeting the required standards and 
#produce a map to show where the Country averages are above or 
#below the State of Washington average.

#Add library features:
library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library("plotly")
library(janitor)
library(tidyverse)
library(dplyr)
library(tidyr)
library(here)
library(janitor)
library(stringr)

#downloading Washington state GIS data:
Washington <- st_read("https://opendata.arcgis.com/datasets/c91c002dccea4ec9a5ac8f0d889ef6bb_122.geojson")

School_Report_Cards <- read.csv("C:/Users/shono/OneDrive/Desktop/GIS_and_Science/Week_2/homework/homework/Report_Card_Assessment_Data_2018-19_School_Year.csv", 
                                header = TRUE, sep = ",", na = "NA")

Datatypelist <- School_Report_Cards %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")
Datatypelist

county_only <- School_Report_Cards %>%
  clean_names() %>%
  select(county, test_subject, count_met_standard, 
         count_of_students_expected_to_test, grade_level)%>%
  # the != means don't select this, but select everything else
  filter(county != "Multiple")%>%
  filter(test_subject == "Science")%>%
  filter(grade_level=="All Grades")%>%
  group_by(county)%>%
  summarise(total_county_met_standard= sum(count_met_standard, na.rm=T),
            total_county_to_test= sum(count_of_students_expected_to_test, na.rm=T)) %>%
  mutate (percent_met_per_county= (total_county_met_standard/total_county_to_test)*100)

county_only

## we need to work out the average percent for all the counties 

state_met<-sum(county_only$total_county_met_standard)
state_test<-sum(county_only$total_county_to_test)

state_that_met<-(state_met/state_test*100)

###percent over or under

county_to_state_difference <- county_only %>%
  mutate(state_diff = percent_met_per_county-state_that_met)%>%
  mutate(across(state_diff, round, 1))

## now we need to join, usually it's best to use a unique ID, but we can do it with strings.

joined_data <- shape %>% 
  clean_names() %>%
  left_join(., 
            county_to_state_difference,
            by = c("countylabe" = "county"))

# If the strings didn't match (e.g. lower and upper case) we can covert them with...

t <- shape %>% 
  mutate(COUNTY2 = tolower(COUNTY))

### let's map

library(tmap)
library(tmaptools)

bbox_county <- joined_data %>%
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "esri", zoom = NULL)

tm_shape(bbox_county)+
  tm_rgb()+
  
  tm_shape(joined_data) + 
  tm_polygons("state_diff", 
              style="pretty",
              palette="Blues",
              midpoint=NA,
              #title="Number of years",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "County to state percent difference in meeting science standards", 
            legend.position = c("right", "bottom"))




