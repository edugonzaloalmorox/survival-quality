# Interactive representation of care homes quality
# Load data corresponding to inspections


# note: data of analysis consider the period November 2015 - June 2017 (October is not considered since it does not have information regarding providers)
# August 2017
# @EduGonzalo (Newcastle University)
# --------------------------------------------------------------


# Libraries
#-----------
library(readxl)
library(dplyr)
library(data.table)
library(stringr)
library(rio)
library(forcats)
library(Hmisc)
library(ggplot2)

# ---------------


inspections = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/inspections_extended.csv")



install.packages("leaflet")

library(leaflet)

library(leaflet) 
library(geosphere) 
library(ggmap) 
library(grid)  # list of all regions: 

# get the postcodes from the postcode directory 

postcode = import("~/Dropbox/PhD/ch1/market entry/care_homes/data/geography/ONSPD_FEB_2017_UK/Data/ONSPD_FEB_2017_UK.csv")

england = postcode %>% filter(ctry == "E92000001")

m = leaflet() %>% addTiles() 

m = m %>% addPolygons(lng = inspections$`Location Longitude`,
                     lat = inspections$`Location Latitude`)
m

# ------------------------------------------------------------- 


library(leaflet)
library(dplyr)

overall = inspections %>% filter(`Key Question` == "Overall" & time == 1)

l <- leaflet() %>% addTiles()

# factor 

overall$`Key Question` = as.factor(overall$`Key Question`)

l %>%
  addMarkers(data=overall, lng = ~`Location Longitude`,
             lat = ~`Location Latitude`,
             label = ~ as.character(`Key Question`),
             clusterOptions = markerClusterOptions(),
             labelOptions = labelOptions(noHide = T,
                                         direction = 'auto'))
        