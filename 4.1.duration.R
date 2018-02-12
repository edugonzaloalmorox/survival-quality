library(magrittr)
library(OIsurv)
library(dplyr)
library(broom)
library(ggplot2)
library(survsim)
library(tidyverse)
library(rio)
library(readxl)
library(janitor)
library(survminer)
library(KMsurv)

install.packages("OIsurv")
install.packages("survsim")
install.packages("KMsurv")
install.packages("muhaz")




# ----------------
# multiple spells  
# -----------------

# load inspections 

    inspections = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/inspections.csv")
    
    deactivated = read_xlsx("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/20170801 De-activated locations.xlsx", sheet = "Sheet1")
    
    ids.deactivated = deactivated %>% 
      select(`Location ID`, `Location HSCA End Date`) %>% 
      group_by(`Location ID`) %>% 
      unique()
    

# care homes inspected and deactivated

      inspected_deactivated = base::intersect(unique(inspections$`Location ID`), unique(ids.deactivated$`Location ID`))
      
      ids_rated_deactivated = ids.deactivated %>% filter(`Location ID` %in% inspected_deactivated)
      
      inspections_test = left_join(inspections, ids_rated_deactivated, by = c("Location ID"))

# care homes with more than one inspection 
       
    more.inspections = overall %>% filter(total.inspections >1)
    ids.inspected.various = unique(more.inspections$`Location ID`)
  
    length(unique(more.inspections$`Location ID`))/length(unique(inspections$`Location ID`))
      
# censoring mechanism - check the rating of those that just one inspection
# ------------------------------------------------------------------------  
 
      one_inspection = inspections_test %>% filter(total.inspections == 1 & `Key Question` == "Overall")
     
      one_inspection_deact =  one_inspection %>% filter(!is.na(`Location HSCA End Date`))
     
      one_inspection_act =  one_inspection %>% filter(is.na(`Location HSCA End Date`))
     
 # proportion of those inspected once that are out of the market
      length(unique(one_inspection_deact$`Location ID`))/length(unique(one_inspection$`Location ID`))
 
 # what are the proportion of ratings

       one_inspection %>% tabyl(`Latest Rating`)
       
       one_inspection_deact %>% tabyl(`Latest Rating`)
      
       one_inspection_act %>% tabyl(`Latest Rating`)
       
# ----------------------------------------------------------------------

overall_1 = overall %>% select(`Location ID`, `Publication Date`, `Latest Rating`)

      overall_1 = overall.extended_test %>% 
        mutate_at(vars(`Publication Date`),funs(as.Date)) %>%
         mutate(wave = ifelse(`Publication Date` <= "2015-09-30", 1,
                              ifelse(`Publication Date` > "2015-09-30" & `Publication Date` <= "2016-09-30", 2,
                                     ifelse(`Publication Date` >= "2016-10-01",3, 4))))
       
       
              
     # restructure the data 
       
       ids.rated = unique(overall$`Location ID`)
       
       ids = rep(ids.rated, each = 3) 
        wave = rep(c(1,2,3), length(ids.rated))
       
       data_overall  =  data.frame(ids, wave)
      
       
       
overall.extended_test = overall.extended_test %>% mutate_at(vars(`Publication Date`),funs(as.Date)) %>%
  mutate(wave = ifelse(`Publication Date` <= "2015-09-30", 1,
                       ifelse(`Publication Date` > "2015-09-30" & `Publication Date` <= "2016-09-30", 2,
                              ifelse(`Publication Date` >= "2016-10-01",3, 4))))

overall_1 = overall.extended_test %>% select(`Location ID`, `Publication Date`, `Latest Rating`, wave,
                                             duration, number_inspection = time, total.inspections,
                                             downgrade, upgrade, `Care homes beds`:`Provider Parliamentary Constituency`)

data_overall = left_join(data_overall, overall_1, by = c("ids" = "Location ID", "wave" = "wave"))

data_wave1 =  data_overall %>% filter(!is.na(total.inspections))


 