#############################
# Discrete Time Hazard Model 
###########################

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
library(stringr)
library("survival")
library(forcats)

#########################################################################
# Identify inconsistent observations in the ratings
######################################################################### 

    ratings = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/inspections_extended.csv")
    
    ratings = clean_names(ratings)
    
    df = ratings %>% filter(key_question == "Overall") %>%  select(location_id, publication_date, latest_rating) 

# check duplicates
    
    rat_dup = df %>% get_dupes(location_id, publication_date) %>% filter(n()>1)
    ids_dup = unique(rat_dup$location_id)
    
    df_clean = df %>% filter(!location_id %in% ids_dup)
    
# number of inspections and total inspection  

    df_clean = df_clean %>% group_by(location_id) %>% mutate(number_inspection = row_number(), 
                                                             total_inspections = sum(n()))

# get the failure
    
    # define failure - if next rating is: inadequate or requires improvement - downgrade
    #                - if next rating is: inadequate or requires improvement - 
    
    
    bad = c("Inadequate", "Requires improvement")
    
    good = c("Outstanding", "Good")
    
    # downgrade
    df_clean = df_clean %>% 
      group_by(location_id) %>% 
      mutate(downgrade = ifelse(latest_rating %in% bad & total_inspections != 1, 1,0),
             downgrade = ifelse(total_inspections != 1 & number_inspection== 1, 0, downgrade))
    # upgrade
    df_clean = df_clean %>% 
      group_by(location_id) %>%
      mutate(upgrade = ifelse(latest_rating %in% good & total_inspections != 1, 1,0),
             upgrade = ifelse(total_inspections != 1 & number_inspection == 1, 0, upgrade)) 
    

##########################################################################################

######################################
# Monthly spells 
######################################

# create variables
    
    ids = unique(df_clean$location_id)
    
    ids_rep = rep(ids, 36) #ids
     
    initial = rep(seq(as.Date("2014-10-02"), by="1 month", len=36) -1, length(ids)) # initial dates
    final = rep(seq(as.Date("2014-11-01"), by="1 month", len=36) -1, length(ids)) # final dates
    
    dates =cbind(initial_date = as.character(initial), final_date = as.character(final))
    
#  link variables and arrange
    
    df = data.frame(location_id = ids_rep) %>% arrange(location_id)

    df = cbind(df, dates)
    
    df = df %>% group_by(location_id) %>% mutate(spell = row_number())
    
# create a key question for linking datasets 

    # create the date
        df_date = df %>%
          select(location_id, initial_date, spell) %>% 
          mutate_at(vars(initial_date), funs(as.Date))
        
        df_clean = df_clean %>%
          mutate_at(vars(publication_date), funs(as.Date))
        
    
        df_insp_date = full_join(df_clean, df_date, by = c("location_id" = "location_id", "publication_date" = "initial_date")) 
    
        df_insp_date = df_insp_date %>% 
          arrange(location_id, publication_date)
       
   # fill the spells that are NA
        df_insp_date_spells = df_insp_date %>%
          group_by(location_id) %>%
          fill(spell, .direction = "down") %>%
          filter(!is.na(latest_rating))
   
   
     df1 =  left_join(df, df_insp_date_spells, by = c("location_id", "spell"))
     
     # note: there are more observations in df1 than in df because some care homes are inspected several times in the same month (spell)
   
     check1 = df1 %>% group_by(location_id, publication_date) %>% filter(n()>1)
     check1 %>% filter(!is.na(publication_date))
   

###################################################################################################
     
     
     # fill the NAs with the data 
        
        # total inspections  
        df1 = df1 %>% group_by(location_id) %>% fill(total_inspections)
     
        df1 = df1 %>% group_by(location_id) %>% fill(total_inspections, .direction = "up")
     
        # failure
        
        df1 = df1 %>% group_by(location_id) %>% mutate(failure_down = ifelse(is.na(downgrade), 0, downgrade),
                                                       failure_up = ifelse(is.na(upgrade), 0, upgrade)) %>% select(-downgrade, -upgrade)

        
###################################################################################################
       
#############################################
# Additional information from the care home
#############################################
        
        data_surv = import("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/data_survival.csv")
        
        care_home = data_surv %>% select(location_id, status, location_name:msoa11) %>% unique()
        
        df2 = left_join(df1, care_home, by = "location_id")
        

####################################
# Information about spending power
####################################
        
      
        
        # create a key variable - year
        
        df2 = df2 %>% mutate_at(vars(initial_date, final_date, location_hsca_start_date), funs(as.Date))
        
       # create a 
        df2 = df2 %>% mutate(year = ifelse(initial_date < "2015-04-01", 1314, 
                                                   ifelse(initial_date >= "2015-04-01" & initial_date < "2016-04-01", 1415, 
                                                          ifelse(initial_date >= "2016-04-01" & initial_date < "2017-04-01", 1516,
                                                                 ifelse(initial_date >= "2017-04-01",1617, 5)))))
        
        
        
        # load sp data
        sp = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/spending_power_1317.csv")
        
        sp  = sp %>% group_by(oslaua) %>% 
          mutate(wave = row_number()) %>% rename(la_sp_district = la)
        
        
        df3 = left_join(df2, sp, by = c("oslaua", "year")) %>% select(-wave)
        
        write.csv(df4, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/df4.csv", row.names = FALSE)
        
        
        
        
        
        
        