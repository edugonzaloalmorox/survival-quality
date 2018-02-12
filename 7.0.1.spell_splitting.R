##########################################################################
# Spell splitting
#-----------------
#           --> split spells for each potential event that may occur. 
#           --> events include: publication of ratings, population changes, spending power changes and de-regsitrations
#           --> things done: recoding the number of inspections, obtain failures and durations
#
# Events are considered since the first inspection of the care home 
# date: october 2017
# @Edu Gonzalo
#########################################################################

# --------------------------------------------------------------------------------------
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
library(lme4)
library(lmerTest)
library(dummies)
library(xtable)
library(ggfortify)

# -------------------------------------------------------------------------------------------

# sample data that contains the sample splited (I need to recode failures and inspections)

    data_test = import("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/data_spells_nofilled.csv")

# recode the number of inspections and the number of inspection 

    data_test = data_test %>% select(location_id:latest_rating)

    inspections = data_test %>% filter(event == "publication_date" & !is.na(latest_rating))

    inspections = inspections %>% group_by(location_id) %>% 
      mutate(number_inspection = row_number(),
             total_inspections = sum(n())) %>% select(location_id:total_inspections)

################
# Failures
###############

# Different failures depending on how the first inspection of the care home is

#         if start bad --> failure upgrade 
#         if start good --> failure downgrade

# Define start tyes
# -------------------

bad = c("Inadequate", "Requires improvement")

good = c("Outstanding", "Good")

# start type: bad 

    inspections = inspections %>% 
      group_by(location_id) %>% 
      mutate(start_type = ifelse(number_inspection == 1 & latest_rating %in% bad, "bad", NA))
    
    inspections = inspections %>% group_by(location_id) %>% fill(start_type, .direction ="down")
    inspections = inspections %>% group_by(location_id) %>% fill(start_type, .direction ="up")

# start type: good 
    
    inspections = inspections %>% group_by(location_id) %>% mutate(start_type = ifelse(is.na(start_type), "good", start_type))
    
# define failures
# ----------------
     
     # those that start badly 
    
     inspections = inspections %>% mutate(failure_bad = ifelse(start_type == "bad" & latest_rating %in% good & total_inspections != 1, 1,0),
                                         failure_bad = ifelse(total_inspections != 1 & number_inspection == 1, 0, failure_bad))
    
     # those that start good
    
    inspections = inspections %>% mutate(failure_good = ifelse(start_type == "good" & latest_rating %in% bad & total_inspections != 1, 1,0),
                                         failure_good = ifelse(total_inspections != 1 & number_inspection == 1, 0, failure_good))
    
    
    
# link data
# ---------

data_test_new = left_join(data_test, inspections, by = c("location_id", "event",
                                                             "initial_dates", "final_dates",
                                                             "latest_rating"))

# clean: drop information of location end date for those that are registered and fill final dates that are NA
  data_clean = data_test_new %>% mutate(var = ifelse(event == "location_hsca_end_date" & is.na(initial_dates), NA, 1)) %>%
          filter(!is.na(var)) %>% select(-var) %>% mutate_at(vars(initial_dates, final_dates), funs(as.character))
                                        
                
# fill final dates that are  NA with the date: "2017-06-01"      
  
 data_clean = data_clean %>% mutate(final_dates = ifelse(is.na(final_dates), "2017-06-01", final_dates))

 ids = data_clean %>% filter(event == "location_hsca_end_date")
 
# drop information before the first event happened (e.g. the first inspection) 
# ----------------------------------------------------------------------------
 
# fill information of the start type and number of inspections 
 
     data_clean = data_clean %>% group_by(location_id) %>% 
           fill(start_type, .direction ="down") %>%
           fill(total_inspections, .direction = "down")
         
     df = data_clean %>% filter(!is.na(start_type)) %>% mutate(failure_bad = ifelse(is.na(failure_bad), 0, failure_bad),
                                                               failure_good = ifelse(is.na(failure_good), 0, failure_good))

######################    
# Calculate durations
#####################
 
 
 
    df = df %>% group_by(location_id) %>% 
        mutate_at(vars(initial_dates, final_dates), funs(as.Date)) %>%
        mutate(duration = final_dates - initial_dates)


    df = df %>% mutate_at(vars(initial_dates, final_dates), funs(as.character))

# some durations are negative --> final date for publication and  initial date for next spell are the same. I increase the spell 
    df1 =  df %>% mutate(fd = ifelse(duration == -1, lead(final_dates), final_dates), 
                           var = ifelse(lag(duration) == -1, 1, 0)) # the last variable is for getting rid of the observations of the spell that don´t correspond to inspection

 
# dates for getting durations
    df1 =  df1 %>% mutate_at(vars(initial_dates, final_dates, fd), funs(as.Date)) 
     
    df1 = df1 %>%  mutate(duration2 = fd- initial_dates) %>% mutate(var = ifelse(is.na(var), 0, var))
     
    df1 = df1 %>% mutate(var2 = ifelse(var == 1, NA, var)) 

# clean variables
    df2 = df1 %>% filter(!is.na(var2)) %>% 
        select(location_id:initial_dates, fd, latest_rating:duration2, -duration, -var, -var2) %>% 
        rename(final_dates = fd, duration = duration2)

 
write.csv(df2,"/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/spells_test.csv", row.names = FALSE )

