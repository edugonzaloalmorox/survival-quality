# Spell splitting
# 6/09/2017
# @Edu Gonzalo 
# --------------------------------------------

library(xlsx)
library(readr)
library(dplyr)
library(tidyverse)
library(readxl)
library(forcats)
library(rio)
library(tidyr)
library(janitor)

# Splitting spells 

data_surv = import("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/data_survival.csv")


##############################
# Remove inconsistent records
##############################

# These are the result of records that have the same date for different rates. It is hard to decide 
# which rate is associated with which date
# --------------------------------------------------------------------------------------------------

# remove levels that are duplicated in terms of rating 
data = data_surv %>% group_by(location_id, publication_date, latest_rating) %>%
  select(-c(time, duration, upgrade, downgrade)) %>% unique()

# there may be locations that have different ratings in the same date - inconsistent

check = data %>% group_by(location_id, publication_date) %>% 
  filter(n()>1) # these are inactive care homes

ids_inconsistent = unique(check$location_id)


data = data %>% filter(! location_id %in% ids_inconsistent)

########################
# Splitting the spells
########################

# Transform the current data into long format --> integrate all the dates into a single variable
#----------------------------------------------------------------------------------------------

# create a new dataset with the necessary information for splitting the spells - I attach the remaining information afterwrds

data_spells = data %>% 
  group_by(location_id) %>%
  mutate(number_inspection = row_number(), 
         total_inspections = n()) %>% # this indicates the number of inspection
  select(location_id, publication_date, latest_rating, location_hsca_end_date, status, number_inspection, total_inspections) %>% 
  mutate_at(vars(publication_date, location_hsca_end_date), funs(as.Date)) %>%
  arrange(location_id, publication_date)


# Unite the dates and create a variable that is called date (I link through this variable)
# -----------------------------------------------------------------------------------------

# get dates

data_spell_dates = data_spells %>% 
  select(location_id, publication_date, location_hsca_end_date) %>%
  group_by(location_id) %>%
  gather(event, date, c("publication_date", "location_hsca_end_date")) %>%
  arrange(location_id, date)

# link other information

data_link = data_spells %>% select(location_id, publication_date:status, -location_hsca_end_date) %>% 
  rename(date = publication_date)

data_spell_long = left_join(data_spell_dates, data_link, by = c("location_id" = "location_id", "date" = "date")) %>% 
  unique()

data_spell_long$date = as.Date(data_spell_long$date)


# Add information referred to dates of other events that can happen --> budget changes, benefits, population
# ----------------------------------------------------------------------------------------------------------

data_budget = data_spell_long %>% select(location_id) %>%
  mutate(sp15_date = as.Date("2015-04-01"), 
         sp16_date = as.Date("2016-04-01"),
         sp17_date = as.Date("2017-04-01")) %>% 
  gather(event, date, sp15_date:sp17_date) %>% 
  unique() %>%
  arrange(location_id, date) 

# data inspections
data_inspections = data_spell_long %>% select(location_id) %>%
  mutate(date= as.Date("2014-10-01"), 
         event = "publication_date") %>%
  unique() %>%
  arrange(location_id, date) 

data_pop = data_spell_long %>% select(location_id) %>%
  mutate(pop_15 = as.Date("2015-07-01"), 
         pop_16 = as.Date("2016-07-01")) %>% 
  gather(event, date, pop_15:pop_16) %>% 
  unique() %>%
  arrange(location_id, date) 




# link information
data_spell_long_test = full_join(data_spell_long, data_budget, by = c("location_id", "date", "event")) %>%
  arrange(location_id, date) %>%
  full_join(data_inspections, by = c("location_id", "date", "event")) %>%
  full_join(data_pop, by = c("location_id", "date", "event")) %>%
  group_by(location_id) %>% arrange(location_id, date)


# supress observations after the deregistration record

df2 <- data_spell_long_test %>% 
  group_by(location_id) %>%
  mutate(Deregistration = ifelse(event == "location_hsca_end_date", 1, NA)) %>%
  fill(Deregistration, .direction = "up") %>%
  drop_na(Deregistration) %>%
  select(-Deregistration)

# Create initial and final dates 
# -------------------------------

df3 = df2 %>% 
  group_by(location_id) %>% 
  mutate(initial_dates = date, final_dates = lead(date)-1) %>%
  arrange(location_id, initial_dates, final_dates) %>% 
  select(location_id, event, initial_dates, final_dates, latest_rating, status, -date)

# those that have the same date for 2 different events --> conflictive ---------------------
check = df3 %>% group_by(location_id, initial_dates) %>% filter(n()>1)

ids_check = unique(check$location_id)

conflictive =  df3 %>% filter(location_id %in% ids_check) %>%
  group_by(location_id) %>% 
  mutate(duration = final_dates - initial_dates)

# transform as character and then link to df3
conflictive$final_dates = as.character(conflictive$final_dates)

conflictive = conflictive %>% 
  mutate(final =  ifelse(duration < 1, lead(final_dates), final_dates)) %>%
  select(location_id, event, initial_dates, final_dates, final) 

conflictive$final_dates = as.Date(conflictive$final_dates)
ids_conflictive = unique(conflictive$location_id)

# link
df3_link = left_join(df3, conflictive, by = c("location_id", "event", "initial_dates", "final_dates"))

df3_link$final_dates = as.character(df3_link$final_dates)

# recode the data corresponding to the final (recoded date)
df3_link =  df3_link %>% 
  mutate(final = ifelse(is.na(final), final_dates, final)) %>%
  select(location_id, event, initial_dates, final, latest_rating, number_inspection, total_inspections) 


df3_link = df3_link %>% rename(final_dates = final)

df3_conflictive = df3_link %>% filter(location_id %in% ids_conflictive)

# --------------------------------------------------------------------------------------------------

##########
# Failure 
##########

# select the inspections


df3_inspections = df3_link %>% 
  filter(event == "publication_date") %>% 
  filter(!is.na(latest_rating)) %>%
  select(location_id, event, initial_dates, final_dates, latest_rating, number_inspection, total_inspections )



# define failure - if next rating is: inadequate or requires improvement - downgrade
#                - if next rating is: inadequate or requires improvement - 


bad = c("Inadequate", "Requires improvement")

good = c("Outstanding", "Good")

df3_inspections = df3_inspections %>% 
  group_by(location_id) %>% 
  mutate(downgrade = ifelse(latest_rating %in% bad & total_inspections != 1, 1,0),
         downgrade = ifelse(total_inspections != 1 & number_inspection== 1, 0, downgrade))

df3_inspections = df3_inspections %>% 
  group_by(location_id) %>%
  mutate(upgrade = ifelse(latest_rating %in% good & total_inspections != 1, 1,0),
         upgrade = ifelse(total_inspections != 1 & number_inspection == 1, 0, upgrade)) 

# link failures with the parent df with the splits 

df3_test = left_join(df3_link, df3_inspections, by = c("location_id", "event", "initial_dates", "final_dates", "latest_rating", 
                                                       "number_inspection", "total_inspections"))

# fill the spells 

df3_test$upgrade = as.factor(df3_test$upgrade)
df3_test$downgrade = as.factor(df3_test$downgrade)

# downgrade  
df3_test =  df3_test %>%
  group_by(location_id) %>%
  mutate(failure_down = downgrade) %>%
  fill(failure_down, .direction = "down")

#upgrade
df3_test =  df3_test %>%
  group_by(location_id) %>%
  mutate(failure_up = upgrade) %>%
  fill(failure_up, .direction = "down")




# clean up and  fill NA in both failure data

df3_test1 = df3_test1 %>% mutate_at(vars(initial_dates, final_dates), funs(as.Date)) 


df3_test1$failure_down = as.numeric(df3_test1$failure_down)
df3_test1$failure_up = as.numeric(df3_test1$failure_up)

df3_test1 = df3_test %>% group_by(location_id) %>% 
  mutate(failure_down = ifelse(is.na(failure_down), "0", failure_down), 
         failure_up = ifelse(is.na(failure_up), "0", failure_up)) %>%
  select(-c(upgrade, downgrade))

df3_test1 = df3_test1 %>% group_by(location_id) %>% fill(total_inspections, .direction = "up")


df3_test1 = df3_test1 %>% group_by(location_id) %>% 
  mutate(split_id = row_number(), 
         duration =  final_dates - initial_dates) 

# work in df3_test (no filled data)

df3_test = df3_test %>% mutate_at(vars(initial_dates, final_dates), funs(as.Date))  

 

df3_test$failure_down = as.numeric(df3_test$failure_down)
df3_test$failure_up = as.numeric(df3_test$failure_up)

df3_test = df3_test %>% group_by(location_id) %>% 
  mutate(split_id = row_number(), 
         duration =  final_dates - initial_dates)



write.csv(df3_test, "/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/data_spells_nofilled.csv", row.names = FALSE)

# samples 

sample_1 = df3_test1 %>% filter(! event %in% c("pop_15", "pop_16") )


table(df3_test1$event)




