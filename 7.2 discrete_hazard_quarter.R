##################################################
# Data preparation for discrete Time Hazard Model 
# Frequency of the spells: quarter 
# Date: september 2017
# @ Edu Gonzalo Almorox
################################################

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

# ratings with all the quality categories
   ratings = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/data_survival.csv")
      
   ratings = clean_names(ratings)

# select overall category 
    df = ratings %>% filter(key_question == "Overall") %>%  select(location_id, publication_date, latest_rating, status) 

# -----------------
# check duplicates
# ------------------

    rat_dup = df %>% get_dupes(location_id, publication_date) %>% filter(n()>1)
    ids_dup = unique(rat_dup$location_id)
    
    df_clean = df %>% filter(!location_id %in% ids_dup)

# number of inspections and total inspection  

df_clean = df_clean %>% group_by(location_id) %>% mutate(number_inspection = row_number(), 
                                                         total_inspections = sum(n()))


# -----------------
# get the failure
# -----------------

# define failure - if next rating is: inadequate or requires improvement
                 


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
# Quartely spells 
######################################

# create variables

ids = unique(df_clean$location_id)

ids_rep = rep(ids, 11) #ids

initial = rep(seq(as.Date("2014-10-02"), by="quarter", len=11) -1, length(ids)) # initial dates
final = rep(seq(as.Date("2015-01-01"), by="quarter", len=11) -1, length(ids)) # final dates

dates =cbind(initial_date = as.character(initial), final_date = as.character(final))

#  link variables and arrange

df = data.frame(location_id = ids_rep) %>% arrange(location_id)

df = cbind(df, dates)

df = df %>% group_by(location_id) %>% mutate(spell = row_number())

# create a key variable for linking datasets 
#-------------------------------------------------


# create the date
    df_date = df %>%
      select(location_id, initial_date, spell) %>% 
      mutate_at(vars(initial_date), funs(as.Date))
    
    df_clean = df_clean %>%
      mutate_at(vars(publication_date), funs(as.Date))


    df_insp_date = full_join(df_clean, df_date, by = c("location_id" = "location_id", "publication_date" = "initial_date")) 

# Arrange by dates 
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

care_home = data_surv %>% select(location_id, location_name:msoa11) %>% unique()


df2 = left_join(df1, care_home, by = "location_id")

geo = import("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/ONSPD_FEB_2017_UK/Data/ONSPD_FEB_2017_UK.csv")

imd = geo %>% filter(grepl("^E", oslaua)) %>% 
     select(oslaua, lsoa11, imd) %>% unique() 

df3 = left_join(df2, imd, by = c("lsoa11", "oslaua"))





####################################
# Information about spending power
####################################



# create a key variable - year

df3 = df3 %>% mutate_at(vars(initial_date, final_date, location_hsca_start_date), funs(as.Date))

# create a 
df3 = df3 %>% mutate(year = ifelse(initial_date < "2015-04-01", 1314, 
                                   ifelse(initial_date >= "2015-04-01" & initial_date < "2016-04-01", 1415, 
                                          ifelse(initial_date >= "2016-04-01" & initial_date < "2017-04-01", 1516,
                                                 ifelse(initial_date >= "2017-04-01",1617, 5)))))



# load sp data
sp = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/spending_power_1317.csv")
      
      sp  = sp %>% group_by(oslaua) %>% 
        mutate(wave = row_number()) %>% rename(la_sp_district = la)
      
      
      df4 = left_join(df3, sp, by = c("oslaua", "year")) %>% select(-wave)

################################
# Create additional variables
################################


bad = c("Inadequate", "Requires improvement")



      df5= df4 %>% group_by(location_id) %>% mutate(start_type = ifelse(number_inspection == 1 & latest_rating %in% bad, "bad", NA))
      
      df5 = df5 %>% group_by(location_id) %>% fill(start_type, .direction ="down")
      df5= df5 %>% group_by(location_id) %>% fill(start_type, .direction ="up")
      
      df5 = df5 %>% group_by(location_id) %>% mutate(start_type = ifelse(is.na(start_type), "good", start_type))
      
      df5 = df5 %>% group_by(location_id) %>% mutate(dimension = ifelse(care_homes_beds < 10, "small",
                                                                        ifelse(care_homes_beds >= 10 & care_homes_beds < 50, "medium", 
                                                                               ifelse(care_homes_beds >= 50, "big"))))
      
  # note: information relative to population is included (see 10.nomis for further details)
      
      # add info relative to providers 
      provider = ratings %>% select(`Location ID`, `Provider ID`)  %>% unique() %>% rename(location_id = `Location ID`,
                                                                                           provider_id = `Provider ID`)  
       
      df5 = left_join(df4, provider, by = "location_id") %>% select(location_id, provider_id, initial_date:df4_11)
      
      
      provider_beds = df4 %>% group_by(provider_id, location_id) %>% select(provider_id, location_id, care_homes_beds) %>% unique()
      
      beds_group = provider_beds %>% group_by(provider_id) %>% summarise(beds_provider = sum(care_homes_beds))
      
      df5 = left_join(df4, beds_group, by = "provider_id")


write.csv(df5, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/df4_test.csv", row.names = FALSE)


#########################################################################################################


###############################################
# Link information regarding inactive care homes
##################################################

# filter inactive care homes
data_surv = import("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/data_survival.csv") 

data_inactive = data_surv %>% 
  filter(status == "inactive") %>% select(location_id, publication_date, latest_rating, provider_id, oslaua, location_hsca_end_date)

# number of inspections and total of inspections

data_inactive = data_inactive %>% group_by(location_id) %>% mutate(number_inspection = row_number(), 
                                                         total_inspections = sum(n()))


check = data_inactive %>% group_by(location_id, publication_date) %>% filter(n()>1)
check.1 = data_inactive %>% group_by(location_id, publication_date) %>% filter(total_inspections == 1)

length(unique(check$location_id)) # 307 care homes that present inconsistent information --> consider check.1

# ----------------
# get the failure
# ----------------


# define failure - if next rating is: inadequate or requires improvement



bad = c("Inadequate", "Requires improvement")

good = c("Outstanding", "Good")

# downgrade
check.1 = check.1 %>% 
  group_by(location_id) %>% 
  mutate(downgrade = 1)
        
# -----------------
# Get the quarters # 
# ------------------
# create variables

ids = unique(check.1$location_id)

ids_rep = rep(ids, 11) #ids

initial = rep(seq(as.Date("2014-10-02"), by="quarter", len=11) -1, length(ids)) # initial dates
final = rep(seq(as.Date("2015-01-01"), by="quarter", len=11) -1, length(ids)) # final dates

dates =cbind(initial_date = as.character(initial), final_date = as.character(final))

#  link variables and arrange

df = data.frame(location_id = ids_rep) %>% arrange(location_id)

df = cbind(df, dates)

df = df %>% group_by(location_id) %>% mutate(spell = row_number())

rm(initial, final, good, bad, ids, ids_rep)

# create a key variable for linking datasets 
#-------------------------------------------------


# create the date
df_date = df %>%
  select(location_id, initial_date, spell) %>% 
  mutate_at(vars(initial_date), funs(as.Date))

# make check.1 long format
 
 check.1_long = check.1 %>% select(location_id, publication_date, end_date =  location_hsca_end_date, latest_rating, downgrade)

 check.1_long = check.1_long %>% gather(event, date, publication_date:end_date) %>% arrange(location_id)

 check.1_long = check.1_long %>% mutate(downgrade =  ifelse(event == "publication_date", 0, downgrade))
                                        

 df_clean = check.1_long %>%
  mutate_at(vars(date), funs(as.Date))

# merge all the dates and arrange
df_insp_date = full_join(df_clean, df_date, by = c("location_id" = "location_id", "date" = "initial_date")) 

# Arrange by dates 
df_insp_date = df_insp_date %>% 
  arrange(location_id, date)

# fill the spells that are NA and get rid of those observations that have no rating
df_insp_date_spells = df_insp_date %>%
  group_by(location_id) %>%
  fill(spell, .direction = "down") %>%
  filter(!is.na(latest_rating))


df1 =  left_join(df, df_insp_date_spells, by = c("location_id", "spell"))

vars = data_inactive %>% select(location_id, oslaua, total_inspections)

df1 = left_join(df1,vars, by = "location_id") %>% mutate(status = "inactive")

write.csv(df1, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/inactive.csv", row.names = FALSE)

###########
# Link information referred to inactive care homes
#####################

data_surv = import("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/data_survival.csv")

df4 =  import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/df4.csv")

inactive = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/inactive.csv")

care_homes_inactive = data_surv %>% filter(location_id %in% unique(inactive$location_id))

care_homes_inactive = care_homes_inactive %>% select(location_id, provider_id, care_homes_beds, location_name:location_inspection_directorate,
                                                     location_city:provider_parliamentary_constituency, postcode:msoa11)

inactive_test = left_join(inactive, care_homes_inactive, by = c("location_id", "oslaua"))

write.csv(inactive_test, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/inactive.csv", row.names = FALSE)


# rename some variables 

inactive_test = inactive_test %>% mutate(number_inspection = total_inspections,
                                         failure_up = 0) %>%
  rename(publication_date = date,
         failure_down = downgrade) 

oslauas_inactive = unique(inactive_test$oslaua)


vars_df4 = df4 %>% select(oslaua,spell,  local, local2, year:js_rate, prop_65:prop_85, -start_type, -change_revenue_pounds) %>% 
  filter(oslaua %in% oslauas_inactive) %>% 
  unique()

inactive_test2 = left_join(inactive_test, vars_df4, by = c("oslaua", "spell")) %>% unique()

       

