####################################
# Build the sample of continous  spells
# October 2017
# @ Edu Gonzalo Almorox
#######################################

df = import("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/spells_test.csv")
df3 =  import("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/df3_test.csv")

df = df %>% select(-duration)

loc_oslaua = df3 %>% select(location_id, oslaua) %>% unique()

df = left_join(df, loc_oslaua, by = "location_id") %>% select(location_id, oslaua, event:failure_good)

####################################################
# key variables for linking data: year and quarters
####################################################


df = df %>% mutate_at(vars(final_dates, initial_dates), funs(as.Date))

df = df %>% mutate(year = ifelse(final_dates < "2015-04-01", 1415, 
                                   ifelse(final_dates >= "2015-04-01" & final_dates < "2016-04-01", 1516, 
                                          ifelse(final_dates >= "2016-04-01" & final_dates < "2017-04-01", 1617,
                                                 ifelse(final_dates >= "2017-04-01",1718, 5)))))


df = df %>% mutate(spell_quarter = ifelse(initial_dates <= "2014-12-31", 1,
                                          ifelse(initial_dates > "2014-12-31" & initial_dates <= "2015-03-31", 2, 
                                                 ifelse(initial_dates > "2015-03-31" & initial_dates <= "2015-06-30", 3,
                                                        ifelse(initial_dates > "2015-06-30" & initial_dates <= "2015-09-30", 4,
                                                               ifelse(initial_dates > "2015-09-30" & initial_dates <= "2015-12-31", 5,
                                                                      ifelse(initial_dates > "2015-12-31" & initial_dates <= "2016-03-31", 6, 
                                                                             ifelse(initial_dates > "2016-03-31" & initial_dates <= "2016-06-30", 7,
                                                                                    ifelse(initial_dates > "2016-06-30" & initial_dates <= "2016-09-30", 8,
                                                                                           ifelse(initial_dates > "2016-09-30" & initial_dates <= "2016-12-31", 9,
                                                                                                 ifelse(initial_dates > "2016-12-31" & initial_dates <= "2017-03-31", 10, 
                                                                                                        ifelse(initial_dates > "2017-03-31" & initial_dates <= "2017-06-30", 11,0))))))))))))
                                                                                                               



##################################################################
# link more events: population, job seekers and pension claimants
##################################################################


population = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/population_long.csv")

# -----------
# population
# ----------

    population = population %>% mutate(year = ifelse(time == 2014, 1415, 
                                          ifelse(time == 2015, 1516,
                                          ifelse(time == 2016, 1617, 
                                          ifelse(time == 2017, 1718, "other"))))) %>%
                                                                  select(oslaua, population:year) 
    
    population   = population %>% mutate_at(vars(year), funs(as.numeric))
    
    df1 = left_join(df, population, by = c("oslaua", "year"))

# -----
# jobs
# -----

    job = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/job_seekers_long.csv")

# Create the sample for the linkage of information
# ------------------------------------------------

#     --> create a key variable
#     --> link that information to incorporate the job events with the other events
#     --> fill and clean up
    
# create initial and final dates
# -------------------------------
    job = job %>% select(oslaua, js_rate, js_number, time) %>% 
      mutate(initial_dates = ifelse(time =="december_2014","2014-10-01", 
                               ifelse(time == "march_2015", "2015-01-01",
                                  ifelse(time =="june_2015", "2015-04-01",
                                   ifelse(time == "september_2015", "2015-07-01", 
                                    ifelse(time == "december_2015","2015-10-01",
                                         ifelse(time == "march_2016", "2016-01-01",
                                          ifelse(time == "june_2016", "2016-04-01",
                                              ifelse( time == "september_2016", "2016-07-01",
                                                ifelse(time == "december_2016","2016-10-01",
                                                    ifelse(time == "march_2017", "2017-01-01",
                                                      ifelse(time == "june_2017", "2017-04-01", 1))))))))))))


    job = job %>% mutate_at(vars(initial_dates), funs(as.Date))
    
    # rename and create final dates 
    job = job  %>% group_by(oslaua) %>% arrange(oslaua, initial_dates) %>%
      mutate(final_dates = lead(initial_dates) -1,
             event = paste("js", time, sep = "_")) %>% 
      select(oslaua, js_rate, js_number, event, initial_dates, final_dates, -time)

    # create a variable for the spells
    job_loc = full_join(loc_oslaua, job, by = c("oslaua")) %>% 
      group_by(location_id) %>%
      mutate(spell_quarter = row_number())
    
    job_loc_test = job_loc %>% 
      select(location_id, oslaua, event:spell_quarter)
    
   
    # link information regarding job seekers
    
    df_test = full_join(df, job_loc_test, by = c("location_id", "oslaua", "initial_dates", "final_dates", "event", "spell_quarter"))
    
    df_test = df_test %>% 
      group_by(location_id) %>% 
      arrange(location_id, initial_dates, final_dates)
    
    # fill and clean up
    df_test =df_test %>% group_by(location_id) %>%
      fill(total_inspections, .direction = "down") 
    
    
    
    
    df_test_clean = df_test %>%
      filter(!is.na(total_inspections)) # this contains all the spells but the ones corresponding to claimants information
    
    # fill the missing years 
    
    df_test_clean = df_test_clean  %>% mutate(year = ifelse(final_dates < "2015-04-01", 1415, 
                               ifelse(final_dates >= "2015-04-01" & final_dates < "2016-04-01", 1516, 
                                      ifelse(final_dates >= "2016-04-01" & final_dates < "2017-04-01", 1617,
                                             ifelse(final_dates >= "2017-04-01",1718, 5)))))
    
    df_test_clean = df_test_clean %>% fill(year, .direction = "down")
    
    




###########################################################
# Link information regarding jobs,claimants and population
###########################################################

# population 
    df_test_clean =  left_join(df_test_clean, population, by = c("oslaua", "year"))

# job 

    job_loc =  as.data.frame(job_loc)
    job_loc_test = job_loc %>%
      select(oslaua, spell_quarter, js_rate,  js_number) %>% 
      unique()
    
    df1_clean = left_join(df_test_clean, job_loc_test, by = c("oslaua", "spell_quarter"))

# claimants 
    
      df_test_clean = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/df_test_clean.csv")
      
      
      claimants = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/claimants_allowance_long.csv")
      
      claimants_test = claimants %>% select(oslaua, claimants, spell) %>% unique()
      
      df3_test = left_join(df3, claimants_test, by = c("oslaua" = "oslaua",
                                                                       "spell_quarter" =  "spell"))

###########################################
# Link information regarding spending power
###########################################

      sp =  import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/sp_clean_1418.csv")
      
      sp_clean = sp %>% select(oslaua, year, change_revenue_percentage) %>% 
        mutate_at(vars(year), funs(as.numeric))
      
      
      df2_clean = left_join(df1_test_clean, sp_clean, by = c("oslaua", "year"))

write.csv(df2_clean, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/df_test_clean.csv", row.names = FALSE)

##########################################################################
# Drop duplicated observations. This may occur for two reasons
#       --> events that do not happen - after the deregistration
#       --> events that are recorded in the same time unit (year, quarter)
###########################################################################

# events after the deregistration
        df2 <- df_test_clean %>% 
          group_by(location_id) %>%
          mutate(Deregistration = ifelse(event == "location_hsca_end_date", 1, NA)) %>%
          fill(Deregistration, .direction = "up") 
        
        deact = df2 %>% filter(event == "location_hsca_end_date")
        
        df2 = df2 %>% 
          mutate(Deregistration = ifelse(!(location_id %in% unique(deact$location_id)), 0, Deregistration))


# drop the missing in Deregistraiton
          df2 =  df2 %>% drop_na(Deregistration)
          
#         --> Deregistration = 0 (active)
#         --> Deregistration = 1 (inactive)
          

write.csv(df2, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/df_test_clean.csv", row.names = FALSE)
          
# events that are duplicated

df2 = df2 %>% select(-final_dates)
      
df2 = df2 %>% mutate_at(vars(initial_dates, final_dates), funs(as.Date))

# check potential duplicates of the initial dates for each location
df2test = df2 %>% group_by(location_id, initial_dates) %>%
  mutate(counts = n()) %>%
  select(location_id:initial_dates, counts, latest_rating:Deregistration)

# select those observations where there is a single initial date or those that are various initial dates but correspond to the publication 

dupes =  df2test %>% 
  group_by(location_id, initial_dates) %>%
  get_dupes(initial_dates)
            

df2test_clean = df2test %>% 
  group_by(location_id, initial_dates) %>%
  filter(counts == 1 | counts >1 & event %in% c("publication_date", "location_hsca_end_date")| row_number() == 1)



write.csv(df2test_clean, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/df5_test.csv", row.names = FALSE)



##########
# Duration
###########

      
df3 = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/df5_test.csv")
      
      df3 = df2test_clean


      df3 = df3 %>% mutate_at(vars(initial_dates), funs(as.Date))

# check potential duplicates of the initial dates for each location
      test_one = df3 %>% group_by(location_id, initial_dates) %>%
        mutate(counts = n()) %>%
        select(location_id:initial_dates, counts, final_dates:claimants)
     
      # select those observations where there is a single initial date or those that are various initial dates but correspond to the publication 
      test_clean_one = test_one %>% 
        filter(counts == 1 | counts >1 & event == "publication_date")
      
# create different final dates --> the existing are providing misleading information  
      test_clean_one = df3 %>%
        group_by(location_id) %>%
        mutate_at(vars(initial_dates), funs(as.Date)) %>% 
        mutate(fd = lead(initial_dates)-1) %>%
        select(location_id:counts, fd, latest_rating:Deregistration) 

# ----------------        
# fill final dates
# -----------------

# transform dates into character --> it is easy to handle
      test_clean_one = test_clean_one %>%
        mutate_at(vars(initial_dates, fd), funs(as.character)) 
      
# those that are de-registered
      test_clean_one =  test_clean_one  %>% 
        mutate(fd = ifelse(event == "location_hsca_end_date", initial_dates, fd))

      
      inact = test_clean_one %>% filter(event == "location_hsca_end_date")
      
      ids_inact = unique(inact$location_id)
      
      df3_inact = test_clean_one %>% 
        filter(location_id %in% ids_inact)
      
# those that are registered
      test_clean_one =  test_clean_one %>% 
        group_by(location_id) %>% 
        mutate_at(vars(initial_dates, fd), funs(as.character)) %>% 
        mutate(fd = ifelse(is.na(fd), "2017-06-01", fd))
      
   
# --------
# duration
# --------
      
      
      test_clean_one= test_clean_one %>%
        mutate_at(vars(initial_dates, fd), funs(as.Date))
      
      test_clean_one =  test_clean_one %>%
        mutate(duration = fd -initial_dates) %>%
        mutate_at(vars(duration), funs(as.numeric)) %>%
        select(location_id:fd, duration,  latest_rating:Deregistration)
      
      check = t2 %>% filter(duration <0 )
      ids_double = unique(check$location_id)
      
      raros = test_clean_one %>% filter(location_id %in% ids_double)
        
      t2 = test_clean_one %>%
        group_by(location_id) %>% 
      filter(duration >= 0)
    
        
      
      summary(t2)
      

      

      
####################
# Fill the failures
#####################
      
    t2 =  import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/df5_test.csv")
      t2 = t2 %>% 
        group_by(location_id) %>%
        mutate(failure_bad = ifelse(is.na(failure_bad), 0, failure_bad),
               failure_good = ifelse(is.na(failure_good), 0, failure_good))
      
      t2= t2 %>%
        group_by(location_id) %>%
        fill(start_type, .direction = "down")
      

              
 write.csv(t2, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/df5_test.csv", row.names = FALSE)
 
 ############
 # Add inforamation regarding the care homes
 ################
 
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
 library(survival)
 library(forcats)
 library(lme4)
 library(lmerTest)
 library(dummies)
 library(xtable)
 library(ggfortify)
 library(muhaz)
 library(tidyverse)
 
 # -------------------------------------------------------------------------------------------
 
 
 
 
 t2 =  import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/df5_test.csv")
 
 
 # select only relevant event: change in the inspection and in the budget 
 
 t3 = t2 %>%
   mutate(event2 = str_sub(event, start = 1, end = 2)) %>%
   mutate_at(vars(initial_dates), funs(as.Date)) %>% 
   mutate(period = format(as.Date(initial_dates), "%m-%d")) %>%
   select(location_id:event, event2, period, initial_dates:Deregistration)%>%
   filter(event2 %in% c("pu", "sp", "lo") | period == "04-01") # select publication date (pu), end (lo) and spending power(sp)
 
 # change final dates
 
 t3 = t3 %>% 
   group_by(location_id) %>%
   mutate_at(vars(initial_dates), funs(as.Date)) %>%
   arrange(location_id, initial_dates) %>%
   mutate(fd = lead(initial_dates)-1)
 
 # final dates for deregistered na Na
 
 t3 = t3 %>% 
   group_by(location_id) %>%
   mutate_at(vars(initial_dates, fd), funs(as.character)) %>%
   mutate(fd = ifelse(event2 == "lo", initial_dates, fd)) %>% 
   mutate(fd = ifelse(is.na(fd), "2017-06-30",fd)) %>%
   mutate_at(vars(initial_dates, fd), funs(as.Date)) %>%
   mutate(duration = fd - initial_dates, 
          number_spell = row_number(), 
          spells = n()) 
 
 
 t3$duration = as.numeric(t3$duration)
 
 t3 = t3 %>%
   group_by(location_id) %>%
   mutate(end_date = cumsum(duration), 
          start_date = lag(end_date))  %>%
   select(location_id, oslaua, number_spell, spells, event:duration, start_date, end_date, latest_rating:Deregistration)
 
 
 t3 = t3 %>%
   group_by(location_id) %>%
   mutate(start_date = ifelse(is.na(start_date), 0, start_date)) 
 
 
 write.csv(t4, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/t3.csv", row.names = FALSE)
 
###########################################
# Add information regarding the care homes
########################################### 
 
# get information from ratings datasets
 
 inspections <- read_csv("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/inspections_extended.csv") 
 
 inspections = clean_names(inspections) 
 
 inspections = inspections %>%
   select(location_id, provider_id, location_name, 
           care_homes_beds:provider_parliamentary_constituency ) %>%
   group_by(location_id) %>%
   unique()
 
t5 = left_join(t4, inspections, by = "location_id")

# split the sample between those locations that are complete and incomplete
       t5_complete = t5 %>% filter(!is.na(location_name))
       
       t5_incomplete = t5 %>% filter(is.na(location_name))
       
# ------------------------------
# analyse the incomplete cases
# ------------------------------
       
       
       deactivated = read_xlsx("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/20170801 De-activated locations.xlsx", sheet = "Sheet1")
       
       # idea: add missing information for those care homes that are deregistered before June 2017 - those that have NA information.
       # ------------------------------------------------------------------------------------------------------------------------------
       
       
       # IDs that have been rated and are registered as deactivated
       
       deact_june = deactivated %>%
         mutate_at(vars(ends_with("Date")), funs(as.Date)) %>% 
         filter(`Location HSCA End Date` < "2017-06-01")
       
       deact_june =  clean_names(deact_june) %>% 
         rename(care_homes_beds = care_homes_beds_at_point_location_de_activated,
                location_ccg_code = location_onspd_ccg_code, 
                location_ccg = location_onspd_ccg)
       
       
       
       # create a group wit hte names of vars
       
       common_in = intersect(names(t5_incomplete), names(deact_june))

       diff = setdiff(unique(t5_incomplete$location_id), unique(deacti$location_id) )
       
       
       vars = deact_june %>% 
         select(one_of(common_in))
       
       names_vars = names(vars)
       
       t6 = left_join(t5_incomplete, vars, by = "location_id")
       
       t6 = t6 %>% select(location_id:change_revenue_pounds, ends_with(".y"))
       
       names(t6) <-  names(t5_incomplete)
       
       # some still missing 
       t6_complete = t6 %>% filter(!is.na(location_name))
       t6_incomplete = t6 %>% filter(is.na(location_name))
       
       
       ids_conflictive = unique(t6_incomplete$location_id)
       
       activated = read_xlsx("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/1_June_2017_HSCA_active_locations.xlsx", sheet = "HSCA Active Locations", range = cell_rows(7:49396))
       
       activated_conflictive = clean_names(activated) %>%
         filter(care_home == "Y") %>% 
         select(one_of(names_vars)) %>%
         filter(location_id %in% ids_conflictive)
       
       t7_incomplete =  left_join(t6_incomplete, activated_conflictive, by = "location_id")
       t7_incomplete = t7_incomplete %>% select(location_id:change_revenue_pounds, ends_with(".y"))
       
       names(t7_incomplete) <- names(t6_incomplete)
       
       # link completed datasets
       t6 = rbind(t7_incomplete, t6_complete) %>% 
         arrange(location_id, initial_dates)
       
       t5_premium = rbind(t5_complete, t6)  %>% 
         arrange(location_id, initial_dates) %>% unique()
       
       
write.csv(df5, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/t3.csv", row.names = FALSE)
  
###########
# Dimension
###########

t5_premium = t5_premium %>% group_by(location_id) %>% mutate(dimension = ifelse(care_homes_beds < 10, "small",
                                                                  ifelse(care_homes_beds >= 10 & care_homes_beds < 50, "medium", 
                                                                         ifelse(care_homes_beds >= 50, "big"))))

#################
# Dementia users
#################

activated = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/1_June_2017_HSCA_active_locations.xlsx", sheet = "HSCA Active Locations", range = cell_rows(7:49396))
deactivated = read_xlsx("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/20170801 De-activated locations.xlsx", sheet = "Sheet1")



df4=  import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/t3.csv")


# Active care homes
activated = clean_names(activated)

rated_ids = unique(df4$location_id)

act_rated_dementia = activated %>% 
  filter(location_id %in% rated_ids) %>%
  select(location_id, user_demential = service_user_band_dementia)

# Deactive care homes

deactivated = clean_names(deactivated)

dif = setdiff(unique(deactivated$location_id), unique(df4$location_id) )

deact_rated_dementia = deactivated %>% 
  filter(location_id %in% rated_ids) %>%
  select(location_id, user_demential = service_user_band_dementia)

dementia = rbind(act_rated_dementia, deact_rated_dementia)


demen = unique(dementia)

write.csv(demen, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/dementia.csv", row.names = FALSE )

check = demen %>% group_by(location_id) %>% filter(n()>1) %>% arrange(location_id)

df6 = left_join(t3, demen, by = "location_id")

df6 = df6 %>% mutate(dementia = ifelse(is.na(user_demential), 0, 1)) %>% select(-user_demential)


write.csv(df7, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/t3.csv", row.names = FALSE )


########################
# Calculate proportions
########################

df6 = df6 %>% mutate_at(vars(old_85, claimants, population, js_number), funs(as.numeric))

df6 = df6 %>% group_by(location_id) %>% 
      mutate(prop_old_65 = (old_65/population),
        prop_old_85 = (old_85/population),
             prop_claimants = (claimants/population),
             pro_js = (js_number/population))


###############################
# Index of Multiple Deprivation
################################

geo = import("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/ONSPD_FEB_2017_UK/Data/ONSPD_FEB_2017_UK.csv")

geo_selection = geo %>% 
  select(pcd,oslaua, lat, long, lsoa11, msoa11, imd) %>%
  mutate(postcode = gsub("[[:blank:]]", "", pcd))

df6 = df6 %>% 
  mutate(postcode = gsub("[[:blank:]]", "", location_postal_code))

posts  =  unique(df6$postcode)

geo_posts =  geo_selection %>% filter(postcode %in% posts)


df7 =  left_join(df6, geo_posts, by = c("oslaua", "postcode"))






 
 