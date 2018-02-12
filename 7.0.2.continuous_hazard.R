


    df = import("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/spells_test.csv")
    
    df = df %>% select(location_id:initial_dates, fd, latest_rating:duration2, -duration, -var, -var2)
    
    df = df %>%
      rename(final_dates =  fd, duration = duration2)  %>% 
      mutate_at(vars(initial_dates, final_dates), funs(as.Date))
    
    df = df %>% group_by(location_id) %>% mutate(split_id = row_number(),
                                                 number_splits = sum(n()))

# ---------------------------------------
# select information of the care home
# ---------------------------------------

# data survival
    data_surv = import("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/data_survival.csv")

    ids = unique(df$location_id)

    dif2 = setdiff(unique(df$location_id), unique(data_surv$location_id))
    
    # select relevant variables
    dsurv = data_surv %>% select(location_id, location_name, care_homes_beds:msoa11, status) %>% unique()
    
    dsurv= dsurv %>% filter(location_id %in% ids) 
    
    df1 = left_join(df, dsurv, by = "location_id")
    
    check =  df1 %>% filter(is.na(category)) # those that have incomplete information
    
    # load inactive
    
    deactivated = read_xlsx("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/20170801 De-activated locations.xlsx", sheet = "Sheet1")
    
        deactivated = clean_names(deactivated)
        
        ids = unique(check$location_id)
        
    deact_mis = deactivated  %>% filter(location_id %in% ids) %>% select(location_id, category = location_primary_inspection_category, 
                                                                         region = location_region, la =  location_local_authority, 
                                                                         location_ccg_code  = location_commissioning_ccg_code, 
                                                                         location_ccg = location_commissioning_ccg)
    
   check_vars = check %>% select(location_id, category, region, la, location_ccg_code, location_ccg)
   
   check_vars = left_join(check_vars, deact_mis, by = c("location_id", "category", "region","la", "location_ccg_code", "location_ccg"))
   

# -----------------------------------------
# select information regarding expenditure
# ------------------------------------------
    
# variable to link information regarding the expenditure power

    df1 = df1 %>% mutate_at(vars(final_dates, initial_dates), funs(as.Date))
       
    df1 = df1 %>% mutate(year = ifelse(final_dates < "2015-04-01", 1415, 
                                       ifelse(final_dates >= "2015-04-01" & final_dates < "2016-04-01", 1516, 
                                              ifelse(final_dates >= "2016-04-01" & final_dates < "2017-04-01", 1617,
                                                     ifelse(final_dates >= "2017-04-01",1718, 5)))))
        
    
        
        
        
    sp_clean = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/sp_clean_1418.csv")
    
    sp = sp_clean %>% select(oslaua, year, change_revenue_percentage) %>% mutate_at(vars(year, change_revenue_percentage), funs(as.numeric))



    df2 =  left_join(df1, sp, by = c("oslaua", "year"))
    
# -----------------------------------------
# select information regarding the market 
# -----------------------------------------
    
    job = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/job_seekers_long.csv")
    population = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/population_long.csv")
    claimants = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/claimants_allowance_long.csv")
    
# population
    
    population = population %>% mutate(year = ifelse(time == 2014, 1415, 
                                                     ifelse(time == 2015, 1516,
                                                            ifelse(time == 2016, 1617, 
                                                                   ifelse(time == 2017, 1718, "other"))))) %>%
      select(oslaua, population:year) 
    
    population   = population %>% mutate_at(vars(year), funs(as.numeric))
    
 df3 = left_join(df2, population, by = c("oslaua", "year"))
    
 
write.csv(df3, "/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/df3_test.csv", row.names = FALSE)


df3 = import("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/df3_test.csv")

# job 

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

job = job  %>% group_by(oslaua) %>% arrange(oslaua, initial_dates) %>%
  mutate(final_dates = lead(initial_dates) -1,
         event = "job_seeker") %>% select(oslaua, js_rate, js_number, event, initial_dates, final_dates)



df3 = df3 %>% mutate_at(vars(initial_dates, final_dates), funs(as.Date))

loc_oslaua = df3 %>% select(location_id, oslaua) %>% unique()

job_loc = full_join(loc_oslaua, job, by = c("oslaua"))


df3_test = full_join(df3, job_loc, by = c("location_id", "oslaua", "initial_dates", "final_dates", "event"))

df3_test = df3_test %>% group_by(location_id) %>% arrange(location_id, initial_dates, final_dates)

df3_test =df3_test %>% group_by(location_id) %>% 
  fill(total_inspections, .direction = "down")

df3_test_clean = df3_test %>% filter(!is.na(total_inspections))
