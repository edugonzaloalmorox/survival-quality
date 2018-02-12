#################################################
# Fill information regarding those de-registered and those registered
# 1 - September 2017
# @ Edu Gonzalo Almorox
###################################################


library(rio)
library(dplyr)
library(forcats)
library(tibble)
library(ggplot2)
library(lubridate)
library(readxl)
library(janitor)
library(stringr)


ds = import("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/data_survival.csv")
deactivated = read_xlsx("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/20170801 De-activated locations.xlsx", sheet = "Sheet1")

# idea: add missing information for those care homes that are deregistered before June 2017 - those that have NA information.
# ------------------------------------------------------------------------------------------------------------------------------


# IDs that have been rated and are registered as deactivated

      deact_june = deactivated %>% mutate_at(vars(ends_with("Date")), funs(as.Date)) %>% filter(`Location HSCA End Date` < "2017-06-01")
      
      deact =  intersect(unique(deact_june$`Location ID`), unique(ds$`Location ID`)) #those that registered as "deactive"
      
      # datasets to work with 
      insp_deact = ds %>% filter (`Location ID` %in% deact) # this is the original dataset that I fill
      deact.inspected = deact_june %>% filter (`Location ID` %in% deact) # this is the dataset where I get the information from

# Select information to fill in 
     
       # note: there are variables that have the same name and variables that have a different name
      
       # check the variables that common and different
      common.variables.inactive =  intersect(names(insp_deact), names(deact.inspected))
      
      diff.variables.inactive = setdiff(names(insp_deact), names(deact.inspected))

    
    test_common = deact.inspected %>% select(one_of(common.variables.inactive)) # select common variables
    
    test_diff = deact.inspected %>% select(`Location ID`, `Location HSCA End Date`, 
                                      `Care homes beds` = `Care homes beds at point location de-activated`,
                                      `Location CCG Code` =  `Location Commissioning CCG Code`, 
                                      `Location CCG` = `Location Commissioning CCG`, `Provider County` = `Provider - County`)
     
    # dataset with information about those that are inspected but inactive
     test_inactive = left_join(test_common, test_diff, by = "Location ID") 
     
     # clean the names and link information
     
     # 1-select variables that come from the CQC
     
     
     insp_deact = insp_deact %>% 
       clean_names() %>% 
       mutate_at(vars(ends_with("date")), funs(as.Date))
     
     insp_deact_cqc = insp_deact %>% select(location_id, location_name:provider_parliamentary_constituency, -category, -region, -la)
     
     insp_deact_cqc_clean = insp_deact_cqc %>% 
       clean_names() %>% 
       mutate_at(vars(ends_with("date")), funs(as.Date))
     
     test_inactive_clean = test_inactive %>%
       clean_names() %>% 
       mutate_at(vars(ends_with("date")), funs(as.Date))
     
      
     
  # Link information to ins_deact (those that are inspected but deregistered)
     
     test_cqc = left_join(insp_deact_cqc_clean, test_inactive_clean, by ="location_id")
     
     # clean up the names
     test_clean_cqc = test_cqc %>% select(location_id, publication_date:provider_county.y)
     
         names.clean = names(test_clean_cqc)
         
         names.clean = gsub("\\.y", "", names.clean)
         
         names(test_clean_cqc) <-  names.clean
         
    # insert information that is missing 
         
         common.names.inactive = intersect(names(insp_deact), names(test_clean_cqc))
         diff.names.inactive = setdiff(names(insp_deact), names(test_clean_cqc))
         
         test_diff_names = insp_deact %>% select(location_id, one_of(diff.names.inactive))
         
         
         test_clean_cqc = left_join(test_clean_cqc, test_diff_names, by = "location_id")
         
    # note: there is an additional variable ---> date of exit from the market    
    
    # check duplicates 
         
         test_clean_cqc = test_clean_cqc %>% group_by(location_id) %>% unique()
         
      write.csv(test_clean_cqc, "/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/data_inspected_inactive.csv", row.names = FALSE)

      
# -----------------------------------
# Complete dataset with duration data
# load data_surv and inspected but deregistered
# -----------------------------------   
      library(rio)
      library(dplyr)
      library(forcats)
      library(tibble)
      library(ggplot2)
      library(lubridate)
      library(readxl)
      library(janitor)
      library(stringr)
      
      
      # Load data 
      
      data.survival = import("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/data_survival.csv")
      
      data.survival.deregistered = import("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/data_inspected_inactive.csv")
      
      
      # homogenize names of variables and reorder
      
      names.survival = data.survival %>%  clean_names()
      
      names.survival = names(names.survival)
      
      names(data.survival) <- names.survival
      
      setdiff(names(data.survival.deregistered), names(data.survival))
      
      data.survival.deregistered = data.survival.deregistered %>% select(one_of(names.survival), location_hsca_end_date) %>% mutate(status = "inactive")
      
      # check -----------------------------------------------------------------------
      dereg  = data.survival %>% filter(is.na(location_hsca_start_date))  
      
      setdiff(unique(dereg$location_id), unique(data.survival.deregistered$location_id))
      # "1-155283083" "RXXY4" --> they do not appear in the registry of de-registered care homes
      
      # split the data.frame into registered and registered
      
      reg  = data.survival %>% filter(!is.na(location_hsca_start_date)) %>% mutate(location_hsca_end_date = NA, status = "active")
      
      
      reg_dereg = rbind(data.survival.deregistered, reg) %>% arrange(location_id, time)
      
      
      
      # ------------------------------------------
      # Complete with geographical information 
      # ------------------------------------------
      
      geo = import("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/ONSPD_FEB_2017_UK/Data/ONSPD_FEB_2017_UK.csv")
      
      geo_selection = geo %>% select(pcd,oslaua, lat, long, lsoa11, msoa11) %>% mutate(postcode = gsub("[[:blank:]]", "", pcd))
      
      # select data frame with the postcodes of the care homes
      
      reg_dereg  = reg_dereg %>% mutate(postcode = gsub("[[:blank:]]", "", location_postal_code))
      
      reg_post = unique(reg_dereg$postcode) 
      
      geoposts = geo_selection %>% filter(postcode %in% reg_post)
      
      test = left_join(reg_dereg, geoposts, by = "postcode") %>% select(-oslaua.x, -lat.x, -long.x, -lsoa11.x, -msoa11.x)
      
      # clean names 
      
      names.clean = names(test)
      
      names.clean = gsub("\\.y", "", names.clean)
      
      names(test) <-  names.clean
      
      names.regdereg = names(reg_dereg)
      names.test = names(test)
      
      setdiff(names.test, names.regdereg)
      # -------------------------------------------------------------
      
      
      test = test %>% select(-pcd) 
      
      test1 =  test %>% select(one_of(names(reg_dereg)))
      
      write.csv(test1, "/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/data_survival.csv", row.names = FALSE)
      
      
      
      
      
      
      
     
     
     

 