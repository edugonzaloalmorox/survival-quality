##############################################
# Spending power
# Data obtained from: https://www.gov.uk/government/statistical-data-sets/spending-power-by-local-authority
# September 2017
# @ EduGonzalo 
###############################################

library(xlsx)
library(readr)
library(dplyr)
library(tidyverse)
library(readxl)
library(forcats)
library(rio)
library(purrr)
library(janitor)
library(tidyr)
library(stringr)




###############################################
# LOAD DATA REGARDING SOCIAL CARE FOR EACH YEAR
################################################

    sp1213 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding//spending_power/spending-power-2011-12.xlsx", sheet = "Sheet2")
    
    sp1314 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding//spending_power/spending-power-2011-12.xlsx", sheet = "Sheet3")
    
    sp1415 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding//spending_power/spending-power-2011-12.xlsx", sheet = "Sheet4")
    
    sp1516 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding//spending_power/spending-power-2011-12.xlsx", sheet = "Sheet5")
    
    sp1617 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding//spending_power/spending-power-2011-12.xlsx", sheet = "Sheet6")
    
    sp1718 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding//spending_power/spending-power-2011-12.xlsx", sheet = "Sheet7")
    

# names of variables
    
    names(sp1213) <- c("local_authority", "adjusted_sp_1112", "estimated_sp_1213", "change_revenue_sp_1213_pounds", "change_revenue_sp_1213_percentage")
    
    names(sp1314) <- c("local_authority", "adjusted_sp_1213", "estimated_sp_1314", "change_revenue_sp_1314_pounds", "change_revenue_sp_1314_percentage")
    
    names(sp1415) <- c("code", "local_authority", "adjusted_sp_1314", "estimated_sp_1415", "change_revenue_sp_1415_pounds", "change_revenue_sp_1415_percentage" )
    
    names(sp1516) <- c("code", "local_authority", "adjusted_sp_1415", "estimated_sp_1516", "change_revenue_sp_1516_pounds", "change_revenue_sp_1516_percentage" )
    
    names(sp1617) <- c("local_authority", "adjusted_sp_1516", "estimated_sp_1617", "change_revenue_sp_1617_pounds", "change_revenue_sp_1617_percentage" )
    
    names(sp1718) <- c("local_authority", "change_revenue_sp_1718_pounds" , "change_revenue_sp_1718_percentage" )

##################
# CLEAN THE DATA    
##################  
    
    
 # create long -  tidy, dataset 
  # ---------------------------
    
    sp1314_long = sp1314 %>% gather(variable, value, estimated_sp_1314:change_revenue_sp_1314_percentage)  %>% select(-adjusted_sp_1213)
    
    sp1415_long = sp1415 %>% 
      select(local_authority, `change_revenue_sp_1415_pounds`, `change_revenue_sp_1415_percentage`) %>% 
    gather(variable, value, change_revenue_sp_1415_pounds:change_revenue_sp_1415_percentage) 
    
    sp1516_long = sp1516 %>% 
      select(local_authority, `change_revenue_sp_1516_pounds`, `change_revenue_sp_1516_percentage`) %>% 
      gather(variable, value, `change_revenue_sp_1516_pounds`:`change_revenue_sp_1516_percentage`)  
    
    sp1617_long = sp1617 %>% 
      select(local_authority, `change_revenue_sp_1617_pounds`, `change_revenue_sp_1617_percentage`) %>% 
      gather(variable, value, `change_revenue_sp_1617_pounds`:`change_revenue_sp_1617_percentage`)  
    
    sp1718_long = sp1718 %>% 
      select(local_authority, `change_revenue_sp_1718_pounds`, `change_revenue_sp_1718_percentage`) %>% 
      gather(variable, value, `change_revenue_sp_1718_pounds`:`change_revenue_sp_1718_percentage`)  
    
    

    
    #  link all the datasets 
    sp_long = rbind( sp1415_long, sp1516_long, sp1617_long, sp1718_long) %>%
      arrange(local_authority, variable) %>% 
      filter(!is.na(local_authority))
    
    # add the codes of the local authorities 
    codes  = import("/Users/Personas/Downloads/Local_Authority_Districts_December_2016_Names_and_Codes_in_the_United_Kingdom.csv")
    
    # check common names
     intersect(unique(sp_long_clean$local_authority), unique(codes$LAD16NM))
      
      intersect(unique(codes$LAD16NM), unique(sp_long_clean$local_authority))
      
   
# Get rid of fire authorities
# ----------------------------
    # idea: get the oslaua codes and clean names of the local authorities
    
    sp_fire = sp_long %>%
        filter(str_detect(local_authority, "Fire"))
    
    fires = unique(sp_fire$local_authority)
    
    sp_long_clean = sp_long %>%
      filter(! local_authority %in% fires)
    
    # clean variables
    sp_long_clean = sp_long_clean %>% mutate(local = gsub("&", "and", local_authority),
                                               gsub(", City of", "", local),
                                     local = gsub(", County of", "", local), 
                                     local = str_trim(local, "both"))
    
    
    
    
    
    codes = codes %>% mutate(la = gsub("&", "and", LAD16NM),
                       la = gsub("UA", "", la),
                       la = gsub("CC", "", la), 
                       la = gsub("BC", "", la),
                       la = gsub("DC", "", la), 
                       la = gsub("MBC", "", la), 
                       la = str_trim(la, "both"), 
                       la = gsub("&", "and", la),
                       la = gsub(", City of", "", la),
                       la = gsub(", County of", "", la), 
                       la = str_trim(la, "both"))
    
    sp_long_clean =  sp_long_clean %>% mutate(local2 = fct_recode(local,
                                                                  "St. Helens" = "St Helens", 
                                                                  "Telford and Wrekin" = "Telford and the Wrekin", 
                                                                  "County Durham" = "Durham",
                                                                  "Isle of Wight" = "Isle of Wight Council"))

    

   commons =  intersect(unique(sp_long_clean$local2), unique(codes$la))
    
  # select common district authorities 
   
       sp_long_district = sp_long_clean %>% 
         filter(local2 %in% commons) %>%
         select(la = local2, variable, value)
       
       codes_district = codes %>%
         filter(la %in% commons) %>%
         select(la, oslaua = LAD16CD)
        
   
  # filter the dataset with the district local authorities 
       
       sp_district_code =  left_join(sp_long_district, codes_district, by = "la")
   
    
library(readr)
       
       sp_district_code = sp_district_code %>% mutate(year =  parse_number(variable), 
                                                      spending_power = gsub("[[:digit:]]", "", variable), 
                                                      spending_power = gsub("_sp_", "", spending_power)) %>% 
         select(oslaua, la, year, spending_power, value)

##############         
# WIDE FORMAT
##############
       
   sp_wide_change_rev_pounds = sp_district_code %>% filter(spending_power == "change_revenue_pounds") %>%
     spread(spending_power, value)
   
   sp_wide_change_rev_percentage = sp_district_code %>% filter(spending_power == "change_revenue_percentage") %>%
     spread(spending_power, value)
   
   sp_wide_estimated = sp_district_code %>% filter(spending_power == "estimated") %>%
     spread(spending_power, value)
   
   
   spending_power = sp_wide_change_rev_percentage %>% 
     left_join(sp_wide_change_rev_pounds, by=c('oslaua', "la", "year")) 
   
   
   
   write.csv(spending_power, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/spending_power_1418.csv", row.names = FALSE)

#-----------------------------------  

################################################################
# get information regarding the change in the revenue percentage
################################################################
   
  sp_change =  sp %>% select(oslaua, la, year,change_revenue_percentage)
   
  sp1718 = sp1718 %>% mutate(year = 1718)


  sp =  import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/spending_power_1317.csv")
# -----------------------------------  
# clean names of the local authorities
# ------------------------------------
  
  # Get rid of fire authorities
  # ----------------------------
  # idea: get the oslaua codes and clean names of the local authorities
  
  sp_fire = sp1718 %>% filter(str_detect(local_authority, "Fire"))
  
  fires = unique(sp_fire$local_authority)
  
  sp1718_clean = sp1718 %>% filter(! local_authority %in% fires)
  
  # clean variable
  # --------------
  
  
  sp1718_clean = sp1718_clean %>% mutate(local = gsub("&", "and", local_authority),
                                          local= gsub(", City of", "", local),
                                           local = gsub(", County of", "", local), 
                                           local = str_trim(local, "both"),
                                         oslaua = NA) %>% 
    select(oslaua, la = local, year, change_revenue_percentage = change_revenue_sp_1718_percentage)
    
  
 
    sp_clean = rbind(sp_change, sp1718_clean) %>% arrange(la)
  
    sp_clean = sp_clean %>% mutate(local = gsub("[[:punct:]]", "", la), 
                                 local = str_to_lower(local), 
                                 local = str_trim(local, "both"))
    
    
   sp_clean =  sp_clean %>% mutate(local2 = fct_recode(local, "telford and wrekin" = "telford and the wrekin", 
                                                                "county durham" = "durham",
                                                                "isle of wight" = "isle of wight council"))
  
   sp_clean = sp_clean %>% group_by(local2, year) %>% 
     arrange(local2, year) 
   
   # fill oslauas that have missing data --> these are not districts
   # ---------------------------------------------------------------
   
   
   sp_clean = sp_clean %>% mutate_at(vars(oslaua, la, year, local, local2), funs(as.factor))
   
   sp_clean = sp_clean %>% group_by(local2)  %>% tidyr::fill(oslaua, .direction = "down")
  
   sp_clean_1418 = sp_clean %>% filter(!is.na(oslaua)) %>%
         select(oslaua, local2, year, change_revenue_percentage) %>%
         rename(la = local2)
   
  
   write.csv(sp_clean_1418, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/sp_clean_1418.csv", row.names = FALSE)
   
   sp = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/sp_clean_1418.csv")
   


