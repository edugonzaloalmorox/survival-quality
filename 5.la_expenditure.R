################################################
# Load data of social care expenditure
# ------------------------------------
#
# Source: Local authority revenue expenditure and financing website (https://www.gov.uk/government/collections/local-authority-revenue-expenditure-and-financing)
# Accessed: August 2017
# 20 - 08 - 2017
# @Edu Gonzalo Almorox
# ##############################################


library(xlsx)
library(readr)
library(dplyr)
library(tidyverse)
library(readxl)
library(forcats)
library(rio)


###############################################
# LOAD DATA REGARDING SOCIAL CARE FOR EACH YEAR
################################################

        sc1718 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/RA_2017-18_data_by_LA.xlsx", sheet = "RA LA Data 2017-18", range = cell_rows(7:361))
        
        sc1617 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/RA_2016-17_data_by_LA.xlsx", sheet = "RA LA Data 2016-17", range = cell_rows(7:361)) 
        
        sc1516 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/RS_2015-16_data_by_LA.xlsx", sheet = "RS LA Data 2015-16", range = cell_rows(5:359)) 
        
        sc1415 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/Revenue_Outturn__RSX__data_2014-15_by_LA_-_02-Feb-2016-3.xls", sheet = "RSX LA Data 2014-15", range = cell_rows(8:363)) 
        
        sc1314 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/Revenue_Outturn_Summary__RS__data_2013-14_by_LA_-_Revised_28-Jan-2015.xls", sheet = "RS LA Data 2013-14 (1)", range = cell_rows(6:361)) 


# -------------------------------------
# clean and select important variables
# ------------------------------------

      sc1718 = sc1718 %>% select(`E-code`:`Local authority`, `TOTAL ADULT SOCIAL CARE`) %>% mutate(year = "2017-18")
      sc1617 = sc1617 %>% select(`E-code`:`Local authority`, `TOTAL ADULT SOCIAL CARE`) %>% mutate(year = "2016-17")
      sc1516 = sc1516 %>% select(`E-code`:`Local authority`, `Adult Social Care`) %>% mutate(year = "2015-16")

# select columns by positions
      positions <- c(1:3, 37)
      sc1415 = sc1415 %>% select(positions) %>% mutate(year = "2014-15")
      sc1415 = sc1415[-1, ]
      
      sc1314 = sc1314 %>% select(`E-code`, `Local authority`, `Adult Social Care`) %>% mutate(year = "2013-14")
      sc1314 = sc1314[-1, ]

# datasets with 4 columns
      names(sc1314) <- c("ecode", "local_authority", "adult_sc", "year")
      names(sc1617) <- c("ecode", "local_authority", "adult_sc", "year")

# datasets with 5 columns
      names(sc1415) <- c("ecode", "oslaua", "local_authority", "adult_sc", "year")
      names(sc1718) <- c("ecode", "oslaua", "local_authority", "adult_sc", "year")
      names(sc1516) <- c("ecode", "oslaua", "local_authority", "adult_sc", "year")
      

# link ons codes 
 codes = sc1415 %>% select(ecode, oslaua) 
 
 sc1314 = left_join(sc1314, codes, by = "ecode") %>% select(ecode, oslaua, local_authority, adult_sc, year)
 sc1617 = left_join(sc1617, codes, by = "ecode") %>% select(ecode, oslaua, local_authority, adult_sc, year)
 
 
 sc = rbind(sc1718, sc1617, sc1516, sc1415, sc1314)
 
 sc = sc %>% arrange(local_authority, year)
 
 
 write.csv(sc, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/adult_socialcare.csv", row.names = FALSE)

 
###########################################################################
# SELECT INFORMATION FOR THOSE LOCAL AUTHORITIES WHERE THERE ARE CARE HOMES
###########################################################################

 # purpose: get those oslauas in social care expenditure that are referred to the oslauas in the care homes data
 
# link information at oslaua level
# ---------------------------------
 
         data_surv = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/data_survival.csv")
         
         
         data_surv =  data_surv %>% mutate(postcode = gsub("[[:blank:]]", "", `Location Postal Code`), 
                                           postcode =  str_trim(postcode, "both"))
 
 # note: NAs in "data_surv" are associated with deactivated care homes
 
 # --------------------------------
 # ---- checkings -----------------
         
       deactivated = read_xlsx("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/20170801 De-activated locations.xlsx", sheet = "Sheet1")
       
       
       not.in.direct = unique(c$`Location ID`)
       
       check_deac = deactivated %>% filter(`Location ID` %in% not.in.direct)
       
       setdiff(not.in.direct, unique(check_deac$`Location ID`)) 
 # -------------------------------------
 # --------------------------------------
 
 # postcode directory
 # ---------------------
 
     codes = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/ONSPD_FEB_2017_UK/Data/ONSPD_FEB_2017_UK.csv")
     
     codes  = codes %>% select(oslaua, pcd, lat, long, lsoa11, msoa11 ) # select relevant variables
     
     codes = codes %>% mutate(postcode = gsub("[[:blank:]]", "", pcd), 
                              postcode =  str_trim(postcode, "both"))
     
     ch_post  = unique(data_surv$postcode) 
     
     codes_ch = codes %>% filter(postcode %in% ch_post)
 
 # link information of data_surv and codes
 # ---------------------------------------
 
 data_surv = left_join(data_surv, codes_ch, by = "postcode") %>% select(-pcd)
 
 write.csv(data_surv, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/data_survival.csv", row.names = FALSE)
 
 
 # common oslauas in both
 # -------------------------
  common = intersect(unique(sc$oslaua), unique(data_surv$oslaua))
 
  # filter local authorities based on care homes

  sc_carehomes =sc %>% filter(oslaua %in% common) 
 
 write.csv(sc_carehomes, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/social_care_expenditure.csv", row.names = FALSE)
 
