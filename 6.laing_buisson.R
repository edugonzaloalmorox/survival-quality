###################################
# Handle Laing Buisson data 
# 29-08-2017
# @EduGonzalo -  Newcastle University
########################################





library(readxl)
library(dplyr)
library(data.table)
library(stringr)
library(rio)
library(forcats)
library(dplyr)
library(janitor)





prices_12 = import("/Users/Personas/Desktop/Care Home Archives 2012-2014/2012 Care Homes.csv")
prices_13 = import("/Users/Personas/Desktop/Care Home Archives 2012-2014/2013 Care Homes.csv")
prices_14 = import("/Users/Personas/Desktop/Care Home Archives 2012-2014/2014 Care Homes.csv")

#####################
# homogenise datasets
######################
    prices12 = prices_12 %>% clean_names() %>% select(-purpose_built_code)
    
    prices13 = prices_13 %>% clean_names()
    
    prices14 = prices_14 %>% clean_names()

# compare names of variables
    names12 = names(prices12)
    
    names13 = names(prices13)
    
    names14 = names(prices14)
    
    # setdiff(names(prices13), names(prices14)) # check
    # character(0) 

# clean datasets
    prices12_clean  = prices12 %>% select(-final_rating, -final_rating_date)
    
    prices13_clean = prices13 %>% select(-inspection_date, -compliance,
                                         -chapter1, -chapter2,
                                         -chapter3, -chapter4, -chapter5) 
    
    prices14_clean = prices14 %>% select(-inspection_date, -compliance,
                                         -chapter1, -chapter2,
                                         -chapter3, -chapter4, -chapter5) 

####################################################
# rename and link information regarding inspections
####################################################
    
    # information of ratings
    insp12 = prices12 %>% select(final_rating, final_rating_date)
    
    insp13 = prices13 %>% select(final_rating = compliance, final_rating_date = inspection_date)
    
    insp14 = prices14 %>% select(final_rating = compliance, final_rating_date = inspection_date)

    # complete datasets with ratings information
    p12complete = cbind(prices12_clean, insp12) %>% mutate(year = 2012)
    
    p13complete = cbind(prices13_clean, insp13)  %>% mutate(year = 2013)
    
    p14complete = cbind(prices14_clean, insp14)  %>% mutate(year = 2014)
  
    
    # link datasets by rows
    
    p1214 = rbind(p12complete, p13complete, p14complete)
    
    write.csv(p1214, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/laingbuisson1214.csv", row.names = FALSE)

l_b = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/laingbuisson1214.csv")

    
