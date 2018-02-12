###################
# Duration analysis
####################

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


####################
# Data preparation # 
####################

# Load data
# ---------

# Load inspections: this contains all the inspections carried out under new system
#       - some (right) censoring due to: care homes inspected just once and care homes exit the market 


    inspections = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/inspections.csv")
    inpections = read.csv("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/inspections.csv")

# sub sample: consider only overall inspections
# ---------------------------------------------

    overall = inspections %>% filter(`Key Question` == "Overall")


# Define event/ transition / failure
# -----------------------------------

#                           - care home is in a bad rating. It downgrades (e.g. from outstanding or good to requires improvement or inadequate) 
#                               maintains bad rating

#                          - care home that maintains the good/outstanding ranking or even improves



      bad = c("Inadequate", "Requires improvement")
      
      good = c("Outstanding", "Good")



      overall = overall %>% group_by(`Location ID`) %>% mutate(downgrade = ifelse(`Latest Rating` %in% bad & total.inspections != 1, 1,0),
                                                               downgrade = ifelse(total.inspections != 1 & time == 1, NA, downgrade))
                                                              
      overall = overall %>% group_by(`Location ID`) %>% mutate(upgrade = ifelse(`Latest Rating` %in% good & total.inspections != 1, 1,0),
                                                               upgrade = ifelse(total.inspections != 1 & time == 1, NA, upgrade))
      

# note: downgrade = 1 if inspection reflects downgrade / 0  otherwise 
#       upgrade =  1 if inspection reflects upgrade / 0  otherwise 

# note: duration - reflects the time until next inspection 

# -------------------------------------------------
# Additional information referred to the care homes
# -------------------------------------------------
      
      directory = read_xlsx("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/1_June_2017_HSCA_active_locations.xlsx", sheet= "Sheet1")
      
      ids.rated = unique(overall$`Location ID`)
      
      # select those ids that are rated 
      directory.rated = directory %>% filter(`Location ID` %in% ids.rated)
      
      # select relevant information
      directory.rated = directory.rated %>% select(`Location ID`,`Location Name`, `Location HSCA start date`, `Care homes beds`,
                                                   `Location Type/Sector`,  `Location Inspection Directorate`,
                                                   `Location Primary Inspection Category`, `Location Region`: `Location CCG`,
                                                   `Location City`: `Location Parliamentary Constituency`, 
                                                   `Provider ID`: `Provider Primary Inspection Category`,
                                                   `Provider City`: `Provider Parliamentary Constituency`)
      
      data = left_join(overall, directory.rated, by = c("Location ID", "Provider ID"))

      
# -------------------------      
# Geographical information  
# -------------------------
      
      data_surv =  import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/data_survival.csv")
      
      # postcode directory 
      
      codes = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/ONSPD_FEB_2017_UK/Data/ONSPD_FEB_2017_UK.csv")
      
      codes  = codes %>% select(oslaua, pcd, lat, long, lsoa11, msoa11 )
      
      codes = codes %>% mutate(postcode = gsub("[[:blank:]]", "", pcd), 
                               postcode =  str_trim(postcode, "both"))
      
      ch_post  = unique(data_surv$postcode)
      
      codes_ch = codes %>% filter(postcode %in% ch_post)
      
      # link information of data_surv and codes
      # ---------------------------------------
      
      data_surv = left_join(data_surv, codes_ch, by = "postcode") %>% select(-pcd)
      
      write.csv(data_surv, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/data_survival.csv", row.names = FALSE)

# -----------------------------------------------------      
# Create new variables for asessing different survival
# -----------------------------------------------------      
      data_surv = data  %>%
      mutate(dimension = ifelse(`Care homes beds` <=10, "small",
                  ifelse(`Care homes beds` > 10 & `Care homes beds`<= 50, "medium", 
                 ifelse(`Care homes beds` > 50, "big", "other"))))
      
      data_surv = data_surv %>% rename(category = `Location Primary Inspection Category`, la = `Location Local Authority`,
                                       region =  `Location Region`)
      
      write.csv(data_surv, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/data_survival.csv", row.names = FALSE )
      
      data_surv =  import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/data_survival.csv")
      
# ----------------------------------------------------     
# Care homes that are inspected but are de-registered
# -----------------------------------------------------    
      
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
      
      
# ---------------------------------      
# Information referred to variables 
# ----------------------------------
  
      # Expenditure 
      # ------------
      
      
      
      
          
###################
# Duration Analysis
####################
      

# Plot density of failure times
      
    
      plot(density(na.omit(data_surv$duration)))

      
# Survival curves by dimension and region
# ----------------------------------------
      
      # dimension
      # ---------
      
      # survival object
      res.dim =  survfit(Surv(duration,downgrade) ~ dimension, data = data_surv)
      
      # plot
      ggsurvplot(res.dim, data_surv, pval = FALSE, palette = "npg", curve.size = 1, legend = "right") + 
        scale_x_continuous(breaks = c(0, 900)) +
        geom_vline(xintercept=c(365,730), linetype="dotted") + labs(x="Duration in a good rating (days)")
      
      # faceted plot
      res.reg =  survfit(Surv(duration,downgrade) ~ region, data = data_surv)
      
      ggsurvplot_facet(res.reg, data_surv, facet.by = "dimension",
                       palette = "jco", pval = FALSE)
      
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
      
      # region 
      # ------
      
      # survival object
      res.reg =  survfit(Surv(duration,downgrade) ~ region, data = data_surv)
      
      # plot 
      ggsurvplot(res.reg, data_surv, pval = FALSE, palette = "npg", curve.size = 1, legend = "top") + 
        scale_x_continuous(breaks = c(0, 900)) +
        geom_vline(xintercept=c(365,730), linetype="dotted") + labs(x="Duration in a good rating (days)")
      
 
      
    
      


      
    

