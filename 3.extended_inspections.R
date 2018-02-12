# Information from the directory of care homes

# Load inspections data frame
# Load data accessible in the directory of care homes CQC (June 2017) 
# Objective: Link additional information to the inspections data set 
 
# note: data of analysis consider the period November 2015 - June 2017 (October is not considered since it does not have information regarding providers)
# August 2017
# @EduGonzalo (Newcastle University)
# --------------------------------------------------------------


# Libraries
#-----------
library(readxl)
library(dplyr)
library(data.table)
library(stringr)
library(rio)
library(forcats)
library(Hmisc)
library(ggplot2)
library(janitor)

# ---------------

# ##########
# Load data 
# #########
directory = read_xlsx("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/1_June_2017_HSCA_active_locations.xlsx", sheet= "Sheet1")

inspections = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/inspections.csv")
# note: this dataset also considers information on inspections in care homes that have been deactivated as by June 2017


# labels #
# -------
#           warning: adding labels alters the attributes of the variables. 
#           It can have problems when merging data


var.labels.inspections <- c(`Location ID` = "location id", 
                `Publication Date` = "date of inspections publication",
                `Key Question` = "quality category",
                `Latest Rating` = "last rating for each category",
                `Provider ID` = "provider ID",
                `time` = "number of inspection",
                `total.inspections` = "number of inspections for each category",
                `duration` =  "duration (in days) between inspections for each category")

inspections <- Hmisc::upData(inspections, labels = var.labels.inspections)



# Select information for those care homes that are rated and active -----------------------------------

      ids.rated = unique(inspections$`Location ID`)
      
      directory.rated = directory %>% filter(`Location ID` %in% ids.rated)

# check that those wich do not appear are effectively deactivated----------------------------------------

      # those that are rated but not in the current directory (June 2017)
      not.in.direct = setdiff(ids.rated, unique(directory.rated$`Location ID`))

# load directory of deactivated care homes

    deactivated = read_xlsx("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/20170801 De-activated locations.xlsx", sheet = "Sheet1")


    check = deactivated %>% filter(`Location ID` %in% not.in.direct)
      
    setdiff(not.in.direct, unique(check$`Location ID`)) 
    
# note: drop: "1-155283083" "RXXY4" since do not appear in the directories 
    
# consider only active care homes as by June 2017
    
   ids.deactive = unique(deactivated$`Location ID`) # IDs of those deactivated
   
   inspections.active = inspections %>% filter(!(`Location ID` %in% ids.deactive)) %>% filter(!(`Location ID` %in% c("1-155283083","RXXY4")))
   
# ------------------------------------------------------------------------

############    
# Merge data 
############  
    
    # Notes:
    # drop publication date in "directory.rated" since it refers to the date of last inspection.
    # "inspections" keeps track of all the inspections carried over each category.
    # select useful variables for the analysis
    # only consider active care homes
    
    directory.rated = directory.rated %>% select(`Location ID`,`Location Name`, `Location HSCA start date`, `Care homes beds`,
                                                 `Location Type/Sector`,  `Location Inspection Directorate`,
                                                 `Location Primary Inspection Category`, `Location Region`: `Location CCG`,
                                                 `Location City`: `Location Parliamentary Constituency`, 
                                                 `Provider ID`: `Provider Primary Inspection Category`,
                                                 `Provider City`: `Provider Parliamentary Constituency`)
    
    rated.extended = left_join(inspections.active, directory.rated, by = c("Location ID", "Provider ID"))
    
    
    write.csv(rated.extended, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/inspections_extended.csv", row.names = FALSE)
    
