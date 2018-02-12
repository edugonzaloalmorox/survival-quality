library(magrittr)

  library(OIsurv)
  library(dplyr)
  library(broom)
  library(ggplot2)
  library(survsim)
  library(tidyverse)
  library(rio)
  library(readxl)

install.packages("OIsurv")
install.packages("survsim")
install.packages("KMsurv")
install.packages("muhaz")

library(KMsurv)

#########################################
# SAMPLING FOR DURATION ANALYSIS 
#########################################



# Load inspections: this contains all the inspections carried out under new system
#       - some (right) censoring due to: care homes inspected just once and care homes exit the market 


    inspections = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/inspections.csv")

# sub sample: consider only overall inspections
# ---------------------------------------------

    overall = inspections %>% filter(`Key Question` == "Overall")

# Define event/ transition:  - care home is in a bad rating. It downgrades (e.g. from outstanding or good to requires improvement or inadequate) 
#                               maintains bad rating
#
#                           - care home that maintains the good/outstanding ranking or even improves

  
    
    bad = c("Inadequate", "Requires improvement")
    
    good = c("Outstanding", "Good")
    
    overall = overall %>% group_by(`Location ID`) %>% mutate(downgrade = ifelse(lag(`Latest Rating`) %in% bad, 1,0), 
                                                             upgrade = ifelse(lag(`Latest Rating`) %in% good, 1, 0))
    
# redefine the duration  
    overall$duration = as.factor(overall$duration)
    
    overall_transformed = overall %>% select(`Location ID`, `Publication Date`) %>% group_by(`Location ID`) %>% 
      mutate(date = paste0("d_", row_number())) %>%
      spread(date, `Publication Date`)
    
   overall_transformed = overall_transformed %>% mutate_at(vars(d_1,d_2, d_3, d_4, d_5, d_6, d_7),funs(as.Date))
    
     overall_transformed =  overall_transformed %>% mutate(d2_d1 = d_2 - d_1, 
                                                          d3_d2 = d_3 - d_2, 
                                                          d4_d3 =  d_4 - d_3,
                                                          d5_d4 = d_5 - d_4,
                                                          d6_d5 = d_6 - d_5,
                                                          d7_d6 = d_7 - d_6)
     
     overall_transformed = overall_transformed %>% select(-d_1, -d_2, -d_3, -d_4, -d_5, -d_6, -d_7)
     
     library(lubridate)
     
     overall_month = overall_transformed %>% select(-`Location ID`) 
     
     overall_month = lapply(overall_month, month(overall_month))
     
     
     # convert to long
     
     overall_long = overall_transformed %>% gather(spell, days, d2_d1:d7_d6)
     
     overall_long = overall_long %>% mutate(time = ifelse(spell =="d2_d1", 1, 
                                           ifelse(spell == "d3_d2", 2,
                                           ifelse(spell == "d4_d3", 3,
                                                  ifelse(spell == "d5-d4", 4,
                                                         ifelse(spell == "d6_d5", 5,
                                                                ifelse(spell == "d7_d6", 6, 0)))))))
                                          
 
                                                          

     
     overall_test = left_join(overall, overall_long, by = c("Location ID", "time"))
     
     overall =  overall_test %>% select(-spell, -duration)
     
     overall$days = as.numeric(overall$days)
     
     overall = overall %>% select(`Location ID`:downgrade, upgrade, days)
     
# Additional information referred to the care homes
# -------------------------------------------------
     
     directory = read_xlsx("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/1_June_2017_HSCA_active_locations.xlsx", sheet= "Sheet1")
     
     ids.rated = unique(overall$`Location ID`)
    
     directory.rated = directory %>% filter(`Location ID` %in% ids.rated)
     
     
     directory.rated = directory.rated %>% select(`Location ID`,`Location Name`, `Location HSCA start date`, `Care homes beds`,
                                                  `Location Type/Sector`,  `Location Inspection Directorate`,
                                                  `Location Primary Inspection Category`, `Location Region`: `Location CCG`,
                                                  `Location City`: `Location Parliamentary Constituency`, 
                                                  `Provider ID`: `Provider Primary Inspection Category`,
                                                  `Provider City`: `Provider Parliamentary Constituency`)
     
     overall.extended = left_join(overall, directory.rated, by = c("Location ID", "Provider ID"))
     
     overall.extended_test= left_join(overall, directory.rated, by = c("Location ID", "Provider ID"))
     
     
     
     
     # test observations that are deactivated 
     
     
     
      check =  overall.extended %>% filter(is.na(`Location CCG Code`))
      
      # load directory of deactivated care homes
      
      deactivated = read_xlsx("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/20170801 De-activated locations.xlsx", sheet = "Sheet1")
      
      
      check = deactivated %>% filter(`Location ID` %in% not.in.direct)
      
      setdiff(unique(check$`Location ID`), unique(deactivated$`Location ID`)) 
      # "1-155283083" "RXXY4" do not appear in any of the directories 

    
    
#########################################
# DURATION ANALYSIS
#########################################
                                                            
    # density of failure times 
     
    survobjectdown <- Surv(time = data_wave1$duration, event = data_wave1$downgrade)
     
      
     
     km.as.one <- survfit(survobjectdown ~ 1, data = overall, conf.type = "log-log")
     plot(km.as.one, xlab = "Days", ylab = "F(t)", main = "Downgrade")
     
     overall$survobjectup <- Surv(time = data_wave1$duration, event = data_wave1$upgrade)
     km.as.two <- survfit(survobjectup ~ 1, data = overall, conf.type = "log-log")
     plot(km.as.two, xlab = "Days", ylab = "F(t)",  main = "Upgrade") 
     
     
   # hazard rate h(t) = f(t) / S(t) or f(t) / (1- f(t))
     
     library(muhaz)
     
     reg_fit <- kphaz.fit(overall$days, ovrrall$downgrade) 
     kphaz.plot(reg_fit)
     
     res.cox.down <- coxph(Surv(duration, downgrade) ~ `Care homes beds`, data = overall.extended_test)
     summary(res.cox.down)
     plot(res.cox.down)
     
     res.cox <- coxph(Surv(duration, upgrade) ~ `Care homes beds`, data =  overall.extended_test)
     summary(res.cox)
     
     res.cox.down <- survfit(Surv(duration, downgrade) ~ `Care homes beds`, data = overall.extended_test)
     

    # small, medium and big
     
     
     
    res.down =  survfit(Surv(duration,downgrade) ~ dimension, data = data_surv)
     
     
     ggsurvplot(res.down)
     
     if(!require(devtools)) install.packages("devtools")
     devtools::install_github("kassambara/survminer")
     
     
      
   
    