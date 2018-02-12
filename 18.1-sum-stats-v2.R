##############
# Analysis
##############


library(SurvLong)
library(JointModel)
library(JM)
library(lmerTest)
library(survival)
library(frailtypack)
library(broom)
library(dummies)
library(psych)
library(xtable)
library(texreg)
library(rio)
library(tidyverse)
library(ggfortify)
library(survminer)
library(coxme)
library(broom)
library(lmtest)

##################################################
# Samples of analysis
##################################################

# note: transform some vars for the descriptive statistics 
# ----------------------------------------------------------
      test <-  import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/t4.csv")
      
      write.csv(test, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/t4.csv", row.names = FALSE)
      
# recode the missing data 
      
      test = test %>% mutate(total_grant = ifelse(is.na(total_grant), 0, total_grant), 
                             pro_js = ifelse(is.na(pro_js), 0, pro_js))
      
      test = cbind(test, dummy(test$change_sp, sep = "_"))
      
# ---------------------------------------------------------------
      
      

      test$change_sp =  factor(test$change_sp, levels = c("positive", "no", "negative"))
      
      t4_bad = test %>% filter(start_type == "bad")
      
      t4_good = test %>% filter(start_type == "good")
      
#----------------------------------------------------------------

########################################
# Description statistics
##########################################      
      # note: function for computing stats
      
      sum_stats = function(vars, df){
        require(psych)
        
        
        x = as.data.frame(describe(vars)) 
        
        
        
        # label variables
        
        lab_vars = c(
          "Positive change revenue spending power (£)",
          "No change revenue spending power (£)",
          "Negative change revenue spending power (£)", 
          "Population 65+ (%)",
          "Job seekers (%)",
          "Pension credit claimants (%)",
          "Total specific and special grants",
          "District (london)", 
          "District (metropolitan)",
          "District (shire)",
          "District (unitary authority)",
          "Dimension big",
          "Dimension medium", 
          "Dimension small",
          "Market oversight (yes = 1)", 
          "Dementia main user (yes = 1)", 
          "IMD Average Score")
        
        row.names(x) = lab_vars
        
        
        x$variable  =  row.names(x)
        
        y = x %>% select(variable, n, mean, sd, min, max) %>% as.data.frame()
        
        
        # round variables
        g = y %>% mutate_at(vars(mean, sd, min, max), funs(as.numeric)) %>%
          mutate_if(is.numeric, round, digits = 2)
        
        
        
        # get other statistics
        
        
        
        variable = c("Care homes","Districts")
        
        
        n = c(length(unique(df$location_id)), length(unique(df$oslaua)))
        
        
        individuals = data.frame(variable, n)
        
        
        # bind with the othe stats
        stats_table = bind_rows(g, individuals) 
        
        return(stats_table)
        
        
      }
##########################################
    
    # select vars
      vars =  test %>% select(test_positive, test_no, test_negative, prop_old_65, 
                                pro_js, prop_claimants, total_grant, district_london, district_metropolitan, 
                                district_shire, district_UA, dim_big, dim_medium, dim_small,  
                                prov_oversight, dementia, imd_average_score)
      
      descriptive_stat_models = sum_stats(vars, test)
      
      # print table (for Latex)
      library(xtable)
      
      xtable(descriptive_stat_models)

