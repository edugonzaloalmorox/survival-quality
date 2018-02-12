##########################################################################
# Summary statistics
# Code for creating the summary statistics table
# Note: Referred to the sample of care homes that start good and start bad. 
############################################################################



test <-  import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/t4.csv")

test <- test %>% group_by(local_authority) %>% 
  mutate(total_grant = ifelse(is.na(total_grant), mean(total_grant, na.rm = TRUE), total_grant)) %>%
  ungroup()

# get inspections

        inspections = test %>% filter(event == "publication_date") %>% as.data.frame()
        
        
        
        
        
  

# function for computing the summary statistics table
# ----------------------------------------------------

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

        
# ------------------------------------------------------------------------

# select variables for the regression
        vars_good = t4_good %>% select(change_revenue_pounds, prop_old_65, 
                                  pro_js, prop_claimants, total_grant,district_metropolitan, 
                                  district_shire, district_UA, dim_big, dim_medium, dim_small, prov_oversight, dementia, imd_average_score, 
                                  net_dwellings,  sc_expen)

        vars_bad = t4_bad  %>% select(change_revenue_pounds, prop_old_65, 
                                      pro_js, prop_claimants, total_grant,district_metropolitan, 
                                      district_shire, district_UA, dim_big, dim_medium, dim_small, prov_oversight, dementia, imd_average_score, 
                                      net_dwellings,  sc_expen)
        
        vars = inspections %>% select(change_revenue_pounds, prop_old_65, 
                               pro_js, prop_claimants, total_grant,district_metropolitan, 
                               district_shire, district_UA, dim_big, dim_medium, dim_small, prov_oversight, dementia, imd_average_score, 
                               net_dwellings,  sc_expen)
        
  
# Statistic sample: only inspections 
        
        stats_inspections = sum_stats(vars, inspections)
        stats_inspections
        
        
        


# Add failures 
        
        t4_good = test %>% filter(start_type == "good")
        
      

          g = as.data.frame(table(t4_good$failure_good))
          
          g = g %>% mutate(Var1 = ifelse(Var1 == 1, "Event (downgrade)", 0)) %>% 
            filter(Var1 != 0) %>% rename(variable = Var1, n = Freq)
                            
          stats_inspections = bind_rows(stats_inspections, g)
          stats_inspections

          
# Statistic sample: bad
# ----------------------

          t4_bad = test %>% filter(start_type == "bad")
          
          b = as.data.frame(table(t4_bad$failure_bad))
          
          b = b %>% mutate(Var1 = ifelse(Var1 == 1, "Event (upgrade)", 0)) %>% 
            filter(Var1 != 0) %>% rename(variable = Var1, n = Freq)
          
          
        
          stats_inspections = bind_rows(stats_inspections, b)
          stats_inspections
                  
          
          
##################
# Printing tables
##################

library(xtable)
          
          xtable(stats_inspections)



 

 

 

                                                          
                                                      
                                                          

