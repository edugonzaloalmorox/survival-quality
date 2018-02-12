###############
# Evolution of spending power
# Data preparation for plotting
# @Edu Gonzalo 
# #######################




library(rio)
library(readxl)
library(tidyverse)
library(stringr)

# each year presents a different structure on the information displayed
# ----------------------------------------------------------------------

      # 2016-17
      spend201617 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2016-17.xlsx",
                               sheet = 3, range = "B6:Q391") %>% mutate(type = NA) %>%
        select(local_authority = X__1, type, change_sp_201617 = X__16) %>%
        filter(!is.na(local_authority))
      
      # 2015-16
      spend201516 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2015-16.xls",
                               sheet = 2, range = "A11:BD394") %>% 
        select(local_authority = X__3, type = X__1, change_sp_201516= X__56) %>% 
        filter(!is.na(local_authority))
      
      eng1516 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2015-16.xls",
                           sheet = 2, range = "C6:BD7") %>% 
        mutate(type = NA) %>% select(local_authority = X__1, type, change_sp_201516= X__54) %>%
        filter(!is.na(local_authority))
      
      spend201516 = bind_rows(eng1516, spend201516)
      

      # 2014-15
      spend201415 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2014-15.xls",
                               sheet = 2, range = "A11:BI394") %>%
        select(local_authority = X__3, type = X__1, change_sp_201415= X__61) %>%
        filter(!is.na(local_authority))
      
      eng1415 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2014-15.xls",
                           sheet = 2, range = "C6:BI7")  %>% mutate(type = NA)  %>%
        select(local_authority = X__1, type, change_sp_201415= X__59) %>% 
        filter(!is.na(local_authority))
      
      spend201415 = bind_rows(eng1415, spend201415)

      
      # 2013-14
      spend201314 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2013-14.xls",
                               sheet = 1, range = "B31:J519")  %>%  select(local_authority = X__1, change_sp_201314= X__9) %>% 
        filter(!is.na(change_sp_201314) & local_authority != "GLA - all functions")
      
      eng1314 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2013-14.xls",
                           sheet = 1, range = "B6:J7")  %>% mutate(type = NA)  %>% 
        select(local_authority = X__1, type, change_sp_201314= X__9) %>% 
        filter(!is.na(local_authority))
      
      spend201314 = bind_rows(eng1314, spend201314)
      
      # note: fire authorities are different to previous years (they include "Authority")
      
      # 2012-13
      spend201213 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2012-13.xls",
                               sheet = 1, range = "B46:J534") %>%  select(local_authority = X__1, change_sp_201213= X__9) %>% 
        filter(!is.na(change_sp_201213) & local_authority != "GLA - all functions")
      
      eng1213 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2012-13.xls",
                           sheet = 1, range = "B6:J7")  %>%
        mutate(type = NA)  %>%
        select(local_authority = X__1, type, change_sp_201213= X__9) %>% 
        filter(!is.na(local_authority))
      
      spend201213 = bind_rows(eng1213, spend201213)
      
      # note: fire authorities are different to previous years (they include "Authority")
      
      # 2011-12
      spend201112 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2011-12.xls",
                               sheet = 1, range = "B46:J534") %>% 
        select(local_authority = X__1, change_sp_201112= X__9) %>% 
        filter(!is.na(change_sp_201112) & local_authority != "GLA - all functions")
      
      eng1112 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2011-12.xls",
                           sheet = 1, range = "B6:J7")  %>% 
        mutate(type = NA)  %>% select(local_authority = X__1, type, change_sp_201112= X__9) %>% 
        filter(!is.na(local_authority))
      
      spend201112 = bind_rows(eng1112, spend201112)

# -----------------------------------------------------------------------------------------
 
# Clean up and Link datatsets
#############################
     
# get rid of fire authorities 
      
      clean_up =  function(df) {
       require(dplyr)
        require(stringr)
       
        x = df %>% filter(!str_detect(local_authority, "Fire|Fire Authority|Greater London")) %>% 
          mutate(local_authority = gsub("&", "and", local_authority)) %>%
          mutate_at(vars(local_authority, type), funs(as.factor))
        
        return(x)
      }
      
      
      spend1112 = clean_up(spend201112)
      spend1213 = clean_up(spend201213)
      spend1314 = clean_up(spend201314)
      spend1415 = clean_up(spend201415) 
      spend1516 = clean_up(spend201516)
      spend1617 = clean_up(spend201617) 

# link data
# ----------      
        spend = left_join(spend1516, spend1415, by = c("local_authority", "type"))
          
        spend = left_join(spend, spend1617, by = c("local_authority"))  %>% select(-type.y, type = type.x)
                            
        spend = left_join(spend, spend1314, by = c("local_authority"))  %>% select(-type.y, type = type.x)
          
        spend = left_join(spend, spend1213, by = c("local_authority"))  %>% select(-type.y, type = type.x)  
        
        spend = left_join(spend, spend1112, by = c("local_authority"))  %>% select(-type.y, type = type.x) 
        
    write.csv(spend, "/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/spending_power_change_201117.csv", row.names = FALSE)
  
    
###############################################
# Get the Core Spending Power 
  
    # 2016-17
    spend201617 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2016-17.xlsx",
                             sheet = 3, range = "B6:P391") %>% mutate(type = NA) %>%
      select(local_authority = X__1, type, change_sp_201617 = X__15) %>%
      filter(!is.na(local_authority))
    
    # 2015-16
    spend201516 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2015-16.xls",
                             sheet = 2, range = "A11:BC394")%>% 
      select(local_authority = X__3, type = X__1, change_sp_201516= X__55) %>% 
      filter(!is.na(local_authority))
    
    eng1516 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2015-16.xls",
                         sheet = 2, range = "C6:BC7")  %>% 
      mutate(type = NA) %>% select(local_authority = X__1, type, change_sp_201516= X__53) %>%
      filter(!is.na(local_authority))
    
    spend201516 = bind_rows(eng1516, spend201516)
    
    
    # 2014-15
    spend201415 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2014-15.xls",
                             sheet = 2, range = "A11:BG394")  %>%
      select(local_authority = X__3, type = X__1, change_sp_201415= X__59) %>%
      filter(!is.na(local_authority))
    
    eng1415 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2014-15.xls",
                         sheet = 2, range = "C6:BG7")  %>% mutate(type = NA)  %>%
      select(local_authority = X__1, type, change_sp_201415= X__57) %>% 
      filter(!is.na(local_authority))
    
    spend201415 = bind_rows(eng1415, spend201415)
    
    
    # 2013-14
    spend201314 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2013-14.xls",
                             sheet = 1, range = "B31:F519")  %>%  select(local_authority = X__1, change_sp_201314= X__5) %>% 
      filter(!is.na(change_sp_201314) & local_authority != "GLA - all functions")
    
    eng1314 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2013-14.xls",
                         sheet = 1, range = "B6:F7")  %>% mutate(type = NA)  %>% 
      select(local_authority = X__1, type, change_sp_201314= X__5) %>% 
      filter(!is.na(local_authority))
    
    spend201314 = bind_rows(eng1314, spend201314)
    
    # note: fire authorities are different to previous years (they include "Authority")
    
    # 2012-13
    spend201213 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2012-13.xls",
                             sheet = 1, range = "B46:F534") %>%  select(local_authority = X__1, change_sp_201213= X__5) %>% 
      filter(!is.na(change_sp_201213) & local_authority != "GLA - all functions")
    
    eng1213 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2012-13.xls",
                         sheet = 1, range = "B6:F7")  %>%
      mutate(type = NA)  %>%
      select(local_authority = X__1, type, change_sp_201213= X__5) %>% 
      filter(!is.na(local_authority))
    
    spend201213 = bind_rows(eng1213, spend201213)
    
    # note: fire authorities are different to previous years (they include "Authority")
    
    # 2011-12
    spend201112 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2011-12.xls",
                             sheet = 1, range = "B46:F534")  %>% 
      select(local_authority = X__1, change_sp_201112= X__5) %>% 
      filter(!is.na(change_sp_201112) & local_authority != "GLA - all functions")
    
    eng1112 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2011-12.xls",
                         sheet = 1, range = "B6:F7")  %>% 
      mutate(type = NA)  %>% select(local_authority = X__1, type, change_sp_201112= X__5) %>% 
      filter(!is.na(local_authority))
    
    spend201112 = bind_rows(eng1112, spend201112)
    
    
  # 2010 -12  
    
    spend201011 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/time-series/spend-2010-12.xls",
                             sheet = 1, range = "A4:AU388")  %>% 
      select(local_authority = X__3, change_sp_201011= X__32, change_sp_201112= X__47) %>% 
      filter(!is.na(change_sp_201112) & local_authority != "GLA - all functions")
    
    spend201011 =  spend201011 %>% filter(!str_detect(local_authority, "Fire|Fire Authority|Greater London")) %>% 
      mutate(local_authority = gsub("&", "and", local_authority)) 
    
   df = data.frame(local_authority = "England",  change_sp_201011 = NA,  change_sp_201112 = NA )  
   
   spend201011 =  bind_rows(df, spend201011)
   
    
   spend_test = left_join(spend, spend201011, by = "local_authority") %>%
     select(-change_sp_201112.x, change_sp_201011, change_sp_201112 = change_sp_201112.y,
            change_sp_201213, change_sp_201314, change_sp_201415, change_sp_201516, change_sp_201617)

    

    write.csv(spend_test, "/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/spending_power_change_pounds_201017.csv", row.names = FALSE)
    
    
                        