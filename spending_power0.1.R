############
# Spending power
#############

library(xlsx)
library(readr)
library(dplyr)
library(tidyverse)
library(readxl)
library(forcats)
library(rio)
library(purrr)
library(janitor)


###############################################
# LOAD DATA REGARDING SOCIAL CARE FOR EACH YEAR
################################################

    sp1213 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding//spending_power/spending-power-2011-12.xlsx", sheet = "Sheet2")
    
    sp1314 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding//spending_power/spending-power-2011-12.xlsx", sheet = "Sheet3")
    
    sp1415 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding//spending_power/spending-power-2011-12.xlsx", sheet = "Sheet4")
    
    sp1516 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding//spending_power/spending-power-2011-12.xlsx", sheet = "Sheet5")
    
    sp1617 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding//spending_power/spending-power-2011-12.xlsx", sheet = "Sheet6")

# names of variables

    sp1213 =  sp1213 %>% clean_names()
    sp1314 = sp1314 %>% clean_names()
    sp1415 = sp1415 %>% clean_names()
    sp1516 = sp1516 %>% clean_names()
    sp1617 = sp1617 %>% clean_names()
    
    

    
    names(sp1213) <- c("local_authority", "adjusted_sp_1112", "estimated_sp_1213", "change_revenue_sp_1213_pounds", "change_revenue_sp_1213_percentage")
    
    names(sp1314) <- c("local_authority", "adjusted_sp_1213", "estimated_sp_1314", "change_revenue_sp_1314_pounds", "change_revenue_sp_1314_percentage")
    
    names(sp1415) <- c("code", "local_authority", "adjusted_sp_1314", "estimated_sp_1415", "change_revenue_sp_1415_pounds", "change_revenue_sp_1415_percentage" )
    
    names(sp1516) <- c("code", "local_authority", "adjusted_sp_1415", "estimated_sp_1516", "change_revenue_sp_1516_pounds", "change_revenue_sp_1516_percentage" )
    
    names(sp1617) <- c("local_authority", "adjusted_sp_1516", "estimated_sp_1617", "change_revenue_sp_1617_pounds", "change_revenue_sp_1617_percentage" )


# add the codes of the local authorities 
    
    
    codes  = import("/Users/Personas/Downloads/Local_Authority_Districts_December_2016_Names_and_Codes_in_the_United_Kingdom.csv")
    
    setdiff(unique(sp1314$local_authority), unique(codes$LAD16NM))
    
    
# select change in spending power

clear_df = function(x){

  positions <- c(1,4,5)
  df = x %>% select(positions)
  return(df)

}

    sp1213_clean = clear_df(sp1213)
    
    sp1314_clean = clear_df(sp1314)
    
    sp1415_clean = clear_df(sp1415)
    
    sp1516_clean = clear_df(sp1516)
    
    sp1617_clean = clear_df(sp1617)

# link datasets 
    
    # make them long format 
    

    sp12_14 =  left_join(sp1213_clean, sp1314_clean, by = "local_authority")
    
library(tidyr)
    
    sp12_14_long = sp12_14 %>% 
        select(local_authority, change_revenue_sp_1213_pounds, change_revenue_sp_1314_pounds) %>%
             gather(ychange_sp_pounds, change_revenue_sp_1213_pounds:change_revenue_sp_1314_pounds)
    
    
    












