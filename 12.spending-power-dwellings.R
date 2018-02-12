############
# spending power per dwelling (English districts)
# november 2017
# Eduardo Gonzalo Almorox - Newcastle (Economics)
###########

library(rgdal)
library(foreign)
library(ggplot2)
library(choroplethr)
library(XML)
library(maptools)
library(dplyr)
library(tidyverse)
library(stringr)
library(rio)
library(grid)
library(gridExtra)
library(viridis)
library(readxl)
library (janitor)
library(forcats)


# -------------------------------
# information on spending power
# -------------------------------

sp = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/spending_power_summary.xlsx", 
                sheet = 3) %>% arrange(local_authority)

# add codes for data linkaage

codes = read_excel("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/specific_special_grants/rg-2017-18.xlsx", 
                   sheet = 3, range = "A7:C361")

# clean up and create the variable
codes = clean_names(codes) %>% arrange(local_authority)

codes = codes %>% mutate(la = gsub(" UA", "", local_authority),
                         la =  gsub("&", "and", la)) 

sp = sp %>% mutate(la = gsub("&", "and", local_authority))

codes$la = as.factor(codes$la)

codes = codes %>% mutate( la = fct_recode(la, "Derby" = "Derby City",
                                          "Nottingham" = "City of Nottingham",
                                          "Leicester" = "Leicester City", 
                                          "Medway" = "The Medway Towns"))


# select codes referred to oslauas
test = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/t4.csv")

oslauas = unique(test$oslaua)

codes_oslauas = codes %>% filter(ons_code %in% oslauas)


sp_codes = left_join(codes_oslauas, sp, by = "la") %>% 
  select(-local_authority.y, e_code, ons_code, code, local_authority = local_authority.x, la, `2015-16`:`2019-20`)


write.csv(sp_codes, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/sp_dwelling.csv", row.names = FALSE)



######################################
# spending power per dwelling 2013-14


sp_dwell_1314 = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/la_funding/spending_power/sp-dwells-201314.xls", 
                sheet = 1, range = "B31:R539") %>%
  select(local_authority = X__1, spdwell_1314 = X__17) %>% filter(!is.na(spdwell_1314) &  local_authority != "GLA - all functions" & !str_detect(local_authority, "Fire|Fire Authority|Greater London")) %>%
  mutate(la = gsub("&", "and", local_authority)) 


sp_dwell_1314_clean = sp_dwell_1314 %>% filter(la %in% unique(sp$la)) %>% select(-local_authority)

sp_test = left_join(sp, sp_dwell_1314_clean, by = "la")

write.csv(sp_test, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/sp_dwelling.csv", row.names = FALSE)





