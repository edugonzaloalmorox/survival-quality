###########################################################
# Further adjustements for including additional variable
# 
##########################################################


test <-  import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/t4.csv")


test = test %>%  mutate_at(vars(change_revenue_percentage, js_rate,
                                old_85, start_date, end_date, total_grant), funs(as.numeric))

# create dummies 

test <- cbind(test, dummy(test$class_district, sep = "_"))

test = test %>% 
  rename(district_london = test_L, 
         district_metropolitan = test_MD, 
         district_shire = test_SD,
         district_UA = test_UA)


library(dplyr)
library(readxl)
library (janitor)

imd_districts = read_excel("/Users/Personas/Dropbox/side_projects/spatial_obesity/data/processed/districts_imd.xlsx", sheet = 2)

imd_districts = clean_names(imd_districts)

imd = imd_districts %>% rename(oslaua = local_authority_district_code_2013) %>%
  select(oslaua, imd_average_rank:imd_rank_of_average_score)


test <-  import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/t4.csv")

t5 = left_join(test, imd, by = "oslaua")

write.csv(t5, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/t4.csv", row.names = FALSE )

#----------------------

                            
