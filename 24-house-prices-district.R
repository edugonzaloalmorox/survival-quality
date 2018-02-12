#####################
# House prices: Median price paid for residential properties at LSOA level
# Source ONS 
# Released on march, june, september, and december
# Years: Fiscal years
# @EduGonzalo
# December 2017
#######################################

library(readxl)

house_price = read_excel("/Users/Personas/Dropbox/PhD/ch2/paper/quality/agglomeration/data/raw/Median price paid for residential properties by LSOA.xls", sheet = "1a", range = "A6:CM32850")

library(janitor)

house_price = clean_names(house_price) %>% select(local_authority_code, local_authority_name, year_ending_jun_2014:year_ending_jun_2017)

house_long = house_price %>% gather(year, price, year_ending_jun_2014:year_ending_jun_2017) 

house_long = house_long %>% 
  arrange(local_authority_code, year)

house_long = house_long %>% mutate(price = ifelse(price == ":", NA, price))

house_long = house_long %>%
  filter(!is.na(price)) %>%
  group_by(local_authority_code) %>% 
  mutate(year_collect = gsub("year_ending_", "", year)) %>% 
  mutate(year = ifelse(year_collect %in% c("jun_2014", "sep_2014", "dec_2014", "mar_2015"), "1415",
                       ifelse(year_collect %in% c("jun_2015", "sep_2015", "dec_2015", "mar_2016"), "1516",  
                                                  ifelse(year_collect %in% c("jun_2016", "sep_2016", "dec_2016", "mar_2017"), "1617", "1718"))))

house_long %>% crosstab(year, year_collect)


house_long$price = as.numeric(house_long$price)

mean_prices = house_long %>% 
  group_by(local_authority_code, year) %>%
  summarise(mean_house_price_lsoa = mean(price))


test_beta = left_join(test, mean_prices, by = c("oslaua" = "local_authority_code", "year" = "year"))

write.csv(test_beta, "~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/t4_new.csv", row.names = FALSE)


