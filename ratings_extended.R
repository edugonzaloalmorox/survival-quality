# Create ratings extended # 



library(tidyverse)
library(readxl)
setwd("/Users/Personas/My Cloud/ch2/chapter2/quality/agglomeration/data/raw")
fileList <- list.files(path="/Users/Personas/My Cloud/ch2/chapter2/quality/agglomeration/data/raw", pattern=".xlsx")

sep15 = read_excel("01 September 2015 Latest Ratings.xlsx", sheet = "Locations")
mar16 = read_excel("01 March 2016 Latest ratings.xlsx", sheet = "Locations")
sep16 = read_excel("1 September 2016 Latest ratings.xlsx", sheet = "Locations")
mar17 = read_excel("1 March 2017 Latest ratings.xlsx", sheet = "Locations")

datos = c("sep15", "mar16", "sep16", "mar17")


# select important variables - apply in all but sep15



m16 <- mar16 %>% select(`Location ID`, `Location Name`, `Care Home?`,  `Location Street Address`,
                       `Location City`, `Location Post Code`, `Location Local Authority`, `Location Region`, 
                       `Key Question`, `Latest Rating` ,  `Publication Date`, `Provider ID`,`Provider Name`) %>% mutate(time = "mar16")

s16 <- sep16 %>% select(`Location ID`, `Location Name`, `Care Home?`,  `Location Street Address`,
                        `Location City`, `Location Post Code`, `Location Local Authority`, `Location Region`, 
                        `Key Question`, `Latest Rating` ,  `Publication Date`, `Provider ID`,`Provider Name`) %>% mutate(time = "sep16")

m17 <- mar17 %>% select(`Location ID`, `Location Name`, `Care Home?`,  `Location Street Address`,
                        `Location City`, `Location Post Code`, `Location Local Authority`, `Location Region`, 
                        `Key Question`, `Latest Rating` ,  `Publication Date`, `Provider ID`,`Provider Name`) %>% mutate(time = "mar17")

data_clean = rbind(m16, s16, m17)


-  