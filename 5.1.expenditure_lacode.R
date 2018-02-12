
# select unique levels of local authorities 
# -----------------------------------------


# load data and select local authorities

      data_surv = import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/data_survival.csv")


    la_data = data_surv %>% select(la) %>% unique() %>% arrange(la) %>% filter(!is.na(la))
    sc_la = sc %>% select(local_authority) %>% unique() %>% arrange(local_authority)

# clean variables
    data_surv = data_surv %>% mutate(local = gsub(", City of", "", la),
                             local = gsub(", County of", "", local), 
                             local = str_trim(local, "both"))

    sc = sc %>% mutate(la = gsub("&", "and", local_authority),
                       la = gsub("UA", "", la),
                       la = gsub("CC", "", la), 
                       la = gsub("BC", "", la),
                       la = gsub("DC", "", la), 
                       la = gsub("MBC", "", la), 
                       la = str_trim(la, "both"))


        la_commons = intersect(unique(la_data$local), unique(sc_la$la)) %>% as.data.frame()
        
        la_different = setdiff(unique(la_data$local), unique(sc_la$la)) %>% as.data.frame()
        

# recode variables

library(stringr)



data_surv = data_surv %>% mutate(local2 = fct_recode(local,
            "Durham" = "County Durham",
            "Leicester City"    = "Leicester",
            "Derby City"       = "Derby",
            "Medway Towns" = "Medway", 
            "City of Nottingham" = "Nottingham",
            "St Helens" = "St. Helens",
            "Telford and the Wrekin" = "Telford and Wrekin"))
            
la_commons = intersect(unique(data_surv$local2), unique(sc$la)) %>% as.data.frame()

# select those authorities at county level 



