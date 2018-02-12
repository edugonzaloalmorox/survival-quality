# Calculate number of inspections per oslaua and year

test <-  import("~/Dropbox/PhD/ch2/paper/quality/agglomeration/data/processed/t4_new.csv")

insp = test %>%
  filter(event == "publication_date") %>%
select(oslaua, event, initial_dates) %>%
  arrange(oslaua, initial_dates) %>%
  group_by(oslaua, initial_dates) %>% 
  tally() %>%
  arrange(oslaua, initial_dates) %>%
  group_by(oslaua) %>%
  mutate(cum_insp_oslaua = cumsum(n)) 


test = left_join(test, insp, by = c("oslaua", "initial_dates"))

test = test %>%
  group_by(location_id) %>% 
  arrange(location_id, initial_dates)

test = test %>% group_by(location_id) %>% fill(cum_insp_oslaua, .direction = "down")

test = test %>% rename(insp_oslaua_day = n)

test %>%select(location_id:number_spell, initial_dates, insp_oslaua_day , cum_insp_oslaua)

mod_bad_sp <- coxph(Surv(start_date, end_date,  failure_bad) ~  change_sp_pounds + strata(location_id),
                    data = t4_bad)

mod_bad_sp <- coxph(Surv(start_date, end_date,  failure_bad) ~  change_sp_pounds  +  cum_insp_oslaua +prop_old_65 + 
                       pro_js + prop_claimants + total_grant + district_metropolitan + 
                       district_shire + district_UA + factor(dimension) +
                       factor(prov_oversight) + factor(dementia) +
                       imd_average_score + strata(location_id),
                     data = t4_bad)

mod_good_sp <- coxph(Surv(start_date, end_date,  failure_good) ~  change_sp_pounds  +  cum_insp_oslaua +prop_old_65 + 
                      pro_js + prop_claimants + total_grant + district_metropolitan + 
                      district_shire + district_UA + factor(dimension) +
                      factor(prov_oversight) + factor(dementia) +
                      imd_average_score + strata(location_id),
                    data = t4_good)

summary(mod_good_sp)


mod_good_sp <-  coxph(Surv(start_date, end_date,  failure_good) ~  change_sp_pounds + strata(location_id),
                     data = t4_good)

mod_good_sp2 <- coxph(Surv(start_date, end_date,  failure_good) ~  sp2 + prop_old_65 + 
                        pro_js + prop_claimants + total_grant + district_metropolitan + 
                        district_shire + district_UA + factor(dimension) +
                        factor(prov_oversight) + factor(dementia) +
                        imd_average_score + strata(oslaua),
                      data = t4_good)

install.packages("survival")

