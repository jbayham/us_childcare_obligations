#This script summarizes the data and produces tables and datasets for creating figures



##################################################
#population wide metrics
total_wt <- as.data.table(cps_data)[labforce==2,sum(wtfinl)]
total_wth <- as.data.table(cps_data)[labforce==2,sum(hwtfinl)]

occ_shares <-
  as.data.table(cps_data) %>%
  .[labforce == 2,
    .(`Fraction of Workforce` = round(sum(wtfinl) /
                                  total_wt, 6)),
    by = c("occ", "statecensus")] %>%
  as_tibble() %>%
  inner_join(.,
             occ_codes,
             by = c("occ"="codes"))

write_csv(occ_shares,path = "outputs/occupation_shares.csv")



###########################################################################
#by select occupations
co_by_occ_cat <- bind_rows(
  as.data.table(full_long) %>%
    .[!is.na(occ_categories),
      lapply(.SD, function(x)
        sum(x * wtfinl) / sum(wtfinl)),
      by = c("occ_categories"),
      .SDcols = str_subset(names(hh_long), "^co")] %>%
    as_tibble() %>% 
    inner_join(.,
               as.data.table(full_long) %>%
                 .[,
                   .(persons=sum(wtfinl)/total_wt,
                     records=.N),
                   by = c("occ_categories")] %>%
                 as_tibble()),
  as.data.table(full_long)[!is.na(occ_health),
                           lapply(.SD, function(x) sum(x*wtfinl)/sum(wtfinl)),
                           by=c("occ_health"),
                           .SDcols=str_subset(names(hh_long),"^co")] %>%
    as_tibble() %>%
    inner_join(.,
               as.data.table(full_long) %>%
                 .[,
                   .(persons=sum(wtfinl)/total_wt,
                     records=.N),
                   by = c("occ_health")] %>%
                 as_tibble())) %>%
  mutate(occ_categories=coalesce(occ_categories,occ_health)) %>%
  select(-occ_health) 

write_csv(co_by_occ_cat,path = "outputs/co_by_occ_cat.csv")


###########################################################################
#Fraction of Households with childcare obligations (by assumption) by occupation
co_by_health_sub <- inner_join(full_long,
                               occ_health_subset,
                               by=c("occ"="codes")) %>%
  as.data.table(.) %>%
  .[,
    lapply(.SD, function(x)
      sum(x * wtfinl) / sum(wtfinl)),
    by = c("description"),
    .SDcols = str_subset(names(hh_long), "^co")] %>%
  as_tibble() %>%
  inner_join(occ_codes,.) %>%
  inner_join(.,
             inner_join(full_long,
                        occ_health_subset,
                        by=c("occ"="codes")) %>%
               as.data.table(.) %>%
               .[,
                 .(persons=sum(wtfinl)/total_wt,
                   records=.N),
                 by = c("description")] %>%
               as_tibble() %>%
               inner_join(occ_codes,.))



write_csv(co_by_health_sub,path = "outputs/co_by_health_sub.csv")

###########################################################################
#Fraction of Households with childcare obligations (by assumption) by state
co_by_state <- as.data.table(full_long) %>%
  .[,
    lapply(.SD, function(x)
      sum(x * wtfinl) / sum(wtfinl)),
    by = c("statecensus"),
    .SDcols = str_subset(names(hh_long), "^co")] %>%
  as_tibble() %>%
  inner_join(.,as.data.table(full_long) %>%
               .[,
                 .(persons=sum(wtfinl)/total_wt,
                   records=.N),
                 by = c("statecensus")] %>%
               as_tibble()) %>%
  mutate(statecensus=to_factor(statecensus))

write_csv(co_by_state,path = "outputs/co_by_state.csv")


##########################################################################
#CO burden for healthcare sector by state
co_health_by_state <- inner_join(full_long,
                              occ_health_subset,
                              by=c("occ"="codes")) %>%
  as.data.table(.) %>%
  .[,
    lapply(.SD, function(x)
      sum(x * wtfinl) / sum(wtfinl)),
    by = c("statecensus"),
    .SDcols = str_subset(names(hh_long), "^co")] %>%
  as_tibble() %>%
  mutate(statecensus=to_factor(statecensus)) %>%
  inner_join(.,
             inner_join(full_long,
                        occ_health_subset,
                        by=c("occ"="codes")) %>%
               as.data.table(.) %>%
               .[,
                 .(persons=sum(wtfinl)/total_wt,
                   records=.N),
                 by = c("statecensus")] %>%
               as_tibble() %>%
               mutate(statecensus=to_factor(statecensus)))


write_csv(co_health_by_state,path = "outputs/co_health_by_state.csv")

#Caching for figure 1
save(co_health_by_state,file = "cache/co_health_by_state.Rdata")



###########################################################################
#Fraction of Households with childcare obligations (by assumption) by individual occupation by state
co_by_health_sub_state <- inner_join(full_long,
                         occ_health_subset,
                         by=c("occ"="codes")) %>%
  as.data.table(.) %>%
  .[,
    lapply(.SD, function(x)
      sum(x * wtfinl) / sum(wtfinl)),
    by = c("description","statecensus"),
    .SDcols = str_subset(names(hh_long), "^co")] %>%
  as_tibble() %>%
  inner_join(occ_codes,.) %>%
  mutate(statecensus=to_factor(statecensus)) %>%
  inner_join(.,
             inner_join(full_long,
                        occ_health_subset,
                        by=c("occ"="codes")) %>%
               as.data.table(.) %>%
               .[,
                 .(persons=sum(wtfinl)/total_wt,
                   records=.N),
                 by = c("description","statecensus")] %>%
               as_tibble() %>%
               inner_join(occ_codes,.) %>%
               mutate(statecensus=to_factor(statecensus)))


write_csv(co_by_health_sub_state,path = "outputs/co_by_health_sub_state.csv")



###########################################################################
#Fraction of Households with childcare obligations (by assumption) by metro area
co_by_metfips <- as.data.table(full_long) %>%
  .[,
    lapply(.SD, function(x)
      sum(x * wtfinl) / sum(wtfinl)),
    by = c("metfips"),
    .SDcols = str_subset(names(hh_long), "^co")] %>%
  as_tibble() %>%
  inner_join(.,as.data.table(full_long) %>%
               .[,
                 .(persons=sum(wtfinl)/total_wt,
                   records=.N),
                 by = c("metfips")] %>%
               as_tibble()) %>%
  mutate(metfips=str_pad(metfips,5,"left","0")) %>%
  inner_join(met_codes,
             .,
             by=c("codes"="metfips"))

write_csv(co_by_metfips,path = "outputs/co_by_metfips.csv")


#Caching for figure 1
save(co_by_metfips,file = "cache/co_by_metfips.Rdata")


##########################################################################
#CO burden for healthcare sector by metfips
co_health_by_metfips <- inner_join(full_long,
                                 occ_health_subset,
                                 by=c("occ"="codes")) %>%
  as.data.table(.) %>%
  .[,
    lapply(.SD, function(x)
      sum(x * wtfinl) / sum(wtfinl)),
    by = c("metfips"),
    .SDcols = str_subset(names(hh_long), "^co")] %>%
  as_tibble() %>%
  inner_join(.,
             inner_join(full_long,
                        occ_health_subset,
                        by=c("occ"="codes")) %>%
               as.data.table(.) %>%
               .[,
                 .(persons=sum(wtfinl)/total_wt,
                   records=.N),
                 by = c("metfips")] %>%
               as_tibble()) %>%
  mutate(metfips=str_pad(metfips,5,"left","0")) %>%
  inner_join(met_codes,
             .,
             by=c("codes"="metfips"))
  
  


write_csv(co_health_by_metfips,path = "outputs/co_health_by_metfips.csv")

#Caching for figure 1 metfips version
save(co_health_by_metfips,file = "cache/co_health_by_metfips.Rdata")

################
#CO burden by metro status (urban,suburban, rural)
co_by_metro <- as.data.table(full_long) %>%
  .[!is.na(occ_categories),
    lapply(.SD, function(x)
      sum(x * wtfinl) / sum(wtfinl)),
    by = c("metro"),
    .SDcols = str_subset(names(hh_long), "^co")] %>%
  as_tibble() %>% 
  inner_join(.,
             as.data.table(full_long) %>%
               .[,
                 .(persons=sum(wtfinl)/total_wt,
                   records=.N),
                 by = c("metro")] %>%
               as_tibble()) %>%
  mutate(metro=to_factor(metro))

write_csv(co_by_metro,path = "outputs/co_by_metro.csv")



