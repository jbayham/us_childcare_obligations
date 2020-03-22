#This script summarizes the data and produces tables and datasets for creating figures



#Focused on only the population in the labor force
full_long <- full_long %>%
  filter(labforce==2)



###########################################################################
#By Occupation Categories
co_by_occ_cat <- bind_rows(
  full_long %>%
    drop_na(occ_categories) %>%
    make_table(.,"occ_categories"),
  full_long %>% 
    mutate(occ_health=ifelse(is.na(occ_health),"nothealth",occ_health)) %>%
    make_table(.,
               "occ_health") %>%
    slice(1) %>%
    rename(occ_categories=occ_health)
)

#Caching the output
save(co_by_occ_cat,file = "cache/co_by_occ_cat.Rdata")


###########################################################################
#By Heathcare Occupations
co_by_health_sub <- inner_join(full_long,
                               occ_health_subset,
                               by = c("occ"="codes")) %>%
  make_table(grp.var = "description")


save(co_by_health_sub,file = "cache/co_by_health_sub.Rdata")


###########################################################################
#By state
co_by_state <- full_long %>%
  mutate(state=as.character(to_factor(statecensus))) %>%
  make_table(.,"state")

save(co_by_state,file = "cache/co_by_state.Rdata")


##########################################################################
#By state (for healthcare sector only)
co_health_by_state <- 
  inner_join(full_long,
             occ_health_subset,
             by = c("occ" = "codes")) %>% 
  mutate(state=as.character(to_factor(statecensus))) %>%
  make_table(.,"state")

save(co_health_by_state,file = "cache/co_health_by_state.Rdata")

###########################################################################
# By individual occupation
co_by_occ <- make_table(full_long,"occ") %>%
  inner_join(occ_codes,
             .,
             by = c("codes"="occ"))

save(co_by_occ,file = "cache/co_by_occ.Rdata")

###########################################################################
#By health occupations by state
co_by_health_sub_state <- 
  inner_join(full_long,
             occ_health_subset,
             by = c("occ" = "codes")) %>%
  mutate(state=as.character(to_factor(statecensus))) %>%
  make_table_health_state()

save(co_by_health_sub_state,file = "cache/co_by_health_sub_state.Rdata")


###########################################################################
#Fraction of Households with childcare obligations (by assumption) by metro area
co_by_metfips <- as.data.table(full_long) %>%
  .[,
    lapply(.SD, function(x)
      sum(x * wtfinl) / sum(wtfinl)),
    by = c("metfips"),
    .SDcols = str_subset(names(full_long), "^co")] %>%
  as_tibble() %>%
  inner_join(.,as.data.table(full_long) %>%
               .[,
                 .(persons=sum(wtfinl)/as.data.table(cps_data)[labforce==2,sum(wtfinl)],
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




