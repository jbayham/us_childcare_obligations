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

#Formatting the table for print
co_by_occ_cat %>%
  format_table_ci() %>%
  write_csv(.,path = "outputs/Childcare Obligations by Occupation Categories.csv")


###########################################################################
#By Heathcare Occupations
co_by_health_sub <- inner_join(full_long,
                               occ_health_subset,
                               by = c("occ"="codes")) %>%
  make_table(grp.var = "description")


save(co_by_health_sub,file = "cache/co_by_health_sub.Rdata")

#Formatting the table for print
co_by_health_sub %>%
  arrange(desc(co_sib)) %>%
  format_table_ci() %>%
  write_csv(.,path = "outputs/Childcare Obligations by Healthcare Occupations.csv")



##########################################################################
#By state (for healthcare sector only)
co_health_by_state <- 
  inner_join(full_long,
             occ_health_subset,
             by = c("occ" = "codes")) %>% 
  mutate(state=as.character(to_factor(statecensus))) %>%
  make_table(.,"state")

save(co_health_by_state,file = "cache/co_health_by_state.Rdata")

#Formatting the table for print
co_health_by_state %>%
  arrange(desc(co_sib)) %>%
  format_table_ci() %>%
  write_csv(.,path = "outputs/Healthcare Childcare Obligations by State.csv")



