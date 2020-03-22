#This script runs some supplementary analyses

#Focused on only the population in the labor force
full_long <- full_long %>%
  filter(labforce==2,empstat<20)




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

co_by_occ_cat %>%
  format_table_ci() %>%
  write_csv(.,path = "outputs/Childcare Obligations by Occupation Categories - employed only.csv")

###########################################################################
#By state (for healthcare sector only)
co_health_by_state <- 
  inner_join(full_long,
             occ_health_subset,
             by = c("occ" = "codes")) %>% 
  mutate(state=as.character(to_factor(statecensus))) %>%
  make_table(.,"state")

co_health_by_state %>%
  format_table_ci() %>%
  write_csv(.,path = "outputs/Healthcare Childcare Obligations by State - employed only.csv")



