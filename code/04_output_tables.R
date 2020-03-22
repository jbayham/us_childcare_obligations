#This script writes out the data into tables

###########################################################################
#By Occupation Categories
load("cache/co_by_occ_cat.Rdata")

#Formatting the table for print
co_by_occ_cat %>%
  format_table_ci() %>%
  write_csv(.,path = "outputs/Childcare Obligations by Occupation Categories.csv")

###########################################################################
#By Heathcare Occupations
load("cache/co_by_health_sub.Rdata")

co_by_health_sub %>%
  arrange(desc(co_sib)) %>%
  format_table_ci() %>%
  write_csv(.,path = "outputs/Childcare Obligations by Healthcare Occupations.csv")

###########################################################################
#By state
load("cache/co_by_state.Rdata")

co_by_state %>%
  format_table_ci() %>%
  write_csv(.,path = "outputs/Childcare Obligations by State.csv")

##########################################################################
#By state (for healthcare sector only)
load("cache/co_health_by_state.Rdata")

co_health_by_state %>%
  arrange(desc(co_sib)) %>%
  format_table_ci() %>%
  write_csv(.,path = "outputs/Healthcare Childcare Obligations by State.csv")


###########################################################################
# By individual occupation
load("cache/co_by_occ.Rdata")

co_by_occ %>%
  format_table_ci() %>%
  write_csv(.,path = "outputs/Childcare Obligations by Occupation.csv")

###########################################################################
#By health occupations by state
load("cache/co_by_health_sub_state.Rdata")

co_by_health_sub_state %>%
  format_table_ci() %>%
write_csv(.,path = "outputs/co_by_health_sub_state.csv")
