#Build dataset

#############################
#Extract zipped data extract from ipums
if(!file.exists("inputs/cps_00005.dat")){
  gunzip("inputs/cps_00005.dat.gz",remove=F)
}


#################################
#Read in data
data_in <- read_ipums_micro(ddi="inputs/cps_00005.xml",
                            data_file = "inputs/cps_00005.dat") %>%
  rename_all(str_to_lower)

#Subset only CPS monthly data 
cps_data <- data_in %>%
  filter((asecflag!=1| is.na(asecflag)),year>2017) %>%
  mutate(occ_categories=case_when(
    dplyr::between(occ,3000,3655) ~ "All Healthcare",
    dplyr::between(occ,1,1965) | dplyr::between(occ,4700,5940) ~ "Office",
    dplyr::between(occ,2200,2550) ~ "Education",
    dplyr::between(occ,3700,3955) ~ "Protective Services",
    dplyr::between(occ,6200,7630) ~ "Construction",
    dplyr::between(occ,7700,8965) ~ "Production"
  ),
  occ_categories = ifelse(is.na(occ_categories) & occ!=0,"Other",occ_categories),
  occ_health = ifelse(occ %in% occ_health_subset$codes,"Healthcare",NA)) %>%
  mutate_at(vars(occ,ind),~str_pad(.,4,"left",0)) %>%
  mutate_at(vars(occ,ind),~na_if(.,"0000")) %>%
  mutate(metfips=str_pad(metfips,5,"left","0"),
         earnweek=ifelse(earnweek==9999.99,NA,earnweek))

save(cps_data,file = "cache/cps_data.Rdata")

