

#Extract occupation codes
get_cps_occ_codes <- function(){
  require(rvest)
  require(dplyr)
  cps.pointer <- read_html("https://cps.ipums.org/cps/codes/occ_2011_codes.shtml")
  
  occ_codes <- cbind(
    cps.pointer %>%
      html_nodes("dt") %>%
      html_text(),
    cps.pointer %>%
      html_nodes("dd") %>%
      html_text()
  ) %>%
    as_tibble(.name_repair = ~ c("codes", "description"))
  
  #############################
  #Defining health occupation subsets
  
  #Define health category
  occ_health_all <- occ_codes %>% 
    filter(codes %in% as.character(seq.int(3000,3655)))
  
  #Define specific health sector
  occ_health_subset <- occ_codes %>%
    filter(codes %in% c(
      "3210",
      "3060",
      "3256",
      "3110",
      "3220",
      "3255",
      "3258",
      "3260",
      "3300",
      "3320",
      "3400",
      "3420",
      "3500",
      "3510",
      "3540",
      "3600",
      "3645",
      "0350",
      "1650",
      "2025"
    ))
  
  
  
  
  save(occ_codes,occ_health_subset,occ_health_all,file = "cache/occ_codes.Rdata")
  
  return(NULL)
}


get_cps_metfips_codes <- function(){
  require(rvest)
  require(dplyr)
  cps.pointer <- read_html("https://cps.ipums.org/cps/codes/metfips_2014onward_codes.shtml")
  
  met_codes <- cbind(
    cps.pointer %>%
      html_nodes("dt") %>%
      html_text(),
    cps.pointer %>%
      html_nodes("dd") %>%
      html_text()
  ) %>%
    as_tibble(.name_repair = ~ c("codes", "description"))
  
  save(met_codes,file = "cache/met_codes.Rdata")
}
