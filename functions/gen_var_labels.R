

#Function to write csv with code labels

gen_var_labels <- function(var.list){
  require(dplyr)
  require(purrr)
  require(readr)
  require(labelled)
  
  ref.list <- cps_data %>% 
    select(one_of(var.list)) %>% 
    val_labels()
  
  #Write out a csv with code labels
  map2_dfr(.x=ref.list,.y=names(ref.list),
           ~enframe(.x) %>% 
             mutate(variable=.y)) %>%
    write_csv(.,path = "cache/variable_reference.csv")
  
}

#unit test
#gen_var_labels(c("relate","marst","race","educ","empstat","wkstat"))
