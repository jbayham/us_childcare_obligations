#This script contains functions for analysis

to_svy <- function(df){
  #This function is a helper function to convert data frames or tibbles to an
  #object for srvyr to process
  #Args:
    #df: a dataframe or tibble coercable to a srvyr object with wtfinl
  
  #Dependencies
  require(srvyr)
  
  ###########################################################################
  #Specification comes from https://github.com/mnpopcenter/ipumsr/issues/50
  if("wtfinl" %in% colnames(df)){
    svy_out <- as_survey(df,weight = wtfinl,type = "JK1", scale = 4/60, rscales = rep(1, 160), mse = TRUE)
  }
  
}
#unit test
#df_svy <- to_svy(df)


############################################################### 
#The following functions are designed to produce certain tables so 
#they are not generalizable


make_table <- function(df,grp.var){
  #This function takes the survey object and calculates the share of workers
  #with childcare obligations, the implied population size, and the number of
  #records

    temp.grp <- df %>%
      to_svy() %>%
      group_by(!!as.name(grp.var))
    
    out <- bind_cols(
      summarise_at(temp.grp,vars(starts_with("co")),~survey_mean(.,na.rm = T,vartype="ci")),
      summarize(temp.grp,employment=survey_total(na.rm = T,vartype = "ci")) %>%
        transmute_at(vars(contains("employment")),~./26),
      summarize(as_tibble(temp.grp) %>% group_by(!!as.name(grp.var)), record_count = n()) %>%
        select(record_count)
    )
    
  return(out)
}
# grp.var=list("state","description")
# grp.var=list("state")
# groups(temp.grp)

make_table_health_state <- function(df){
  #This function takes the survey object and calculates the share of workers
  #with childcare obligations, the implied population size, and the number of
  #records
  
  temp.grp <- df %>%
    to_svy() %>%
    group_by(state,description)
  
  out <- bind_cols(
    summarise_at(temp.grp,vars(starts_with("co")),~survey_mean(.,na.rm = T,vartype="ci")),
    summarize(temp.grp,employment=survey_total(na.rm = T,vartype = "ci")) %>%
      transmute_at(vars(contains("employment")),~./26),
    summarize(as_tibble(temp.grp) %>% group_by(state,description), record_count = n()) %>%
      select(record_count)
  )
  
  return(out)
}





format_table_ci <- function(df,pct.vars = c("co","co_sib","co_sing")){
  #This function takes a dataframe of point estimates with 95% CI and formats a table
  
  
  #Format dat
  base.df <- df %>% 
    select(1,one_of(pct.vars)) %>%
    mutate_if(is.numeric,~number(.,.1,scale = 100))
  
  #Create df of CIs
  ci.df <- map_dfc(pct.vars,
                   function(x){
                     tibble(ci=str_c("[",
                                     number(pull(df[,str_c(x,"_low")]),.1,scale = 100),
                                     ", ", 
                                     number(pull(df[,str_c(x,"_upp")]),.1,scale = 100),
                                     "]"))
                   }) %>%
    rename_all(~str_c(pct.vars,"_ci"))
  
  out <- map(pct.vars,
           function(y){
             bind_cols(
               base.df %>% select(1,one_of(y)),
               ci.df %>% select(one_of(str_c(y,"_ci")))
             ) %>%
               pivot_longer(-1,values_to = y) %>%
               select(3)
             
           })
  
  #Doing the same for employment numbers
  out[[length(out)+1]] <- 
    bind_cols(
    df %>% 
      select(1,employment) %>%
      mutate_if(is.numeric,~comma(./1000,1)),
    tibble(ci=str_c("[",
                    comma(pull(df[,"employment_low"])/1000,1),
                    ", ", 
                    comma(pull(df[,"employment_upp"])/1000,1),
                    "]"))
  ) %>% 
    pivot_longer(-1,values_to = "employment") %>%
    select(3)
  
  
  final <- 
    bind_cols(
      tibble(variable=rep(pull(select(df,1)),each=2)),
      tibble(records=rep(pull(select(df,tail(names(df),1))),each=2)),
      reduce(out,bind_cols)
      ) %>%
    mutate_at(vars(variable,records),~ifelse(str_detect(co,","),"",.))
  
  return(final)
  
}

#format_table_ci(co_by_occ_cat,c("co","co_sib","co_sing","co_ptw"))



