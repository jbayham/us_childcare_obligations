
#Calculating indicators for school aged kids and potential caregivers in long
if(!file.exists("cache/cps_co.Rdata")){
  cps_recoded <- cps_data %>%
    mutate(saktotal=ifelse(dplyr::between(age,3,12),1,0),
           parents=as.numeric(relate %in% c(101,201,202,203,1113,1114,1116,1117)),
           nwa=ifelse(!(relate %in% c(301,303,1242,9200)) & !dplyr::between(empstat,1,19),1,0),
           sib=ifelse(dplyr::between(age,13,20) & !dplyr::between(empstat,1,19),1,0),
           ptw=ifelse(dplyr::between(wkstat,20,42) & age>18,1,0)
    ) %>%
    group_by(hrhhid,hrhhid2,mish)
  
  #Aggregating the indicators to the household level
  cps_co_prelims <- bind_cols(
    cps_recoded %>%
      summarise(saktotal=sum(saktotal),
                parents=sum(parents)) %>%
      ungroup(),
    cps_recoded %>%
      summarise_at(vars(nwa,sib,ptw),max) %>%
      ungroup() %>%
      select(-hrhhid,-hrhhid2,-mish)
  )
  
  #Creating indicator for childcare obligations at the household level
  hh_co <- cps_co_prelims %>%
    mutate(co=ifelse(saktotal>0,1,0),
           co_nwa=ifelse(co==1 & nwa==0,1,0),
           co_sib=ifelse(co_nwa==1 & sib==0,1,0),
           co_ptw=ifelse(co_sib==1 & ptw==0,1,0),
           parents=ifelse(parents>0 & co==1,parents,0),
           co_sing=ifelse(parents==1,1,0)
    ) %>%
    select(-one_of("nwa","sib","ptw"))
  
  
  
  
  ################
  
  #Joining back to original data (full long)
  full_long <- inner_join(cps_recoded %>%
                            select(hrhhid,hrhhid2,mish,year,month,wtfinl,statecensus,metfips,metro,
                                   pernum,relate,age,sex,race,educ,
                                   ind,occ,occ_categories,occ_health,empstat,wkstat,earnweek),
                          hh_co,
                          by=c("hrhhid","hrhhid2","mish"))
  
  
  
  #Joining back to household level data (long)
  hh_long <- inner_join(cps_recoded %>%
                          select(hrhhid,hrhhid2,mish,year,month,hwtfinl,statecensus,metfips,metro,hhincome,famsize) %>%
                          distinct(hrhhid,hrhhid2,mish,.keep_all=T),
                        hh_co,
                        by=c("hrhhid","hrhhid2","mish"))
  
  
  
  save(full_long,hh_long,file="cache/cps_co.Rdata")
}
