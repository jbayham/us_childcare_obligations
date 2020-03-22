#Trends over time for appendix

temp.grp <- 
  inner_join(full_long,
             occ_health_subset,
             by = c("occ"="codes")) %>%
  mutate(date=as_date(str_c(year,"-",month,"-01")))%>%
  select(description,date,co:co_sing,wtfinl) %>%
  to_svy() %>%
  group_by(date,description)

health_time <- summarise_at(temp.grp,vars(starts_with("co")),~survey_mean(.,na.rm = T,vartype="ci"))

health_time %>%
  filter(description %in% occ_health_subset$description[occ_health_subset$codes %in% c("3600","3258","3060")]) %>%
  ggplot(aes(x=date,y=co_sib,color=description,fill=description)) +
  geom_smooth(show.legend = T) +
  scale_x_date(labels = date_format("%b-%Y")) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = c(.05,.93),
        legend.direction = "vertical",
        legend.title = element_blank()) +
  labs(x="", y="",
       title = "Childcare obligations without \nnon-working adult or sibling")

ggsave(filename = "outputs/fig_S1.png",width = 6.5, height = 4.5, units = "in")

