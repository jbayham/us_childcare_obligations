# This script generates the data and produces Figure 2

#Generate grid and calculate the beta critical value below which school closures
#would be detriminental to health objectives
to.plot <- expand.grid(fc=seq(0.01,.35,.001),
                       gc=seq(0.01,.35,.001)) %>%
  as_tibble() %>%
  mutate(beta=(((fc/(1-fc))*(1/gc)-1)),
         beta=ifelse(beta>1,1,beta),   
         beta=ifelse(beta<=0,0,beta)
  )
library(latex2exp)
ggplot(data=to.plot,aes(x=gc,y=fc,fill=beta)) +
  geom_tile() +
  geom_point(x=.15,y=.15,size=3,color="red") +
  geom_text(aes(x=.19,y=.15),label="Best National \nEstimate",size=3,color="red") +
  #geom_text(aes(x=.24,y=.143),label="(kappa==0.17)",parse = T,size=3,color="red") +
  geom_text(aes(x=.25,y=.08,label="School Closures \nNet Increase in Mortality"),color="white") +
  geom_text(aes(x=.09,y=.28,label="School Closures \nNet Decrease in Mortality"),color="black") +
  scale_x_continuous(expand = c(0, 0),labels = percent_format(accuracy = 1)) +
  scale_y_continuous(expand = c(0, 0),labels = percent_format(accuracy = 1)) +
  scale_fill_viridis_c(name="",option = "D") +
  theme_bw() +
  theme(text = element_text(size = 15),
        panel.grid = element_blank(),
        panel.border = element_blank()) +
  labs(x="Percent of Healthcare Workforce Loss \nDue to Childcare Obligations",
       y="Percent Cases Avoided",
       title = "",
       caption = "")
  

ggsave("outputs/beta_simulation_vword.png")
ggsave("outputs/beta_simulation_submission.pdf",dpi = 600)


