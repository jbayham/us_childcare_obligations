#This script generates the three part map in figure 1
library(usmap)
st_map <- us_map(regions = "states")

plot_usmap("states",data = co_health_by_state, values = "co") +
  scale_fill_viridis_c(name="",option = "D",labels=percent_format(accuracy = 1)) +
  theme_void() 
  #theme(legend.direction = "horizontal", legend.position = c(.2,.1)) +
  # labs(title = y,
  #      caption = z)

load("cache/co_health_by_state.Rdata")
##################
#map of childcare obs by state
us_st <- us_states(resolution = "low") %>% 
  filter(!(stusps %in% c("AK","HI","PR"))) %>%
  st_transform(5070)

g.state <- inner_join(us_st %>% select(name,jurisdiction_type),
                      co_health_by_state,
                      by = c("name"="state")) %>% 
  filter(jurisdiction_type=="state")

plot.list <- 
  pmap(list(c("co", "co_sib", "co_sing"),
            c("A. Any Childcare Obligations ",
              "B. Likely Childcare Obligations",
              "C. Single-Parent Households"),
            c("","","Source: Current Population Survey 2018-2019")),
       function(x,y,z) {
         temp.dat <- g.state %>%
           select(value=one_of(x)) 
         
         min.max <- c(signif(min(temp.dat$value)-.005,3),signif(max(temp.dat$value)+0.005,3))
         
         b.fun <- function(q){
           temp.vec <- round(seq(min(q),max(q),(max(q)-min(q))/3),digits = 3)
           temp.vec[1] <- min.max[1]
           temp.vec[length(temp.vec)] <- min.max[2]
           return(temp.vec)
         } 
         
         p.map <- plot_usmap("states",data = co_health_by_state, values = x, color="gray30") +
           #ggplot(data=temp.dat,aes(fill=value)) +
           #geom_sf() +
           scale_fill_viridis_c(name="",option = "D",limits=min.max,breaks=b.fun,labels=percent_format(accuracy = 1)) +
           theme_void() +
           #theme(legend.direction = "horizontal", legend.position = c(.2,.1)) +
           labs(title = y,
                caption = z)
         
         return(p.map)
       })


plot.out <- plot_grid(plotlist=plot.list,ncol = 1,nrow = 3)

cowplot::ggsave("outputs/co_map_vword.png",plot.out,
                width =5.73,height = 9, units = "in" )

cowplot::ggsave("outputs/co_map_print.eps",plot.out,
                width =5.73,height = 9, units = "in" )
