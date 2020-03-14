#This script generates the three part map in figure 1

load("cache/co_health_by_metfips.Rdata")
##################
#map of childcare obs by state
us_st <- us_states(resolution = "low") %>% 
  filter(!(stusps %in% c("AK","HI","PR"))) %>%
  st_transform(5070)

g.metfips <- tigris::core_based_statistical_areas(cb=T) %>%
  st_as_sf() %>%
  rename_all(str_to_lower)  %>%
  st_transform(5070)

g.metfips <- g.metfips[us_st,]

g.data <- inner_join(g.metfips %>% select(geoid,name),
                      co_health_by_metfips,
                      by = c("geoid"="codes")) %>% 
  filter(records>99)

plot.list <- 
  pmap(list(c("co", "co_sib", "co_sing"),
            c("A. Any Childcare Obligations ",
              "B. Likely Childcare Obligations",
              "C. Single-Parent Households"),
            c("","","Source: Current Population Survey 2018-2019")),
       function(x,y,z) {
         temp.dat <- g.data %>%
           select(value=one_of(x)) 
         
         min.max <- c(signif(min(temp.dat$value)-.005,3),signif(max(temp.dat$value)+0.005,3))
         
         b.fun <- function(q){
           temp.vec <- round(seq(min(q),max(q),(max(q)-min(q))/3),digits = 3)
           temp.vec[1] <- min.max[1]
           temp.vec[length(temp.vec)] <- min.max[2]
           return(temp.vec)
         } 
         
         p.map <- ggplot() +
           geom_sf(data=us_st,fill="gray",color=NA) +
           geom_sf(data=temp.dat,aes(fill=value),color=NA) +
           geom_sf(data=us_st,fill=NA,color="darkgray") +
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

cowplot::ggsave("outputs/co_map_print.pdf",plot.out,
                width =5.73,height = 9, units = "in" )
