#School closure calculations

school.close.calc <- function(case.prev, base.mortality,chw){
  y <- case.prev/(1-case.prev)  #calculate y
  a.gc <- base.mortality/chw    #\alpha / G(c)
  beta <- y*a.gc
  mort.mult <- (beta*(a.gc)^-1)
  new.mort <-  base.mortality*(1+mort.mult)
  elasticity <- log((1-base.mortality)/(1-new.mort))/chw
  
  results <- c(beta, mort.mult, new.mort, elasticity)
  results <- as.data.frame(results)
  rownames(results) <- c("beta",
                         "percent.mortality.increase",
                         "new.min.mortality",
                         "survival.elasticity.avoided.HCworkers")

  
  return(results)
}

#national mean
navg <- school.close.calc(0.15, 0.02, 0.15)

#South Dakota
sd<-school.close.calc(0.15, 0.02, 0.21)

#Washington DC
dc<-school.close.calc(0.15, 0.02, 0.087)

#peak prev
school.close.calc(0.42, 0.02, 0.15)

l.rate <- seq(0.087, 0.21, by = 0.001)
el1 <- lapply(l.rate, school.close.calc, base.mortality = 0.02, case.prev = 0.15)

outs<- rep(l.rate, 0)
for(j in 1: length(l.rate)){
  outs[j] <- school.close.calc(0.15, 0.02, l.rate[j])[4,1]
}
comb <- as.data.frame(cbind(l.rate, outs))

if (!require("pacman")) install.packages("pacman")
p_load(ggplot2)

ggplot() +
  geom_line(data = comb, aes(x = l.rate, y = outs),
            color = 'blue') +
  #ylim(0,1.2*max(data1$effort))+
  labs( 
    x= "Avoided health care labor 
    loss with open schools",
    y = "Critical survival elasticity")  +
  theme(  #http://ggplot2.tidyverse.org/reference/theme.html
    axis.line = element_line(color = "black"), 
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA)
  )



