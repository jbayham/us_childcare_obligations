#School closure calculations

school.close.calc <- function(case.prev, base.mortality,chw){
  #This function calculates the elasticity reported in the manuscript
  #Args:
    #case.prev: the cases prevented 15% [13%, 17%] (Cauchemez et al. 2008)
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


school.close.calc.boot <- function(n,cp,m,chw,stat){
  require(purrr)
  set.seed(20) #so reproducible
  
  cp.d <- rnorm(n, mean = cp[1], sd = cp[2])
  m.d <- rnorm(n, mean = m[1], sd = m[2])
  chw.d <- rnorm(n, mean = chw[1], sd = chw[2])
  out <- 
    map_dbl(1:n,
            function(x){
              
              school.close.calc(case.prev = cp.d[x], 
                                base.mortality = m.d[x],
                                chw = chw.d[x])[stat,1]
            })
  return(out)
}




#national mean
n=1000
cp=c(.15,.0102)
m=c(.02,.00255)
chw=c(.15,.00102)

navg <- school.close.calc.boot(n,cp,m,chw,4)
hist(navg)
c(mean(navg),quantile(navg,p=c(.05,.95)))*100

#South Dakota
n=1000
cp=c(.15,.0102)
m=c(.02,.00255)
chw=c(.21,.0107)

sd <- school.close.calc.boot(n,cp,m,chw,4)
hist(sd)
c(mean(sd),quantile(sd,p=c(.05,.95)))*100

#Washington DC
n=1000
cp=c(.15,.0102)
m=c(.02,.00255)
chw=c(.088,.00867)

dc <- school.close.calc.boot(n,cp,m,chw,4)
hist(dc)
c(mean(dc),quantile(dc,p=c(.05,.95)))*100


#peak prev
school.close.calc(0.42, 0.02, 0.15)
n=10000
cp=c(.42,.0153)
m=c(.02,.00255)
chw=c(.15,.00102)

peak <- school.close.calc.boot(n,cp,m,chw,3)
hist(peak)
c(mean(peak),quantile(peak,p=c(.05,.95)))*100

