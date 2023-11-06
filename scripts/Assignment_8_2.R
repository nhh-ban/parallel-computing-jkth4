# Assignment 1:  
library(tweedie) 
library(ggplot2)
library(doParallel) #read doParallel function
library(dplyr)
library(tidyr)
library(stringr)


simTweedieTest <-  
  function(N){ 
    t.test( 
      rtweedie(N, mu=10000, phi=100, power=1.9), 
      mu=10000 
    )$p.value 
  } 


# Assignment 2:  
MTweedieTests <-  
  function(N,M,sig){ 
    sum(replicate(M,simTweedieTest(N)) < sig)/M
  } 


# Assignment 3:  
df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 

#use more cores
maxcores <- 8
Cores <- min(parallel::detectCores(), maxcores)

#Instantiate the cores:
cl <- makeCluster(Cores)

# Next we register the cluster..
registerDoParallel(cl)

#rewrite the part where you fill in the share_reject column, doing it parallel
res <-
  foreach(
    i = 1:nrow(df),
    .combine = 'rbind',
    .packages = c('magrittr', 'dplyr')
  ) %dopar%
  tibble(
    N = df$N[i],
    M = df$M[i],
    share_reject =
      MTweedieTests(
        N = df$N[i],
        M = df$M[i],
        sig = 0.05
      )
  )
