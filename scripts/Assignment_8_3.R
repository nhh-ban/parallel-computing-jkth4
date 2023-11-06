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

#use more cores
maxcores <- 8
Cores <- min(parallel::detectCores(), maxcores)

#Instantiate the cores:
cl <- makeCluster(Cores)

# Next we register the cluster..
registerDoParallel(cl)


#rewrite the replicate function, using parallel computing
#I tried this, but unfortunately it didn't work...I don't know how to solve this
MTweedieTests <-  
  function(N,M,sig){ 
    res <-
      foreach(
        i = 1:M,
        .combine = 'rbind',
        .packages = c('magrittr', 'dplyr')
      ) %dopar% {
      simTweedieTest(N) < sig
      }
    sum(res/M)
  }


# Assignment 3:  
df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 


for(i in 1:nrow(df)){ 
  df$share_reject[i] <-  
    MTweedieTests( 
      N=df$N[i], 
      M=df$M[i], 
      sig=.05) 
} 
