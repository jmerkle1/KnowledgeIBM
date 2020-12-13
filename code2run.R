# This code runs the information transfer IBM (info.transfer.IBM.R) and the plots the results
# Written by Zach Bell and Jerod Merkle
# June 2019

library(parallel)
library(demogR)
require(here)
require(tidyverse)


# Jerod's folder paths
box_fldr <- "C:/Users/jmerkle_local/Box Sync/KnowledgeIBM_results"
github_fldr <- here()

# Shannon's folder paths
box_fldr <- "E:/Box Sync/KnowledgeIBM_results"
# github_fldr <- ".../GitHub/KnowledgeIBM"


#source the functions
source(paste0(github_fldr, "/info.transfer.IBM.R"))

#### prepare test data ####

# K selected example
d <- data.frame(age=c(0,1,2,3),
                birthRate=c(0,1.4,1.9,1.75),
                survivalRate=c(0.5,0.95,0.85,0))

# R selected example
d <- data.frame(age=c(0,1,2,3),
                birthRate=c(0,14,16,12),
                survivalRate=c(0.14,0.18,0.22,0))


# calculate stable age distribution based on d and N0
K <- odiag(d$survivalRate[1:(nrow(d)-1)],-1) # Px, survival rates (note, the survival rate in final age class is 0 automatically)
K[1,] <- d$birthRate # Fertility
stbl_age <- eigen.analysis(K)$stable.age
d$N0_proportion <- c(0,stbl_age[2:length(stbl_age)])
d$N0_proportion <- stbl_age
d$N0_prob_knowing <- c(0,.05,.1,.15)   # add the probabilities of having information at the beginning of the simulation
d
eigen.analysis(K)$lambda1
rm(K, stbl_age)

# Store the above parameters as well as others so we can track different model runs more easily
paramDF<- data.frame(Scenario = "test K selected",
                     vitalRates = nest(d),
                     N0 = 50,
                     carryCapcity = 500,
                     years = 50,
                     sexRatio = 0.5,
                     boldDist = nest(data.frame(shape1 = 2, shape2 = 5)),
                     poisLambda = 2,
                     infoTransfer = 0.01,
                     infoDeath = 0.2,
                     naiveLearn = 0.01,
                     vertTransmission = TRUE) %>% 
  rename(vitalRates = data, 
         boldDist = data.1)

as.numeric(data.frame(paramDF$boldDist[1]))
#### Model Run ####
# Run the function (test)
result <- info.transfer.IBM(
  d = data.frame(paramDF$vitalRates[1]),   # these are the starting params of the population including birth and death rates. must have 5 columns: "age","birthRate","survivalRate","N0_proportion","N0_prob_knowing"
  N0 = paramDF$N0[1], # starting number of individuals
  K = paramDF$carryCapcity[1], # carrying capacity
  t = paramDF$years[1], # how many years should the simulation run for?
  sex.ratio = paramDF$sexRatio[1], #what is the sex ratio of of the population/births?
  bold.distr.beta = as.numeric(data.frame(paramDF$boldDist[1])), # starting probability distribution of being bold, beta distribution (vector of 2 values: shape1 and shape2)
  si = paramDF$poisLambda[1], # lambda of poison distribution, representing maximum number of interactions between two individuals if both have a boldness of 1. if both animals have boldness of 0, then there will be no interactions
  infotransfer = paramDF$infoTransfer[1], # given an interaction, what is the probability that information is transferred (min=0, max=1)
  h = paramDF$infoDeath[1], #increase in probability of death for uninformed, as a proportion of death rate.
  nl = paramDF$naiveLearn[1], # naive learning probability of the oldest animals (i.e., the ones that have the highest naive learning)
  vertTransmission = paramDF$vertTransmission[1], # When giving birth, should your information status be given to your offspring? TRUE/FALSE 
  result.folder = paste0(box_fldr,"/practice"), #an empty folder where results will be saved.
  set_seed = FALSE, # want to make results reproducible? Then set as TRUE
  save_at_each_iter = TRUE #should all results be written to file at each time step?
)

# Some quick plotting code
results <- read.csv(paste0(box_fldr,"/practice/population_stats.csv"))
par(mfrow=c(3,2), mai=c(.3,.3,.03,.03), mgp=c(1,.1,0),
    tck=-0.02, cex.axis=.8)
plot(results$t, results$pop.size, type="l",ylab="N", xlab="Time step", lwd=3,ylim=c(0,max(results$pop.size)))
plot(results$t, results$births, type="l",ylab="Births", xlab="Time step", lwd=3,ylim=c(0,max(c(results$births,results$deaths),na.rm=TRUE)))
plot(results$t, results$deaths, type="l",ylab="Deaths", xlab="Time step", lwd=3,ylim=c(0,max(c(results$births,results$deaths),na.rm=TRUE)))
plot(results$t, results$med.age, type="l",ylab="Median age", xlab="Time step", lwd=3, ylim=c(0,max(results$med.age)))
plot(results$t, results$frac.informed, type="l", 
     ylim=c(0,1.2),ylab="% Informed", xlab="Time step", lwd=3)


       
# Write code to use parallel processing to through a number of different options for the params. 
# Including replication in each because there is stochasticity