# This code runs the information transfer IBM (info.transfer.IBM.R) and the plots the results
# Written by Zach Bell and Jerod Merkle
# June 2019

library(parallel)
library(demogR)
require(here)
require(tidyverse)
require(RSQLite)
require(foreach)
require(doParallel)
require(jsonlite)

# # Jerod's folder paths
# box_fldr <- "C:/Users/jmerkle_local/Box Sync/KnowledgeIBM_results"
# github_fldr <- here()

# Shannon's folder paths
box_fldr <- "E:/Box Sync/KnowledgeIBM_results"
# github_fldr <- ".../GitHub/KnowledgeIBM"
github_fldr <- ""
# Create/Connect to SQLite DB for storage of model runs
# dbConn <- dbConnect(SQLite(), paste0(box_fldr, "/KnowledgeIBM_ModelRuns.sqlite"))
dbConn <- dbConnect(SQLite(), paste0(box_fldr, "/KnowledgeIBM_ModelTuning_20201215.sqlite"))
dbConnSup <- dbConnect(SQLite(), paste0(box_fldr, "/KnowledgeIBM_ModelTuningSupplemental_20201215.sqlite"))


#source the functions
#source(paste0(github_fldr, "/info.transfer.IBM.R"))
source("info.transfer.IBM.R")

#### prepare test data ####


#### Model Run ####
paramDF<- dbSendQuery(dbConn, "SELECT * FROM tblModelScenarios") %>% 
  dbFetch() %>% 
  mutate(vertTransmission = case_when(vertTransmission == 1 ~ TRUE,
                                      TRUE ~ FALSE))

# Set up the multithread
cl<- makeCluster(24)
registerDoParallel(cl)

# Initialize the number of iterations of the same model
boot<- 50

# Loop through the data.frame of model parameters for each scenario
# Failed = c(12)
for(i in 1:nrow(paramDF)){
  df<- paramDF[i, ]
  # Now parallelize the individual model runs
  outDF<- foreach(j = 1:boot, .packages = c("tidyverse", "Matrix", "jsonlite")) %dopar% {
    # Run the function (test)
    result <- info.transfer.IBM(d = fromJSON(df$vitalRates[1]),N0 = df$N0[1], K = df$carryCapcity[1], yrs = df$years[1], sex.ratio = df$sexRatio[1], bold.distr.beta = as.numeric(fromJSON(df$boldDist[1])), si = df$poisLambda[1], infotransfer = df$infoTransfer[1], h = df$infoDeath[1], nl = df$naiveLearn[1], vertTransmission = df$vertTransmission[1], set_seed = FALSE)
    
    # write the data.frame out to the database
    outStats<- result$PopStats %>% 
      mutate(Scenario = df$Scenario[1],
             ModelRun = j) %>% 
      select(Scenario, ModelRun, `time.stamp`:`med.age`)
    
    # converting to dataframe for each matrix for Interactions
    dfInt <- lapply(1:length(result$Interactions), function(ii)
      cbind(as.data.frame(as.matrix(result$Interactions[[ii]]))))
    
    intDF<- data.frame()
    for(k in 1:length(dfInt)){
      intDF<- rbind(intDF, data.frame(Scenario = df$Scenario[1],
                                      ModelRun = j,
                                      yr = (k - 1),
                                      Interactions = as.character(toJSON(dfInt[[k]]))))
    }
    
    
    # converting to dataframe for each matrix for Individuals
    dfInd <- lapply(1:length(result$Individuals), function(i)
      cbind(as.data.frame(result$Individuals[[i]])))
    
    dfInd<- do.call(rbind, dfInd) %>% 
      mutate(Scenario = df$Scenario[1],
             ModelRun = j,
             ID = 1:n()) %>% 
      select(Scenario, ModelRun, ID, alive:birthYr)
    
    out<- list(Stats = outStats, Interactions = intDF, Individuals = dfInd)
    
    # dbWriteTable(dbConn, name = "tblModelOutput", value = outDF, append = TRUE)
  }
  
  # Bind the data.frames together
  wStat<- data.frame()
  for(k in 1:length(outDF)){
    wStat<- rbind(wStat, outDF[[k]]$Stats)
  }
  wInt<- data.frame()
  for(k in 1:length(outDF)){
    wInt<- rbind(wInt, outDF[[k]]$Interactions)
  }
  wInd<- data.frame()
  for(k in 1:length(outDF)){
    wInd<- rbind(wInd, outDF[[k]]$Individuals)
  }
  # Write the tables to the database
  dbWriteTable(dbConn, name = "tblModelStats", value = wStat, append = TRUE)
  # dbWriteTable(dbConnSup, name = "tblInteractions", value = wInt, append = TRUE)
  # dbWriteTable(dbConnSup, name = "tblIndividuals", value = wInd, append = TRUE)
  
  print(paste0("Finished with ", i, " of ", nrow(paramDF), ", Scenario: ", df$Scenario[1], " at ", Sys.time()))
}
# Stop multithread
stopCluster(cl)
# Close the database connection
dbDisconnect(dbConn)




# Run the function (test)
result <- info.transfer.IBM(
  d = data.frame(paramDF$vitalRates[1]),   # these are the starting params of the population including birth and death rates. must have 5 columns: "age","birthRate","survivalRate","N0_proportion","N0_prob_knowing"
  N0 = paramDF$N0[1], # starting number of individuals
  K = paramDF$carryCapcity[1], # carrying capacity
  yrs = paramDF$years[1], # how many years should the simulation run for?
  sex.ratio = paramDF$sexRatio[1], #what is the sex ratio of of the population/births?
  bold.distr.beta = as.numeric(data.frame(paramDF$boldDist[1])), # starting probability distribution of being bold, beta distribution (vector of 2 values: shape1 and shape2)
  si = paramDF$poisLambda[1], # lambda of poison distribution, representing maximum number of interactions between two individuals if both have a boldness of 1. if both animals have boldness of 0, then there will be no interactions
  infotransfer = paramDF$infoTransfer[1], # given an interaction, what is the probability that information is transferred (min=0, max=1)
  h = paramDF$infoDeath[1], #increase in probability of death for uninformed, as a proportion of death rate.
  nl = paramDF$naiveLearn[1], # naive learning probability of the oldest animals (i.e., the ones that have the highest naive learning)
  vertTransmission = paramDF$vertTransmission[1], # When giving birth, should your information status be given to your offspring? TRUE/FALSE 
  result.folder = paste0(box_fldr,"/practice"), #an empty folder where results will be saved.
  set_seed = FALSE, # want to make results reproducible? Then set as TRUE
  save_at_each_iter = FALSE #should all results be written to file at each time step?
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