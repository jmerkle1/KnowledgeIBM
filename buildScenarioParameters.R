require(RSQLite)
require(jsonlite)
require(tidyverse)

# Connect to the database
box_fldr <- "E:/Box Sync/KnowledgeIBM_results"

# Create/Connect to SQLite DB for storage of model runs
dbConn <- dbConnect(SQLite(), paste0(box_fldr, "/KnowledgeIBM_ModelRuns.sqlite"))

# Use this code to build additional model scenarios and append to database

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

# Parameters to adjust for sensitivity analysis
sexRatio <- c(0.4, 0.5, 0.6)

boldDist <- list(uniform = c(1, 1),
                 skewLow = c(2, 5),
                 skewHigh = c(5, 2))

poisLambda <- c(1, 2, 3)

infotransfer <- c(0.005, 0.01, 0.1)

infoDeath <- c(0.05, 0.2, 0.4)

naiveLearn <- c(0.005, 0.01, 0.1)

vertTransmission <- c(TRUE, FALSE)

# Adjust by +/- 0.5
birthRateK <- list(baseline = c(0, 1.4, 1.9, 1.75),
                   low = c(0, 1, 1.4, 1.25),
                   high = c(0, 1.9, 2.4, 2.25))

# Adjust by +/- 5
birthRateR <- list(baseline = c(0, 14, 16, 12),
                   low = c(0, 9, 11, 7),
                   high = c(0, 19, 21, 17))

# Adjust by +/- 0.2
survivalRateK <- list(baseline = c(0.5,0.95,0.85,0),
                   low = c(0.3,0.75,0.65,0),
                   high = c(0.7,0.99,0.95,0))

# Adjust by +/- 0.07
survivalRateR <- list(baseline = c(0.14,0.18,0.22,0),
                   low = c(0.07,0.11,0.15,0),
                   high = c(0.21,0.25,0.29,0))


hist(rbeta(1000, 5, 1))
hist(rpois(1000, 3))
# Store the above parameters as well as others so we can track different model runs more easily
tmp<- data.frame(Scenario = "K selected: Baseline",
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

# Create blank
param<- data.frame()
# Populate with latest scenario
param<- rbind(param, tmp)