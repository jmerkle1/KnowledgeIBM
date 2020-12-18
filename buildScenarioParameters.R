require(RSQLite)
require(jsonlite)
require(tidyverse)

# Connect to the database
box_fldr <- "E:/Box Sync/KnowledgeIBM_results"

# Create/Connect to SQLite DB for storage of model runs
dbConn <- dbConnect(SQLite(), paste0(box_fldr, "/KnowledgeIBM_ModelTuning_20201215.sqlite"))

# Use this code to build additional model scenarios and append to database

# K selected example
dK <- data.frame(age=c(0,1,2,3),
                birthRate=c(0,1.4,1.9,1.75),
                survivalRate=c(0.7,0.95,0.9,0))
K <- odiag(dK$survivalRate[1:(nrow(dK)-1)],-1) # Px, survival rates (note, the survival rate in final age class is 0 automatically)
K[1,] <- dK$birthRate # Fertility
stbl_age <- eigen.analysis(K)$stable.age
dK$N0_proportion <- c(0,stbl_age[2:length(stbl_age)])
dK$N0_proportion <- stbl_age
# dK$N0_prob_knowing <- c(0.05, 0.05, 0.05, 0.05)

# R selected example
dR <- data.frame(age=c(0,1,2,3),
                birthRate=c(0,14,16,12),
                survivalRate=c(0.14,0.18,0.22,0))
K <- odiag(dR$survivalRate[1:(nrow(dR)-1)],-1) # Px, survival rates (note, the survival rate in final age class is 0 automatically)
K[1,] <- dR$birthRate # Fertility
stbl_age <- eigen.analysis(K)$stable.age
dR$N0_proportion <- c(0,stbl_age[2:length(stbl_age)])
dR$N0_proportion <- stbl_age
# dR$N0_prob_knowing <- c(0.05, 0.05, 0.05, 0.05)


# Parameters to adjust for sensitivity analysis
#sexRatio <- c(0.4, 0.5, 0.6)

d <- list(Kselected = dK,
          Rselected = dR)

boldDist <- list(skewLow = c(1, 5),
                 skewMedLow = c(2, 5),
                 uniform = c(1, 1),
                 skewMedHigh = c(5, 2),
                 skewHigh = c(5, 1))

poisLambda <- list(Low = 0,
                   MedLow = 1,
                   Med = 2,
                   MedHigh = 3,
                   High = 4)

infotransfer <- list(Low = 0.005, 
                     MedLow = 0.01,
                     Med = 0.05, 
                     MedHigh = 0.1, 
                     High = 0.5)

# infoDeath <- c(0.05, 0.2, 0.4)
infoDeath <- list(Low = 0.01,
                  MedLow = 0.1,
                  Med = 0.2,
                  MedHigh = 0.4,
                  High = 0.6)

naiveLearn <- list(Low = 0, 
                   MedLow = 0.005,
                   Med = 0.01,
                   MedHigh = 0.1,
                   High = 0.5)

vertTransmission <- list(True = TRUE,
                         False = FALSE)

# # Adjust by +/- 0.5
# birthRateK <- list(baseline = c(0, 1.4, 1.9, 1.75),
#                    low = c(0, 1, 1.4, 1.25),
#                    high = c(0, 1.9, 2.4, 2.25))
# 
# # Adjust by +/- 5
# birthRateR <- list(baseline = c(0, 14, 16, 12),
#                    low = c(0, 9, 11, 7),
#                    high = c(0, 19, 21, 17))
# 
# # Adjust by +/- 0.2
# survivalRateK <- list(baseline = c(0.5,0.95,0.85,0),
#                    low = c(0.3,0.75,0.65,0),
#                    high = c(0.7,0.99,0.95,0))
# 
# # Adjust by +/- 0.07
# survivalRateR <- list(baseline = c(0.14,0.18,0.22,0),
#                    low = c(0.07,0.11,0.15,0),
#                    high = c(0.21,0.25,0.29,0))


hist(rbeta(1000, 5, 1))
hist(rpois(1000, 3))

# Create a loop the generate the Scenarios given above potential values
outParam <- data.frame()
# Start with vital rates
for(vital in 1:length(d)){
  # now boldness
  for(bold in 1:length(boldDist)){
    # now poisson lambda
    for(lamb in 1:length(poisLambda)){
      # now info transfer
      for(tran in 1:length(infotransfer)){
        # now info death
        for(death in 1:length(infoDeath)){
          # now naive learning
          for(naive in 1:length(naiveLearn)){
            # now vertical transmission
            for(vert in 1:length(vertTransmission)){
              # populate the row of data
              tmp<- data.frame(Scenario = as.character(toJSON(data.frame(Parameter = c("Vital", "Boldness", "PoisLambda", "InfoTransfer", "InfoDeath", "NaiveLearn", "VertTrans"),
                                                                         Level = c(names(d)[vital], names(boldDist)[bold], names(poisLambda)[lamb], names(infotransfer)[tran], names(infoDeath)[death], names(naiveLearn)[naive], names(vertTransmission)[vert])))),
                               vitalRates = as.character(toJSON(d[[vital]])),
                               N0 = 50,
                               carryCapcity = 500,
                               years = 50,
                               sexRatio = 0.5,
                               boldDist = as.character(toJSON(data.frame(shape1 = boldDist[[bold]][1], shape2 = boldDist[[bold]][2]))),
                               poisLambda = poisLambda[[lamb]],
                               infoTransfer = infotransfer[[tran]],
                               infoDeath = infoDeath[[death]],
                               naiveLearn = naiveLearn[[naive]],
                               vertTransmission = vertTransmission[[vert]]) 
              # add the new row
              outParam<- rbind(outParam, tmp)
            } # close vert
          } # close naive
        } # close death
      } # close tran
    } # close lamb
  } # close bold
} # close vital

# Save to the database
dbWriteTable(dbConn, name = "tblModelScenarios", value = outParam)

