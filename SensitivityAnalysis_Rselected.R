library(parallel)
library(demogR)
require(here)
require(tidyverse)
require(RSQLite)
require(foreach)
require(doParallel)
require(jsonlite)

### This code runs the Sensitivity Analysis for R-Selected animals


#source the functions
source(paste0("info.transfer.IBM.R"))

# Global variables
box_fldr <- "E:/Box Sync/KnowledgeIBM_results"

# Create/Connect to SQLite DB for storage of model runs
dbConn <- dbConnect(SQLite(), paste0(box_fldr, "/KnowledgeIBM_R_ModelSensitivity_20230320.sqlite"))

# Set up the multithread to specify how many workers we want to use
cl<- makeCluster(20)
registerDoParallel(cl)

# K-selected:  baselne parameters
# bold.distr.beta: uniform mean (1,1)
# h: medhigh mean (0.4)
# infotransfer: med mean (0.05)
# nl: med mean (0.01)
# si: med mean (2)
# vertTransmission: True always bc these are spp with long periods of maternal care/investment

dR <- data.frame(age=c(0,1,2,3),
                 birthRate=c(0,9.3,11.8,5.6),
                 survivalRate=c(0.3,0.679,0.594,0))# parameter df, VitalRates only
K <- odiag(dR$survivalRate[1:(nrow(dR)-1)],-1) # Px, survival rates (note, the survival rate in final age class is 0 automatically)
K[1,] <- dR$birthRate # Fertility
stbl_age <- eigen.analysis(K)$stable.age
dR$N0_proportion <- c(0,stbl_age[2:length(stbl_age)])
dR$N0_proportion <- stbl_age
dR$N0_prob_knowing <- c(0.05, 0.05, 0.05, 0.05)

K <- 500 # carrying capacity
yrs <- 50 
sex.ratio<- 0.5 # sex ratio is static
vertTransmission = FALSE

# number of iterations of each scenario
boot<- 100 

# variable parameters
N0<- c(base = 25,
       low = 10,
       high = 50)

# Boldness: uniform mean
bold<- list(base = c(1, 1),
            low = c(2, 5),
            high = c(5, 2))

# infodeath: medhigh mean
h<- c(base = 0.4,
      low = 0.2,
      high = 0.6)

# naivelearn: med mean
nl<- c(base = 0.1,
       low = 0.05,
       high = 0.2)

# poislambda: med mean
si<- c(base = 2,
       low = 1,
       high = 3)

# infotransfer: med mean
infotransfer<- c(base = 0.05,
                 low = 0.01,
                 high = 0.1)




for(i in 1:length(N0)){
  for(j in 1:length(bold)){
    for(k in 1:length(h)){
      for(m in 1:length(nl)){
        for(n in 1:length(si)){
          for(p in 1:length(infotransfer)){
            
            # set the scenario
            scen<- paste0("R;N0:", names(N0[i]), ";Bold:", names(bold[j]), ";InfoDeath:", names(h[k]), ";NaiveLearn:", names(nl[m]), ";PoisLambda:", names(si[n]), ";InfoTransfer:", names(infotransfer[p]))
            
            outDF<- foreach(q = 1:boot, .packages = c("tidyverse", "jsonlite")) %dopar% {
              # Run the function (test)
              result <- info.transfer.IBM(d = dR,N0 = N0[i], K = K, yrs = yrs, sex.ratio = sex.ratio, bold.distr.beta = bold[[j]], si = si[n], infotransfer = infotransfer[p], h = h[k], nl = nl[m], vertTransmission = vertTransmission, set_seed = FALSE)
              
              # write the data.frame out to the database
              outStats<- result$PopStats %>% 
                mutate(Scenario = scen,
                       ModelRun = q) %>% 
                select(Scenario, ModelRun, `time.stamp`:`med.age`)
              
              # converting to dataframe for each matrix for Interactions
              dfInt <- lapply(1:length(result$Interactions), function(ii)
                as.data.frame(as.matrix(result$Interactions[[ii]])))
              
              intDF<- data.frame()
              for(r in 1:length(dfInt)){
                intDF<- rbind(intDF, data.frame(Scenario = scen,
                                                StartPop = N0[i],
                                                Boldness = paste0(bold[j]),
                                                InfoDeath = h[k],
                                                NaiveLearning = nl[m],
                                                PoisLambda = si[n],
                                                InfoTransfer = infotransfer[p],
                                                ModelRun = q,
                                                SimYear = (r - 1),
                                                Interactions = as.character(toJSON(dfInt[[r]]))))
              }
              
              
              # converting to dataframe for each matrix for Individuals
              dfInd <- lapply(1:length(result$Individuals), function(ii)
                cbind(as.data.frame(result$Individuals[[ii]])))
              
              dfInd<- do.call(rbind, dfInd) %>% 
                mutate(Scenario = scen,
                       ModelRun = q,
                       ID = 1:n()) %>% 
                dplyr::select(Scenario, ModelRun, ID, alive:birthYr)
              
              out<- list(Stats = outStats, Interactions = intDF, Individuals = dfInd)
              
              # dbWriteTable(dbConn, name = "tblModelOutput", value = outDF, append = TRUE)
            }
            
            # Bind the data.frames together
            wStat<- data.frame()
            wInd<- data.frame()
            wInt<- data.frame()
            for(r in 1:length(outDF)){
              wStat<- rbind(wStat, outDF[[r]]$Stats)
              wInd<- rbind(wInd, outDF[[r]]$Individuals)
              wInt<- rbind(wInt, outDF[[r]]$Interactions)
            }
            
            # Write the tables to the database
            dbWriteTable(dbConn, name = "tblModelStat_Sensitivity_Rselected", value = wStat, append = TRUE)
            dbWriteTable(dbConn, name = "tblInteractions_Sensitivity_Rselected", value = wInt, append = TRUE)
            dbWriteTable(dbConn, name = "tblIndividuals_Sensitivity_Rselected", value = wInd, append = TRUE)
            
            print(paste0("Finished with i=", i, ", j=", j, ", k=", k, ", m=", m, ", n=", n, ", p=", p," at ", Sys.time()))
            Sys.sleep(0.001)
          }
        }
      }
    }
  }
}

stopCluster(cl)
dbDisconnect(dbConn)
