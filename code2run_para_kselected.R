# This code runs the information transfer IBM (info.transfer.IBM.R) and the plots the results
# Written by Zach Bell and Jerod Merkle
# June 2019


#source the function
source("C:/Users/Zach/Documents/GitHub/Memory_IBM/Memory_IBM/info.transfer.IBM.para.R")
source("C:/Users/Zach/Documents/GitHub/Memory_IBM/Memory_IBM/plotInfoTransferIBM_para.R")
birthdeath.file <- "C:/Users/Zach/Documents/GitHub/Memory_IBM/Memory_IBM/kSelected/ageClass_kSelect.csv" #dataframe of age based birth and death rate for FEMALES only. The columns should be age, ageClass, birthRate, and survivalRate, in that order.
  if(file.exists(birthdeath.file)==FALSE){
    stop("You didn't provide an existing file for birthdeath.file!")
  }
  d <- read.csv(file = birthdeath.file, colClasses = c("numeric", "numeric", "numeric", "numeric")) 
  if(ncol(d)!= 4){
    stop("Your birthdeath.file does not have 4 columns. The columns should be age, ageClass, birthRate, and survivalRate, in that order.")
  }
  
args <- list(c(.095, .19, .285), #increase in probability of death for uninformed
             c(.1, .2, .3), # naive learning probability of the oldest animals (i.e., the ones that have the highest naive learning)
             c(2, 6, 10, 20, 30), # maximum mean (i.e., lambda of poison distribution) number of interactions per pair (if animal has 1 bold, it interacts with an animal with 1 boldness, and population is at or above K, this is the lambda of the interaction distributions)
             c(.05, .10, .20), # given an interaction, what is the probability that information is transfered (min=0, max=1)
             1000, # carrying capacity
             c(100, 200, 300), # starting number of individuals
             30, # how many years should the simulation run for?
             0.45, #what is the sex ratio of of the population/births?
             c(.5, 1.5, 2.5), # lambda value for starting age distribution based on poison distribution
             ".5 5", # starting probability distribution of knowing information; beta distribution ranges from 0 to 1 (vector of 2 values: shape1 and shape2)
             "2 5", # starting probability distribution of being bold, beta distribution (vector of 2 values: shape1 and shape2)
             "C:/Users/Zach/Desktop/results/kSelect", #an empty folder where results will be saved.
             FALSE, # want to make results reproducible? Then set as TRUE
             TRUE, #should all results be written to file at each time step?
             c(1, 0), # When giving birth, should your information status be given to your offspring? 0 if false, 1 if true (i.e., is there vertical transmission of information?)
             0, #density dependence of interactions, set to 1 for positive density dependence, 0 for none, -1 for negative density dependence
             c(.2, .5, .8))

args <- expand.grid(args)
args <- args[1964:nrow(args),]
time1 <- Sys.time()
results.folders <- paste("C:/Users/Zach/Desktop/results/kSelect", paste0(1:nrow(args), gsub(":", "",strsplit(as.character(Sys.time()), " ")[[1]][2])), sep = "/")
for(i in 1:length(results.folders)){
  if(!dir.exists(results.folders[i])){
    dir.create(results.folders[i])
  }
}
args[,12] <- results.folders
colnames(args) <- c("h", "nl", "si", "infotransfer", "K", "N0", "t", "sex.ratio", "age.distr.lamba", "informed.distr.beta", 
                    "bold.distr.beta", "result.folder", "set_seed", "save_at_each_iter", "vertTransmission", "densityDependType", 
                    "familiarBias")
require(snowfall)
require(Matrix)
require(igraph)
require(stringr)
sfInit(parallel = TRUE, cpus = 2)
sfLibrary(igraph)
sfLibrary(Matrix)
sfLibrary(stringr)
sfExport("args", "d", "info.transfer.IBM")
sfClusterApplyLB(1:nrow(args), function(i){
  Sys.sleep(.1)
  info.transfer.IBM(h=args[i,1],
                    nl=args[i,2],
                    si=args[i,3], infotransfer=args[i,4], K=args[i,5], N0=args[i,6], t=args[i,7], sex.ratio=args[i,8], age.distr.lamba=args[i,9], 
                    informed.distr.beta=as.character(args[i,10]), bold.distr.beta=as.character(args[i,11]), result.folder=args[i,12],
                    set_seed=args[i,13], save_at_each_iter=args[i,14], vertTransmission=args[i,15], densityDependType = args[i,16], 
                    familiarBias = args[i,17], d = d)
  write.csv(file = paste0(args[i,12],"/params.csv"), args[i,])
})

#sys.wait to stagger reads
for(i in 1:length(results.folders)){
  folder <- results.folders[i]
  load(paste0(results.folders[i],"/interaction_matricies.RData"))
  load(paste0(results.folders[i],"/population_data.RData"))  #don't really need this if ind data is written out
  load(paste0(results.folders[i],"/individual_data.RData"))
  results <- read.csv(paste0(results.folders[i],"/population_stats.csv"))
  sfExport("results", "ind", "pop", "interactions", "folder", "plotSocialIBMResults")
  sfClusterApplyLB(1, function(i){plotSocialIBMResults(results, ind, pop, interactions, folder)})
}
sfStop()
time2 <- Sys.time()
Sys.sleep(.001) 
print(time2 - time1)
