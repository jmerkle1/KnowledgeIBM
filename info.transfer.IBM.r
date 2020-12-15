# Function to run information transfer IBM
# developed by Zach Bell, Jerod Merkle, and Shannon Albeke
# last update: December 2020

info.transfer.IBM <- function(d = d,   # these are the starting params of the population including birth and death rates. must have 5 columns: "age","birthRate","survivalRate","N0_proportion","N0_prob_knowing"
                              N0 = 50, # starting number of individuals
                              K = 200, # carrying capacity
                              t = 25, # how many years should the simulation run for?
                              sex.ratio = 0.5, #what is the sex ratio of of the population/births?
                              bold.distr.beta = c(2, 5), # starting probability distribution of being bold, beta distribution (vector of 2 values: shape1 and shape2)
                              h = 0.20, #increase in probability of death for uninformed, as a proportion of current death rate.
                              nl = 0.01, # naive learning probability of the oldest animals (i.e., the ones that have the highest naive learning)
                              si = 5, # lambda of poison distribution, representing maximum number of interactions between two individuals if both have a boldness of 1. if both animals have boldness of 0, then there will be no interactions
                              infotransfer = 0.03, # given an interaction, what is the probability that information is transfered (min=0, max=1)
                              vertTransmission = TRUE, # When giving birth, should your information status be given to your offspring? TRUE/FALSE 
                              result.folder = "./KnowledgeIBM_results", #an empty folder where results will be saved.
                              set_seed = FALSE, # want to make results reproducible? Then set as TRUE
                              save_at_each_iter = TRUE #should all results be written to file at each time step?
                              
){
  
  # Major questions:
  # How do we tune the model so we can throw a number of different r/K selected species in? 
  # Do we really need boldness, interactions, and probability of knowledge transfer given an interaction all in the model?
  
  # Things I did on 12 Dec 2020:
  # 1. Removed naive learning being affected by age. Now is same rate for all ages.
  # 2. I removed any method for keeping interactions over time. In other words, who you interacted with in past year does not influence who you interact with in subsequent year
  # 3. removed the density component of interactions. Not necessary because as population increases, there is higher probability of interactuibs anyway.
  
  # Things to do:
  # 1. Develop visual representation of the model
  # 2. Tune the model so the crashing and shooting up to big numbers happens less often (how to reduce stochasticity?)
  # 3. Optimize the code so it runs faster? We'll have to do so many simulations. 
  # 4. Think about how to plot results, mainly what are the outputs were looking at to compare? e.g., mean time to 75% K? and % informed? We did already develop some code for this.
  # 5. 
  
  #manage packages
  if(all(c("Matrix") %in% installed.packages()[,1])==FALSE)
    stop("You must install the following packages: , and Matrix.")
  require(Matrix)
  
  #identify initial time
  t1 <- Sys.time()
  print(paste0("start time: ",t1,"."))
  
  #check the results folder
  if(dir.exists(result.folder)==FALSE){
    dir.create(result.folder)
  }
  if(length(dir(result.folder))> 0)
    print("Warning! Your result.folder has something in it. Those files will be overwritten!")
  
  maxAge <- max(d$age) #max age in order to get a proportion of age for age based rates of naive learning
  
  #--------------------------------#
  # create starting individuals ####
  # w attributes ("alive", "age", "informed"), initial population
  print("Creating initial individual database.")
  if(set_seed){
    set.seed(4008)
  }
  
  ind <- vector(mode="list", N0) 
  for(i in seq(ind)){
    ind[[i]]$alive <- 1 #0 if false, 1 if true
    ind[[i]]$sex <- rbinom(1, 1, sex.ratio) #coinflip for sex, 0 = male, 1 = female  
    ind[[i]]$age <- sample(d$age, 1, prob=d$N0_proportion)  # sample age based on stable age distribution
    ind[[i]]$informed <- rbinom(1,1, d$N0_prob_knowing[d$age == ind[[i]]$age])  # sample whether the animal knows the info 1 if have info, 0 if not
    ind[[i]]$boldness <- rbeta(1, bold.distr.beta[1], bold.distr.beta[2]) #beta distribution, ranges from 0 to 1
    ind[[i]]$mother <- 0
    ind[[i]]$birthYr <- 0
    #ind[[i]]$maternalSurvivalMod <- rbeta(1, .8, 1)
  }
  
  # build empty list to store matricies of interactions among individuals
  interactions <- list()
  # built matrix of individuals that are alive
  interactionMatrix <- Matrix(data = 0,
                              nrow = length(seq(ind)),
                              ncol = length(seq(ind)), sparse = TRUE)
  interactions[[1]] <- interactionMatrix
  
  
  #make empty vectors to record population statistics for each time step
  time <- seq(t + 1)
  
  pop <- list() # population size
  pop[[1]] <- which(sapply(ind, function(x) x$alive) == 1)  # this is actually who is alive at each time step

  frac.informed <- NaN * time # fraction of population that is informed
  info <- sapply(ind, function(x) x$informed)   
  frac.informed[1] <- ifelse(is.na(mean(info)), 0, mean(info))
  
  med.age <- NaN * time
  ages <- sapply(ind, function(x) x$age)
  med.age[1] <- median(ages)
  
  #this dataframe will be saved out at every t iteration no matter what.
  tosave <- data.frame(time.stamp = Sys.time(), 
                       t = 0,
                       pop.size = length(pop[[1]]),
                       births = NA, 
                       deaths = NA,
                       frac.informed = frac.informed[1], 
                       num.socialLearn = NA,
                       num.asocialLearn = NA,
                       total.num.interactions = NA,
                       med.age = med.age[1])
  
  
  #simulation starts here
  print(paste0("Looping through the ", t, " years."))
  for(i in seq(t)){ # loop for each time increment
    #prep that years data
    # print(i)
    # i=1
    is.alive <- which(sapply(ind, function(x) x$alive) == 1) #just alive individuals from total list
    is.alive <- sample(is.alive, length(is.alive))  # randomize the order of ids
    boldness <- sapply(ind[is.alive], function(x) x$boldness) #boldness of alive individuals
    interactionMatrix <- Matrix(data = 0,   #build a new interaction matrix for this time step!
                                nrow = length(seq(is.alive)),
                                ncol = length(seq(is.alive)), sparse = TRUE)
    naiveLearn<- ifelse(sapply(ind[is.alive], function(x) x$informed)==1, "Informed","Uninformed")
       # States == 
    
    # two if statements to see if we are done with the simulation.
    if(length(is.alive) > 1){   # is there anyone else left in the population?
      if(length(is.alive) / K < 0.75){  # has the population reached 75% of K? 
        
        for(j in is.alive){ #loop for each alive individual
          # print(j)
          # j = is.alive[1]
          
          # some preparation code
          curIndividual <- ind[[j]] #assigns current individual
          indexJ <- which(is.alive == j)    
          birthRate <- d$birthRate[which(d$age == curIndividual$age)] #birth rate of current age 
          ProbDeath <- 1 - d$survivalRate[which(d$age == curIndividual$age)] #survival rate of current age 
          
          #-----------------------------------#
          # interactions and info exchange ####
          #-----------------------------------#
          
          # if not informed, calculate whether it becomes informed through naive learning
          if(curIndividual$informed == 0){
            curIndividual$informed <- rbinom(1, 1, nl) 
            if(curIndividual$informed == 1){
              naiveLearn[indexJ] <- "Learned_Asocially"
            }
          }
          
          # #### Shannon attempt and doing this using tidy
          # # Custom function for determining if information is transfer
          # rBin<- function(x, y = infotransfer){
          #   tmp<- rbinom(x, 1, y)
          #   return(ifelse(any(tmp > 0), 1, 0))
          # }
          # 
          # socialPool <- data.frame(is.alive=is.alive[-indexJ], 
          #                          boldness=boldness[-indexJ]) %>% #pool of available individuals to socialize with. If both bold = 1, then it'll be drawn from pois distr with lambda si. as bold values decrease, so does the si value for lambda of the draw
          #   mutate(intIDinformed = sapply(ind[is.alive], function(x) x$informed)) %>%  # get other inds. informed level
          #   mutate(infoTransfer = case_when(curIndividual$informed == 0 & intIDinformed == 0 ~ 0,
          #                                   TRUE ~ 1)) %>% # updated information passing to others
          #   group_by(`is.alive`) %>% 
          #   mutate(numInteractions = rpois(n(), (si * curIndividual$boldness * boldness))) %>% # determine interactions
          #   mutate(numInteractions = rBin(numInteractions, infotransfer)) # update interaction based on probability of passing knowledge
          #   
          # # update status if previously uninformed and now there was at least one interaction resulting in knowledge transfer
          # if(curIndividual$informed==0 & sum(socialPool$infotransfer)>0){ 
          #   curIndividual$informed <- 1
          # }
          # 
          # # now we need to update the interacting individuals (other than the current individual) and their info status (given info transfer)
          # for(g in which(socialPool$infoTransfer==1)){ #loop through the other individuals that gained information at this step
          #   ind[[socialPool$is.alive[g]]]$informed <- socialPool$infoTransfer[g] #update each IDs interaction individual in total individuals dataset
          # }
          # 
          # #### End tidy experiment
          
          #### Orignial Jerod method
          # calculate the number of interactions with each other living animal
          socialPool <- data.frame(is.alive=is.alive[-indexJ],
                                   boldness=boldness[-indexJ]) #pool of available individuals to socialize with
          # if both bold = 1, then it'll be drawn from pois distr with lambda si. as bold values decrease, so does the si value for lambda of the draw
          socialPool$numInteractions <- rpois(nrow(socialPool), (si * curIndividual$boldness * socialPool$boldness))

          # add a column for whether or not each individual is informed or not from the pool
          socialPool$intIDinformed <- sapply(ind[socialPool$is.alive], function(x) x$informed)

          #update the interaction matricies
          interactionMatrix[-indexJ,indexJ] <- socialPool$numInteractions #update interaction matrix
          interactionMatrix[indexJ,-indexJ] <- socialPool$numInteractions #update interaction matrix


          #now calculate whether the information was transfered based on infotransfer and number of interactions
          socialPool$infotransfer <- do.call(c, lapply(1:nrow(socialPool), function(ii){
            return(ifelse(sum(rbinom(socialPool$numInteractions[ii], 1, infotransfer)) > 0, 1, 0))
          }))

          # !! this line could be integrated into equation above.
          # change info transfer to 0 if both of the two indidividuals did NOT have information
          socialPool$infotransfer <- ifelse(curIndividual$informed == 0 & socialPool$intIDinformed == 0, 0, socialPool$infotransfer)

          # update status if previously uninformed and now there was at least one interaction resulting in knowledge transfer
          if(curIndividual$informed == 0 & sum(socialPool$infotransfer) > 0){
            curIndividual$informed <- 1
            naiveLearn[indexJ] <- "Learned_Socially"
          }

          # now we need to update the interacting individuals (other than the current individual) and their info status (given info transfer)
          socialPool$infotransfer <- ifelse(socialPool$intIDinformed == 0 & socialPool$infotransfer == 1, 1, 0)
          for(g in which(socialPool$infotransfer == 1)){ #loop through the other individuals that gained information at this step
            ind[[socialPool$is.alive[g]]]$informed <- socialPool$infotransfer[g] #update each IDs interaction individual in total individuals dataset
            # Update the learning vector
            naiveLearn[which(is.alive == socialPool$is.alive[g])] <- "Learned_Socially"
          }

          # -----------------#
          # birth section ####
          #------------------#
          birth <- rpois(1, birthRate)
          if(birth >= 1 & (curIndividual$sex == 1)){ #checks for successful birth and female sex
            #create new individual
            len.ind <- length(ind)
            for(z in 1:birth){  # loop over the number of births that occured
              ind[[(len.ind + z)]] <- list(alive = 1, 
                                           age = 0, 
                                           sex = rbinom(1, 1, sex.ratio),
                                           informed = ifelse(vertTransmission == TRUE, curIndividual$informed, 0),   # give informed status of mom if vertransmission = TRUE.
                                           boldness = rbeta(1, bold.distr.beta[1], bold.distr.beta[2]),
                                           mother = j, 
                                           birthYr = i) # create offspring, inherits informed status of parent
              
            }   # end of loop over number of births that occured
          }  # end of in statement whether j is female and she gave birth
          
          #------------------#
          # death section ####
          #------------------#
          #death decided by survival Rate, and increased uniformed mortality rate
          death <- (ProbDeath + ((1 - curIndividual$informed) * h * ProbDeath)) # calculate a death probability for each individual, decreased by the proportion h, when uninformed
          death <- rbinom(1, 1, ifelse(death > 1, 1, death))
          
          # Update individuals status of dead or increase age
          ifelse(death == 1, curIndividual$alive <- 0, curIndividual$age <- curIndividual$age + 1)
          # if(death==1){
          #   curIndividual$alive <- 0 # if death, reset alive = 0
          # } else { #if didn't die, advance age + 1
          #   curIndividual$age <- curIndividual$age + 1 # advance age of parent
          # }
          
          #update current individual in the total ind list
          ind[[j]] <- curIndividual 
          
        }   # end of loop over living animals (i.e,. is.alive)
        
        # ---------------------------#
        # saving Population stats ####
        # ---------------------------#
        # calculate the number that died at this time step
        numb.died <- length(which(sapply(ind[is.alive], function(x) x$alive) == 0))
        
        # update pop vector (i.e., the vector of who's alive at each time step)
        is.alive <- which(sapply(ind, function(x) x$alive) == 1)
        pop[[(i + 1)]] <- is.alive 
        
        # update frac.informed vector
        info <- sapply(ind, function(x) x$informed)
        frac.informed[(i + 1)] <- sum(info[is.alive]  == 1) / length(is.alive)
        
        #update median age vector
        ages <- sapply(ind, function(x) x$age)
        med.age[(i + 1)] <- median(ages[is.alive])
        
        # update interactions list with the latest matrix
        interactions[[(i + 1)]] <- interactionMatrix 
        
        tosave <- rbind(tosave, data.frame(time.stamp = as.character(Sys.time()), 
                                           t = i, 
                                           pop.size = length(pop[[(i + 1)]]),
                                           frac.informed = frac.informed[(i + 1)],
                                           num.socialLearn = length(naiveLearn[naiveLearn == "Learned_Socially"]),
                                           num.asocialLearn = length(naiveLearn[naiveLearn == "Learned_Asocially"]),
                                           total.num.interactions = sum(interactionMatrix) / 2,
                                           births = length(which(sapply(ind, function(x) x$birthYr) == i)), 
                                           deaths = numb.died, 
                                           med.age = med.age[(i + 1)]))
        # write out dataframe. Note that it rewrites itself each time.
        # write.csv(tosave, paste0(result.folder,"/population_stats.csv"), row.names = FALSE)
        
        # #save out all the main files?
        # # !! change to RDS
        # if(save_at_each_iter){
        #   save(interactions, file = paste0(result.folder,"/interaction_matricies.RData"))
        #   save(pop, file = paste0(result.folder,"/population_data.RData"))  #don't really need this if ind data is written out
        #   save(ind, file = paste0(result.folder,"/individual_data.RData"))
        # }
        
        #simulation progress
        Sys.sleep(.1)
        print(paste0(i, " of ", t, " finished ", "[", round(i / t * 100,0), "%]"))
        
      }   # end of if statement if the population made it to 0.75 of K
    }   # end of if statement whether any animals are alive
  } # end of loop through t
  
  # #write out final files
  # write.csv(tosave, paste0(result.folder,"/population_stats.csv"), row.names = FALSE)
  # save(interactions, file = paste0(result.folder,"/interaction_matricies.RData"))
  # save(pop, file = paste0(result.folder,"/population_data.RData"))  #don't really need this if ind data is written out
  # save(ind, file = paste0(result.folder,"/individual_data.RData"))
  
  return(list(PopStats = tosave, Interactions = interactions, Individuals = ind))
  
  #time to run simulation
  # return(paste0("Completed in ", round((as.numeric(Sys.time())-as.numeric(t1))/60,2), " mins! Check your result.folder for results."))
  
}


