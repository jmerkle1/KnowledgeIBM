require(RSQLite)
require(jsonlite)
require(tidyverse)

# Connect to the database
box_fldr <- "E:/Box Sync/KnowledgeIBM_results"

# Create/Connect to SQLite DB for storage of model runs
dbConn <- dbConnect(SQLite(), paste0(box_fldr, "/KnowledgeIBM_ModelTuning_20201215.sqlite"))

# What tables exist within the database
dbListTables(dbConn)

# Read in the model summary data
dat<- dbSendQuery(dbConn, "SELECT * FROM tblModelStats") %>% 
  dbFetch()

# Summarize the data by scenario
sumDat<- dat %>% 
  filter(yr != 0) %>% 
  group_by(Scenario, yr) %>% 
  summarise(pop.size_Mean = mean(pop.size, na.rm = TRUE),
            pop.size_SD = sd(pop.size, na.rm = TRUE),
            births_Mean = mean(births, na.rm = TRUE),
            births_SD = sd(births, na.rm = TRUE),
            deaths_Mean = mean(deaths, na.rm = TRUE),
            deaths_SD = sd(deaths, na.rm = TRUE),
            frac.informed_Mean = mean(frac.informed, na.rm = TRUE),
            frac.informed_SD = sd(frac.informed, na.rm = TRUE),
            num.socialLearn_Mean = mean(num.socialLearn, na.rm = TRUE),
            num.socialLearn_SD = sd(num.socialLearn, na.rm = TRUE),
            num.asocialLearn_Mean = mean(num.asocialLearn, na.rm = TRUE),
            num.asocialLearn_SD = sd(num.asocialLearn, na.rm = TRUE),
            total.num.interactions_Mean = mean(total.num.interactions, na.rm = TRUE),
            total.num.interactions_SD = sd(total.num.interactions, na.rm = TRUE),
            med.age_Mean = mean(med.age, na.rm = TRUE),
            med.age_SD = sd(med.age, na.rm = TRUE),
            Count = n()
            ) %>% 
  mutate(pop.size_CI = (pop.size_SD / sqrt(Count)) * 1.96,
         births_CI = (births_SD / sqrt(Count)) * 1.96,
         deaths_CI = (deaths_SD / sqrt(Count)) * 1.96,
         frac.informed_CI = (frac.informed_SD / sqrt(Count)) * 1.96,
         num.socialLearn_CI = (num.socialLearn_SD / sqrt(Count)) * 1.96,
         num.asocialLearn_CI = (num.asocialLearn_SD / sqrt(Count)) * 1.96,
         total.num.interactions_CI = (total.num.interactions_SD / sqrt(Count)) * 1.96,
         med.age_CI = (med.age_SD / sqrt(Count)) * 1.96)

nScenario<- dat %>% 
  distinct(Scenario, ModelRun) %>% 
  group_by(Scenario) %>% 
  summarise(Count = n())

# Determine max year for each run
maxYr<- dat %>% 
  group_by(Scenario, ModelRun) %>% 
  filter(yr == max(yr)) %>% 
  mutate(PopSurvive = case_when(pop.size < 2 ~ "Extirpated",
                                TRUE ~ "Survived")) %>% 
  ungroup() %>% 
  group_by(Scenario, PopSurvive) %>% 
  summarise(Mean = mean(yr),
            SD = sd(yr),
            Count = n()) %>% 
  mutate(CI95 = (SD / sqrt(Count)) * 1.96) %>% 
  pivot_wider(id_cols = Scenario, names_from = PopSurvive, values_from = c(Mean, CI95, Count)) %>% 
  mutate(Count_Survived = replace_na(Count_Survived, 0),
         Count_Extirpated = replace_na(Count_Extirpated, 0)) %>% 
  mutate(PropSurvive = Count_Survived / (Count_Survived + Count_Extirpated))

# Loop through the rows and populate the model parameters
getModelParams<- function(x){
  return(fromJSON(x) %>% 
           pivot_wider(names_from = Parameter, values_from = Level))
}

tmpParam<- data.frame()
for(i in 1:nrow(maxYr)){
  tmpParam<- rbind(tmpParam, data.frame(Scenario = maxYr$Scenario[i], getModelParams(maxYr$Scenario[i])))
}

# Bind the tables
maxYr<- maxYr %>% 
  left_join(tmpParam, by = "Scenario")


#### Populations Size ####
sumDat %>% 
  ggplot(aes(x = yr, y = pop.size_Mean, group = Scenario, color = Scenario)) +
  geom_ribbon(aes(ymin = pop.size_Mean - pop.size_CI, ymax = pop.size_Mean + pop.size_CI), fill = "grey70") +
  geom_line() 
  
#### Births ####
sumDat %>% 
  ggplot(aes(x = yr, y = births_Mean, group = Scenario, color = Scenario)) +
  geom_ribbon(aes(ymin = births_Mean - births_CI, ymax = births_Mean + births_CI), fill = "grey70") +
  geom_line() 

#### Deaths ####
sumDat %>% 
  ggplot(aes(x = yr, y = deaths_Mean, group = Scenario, color = Scenario)) +
  geom_ribbon(aes(ymin = deaths_Mean - deaths_CI, ymax = deaths_Mean + deaths_CI), fill = "grey70") +
  geom_line() 

#### Fraction Informed ####
sumDat %>% 
  ggplot(aes(x = yr, y = frac.informed_Mean, group = Scenario, color = Scenario)) +
  geom_ribbon(aes(ymin = frac.informed_Mean - frac.informed_CI, ymax = frac.informed_Mean + frac.informed_CI), fill = "grey70") +
  geom_line() 

#### Social Learning ####
sumDat %>% 
  ggplot(aes(x = yr, y = num.socialLearn_Mean, group = Scenario, color = Scenario)) +
  geom_ribbon(aes(ymin = num.socialLearn_Mean - num.socialLearn_CI, ymax = num.socialLearn_Mean + num.socialLearn_CI), fill = "grey70") +
  geom_line() 

#### Asocial Learning ####
sumDat %>% 
  ggplot(aes(x = yr, y = num.asocialLearn_Mean, group = Scenario, color = Scenario)) +
  geom_ribbon(aes(ymin = num.asocialLearn_Mean - num.asocialLearn_CI, ymax = num.asocialLearn_Mean + num.asocialLearn_CI), fill = "grey70") +
  geom_line() 

#### Total Interactions ####
sumDat %>% 
  ggplot(aes(x = yr, y = total.num.interactions_Mean, group = Scenario, color = Scenario)) +
  geom_ribbon(aes(ymin = total.num.interactions_Mean - total.num.interactions_CI, ymax = total.num.interactions_Mean + total.num.interactions_CI), fill = "grey70") +
  geom_line() 

#### Median Age ####
sumDat %>% 
  ggplot(aes(x = yr, y = med.age_Mean, group = Scenario, color = Scenario)) +
  geom_ribbon(aes(ymin = med.age_Mean - med.age_CI, ymax = med.age_Mean + med.age_CI), fill = "grey70") +
  geom_line() 
