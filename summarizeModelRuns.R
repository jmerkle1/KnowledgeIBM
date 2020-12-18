require(RSQLite)
require(jsonlite)
require(tidyverse)
require(gridExtra)
require(here)

# Population size, births, deaths, sex ratio % Informed
# IDs learning asocially 
# Interactions
# IDs learned vertically
# IDs learning socially


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
            pop.size_Q03 = quantile(pop.size, probs = 0.03, na.rm = TRUE),
            pop.size_Q97 = quantile(pop.size, probs = 0.97, na.rm = TRUE),
            births_Mean = mean(births, na.rm = TRUE),
            births_SD = sd(births, na.rm = TRUE),
            births_Q03 = quantile(births, probs = 0.03, na.rm = TRUE),
            births_Q97 = quantile(births, probs = 0.97, na.rm = TRUE),
            deaths_Mean = mean(deaths, na.rm = TRUE),
            deaths_SD = sd(deaths, na.rm = TRUE),
            deaths_Q03 = quantile(deaths, probs = 0.03, na.rm = TRUE),
            deaths_Q97 = quantile(deaths, probs = 0.97, na.rm = TRUE),
            frac.informed_Mean = mean(frac.informed, na.rm = TRUE),
            frac.informed_SD = sd(frac.informed, na.rm = TRUE),
            frac.informed_Q03 = quantile(frac.informed, probs = 0.03, na.rm = TRUE),
            frac.informed_Q97 = quantile(frac.informed, probs = 0.97, na.rm = TRUE),
            num.socialLearn_Mean = mean(num.socialLearn, na.rm = TRUE),
            num.socialLearn_SD = sd(num.socialLearn, na.rm = TRUE),
            num.socialLearn_Q03 = quantile(num.socialLearn, probs = 0.03, na.rm = TRUE),
            num.socialLearn_Q97 = quantile(num.socialLearn, probs = 0.97, na.rm = TRUE),
            num.asocialLearn_Mean = mean(num.asocialLearn, na.rm = TRUE),
            num.asocialLearn_SD = sd(num.asocialLearn, na.rm = TRUE),
            num.asocialLearn_Q03 = quantile(num.asocialLearn, probs = 0.03, na.rm = TRUE),
            num.asocialLearn_Q97 = quantile(num.asocialLearn, probs = 0.97, na.rm = TRUE),
            total.num.interactions_Mean = mean(total.num.interactions, na.rm = TRUE),
            total.num.interactions_SD = sd(total.num.interactions, na.rm = TRUE),
            total.num.interactions_Q03 = quantile(total.num.interactions, probs = 0.03, na.rm = TRUE),
            total.num.interactions_Q97 = quantile(total.num.interactions, probs = 0.97, na.rm = TRUE),
            med.age_Mean = mean(med.age, na.rm = TRUE),
            med.age_SD = sd(med.age, na.rm = TRUE),
            med.age_Q03 = quantile(med.age, probs = 0.03, na.rm = TRUE),
            med.age_Q97 = quantile(med.age, probs = 0.97, na.rm = TRUE),
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

#### Determine max year for each run ####
maxYr<- dat %>% 
  group_by(Scenario, ModelRun) %>% 
  filter(yr == max(yr)) %>% 
  mutate(PopSurvive = case_when(pop.size < 2 ~ "Extirpated",
                                TRUE ~ "Survived")) %>% 
  ungroup() %>% 
  group_by(Scenario, PopSurvive) %>% 
  summarise(Mean = mean(yr),
            SD = sd(yr),
            Q03 = quantile(yr, probs = 0.03),
            Q97 = quantile(yr, probs = 0.97),
            Count = n()) %>% 
  mutate(CI95 = (SD / sqrt(Count)) * 1.96) %>% 
  pivot_wider(id_cols = Scenario, names_from = PopSurvive, values_from = c(Mean, CI95, Q03, Q97, Count)) %>% 
  mutate(Count_Survived = replace_na(Count_Survived, 0),
         Count_Extirpated = replace_na(Count_Extirpated, 0)) %>% 
  mutate(PropSurvive = Count_Survived / (Count_Survived + Count_Extirpated),
         EndResult = case_when(Count_Survived > 0 & Count_Extirpated > 0 ~ "Mixed Result",
                               Count_Extirpated == 0 ~ "All Survived",
                               Count_Survived == 0 ~ "All Crashed"))

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

#### Investigate Crash / Survive Scenarios ####
if(!dir.exists(paste0(box_fldr, "/GeneralPlots"))){
  dir.create(paste0(box_fldr, "/GeneralPlots"))
}


g<- arrangeGrob(
maxYr %>% 
  group_by(EndResult, Vital) %>% 
  summarise(Count = n()) %>% 
  ggplot(aes(x = EndResult, y = Count, fill = Vital)) +
  geom_col() +
  ggtitle("# of Scenarios by End Result \n (50 iterations / scenario)") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90)),
maxYr %>% 
  select(Scenario, Mean_Survived, Mean_Extirpated, Vital, EndResult) %>% 
  pivot_longer(cols = c(Mean_Survived, Mean_Extirpated), names_to = "Param", values_to = "MeanYears", values_drop_na = TRUE) %>% 
  ggplot(aes(x = paste(Vital, EndResult, Param), y = MeanYears)) +
  geom_boxplot() +
  ggtitle("Mean Years to K or Crash by Vitals") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90)),
ncol = 2)
# Save the plot
ggsave(g, filename = paste0(box_fldr, "/GeneralPlots/Vitals_EffectsOnPopulationSurvival.png"), width = 7, height = 5.25, units = "in")

# Drivers forcing All Crashed
allCrashed<- maxYr %>% 
  filter(is.na(Mean_Survived))

unique(allCrashed$Vital) # Rselected Only
unique(allCrashed$Boldness)
unique(allCrashed$PoisLambda)
unique(allCrashed$InfoTransfer)
unique(allCrashed$InfoDeath) # No Low
unique(allCrashed$NaiveLearn)
unique(allCrashed$VertTrans)


allSurvive<- maxYr %>% 
  filter(is.na(Mean_Extirpated))

unique(allSurvive$Vital) # Kselected Only
unique(allSurvive$Boldness)
unique(allSurvive$PoisLambda)
unique(allSurvive$InfoTransfer)
unique(allSurvive$InfoDeath) 
unique(allSurvive$NaiveLearn)
unique(allSurvive$VertTrans)

mixSurvive<- maxYr %>% 
  filter(!is.na(Mean_Extirpated & !is.na(Mean_Survived)))

unique(mixSurvive$Vital) # Both exist
unique(mixSurvive$Boldness)
unique(mixSurvive$PoisLambda)
unique(mixSurvive$InfoTransfer)
unique(mixSurvive$InfoDeath) 
unique(mixSurvive$NaiveLearn)
unique(mixSurvive$VertTrans)


# Set up ordered factors
maxYr<- maxYr %>% 
  mutate(Boldness = ordered(Boldness, c("skewLow", "skewMedLow", "uniform", "skewMedHigh", "skewHigh")),
         PoisLambda = ordered(PoisLambda, c("Low", "MedLow", "Med", "MedHigh", "High")),
         InfoTransfer = ordered(InfoTransfer, c("Low", "MedLow", "Med", "MedHigh", "High")),
         InfoDeath = ordered(InfoDeath, c("Low", "MedLow", "Med", "MedHigh", "High")),
         NaiveLearn = ordered(NaiveLearn, c("Low", "MedLow", "Med", "MedHigh", "High")))

# Boxplot of Mean Years to Carrying capacity by Vital rate and Boldness level
maxYr %>% 
  select(Scenario, Mean_Survived, Mean_Extirpated, Vital, Boldness, EndResult) %>% 
  pivot_longer(cols = c(Mean_Survived, Mean_Extirpated), names_to = "Param", values_to = "MeanYears", values_drop_na = TRUE) %>% 
  mutate(xcol = paste(Vital, EndResult, Boldness, Param)) %>% 
  mutate(xcol = ordered(xcol, c("Kselected All Survived skewLow Mean_Survived", "Kselected All Survived skewMedLow Mean_Survived", "Kselected All Survived uniform Mean_Survived", "Kselected All Survived skewMedHigh Mean_Survived", "Kselected All Survived skewHigh Mean_Survived", "Kselected Mixed Result skewLow Mean_Survived", "Kselected Mixed Result skewLow Mean_Extirpated", "Kselected Mixed Result skewMedLow Mean_Survived", "Kselected Mixed Result skewMedLow Mean_Extirpated", "Kselected Mixed Result uniform Mean_Survived", "Kselected Mixed Result uniform Mean_Extirpated", "Kselected Mixed Result skewMedHigh Mean_Survived", "Kselected Mixed Result skewMedHigh Mean_Extirpated", "Kselected Mixed Result skewHigh Mean_Survived", "Kselected Mixed Result skewHigh Mean_Extirpated", "Rselected Mixed Result skewLow Mean_Survived", "Rselected Mixed Result skewLow Mean_Extirpated", "Rselected Mixed Result skewMedLow Mean_Survived", "Rselected Mixed Result skewMedLow Mean_Extirpated", "Rselected Mixed Result uniform Mean_Survived", "Rselected Mixed Result uniform Mean_Extirpated", "Rselected Mixed Result skewMedHigh Mean_Survived", "Rselected Mixed Result skewMedHigh Mean_Extirpated", "Rselected Mixed Result skewHigh Mean_Survived", "Rselected Mixed Result skewHigh Mean_Extirpated", "Rselected All Crashed skewLow Mean_Extirpated", "Rselected All Crashed skewMedLow Mean_Extirpated", "Rselected All Crashed uniform Mean_Extirpated", "Rselected All Crashed skewMedHigh Mean_Extirpated", "Rselected All Crashed skewHigh Mean_Extirpated"))) %>% 
  ggplot(aes(x = xcol, y = MeanYears)) + 
  geom_boxplot() +
  ggtitle("Mean Years to K or Crash by Boldness") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90))

# Save the plot
ggsave(filename = paste0(box_fldr, "/GeneralPlots/Boxplot_YearsToKcrash_ByBoldness.png"), width = 7, height = 5.25, units = "in")

# Boxplot of Mean Years to Carrying capacity by Vital rate and Poisson Lambda level
maxYr %>% 
  select(Scenario, Mean_Survived, Mean_Extirpated, Vital, PoisLambda, EndResult) %>% 
  pivot_longer(cols = c(Mean_Survived, Mean_Extirpated), names_to = "Param", values_to = "MeanYears", values_drop_na = TRUE) %>% 
  mutate(xcol = paste(Vital, EndResult, PoisLambda, Param)) %>% 
  mutate(xcol = ordered(xcol, c("Kselected All Survived Low Mean_Survived", "Kselected All Survived MedLow Mean_Survived", "Kselected All Survived Med Mean_Survived", "Kselected All Survived MedHigh Mean_Survived", "Kselected All Survived High Mean_Survived", "Kselected Mixed Result Low Mean_Survived", "Kselected Mixed Result Low Mean_Extirpated", "Kselected Mixed Result MedLow Mean_Survived", "Kselected Mixed Result MedLow Mean_Extirpated", "Kselected Mixed Result Med Mean_Survived", "Kselected Mixed Result Med Mean_Extirpated", "Kselected Mixed Result MedHigh Mean_Survived", "Kselected Mixed Result MedHigh Mean_Extirpated", "Kselected Mixed Result High Mean_Survived", "Kselected Mixed Result High Mean_Extirpated", "Rselected Mixed Result Low Mean_Survived", "Rselected Mixed Result Low Mean_Extirpated", "Rselected Mixed Result MedLow Mean_Survived", "Rselected Mixed Result MedLow Mean_Extirpated", "Rselected Mixed Result Med Mean_Survived", "Rselected Mixed Result Med Mean_Extirpated", "Rselected Mixed Result MedHigh Mean_Survived", "Rselected Mixed Result MedHigh Mean_Extirpated", "Rselected Mixed Result High Mean_Survived", "Rselected Mixed Result High Mean_Extirpated", "Rselected All Crashed Low Mean_Extirpated", "Rselected All Crashed MedLow Mean_Extirpated", "Rselected All Crashed Med Mean_Extirpated", "Rselected All Crashed MedHigh Mean_Extirpated", "Rselected All Crashed High Mean_Extirpated"))) %>% 
  ggplot(aes(x = xcol, y = MeanYears)) + 
  geom_boxplot() +
  ggtitle("Mean Years to K or Crash by Poisson Lambda") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90))

# Save the plot
ggsave(filename = paste0(box_fldr, "/GeneralPlots/Boxplot_YearsToKcrash_ByPoisLambda.png"), width = 7, height = 5.25, units = "in")


# Boxplot of Mean Years to Carrying capacity by Vital rate and InfoTransfer level
maxYr %>% 
  select(Scenario, Mean_Survived, Mean_Extirpated, Vital, InfoTransfer, EndResult) %>% 
  pivot_longer(cols = c(Mean_Survived, Mean_Extirpated), names_to = "Param", values_to = "MeanYears", values_drop_na = TRUE) %>% 
  mutate(xcol = paste(Vital, EndResult, InfoTransfer, Param)) %>% 
  mutate(xcol = ordered(xcol, c("Kselected All Survived Low Mean_Survived", "Kselected All Survived MedLow Mean_Survived", "Kselected All Survived Med Mean_Survived", "Kselected All Survived MedHigh Mean_Survived", "Kselected All Survived High Mean_Survived", "Kselected Mixed Result Low Mean_Survived", "Kselected Mixed Result Low Mean_Extirpated", "Kselected Mixed Result MedLow Mean_Survived", "Kselected Mixed Result MedLow Mean_Extirpated", "Kselected Mixed Result Med Mean_Survived", "Kselected Mixed Result Med Mean_Extirpated", "Kselected Mixed Result MedHigh Mean_Survived", "Kselected Mixed Result MedHigh Mean_Extirpated", "Kselected Mixed Result High Mean_Survived", "Kselected Mixed Result High Mean_Extirpated", "Rselected Mixed Result Low Mean_Survived", "Rselected Mixed Result Low Mean_Extirpated", "Rselected Mixed Result MedLow Mean_Survived", "Rselected Mixed Result MedLow Mean_Extirpated", "Rselected Mixed Result Med Mean_Survived", "Rselected Mixed Result Med Mean_Extirpated", "Rselected Mixed Result MedHigh Mean_Survived", "Rselected Mixed Result MedHigh Mean_Extirpated", "Rselected Mixed Result High Mean_Survived", "Rselected Mixed Result High Mean_Extirpated", "Rselected All Crashed Low Mean_Extirpated", "Rselected All Crashed MedLow Mean_Extirpated", "Rselected All Crashed Med Mean_Extirpated", "Rselected All Crashed MedHigh Mean_Extirpated", "Rselected All Crashed High Mean_Extirpated"))) %>% 
  ggplot(aes(x = xcol, y = MeanYears)) + 
  geom_boxplot() +
  ggtitle("Mean Years to K or Crash by InfoTransfer") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90))

# Save the plot
ggsave(filename = paste0(box_fldr, "/GeneralPlots/Boxplot_YearsToKcrash_ByInfoTransfer.png"), width = 7, height = 5.25, units = "in")


# Boxplot of Mean Years to Carrying capacity by Vital rate and InfoDeath level
maxYr %>% 
  select(Scenario, Mean_Survived, Mean_Extirpated, Vital, InfoDeath, EndResult) %>% 
  pivot_longer(cols = c(Mean_Survived, Mean_Extirpated), names_to = "Param", values_to = "MeanYears", values_drop_na = TRUE) %>% 
  mutate(xcol = paste(Vital, EndResult, InfoDeath, Param)) %>% 
  mutate(xcol = ordered(xcol, c("Kselected All Survived Low Mean_Survived", "Kselected All Survived MedLow Mean_Survived", "Kselected All Survived Med Mean_Survived", "Kselected All Survived MedHigh Mean_Survived", "Kselected All Survived High Mean_Survived", "Kselected Mixed Result Low Mean_Survived", "Kselected Mixed Result Low Mean_Extirpated", "Kselected Mixed Result MedLow Mean_Survived", "Kselected Mixed Result MedLow Mean_Extirpated", "Kselected Mixed Result Med Mean_Survived", "Kselected Mixed Result Med Mean_Extirpated", "Kselected Mixed Result MedHigh Mean_Survived", "Kselected Mixed Result MedHigh Mean_Extirpated", "Kselected Mixed Result High Mean_Survived", "Kselected Mixed Result High Mean_Extirpated", "Rselected Mixed Result Low Mean_Survived", "Rselected Mixed Result Low Mean_Extirpated", "Rselected Mixed Result MedLow Mean_Survived", "Rselected Mixed Result MedLow Mean_Extirpated", "Rselected Mixed Result Med Mean_Survived", "Rselected Mixed Result Med Mean_Extirpated", "Rselected Mixed Result MedHigh Mean_Survived", "Rselected Mixed Result MedHigh Mean_Extirpated", "Rselected Mixed Result High Mean_Survived", "Rselected Mixed Result High Mean_Extirpated", "Rselected All Crashed Low Mean_Extirpated", "Rselected All Crashed MedLow Mean_Extirpated", "Rselected All Crashed Med Mean_Extirpated", "Rselected All Crashed MedHigh Mean_Extirpated", "Rselected All Crashed High Mean_Extirpated"))) %>% 
  ggplot(aes(x = xcol, y = MeanYears)) + 
  geom_boxplot() +
  ggtitle("Mean Years to K or Crash by InfoDeath") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90))

# Save the plot
ggsave(filename = paste0(box_fldr, "/GeneralPlots/Boxplot_YearsToKcrash_ByInfoDeath.png"), width = 7, height = 5.25, units = "in")


# Boxplot of Mean Years to Carrying capacity by Vital rate and NaiveLearn level
maxYr %>% 
  select(Scenario, Mean_Survived, Mean_Extirpated, Vital, NaiveLearn, EndResult) %>% 
  pivot_longer(cols = c(Mean_Survived, Mean_Extirpated), names_to = "Param", values_to = "MeanYears", values_drop_na = TRUE) %>% 
  mutate(xcol = paste(Vital, EndResult, NaiveLearn, Param)) %>% 
  mutate(xcol = ordered(xcol, c("Kselected All Survived Low Mean_Survived", "Kselected All Survived MedLow Mean_Survived", "Kselected All Survived Med Mean_Survived", "Kselected All Survived MedHigh Mean_Survived", "Kselected All Survived High Mean_Survived", "Kselected Mixed Result Low Mean_Survived", "Kselected Mixed Result Low Mean_Extirpated", "Kselected Mixed Result MedLow Mean_Survived", "Kselected Mixed Result MedLow Mean_Extirpated", "Kselected Mixed Result Med Mean_Survived", "Kselected Mixed Result Med Mean_Extirpated", "Kselected Mixed Result MedHigh Mean_Survived", "Kselected Mixed Result MedHigh Mean_Extirpated", "Kselected Mixed Result High Mean_Survived", "Kselected Mixed Result High Mean_Extirpated", "Rselected Mixed Result Low Mean_Survived", "Rselected Mixed Result Low Mean_Extirpated", "Rselected Mixed Result MedLow Mean_Survived", "Rselected Mixed Result MedLow Mean_Extirpated", "Rselected Mixed Result Med Mean_Survived", "Rselected Mixed Result Med Mean_Extirpated", "Rselected Mixed Result MedHigh Mean_Survived", "Rselected Mixed Result MedHigh Mean_Extirpated", "Rselected Mixed Result High Mean_Survived", "Rselected Mixed Result High Mean_Extirpated", "Rselected All Crashed Low Mean_Extirpated", "Rselected All Crashed MedLow Mean_Extirpated", "Rselected All Crashed Med Mean_Extirpated", "Rselected All Crashed MedHigh Mean_Extirpated", "Rselected All Crashed High Mean_Extirpated"))) %>% 
  ggplot(aes(x = xcol, y = MeanYears)) + 
  geom_boxplot() +
  ggtitle("Mean Years to K or Crash by NaiveLearn") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90))

# Save the plot
ggsave(filename = paste0(box_fldr, "/GeneralPlots/Boxplot_YearsToKcrash_ByNaiveLearn.png"), width = 7, height = 5.25, units = "in")


# Boxplot of Mean Years to Carrying capacity by Vital rate and Vertical Transfer level
maxYr %>% 
  select(Scenario, Mean_Survived, Mean_Extirpated, Vital, VertTrans, EndResult) %>% 
  pivot_longer(cols = c(Mean_Survived, Mean_Extirpated), names_to = "Param", values_to = "MeanYears", values_drop_na = TRUE) %>% 
  mutate(xcol = paste(Vital, EndResult, VertTrans, Param)) %>% 
  mutate(xcol = ordered(xcol, c("Kselected All Survived False Mean_Survived", "Kselected All Survived True Mean_Survived", "Kselected Mixed Result False Mean_Survived", "Kselected Mixed Result False Mean_Extirpated", "Kselected Mixed Result True Mean_Survived", "Kselected Mixed Result True Mean_Extirpated", "Rselected Mixed Result False Mean_Survived", "Rselected Mixed Result False Mean_Extirpated", "Rselected Mixed Result True Mean_Survived", "Rselected Mixed Result True Mean_Extirpated", "Rselected All Crashed False Mean_Extirpated", "Rselected All Crashed True Mean_Extirpated"))) %>% 
  ggplot(aes(x = xcol, y = MeanYears)) + 
  geom_boxplot() +
  ggtitle("Mean Years to K or Crash by Vertical Transfer") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90))

# Save the plot
ggsave(filename = paste0(box_fldr, "/GeneralPlots/Boxplot_YearsToKcrash_ByVertTrans.png"), width = 7, height = 5.25, units = "in")


#### Populations Size ####
scen<- data.frame(Scenario = unique(sumDat$Scenario))
tmpScen<- data.frame()
for(i in 1:nrow(scen)){
  tmpScen<- rbind(tmpScen, data.frame(Scenario = scen$Scenario[i], getModelParams(scen$Scenario[i])))
}

scen<- scen %>% 
  left_join(tmpScen, by = "Scenario")

if(!dir.exists(paste0(box_fldr, "/PopulationSize"))){
  dir.create(paste0(box_fldr, "/PopulationSize"))
}

for(i in 1:length(scen)){
  sumDat %>% 
    filter(Scenario == scen$Scenario[i]) %>% 
    ggplot(aes(x = yr, y = pop.size_Mean)) +
    geom_ribbon(aes(ymin = pop.size_Q03, ymax = pop.size_Q97), fill = "grey70") +
    geom_line() +
    xlim(0, 50) +
    ylim(0, 700) +
    ggtitle(paste0("PopSize: Vital-", scen$Vital[i], "; Bold-", scen$Boldness[i], "; Lambda-", scen$PoisLambda[i], "; \n InfoTran-", scen$InfoTransfer[i], "; InfoDeath-", scen$InfoDeath[i], "; Naive-", scen$NaiveLearn[i], "; Vert-", scen$VertTrans[i])) +
    theme(plot.title = element_text(hjust = 0.5, size = 8)) 
  # Save the file
  ggsave(filename = paste0(box_fldr, "/PopulationSize/Vital-", scen$Vital[i], "_Bold-", scen$Boldness[i], "_Lambda-", scen$PoisLambda[i], "_InfoTran-", scen$InfoTransfer[i], "_InfoDeath-", scen$InfoDeath[i], "_Naive-", scen$NaiveLearn[i], "_Vert-", scen$VertTrans[i], ".png"), width = 7, height = 5.25, units = "in", device = "png")
    
}


#### Births ####
if(!dir.exists(paste0(box_fldr, "/Births"))){
  dir.create(paste0(box_fldr, "/Births"))
}

for(i in 1:length(scen)){
  sumDat %>% 
    filter(Scenario == scen$Scenario[i]) %>% 
    ggplot(aes(x = yr, y = births_Mean)) +
    geom_ribbon(aes(ymin = births_Q03, ymax = births_Q97), fill = "grey70") +
    geom_line() +
    xlim(0, 50) +
    ylim(0, 650) +
    ggtitle(paste0("Births: Vital-", scen$Vital[i], "; Bold-", scen$Boldness[i], "; Lambda-", scen$PoisLambda[i], "; \n InfoTran-", scen$InfoTransfer[i], "; InfoDeath-", scen$InfoDeath[i], "; Naive-", scen$NaiveLearn[i], "; Vert-", scen$VertTrans[i])) +
    theme(plot.title = element_text(hjust = 0.5, size = 8)) 
  # Save the file
  ggsave(filename = paste0(box_fldr, "/Births/Vital-", scen$Vital[i], "_Bold-", scen$Boldness[i], "_Lambda-", scen$PoisLambda[i], "_InfoTran-", scen$InfoTransfer[i], "_InfoDeath-", scen$InfoDeath[i], "_Naive-", scen$NaiveLearn[i], "_Vert-", scen$VertTrans[i], ".png"), width = 7, height = 5.25, units = "in", device = "png")
  
}

#### Deaths ####
if(!dir.exists(paste0(box_fldr, "/Deaths"))){
  dir.create(paste0(box_fldr, "/Deaths"))
}

for(i in 1:length(scen)){
  sumDat %>% 
    filter(Scenario == scen$Scenario[i]) %>% 
    ggplot(aes(x = yr, y = deaths_Mean)) +
    geom_ribbon(aes(ymin = deaths_Q03, ymax = deaths_Q97), fill = "grey70") +
    geom_line() +
    xlim(0, 50) +
    ylim(0, 350) +
    ggtitle(paste0("Deaths: Vital-", scen$Vital[i], "; Bold-", scen$Boldness[i], "; Lambda-", scen$PoisLambda[i], "; \n InfoTran-", scen$InfoTransfer[i], "; InfoDeath-", scen$InfoDeath[i], "; Naive-", scen$NaiveLearn[i], "; Vert-", scen$VertTrans[i])) +
    theme(plot.title = element_text(hjust = 0.5, size = 8)) 
  # Save the file
  ggsave(filename = paste0(box_fldr, "/Deaths/Vital-", scen$Vital[i], "_Bold-", scen$Boldness[i], "_Lambda-", scen$PoisLambda[i], "_InfoTran-", scen$InfoTransfer[i], "_InfoDeath-", scen$InfoDeath[i], "_Naive-", scen$NaiveLearn[i], "_Vert-", scen$VertTrans[i], ".png"), width = 7, height = 5.25, units = "in", device = "png")
  
}

#### Fraction Informed ####
if(!dir.exists(paste0(box_fldr, "/FractionInformed"))){
  dir.create(paste0(box_fldr, "/FractionInformed"))
}

for(i in 1:length(scen)){
  sumDat %>% 
    filter(Scenario == scen$Scenario[i]) %>% 
    ggplot(aes(x = yr, y = frac.informed_Mean)) +
    geom_ribbon(aes(ymin = frac.informed_Q03, ymax = frac.informed_Q97), fill = "grey70") +
    geom_line() +
    xlim(0, 50) +
    ylim(0, 1) +
    ggtitle(paste0("Fraction Informed: Vital-", scen$Vital[i], "; Bold-", scen$Boldness[i], "; Lambda-", scen$PoisLambda[i], "; \n InfoTran-", scen$InfoTransfer[i], "; InfoDeath-", scen$InfoDeath[i], "; Naive-", scen$NaiveLearn[i], "; Vert-", scen$VertTrans[i])) +
    theme(plot.title = element_text(hjust = 0.5, size = 8)) 
  # Save the file
  ggsave(filename = paste0(box_fldr, "/FractionInformed/Vital-", scen$Vital[i], "_Bold-", scen$Boldness[i], "_Lambda-", scen$PoisLambda[i], "_InfoTran-", scen$InfoTransfer[i], "_InfoDeath-", scen$InfoDeath[i], "_Naive-", scen$NaiveLearn[i], "_Vert-", scen$VertTrans[i], ".png"), width = 7, height = 5.25, units = "in", device = "png")
  
}


#### Social Learning ####
if(!dir.exists(paste0(box_fldr, "/SocialLearning"))){
  dir.create(paste0(box_fldr, "/SocialLearning"))
}

for(i in 1:length(scen)){
  sumDat %>% 
    filter(Scenario == scen$Scenario[i]) %>% 
    ggplot(aes(x = yr, y = num.socialLearn_Mean)) +
    geom_ribbon(aes(ymin = num.socialLearn_Q03, ymax = num.socialLearn_Q97), fill = "grey70") +
    geom_line() +
    xlim(0, 50) +
    ylim(0, 350) +
    ggtitle(paste0("Social Learning: Vital-", scen$Vital[i], "; Bold-", scen$Boldness[i], "; Lambda-", scen$PoisLambda[i], "; \n InfoTran-", scen$InfoTransfer[i], "; InfoDeath-", scen$InfoDeath[i], "; Naive-", scen$NaiveLearn[i], "; Vert-", scen$VertTrans[i])) +
    theme(plot.title = element_text(hjust = 0.5, size = 8)) 
  # Save the file
  ggsave(filename = paste0(box_fldr, "/SocialLearning/Vital-", scen$Vital[i], "_Bold-", scen$Boldness[i], "_Lambda-", scen$PoisLambda[i], "_InfoTran-", scen$InfoTransfer[i], "_InfoDeath-", scen$InfoDeath[i], "_Naive-", scen$NaiveLearn[i], "_Vert-", scen$VertTrans[i], ".png"), width = 7, height = 5.25, units = "in", device = "png")
  
}

#### Asocial Learning ####
if(!dir.exists(paste0(box_fldr, "/AsocialLearning"))){
  dir.create(paste0(box_fldr, "/AsocialLearning"))
}

for(i in 1:length(scen)){
  sumDat %>% 
    filter(Scenario == scen$Scenario[i]) %>% 
    ggplot(aes(x = yr, y = num.asocialLearn_Mean)) +
    geom_ribbon(aes(ymin = num.asocialLearn_Q03, ymax = num.asocialLearn_Q97), fill = "grey70") +
    geom_line() +
    xlim(0, 50) +
    ylim(0, 350) +
    ggtitle(paste0("Asocial Learning: Vital-", scen$Vital[i], "; Bold-", scen$Boldness[i], "; Lambda-", scen$PoisLambda[i], "; \n InfoTran-", scen$InfoTransfer[i], "; InfoDeath-", scen$InfoDeath[i], "; Naive-", scen$NaiveLearn[i], "; Vert-", scen$VertTrans[i])) +
    theme(plot.title = element_text(hjust = 0.5, size = 8)) 
  # Save the file
  ggsave(filename = paste0(box_fldr, "/AsocialLearning/Vital-", scen$Vital[i], "_Bold-", scen$Boldness[i], "_Lambda-", scen$PoisLambda[i], "_InfoTran-", scen$InfoTransfer[i], "_InfoDeath-", scen$InfoDeath[i], "_Naive-", scen$NaiveLearn[i], "_Vert-", scen$VertTrans[i], ".png"), width = 7, height = 5.25, units = "in", device = "png")
  
}

#### Total Interactions ####
if(!dir.exists(paste0(box_fldr, "/TotalInteractions"))){
  dir.create(paste0(box_fldr, "/TotalInteractions"))
}

for(i in 1:length(scen)){
  sumDat %>% 
    filter(Scenario == scen$Scenario[i]) %>% 
    ggplot(aes(x = yr, y = total.num.interactions_Mean)) +
    geom_ribbon(aes(ymin = total.num.interactions_Q03, ymax = total.num.interactions_Q97), fill = "grey70") +
    geom_line() +
    xlim(0, 50) +
    ylim(0, 201000) +
    ggtitle(paste0("Total Interactions: Vital-", scen$Vital[i], "; Bold-", scen$Boldness[i], "; Lambda-", scen$PoisLambda[i], "; \n InfoTran-", scen$InfoTransfer[i], "; InfoDeath-", scen$InfoDeath[i], "; Naive-", scen$NaiveLearn[i], "; Vert-", scen$VertTrans[i])) +
    theme(plot.title = element_text(hjust = 0.5, size = 8)) 
  # Save the file
  ggsave(filename = paste0(box_fldr, "/TotalInteractions/Vital-", scen$Vital[i], "_Bold-", scen$Boldness[i], "_Lambda-", scen$PoisLambda[i], "_InfoTran-", scen$InfoTransfer[i], "_InfoDeath-", scen$InfoDeath[i], "_Naive-", scen$NaiveLearn[i], "_Vert-", scen$VertTrans[i], ".png"), width = 7, height = 5.25, units = "in", device = "png")
  
}

#### Median Age ####
if(!dir.exists(paste0(box_fldr, "/MedianAge"))){
  dir.create(paste0(box_fldr, "/MedianAge"))
}

for(i in 1:length(scen)){
  sumDat %>% 
    filter(Scenario == scen$Scenario[i]) %>% 
    ggplot(aes(x = yr, y = med.age_Mean)) +
    geom_ribbon(aes(ymin = med.age_Q03, ymax = med.age_Q97), fill = "grey70") +
    geom_line() +
    xlim(0, 50) +
    ylim(0, 4) +
    ggtitle(paste0("Median Age: Vital-", scen$Vital[i], "; Bold-", scen$Boldness[i], "; Lambda-", scen$PoisLambda[i], "; \n InfoTran-", scen$InfoTransfer[i], "; InfoDeath-", scen$InfoDeath[i], "; Naive-", scen$NaiveLearn[i], "; Vert-", scen$VertTrans[i])) +
    theme(plot.title = element_text(hjust = 0.5, size = 8)) 
  # Save the file
  ggsave(filename = paste0(box_fldr, "/MedianAge/Vital-", scen$Vital[i], "_Bold-", scen$Boldness[i], "_Lambda-", scen$PoisLambda[i], "_InfoTran-", scen$InfoTransfer[i], "_InfoDeath-", scen$InfoDeath[i], "_Naive-", scen$NaiveLearn[i], "_Vert-", scen$VertTrans[i], ".png"), width = 7, height = 5.25, units = "in", device = "png")
  
}

