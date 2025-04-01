###############################################################################
###############################################################################
###############################################################################
###############################################################################
# TEMPLATE FOR DATASET PREPARATION
###############################################################################
###############################################################################
###############################################################################
###############################################################################

#### Install packages

# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("magrittr")
# install.packages("readxl")
# install.packages("lme4")
# install.packages("gridExtra")
# install.packages("DHARMa")
# install.packages("broom.mixed")
# install.packages("gdata")
# install.packages("car")
# install.packages("phia")
# install.packages("emmeans")
# install.packages("simr")
# install.packages("pbkrtest")
# install.packages("lmerTest")
# install.packages("multcomp")
# install.packages("nlme")
# install.packages("ggeffects")
# install.packages("nortest")
# install.packages("openxlsx")
# install.packages("ggplot2")
# install.packages("systemfonts")
 install.packages("ggpubr") 
install.packages("margins")
install.packages("effects")

#### Load all required packages

library(tidyverse)
library(dplyr)
library(magrittr)
library(readxl)
library(lme4)
library(gridExtra)
library(DHARMa)
library(broom.mixed)
library(gdata)
library(car)
library(phia)
library(emmeans)
library(simr)
library(pbkrtest)
library(lmerTest)
library(multcomp)
library(nlme)
library(ggeffects)
library(nortest)
library(openxlsx)
library(ggplot2)
library(systemfonts)
library(ggpubr)
library(margins)
library(effects)

#### Import from Excel
# R only reads forward slashes "/", so you may have to replace backward slashes "\"
Data <- read_excel("C:/Users/cataduta/OneDrive - Vrije Universiteit Brussel/PhD/Experiments/PSL1_Perceptual sequence learning - Garvert task/Analyses_R/New_analyses_nothing_removed/52_participants_onlyfiltersonBlock.xlsx", skip = 0)

#### To save as R dataset
# save(Data, file = "Data.RDA")


### Rename
#Selected_Columns <- Data %>%

### Convert data types; 
# numbers are imported as text by default
Selected_Columns <- Data %>%
  mutate(Subject = as.character(Subject),
         Block_type = as.factor(Block_type),
         Trial = as.numeric(Trial),
         Obey_sequence = as.character(Obey_sequence),
         RT = as.numeric(RT),
         Acc = as.character(Acc),
         Explicit_knowledge_acc = as.character(Explicit_knowledge_acc),
         Communicability = as.numeric(Communicability))

### Centre your Block_number variable before you remove any participants and trials


# Create a new variable 'TrialType' to distinguish between trial types
Selected_Columns$TrialType <- with(Selected_Columns, ifelse(Block_type == "RegularBlock", "RegularBlock",
                                                      ifelse(Obey_sequence == "Yes", "Randomblock_non-violation", "Randomblock_violation")))

head(Selected_Columns)  # View the first few rows to see the new column


Testing_Phase <- Selected_Columns %>%
  mutate(TrialType = as.factor(TrialType))



## Create a subdataset  containing only Learning Phase Blocks (i.e., Blocks 1-8)
Learning_Phase <- Selected_Columns %>%
  filter(Block_number < 9)

## Center the Block_number in Testing Phase
#Testing_Phase$Block_number_centered <- Testing_Phase$Block_number - mean(Testing_Phase$Block_number, na.rm = TRUE)

## Center the Block_number in Learning Phase
#Learning_Phase$Block_number_centered <- Learning_Phase$Block_number - mean(Learning_Phase$Block_number, na.rm = TRUE)


### Filter out incorrect responses, practice trials and RTs lower than 100
Testing_Phase_Filtered <- Testing_Phase %>%
  # filter(Block_type != "PracTrialList") %>%
  filter(RT > 100)%>%
  filter(RT < 1000)%>%
  filter (Acc == 1)%>%
  filter(Communicability != "NULL") %>%
  filter(Communicability != "?")

### Filter out unwanted participants (in this case 2 older pps for age effects)
# Inspect the structure of the dataset
str(Testing_Phase_Filtered)
# Define the subjects to remove
subjects_to_remove <- c("104", "150", "107", "123")
# Remove the specified subjects
Testing_Phase_Filtered <- Testing_Phase_Filtered %>%
  filter(!Subject %in% subjects_to_remove)
# Verify the subjects have been removed
unique(Testing_Phase_Filtered$Subject)


# Rename again

Testing_Phase <- Testing_Phase_Filtered



