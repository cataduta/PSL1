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

install.packages("tidyverse")
install.packages("dplyr")
install.packages("magrittr")
install.packages("readxl")
install.packages("lme4")
install.packages("gridExtra")
install.packages("DHARMa")
install.packages("broom.mixed")
install.packages("gdata")
install.packages("car")
install.packages("phia")
install.packages("emmeans")
install.packages("simr")
install.packages("pbkrtest")
install.packages("lmerTest")
install.packages("multcomp")
install.packages("nlme")
install.packages("ggeffects")
install.packages("nortest")
install.packages("openxlsx")
install.packages("ggplot2")
install.packages('systemfonts')

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


#### Import from Excel
# R only reads forward slashes "/", so you may have to replace backward slashes "\"
Data <- read_excel("C:/Users/cataduta/OneDrive - Vrije Universiteit Brussel/PhD/Experiments/PSL1_Perceptual sequence learning - Garvert task/Analyses_R/New_analyses_nothing_removed/GLE/52_participants_onlyfiltersonBlock.xlsx", skip = 0)

#### To save as R dataset
# save(Data, file = "Data.RDA")


### Rename
#Selected_Columns <- Data %>%

### Convert data types; 
# numbers are imported as text by default
Selected_Columns <- Data %>%
  mutate(Subject = as.character(Subject),
         Block_number = as.numeric(Block_number),
         Block_type = as.character(Block_type),
         Trial = as.numeric(Trial),
         Obey_sequence = as.character(Obey_sequence),
         RT = as.numeric(RT),
         Acc = as.character(Acc),
         Explicit_knowledge_acc = as.character(Explicit_knowledge_acc),
         Communicability = as.numeric(Communicability))


## Create a subdataset  containing only Learning Phase Blocks (i.e., Blocks 1-8)
Learning_Phase <- Selected_Columns %>%
  filter(Block_number < 9)

## Center the Block_number in Learning Phase
Learning_Phase$Block_number_centered <- Learning_Phase$Block_number - mean(Learning_Phase$Block_number, na.rm = TRUE)

### Filter out incorrect responses, practice trials and RTs lower than 100
Learning_Phase_Filtered <- Learning_Phase %>%
  # filter(Block_type != "PracTrialList") %>%
  filter(RT > 100)%>%
  filter(RT < 1000)%>%
  filter (Acc == 1)%>%
  filter(Communicability != "NULL") %>%
  filter(Communicability != "?")

### Filter out unwanted participants (in this case 2 older pps for age effects)
# Inspect the structure of the dataset
str(Learning_Phase_Filtered)
# Define the subjects to remove
subjects_to_remove <- c("104", "150", "107", "123")
# Remove the specified subjects
Learning_Phase_Filtered <- Learning_Phase_Filtered %>%
  filter(!Subject %in% subjects_to_remove)
# Verify the subjects have been removed
unique(Learning_Phase_Filtered$Subject)


# Rename again

Learning_Phase <- Learning_Phase_Filtered


# Write the dataset to an Excel file

# write.xlsx(Learning_Phase, file = "Learning_Phase_data.xlsx")



# Effect of Block number

# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Learning_Phase_gamma_log_1 <- glmer(RT ~ Block_number + (1|Subject), data = Learning_Phase, family = Gamma(link = "log"))
# GLMER_model_Learning_Phase_gamma_log_1
# car::Anova(GLMER_model_Learning_Phase_gamma_log_1, type=3)
# 
# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Learning_Phase_invgauss_log_1 <- glmer(RT ~ Block_number + (1|Subject), data = Learning_Phase, family = inverse.gaussian(link = "log"))
# GLMER_model_Learning_Phase_invgauss_log_1
# car::Anova(GLMER_model_Learning_Phase_invgauss_log_1, type=3)
# 
# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Learning_Phase_gamma_identity_1 <- glmer(RT ~ Block_number + (1|Subject), data = Learning_Phase, family = Gamma(link = "identity"))
# GLMER_model_Learning_Phase_gamma_identity_1
# car::Anova(GLMER_model_Learning_Phase_gamma_identity_1, type=3)
# 
# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Learning_Phase_invgauss_identity_1 <- glmer(RT ~ Block_number + (1|Subject), data = Learning_Phase, family = inverse.gaussian(link = "identity"))
# GLMER_model_Learning_Phase_invgauss_identity_1
# car::Anova(GLMER_model_Learning_Phase_invgauss_identity_1, type=3)
# 
# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Learning_Phase_gamma_log_2 <- glmer(RT ~ Block_number + (1 + Block_number|Subject), data = Learning_Phase, family = Gamma(link = "log"))
# GLMER_model_Learning_Phase_invgauss_log_2
# car::Anova(GLMER_model_Learning_Phase_gamma_log_2, type=3)
# 
# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Learning_Phase_invgauss_log_2 <- glmer(RT ~ Block_number + (1 + Block_number|Subject), data = Learning_Phase, family = inverse.gaussian(link = "log"))
# GLMER_model_Learning_Phase_invgauss_log_2
# car::Anova(GLMER_model_Learning_Phase_invgauss_log_2, type=3)

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
GLMER_model_Learning_Phase_gamma_identity_2 <- glmer(RT ~ Block_number + (1 |Subject), data = Learning_Phase, family = Gamma(link = "identity"))
GLMER_model_Learning_Phase_gamma_identity_2
car::Anova(GLMER_model_Learning_Phase_gamma_identity_2, type=3)

# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Learning_Phase_invgauss_identity_2 <- glmer(RT ~ Block_number + (1 + Block_number|Subject), data = Learning_Phase, family = inverse.gaussian(link = "identity"))
# GLMER_model_Learning_Phase_invgauss_identity_2
# car::Anova(GLMER_model_Learning_Phase_invgauss_identity_2, type=3)


# anova(GLMER_model_Learning_Phase_gamma_log_1,
#       GLMER_model_Learning_Phase_invgauss_log_1,
#       GLMER_model_Learning_Phase_gamma_identity_1,
#       GLMER_model_Learning_Phase_invgauss_identity_1,
#       GLMER_model_Learning_Phase_gamma_log_2,
#       GLMER_model_Learning_Phase_invgauss_log_2,
#       GLMER_model_Learning_Phase_gamma_identity_2,
#       GLMER_model_Learning_Phase_invgauss_identity_2)

# This disables the feature in the emmeans package whereby results are displayed in reasonable precision relative to their standard errors!
emm_options(opt.digits = FALSE)
# emmeans automatically adjusts p-values using the Tukey method (!)
# The Tukey correction method controls the overall error rate for all pairwise comparisons, so it is not necessary to apply additional corrections for follow-up contrasts (!)
# emmeans can back-transform the transformed emmeans to the response (RT) type, by using type = "response"

options(max.print=10000)


GLMER_model_Learning_Phase_gamma_identity_2_emm1 <- emmeans(GLMER_model_Learning_Phase_gamma_identity_2, pairwise ~ Block_number, type = "response")
GLMER_model_Learning_Phase_gamma_identity_2_emm1


# Create graph for Block_number

emmip(GLMER_model_Learning_Phase_gamma_identity_2_emm1, ~ Block_number, type = "response", CIs = TRUE)


emmip(GLMER_model_Learning_Phase_gamma_identity_2_emm1, ~ Block_number, 
      type = "response", 
      main = "Estimated Marginal Means of Reaction Time", 
      xlab = "Block number", 
      ylab = "Estimated Reaction Time",
      CIs = TRUE)

emm <- GLMER_model_Learning_Phase_gamma_identity_2_emm1
emm


# Extract estimated marginal means and confidence intervals
emm_data <- as.data.frame(emmeans(GLMER_model_Learning_Phase_gamma_identity_2, pairwise ~ Block_number, type = "response"))

# Check the structure of the extracted data
str(emm_data)

# Filter to keep only blocks 1-8
emm_data <- emm_data %>%
  filter(Block_number %in% c("1", "2", "3", "4", "5", "6", "7", "8"))

# Calculate SE from the confidence intervals
emm_data <- emm_data %>%
  mutate(SE = (asymp.UCL - asymp.LCL) / (2 * qnorm(0.975)))  # qnorm(0.975) gives the critical value for 95% CI


# Convert Block_number to numeric for trend line
emm_data <- emm_data %>%
  mutate(Block_number_numeric = as.numeric(as.character(Block_number)))  # Ensure Block_number is numeric

# Create the bar plot with error bars and a trend line
ggplot(data = emm_data, aes(x = Block_number_numeric, y = emmean)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7, fill = "salmon") +  # Bars
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),   # Error bars
                position = position_dodge(0.9), width = 0.2) +
  geom_smooth(method = "lm", aes(group = 1), color = "black", linetype = "dotted", se = FALSE) +  # Add a dotted black linear trend line
  labs(x = "Block Number", y = "Estimated Marginal Mean (emmean)", 
       title = "Estimated Marginal Means of Reaction Time with Trend Line") +  # Labels
  theme_minimal() +  # Use a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank()) +  # Remove minor gridlines
  coord_cartesian(ylim = c(600, 750))  # Set y-axis limits from 600 to 750 without removing data




############# To save as .png (copy/paste code from above, but change the first and last lines) 

png("my_plot_apa_GLE.png", width = 750, height = 600, res = 100)  # Set file name and dimensions
ggplot(data = emm_data, aes(x = Block_number_numeric, y = emmean)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7, fill = "salmon") +  # Bars
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),   # Error bars
                position = position_dodge(0.9), width = 0.2) +
  geom_smooth(method = "lm", aes(group = 1), color = "black", linetype = "dotted", se = FALSE) +  # Add a dotted black linear trend line
  labs(x = "Block Number", y = "Estimated Marginal Mean (emmean)", 
       title = "Estimated Marginal Means of Reaction Time with Trend Line") +  # Labels
  theme_minimal() +  # Use a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank()) +  # Remove minor gridlines
  coord_cartesian(ylim = c(600, 750))  # Set y-axis limits from 600 to 750 without removing data
dev.off()  # Close the PNG device
















