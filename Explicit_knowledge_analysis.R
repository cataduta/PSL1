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
#install.packages("writexl")
install.packages("effects")


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
library(writexl)
library(effects)

#### Import from Excel
# R only reads forward slashes "/", so you may have to replace backward slashes "\"
Data <- read_excel("C:/Users/cataduta/OneDrive - Vrije Universiteit Brussel/PhD/Experiments/PSL1_Perceptual sequence learning - Garvert task/Analyses_R/New_analyses_nothing_removed/52_participants_onlyfiltersonBlock.xlsx", skip = 0)

### Convert data types; 
# numbers are imported as text by default
Selected_Columns <- Data %>%
  mutate(Subject = as.character(Subject),
         Block_type = as.factor(Block_type),
         Trial = as.numeric(Trial),
         Obey_sequence = as.character(Obey_sequence),
         RT = as.numeric(RT),
         Acc = as.character(Acc),
         Explicit_knowledge_acc = as.numeric(Explicit_knowledge_acc),
         Communicability = as.factor(Communicability))


### Filter out unwanted participants (in this case 2 older pps for age effects)
# Inspect the structure of the dataset
str(Selected_Columns)
# Define the subjects to remove
subjects_to_remove <- c("104", "150", "107", "123")
# Remove the specified subjects
Selected_Columns <- Selected_Columns %>%
  filter(!Subject %in% subjects_to_remove)
# Verify the subjects have been removed
unique(Selected_Columns$Subject)

### To remove last 20 trials of explicit knowledge
Selected_Columns <- Selected_Columns %>%
  filter(!(Block_number == 19 & Trial > 20))


# Calculate average accuracy score per participant
Explicit_knowledge_score <- Selected_Columns %>%
  group_by(Subject) %>%
  summarise(
    average_accuracy = mean(Explicit_knowledge_acc, na.rm = TRUE),
    .groups = 'drop'
    )

# Check the resulting scores
head(Explicit_knowledge_score)

# Scale the average accuracy score to 0-100
Explicit_knowledge_score <- Explicit_knowledge_score %>%
  mutate(Explicit_knowledge_score = average_accuracy * 100)

# Check the scaled scores
head(Explicit_knowledge_score)

# Merge the knowledge scores with the original trial-level data
Selected_Columns_with_Score <- Selected_Columns %>%
  left_join(Explicit_knowledge_score, by = "Subject")

# Check the updated data
head(Selected_Columns_with_Score)

Selected_Columns_with_Score <- Selected_Columns_with_Score %>%
  mutate(Explicit_knowledge_score = as.numeric(Explicit_knowledge_score))

## Create a subdataset  containing only Testing Phase Blocks (i.e., Blocks 9-18)
Testing_Phase <- Selected_Columns_with_Score %>%
  filter(Block_number > 8) %>%
  filter(Block_number != 19)


### Filter out incorrect responses, practice trials and RTs lower than 100
Testing_Phase_Filtered <- Testing_Phase %>%
  filter(RT > 100)%>%
  filter(RT < 1000)%>%
  filter (Acc == 1) %>%
  filter(Communicability != "NULL") %>%
  filter(Communicability != "?")



## Center Explicit knowledge
#Testing_Phase_Filtered$Explicit_knowledge_centered <- Testing_Phase_Filtered$Explicit_knowledge_score - mean(Testing_Phase_Filtered$Explicit_knowledge_score, na.rm = TRUE)

## Center the Block_number in Testing Phase
#Testing_Phase_Filtered$Block_number_centered <- Testing_Phase_Filtered$Block_number - mean(Testing_Phase_Filtered$Block_number, na.rm = TRUE)

# Rename again

Testing_Phase <- Testing_Phase_Filtered

Testing_Phase <- Testing_Phase %>%
  mutate(Explicit_knowledge_score = as.numeric(Explicit_knowledge_score))



# Check unique values in the filtered data
unique_filtered_scores <- unique(Testing_Phase$Explicit_knowledge_score)

# Print unique filtered scores
print(unique_filtered_scores)



############################# Filter to keep only participants with Explicit_knowledge_score of 50 or lower
  filtered_data_under_or_equal_50 <- Testing_Phase %>%
  filter(Explicit_knowledge_score <= 50)

# View the filtered data
 print(filtered_data_under_or_equal_50)

 # Check unique values in the filtered_data_under_or_equal_50
 unique_filtered_scores_under_50 <- unique(filtered_data_under_or_equal_50$Explicit_knowledge_score)

 # Print unique filtered scores
print(unique_filtered_scores_under_50)


############# One-sample t-test#####################

# Perform one-sample t-test on 'Explicit_knowledge_score' assuming the null hypothesis mean is 50
 t_test_result <- t.test(Testing_Phase$Explicit_knowledge_score, mu = 50)

 # Print the t-test result
 print(t_test_result)

# Calculate the standard deviation for Explicit_knowledge_score
sd(Testing_Phase$Explicit_knowledge_score)


########################################################


##################### GLMM on under 50% responders

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
GLMER_model_Testing_Phase_gamma_identity_6 <- glmer(RT ~ Block_number*Block_type + (1|Subject), data = filtered_data_under_or_equal_50, family = Gamma(link = "identity"))
GLMER_model_Testing_Phase_gamma_identity_6
car::Anova(GLMER_model_Testing_Phase_gamma_identity_6, type=3)





###############################################################################
###############################################################################
###############################################################################
###############################################################################
# TEMPLATE FOR MIXED MODEL ANALYSES: SEQUENCE-SPECIFIC LEARNING
###############################################################################
###############################################################################
###############################################################################
###############################################################################





# Effect of Explicit knowledge/block type/block number 

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
GLMER_model_Testing_Phase_gamma_identity_1 <- glmer(RT ~ Explicit_knowledge_score*Block_type + (1|Subject), data = Testing_Phase, family = Gamma(link = "identity"))
GLMER_model_Testing_Phase_gamma_identity_1
car::Anova(GLMER_model_Testing_Phase_gamma_identity_1, type=3)

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
GLMER_model_Testing_Phase_gamma_identity_2 <- glmer(RT ~ Explicit_knowledge_score + (1|Subject), data = Testing_Phase, family = Gamma(link = "identity"))
GLMER_model_Testing_Phase_gamma_identity_2
car::Anova(GLMER_model_Testing_Phase_gamma_identity_2, type=3)

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
GLMER_model_Testing_Phase_gamma_identity_3 <- glmer(RT ~ Explicit_knowledge_score*Block_type*Block_number + (1|Subject), data = Testing_Phase, family = Gamma(link = "identity"))
GLMER_model_Testing_Phase_gamma_identity_3
car::Anova(GLMER_model_Testing_Phase_gamma_identity_3, type=3)


ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
GLMER_model_Testing_Phase_gamma_identity_4 <- glmer(RT ~ Explicit_knowledge_score*Block_number + (1|Subject), data = Testing_Phase, family = Gamma(link = "identity"))
GLMER_model_Testing_Phase_gamma_identity_4
car::Anova(GLMER_model_Testing_Phase_gamma_identity_4, type=3)

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
GLMER_model_Testing_Phase_gamma_identity_5 <- glmer(RT ~ Explicit_knowledge_score*TrialType + (1|Subject), data = Testing_Phase, family = Gamma(link = "identity"))
GLMER_model_Testing_Phase_gamma_identity_5
car::Anova(GLMER_model_Testing_Phase_gamma_identity_5, type=3)

anova(GLMER_model_Testing_Phase_gamma_identity_1,
      GLMER_model_Testing_Phase_gamma_identity_2,
      GLMER_model_Testing_Phase_gamma_identity_3,
      GLMER_model_Testing_Phase_gamma_identity_4,
      GLMER_model_Testing_Phase_gamma_identity_5)

emm_options(opt.digits = FALSE)
# emmeans automatically adjusts p-values using the Tukey method (!)
# The Tukey correction method controls the overall error rate for all pairwise comparisons, so it is not necessary to apply additional corrections for follow-up contrasts (!)
# emmeans can back-transform the transformed emmeans to the response (RT) type, by using type = "response"

options(max.print=10000)

GLMER_model_Testing_Phase_gamma_identity_1_emm1 <- emmeans(GLMER_model_Testing_Phase_gamma_identity_1, pairwise ~ Block_type, type = "response")
GLMER_model_Testing_Phase_gamma_identity_1_emm1

GLMER_model_Testing_Phase_gamma_identity_1_emm2 <- emmeans(GLMER_model_Testing_Phase_gamma_identity_2, pairwise ~ Explicit_knowledge_score, type = "response")
GLMER_model_Testing_Phase_gamma_identity_1_emm2

GLMER_model_Testing_Phase_gamma_identity_1_emm3 <- emmeans(GLMER_model_Testing_Phase_gamma_identity_3, pairwise ~ Explicit_knowledge_score*Block_type*Block_number, type = "response")
GLMER_model_Testing_Phase_gamma_identity_1_emm3



################################creating a proper scatterplot#####################

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
GLMER_model_Testing_Phase_gamma_identity_1 <- glmer(RT ~ Explicit_knowledge_score*Block_type + (1|Subject), data = Testing_Phase, family = Gamma(link = "identity"))
GLMER_model_Testing_Phase_gamma_identity_1
car::Anova(GLMER_model_Testing_Phase_gamma_identity_1, type=3)

summary(GLMER_model_Testing_Phase_gamma_identity_1)

# Get the predicted values from the model
predicted_values <- as.data.frame(Effect(c("Explicit_knowledge_score", "Block_type"), GLMER_model_Testing_Phase_gamma_identity_1))

# Customizing the plot further, with facets and color adjustments
ggplot(predicted_values, aes(x = Explicit_knowledge_score, y = fit, color = Block_type)) +
  geom_point(size = 2) +  # Increase point size
  geom_smooth(method = "lm", se = TRUE) +  # Add trend line
  facet_wrap(~Block_type) +  # Create separate plots for each block type
  labs(title = "Reaction Time vs Explicit Knowledge by Block Type",
       x = "Explicit Knowledge",
       y = "Predicted Reaction Time (RT)") +
  scale_color_manual(values = c("blue", "red")) +  # Custom colors
  theme_minimal()

# Summarize the data
summarized_data <- Testing_Phase %>%
  group_by(Subject) %>%
  summarize(
    Mean_Explicit_Knowledge = mean(Explicit_knowledge_score, na.rm = TRUE),
    Mean_RT = mean(RT, na.rm = TRUE)
  )

  
# Create the scatterplot with average values
scatter_plot <- ggplot(summarized_data, aes(x = Mean_Explicit_Knowledge, y = Mean_RT)) +
  geom_point(size = 4, color = "blue", alpha = 0.7) +  # Points for each subject
  
  # Force a strict linear trend line using lm with a simple formula
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", linetype = "solid") +  # Add a straight trendline (linear model)
  
  labs(x = "Average Explicit Knowledge Score",
       y = "Average Reaction Time (RT) in ms") +  # Label the axes
  theme_minimal() +  # Clean theme
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text.x = element_text(size = 12),  # Optional: customize x-axis text size
        axis.text.y = element_text(size = 12)) +  # Optional: customize y-axis text size
  coord_cartesian(ylim = c(NA, 760))  # Set the upper limit of the y-axis to 800 without removing data

print(scatter_plot)

  
  # Save the plot as a PNG file
  ggsave("scatter_plot.png", plot = scatter_plot, width = 8, height = 6, dpi = 300)

######################################

Testing_Phase <- data.frame(
  Subject = rep(1:48, each = 2),
  Block_type = rep(c("RandomBlock", "RegularBlock"), times = 48),
  Explicit_knowledge_centered = rnorm(40, mean = 50, sd = 10),  # Simulating scores
  emmean = c(rnorm(48, mean = 681.2302, sd = 6.906767),  # Simulating RandomBlock scores
             rnorm(48, mean = 668.0998, sd = 6.903803))  # Simulating RegularBlock scores
)

# Create a scatterplot with Explicit_knowledge_centered on the X-axis
scatter_plot <- ggplot(Testing_Phase, aes(x = Explicit_knowledge_score, y = emmean, color = as.factor(Subject))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +  # Add regression line
  labs(title = "Scatterplot of Estimated Marginal Means vs. Explicit Knowledge",
       x = "Explicit Knowledge (Centered)",
       y = "Estimated Mean (emmean)",
       color = "Participant") +
  theme_minimal()

print(scatter_plot)

##############################################################################


emmip(GLMER_model_Testing_Phase_gamma_identity_1_emm2, ~ Explicit_knowledge_centered, type = "response", CIs = TRUE)


# Extract estimated marginal means and confidence intervals for TrialType
emm_data_Explicit_knowledge_centered <- as_tibble(GLMER_model_Testing_Phase_gamma_identity_1_emm2)
emm_pred_Explicit_knowledge_centered <- predict(GLMER_model_Testing_Phase_gamma_identity_1_emm2, type = "response", interval = "confidence")

plot_data <- as.data.frame(emm_pred_Explicit_knowledge_centered)
plot_data

# Define data frame

plot_data <- data.frame(
  Explicit_knowledge_centered = c("Randomblock_non-violation", "Randomblock_violation", "RegularBlock"),
  emmean = c(683.4225, 680.1400, 668.1251),
  SE = c(1.539083, 2.253742, 2.122725),
  df = c(Inf, Inf, Inf),
  asymp.LCL = c(680.4060, 675.7228, 663.9647),
  asymp.UCL = c(686.4391, 684.5573, 672.2856)
)

# Convert TrialType to a factor to control the order and labels in the legend
plot_data$TrialType <- factor(plot_data$TrialType,
                              levels = c("Randomblock_non-violation", "Randomblock_violation", "RegularBlock"))

