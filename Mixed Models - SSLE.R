###############################################################################
###############################################################################
###############################################################################
###############################################################################
# TEMPLATE FOR MIXED MODEL ANALYSES: SEQUENCE-SPECIFIC LEARNING
###############################################################################
###############################################################################
###############################################################################
###############################################################################


###############################################################################
# Check distributions
###############################################################################

# Create a histogram of RT data
ggplot(Testing_Phase, aes(x = RT)) + 
  geom_histogram(binwidth = 100, color = "black", fill = "white") +
  labs(x = "RT", y = "Frequency", title = "Histogram of RT Data")

# Create a line graph of RT data
ggplot(Testing_Phase, aes(x = RT)) +
  geom_freqpoly(binwidth = 100, color = "black") +
  labs(x = "RT", y = "Frequency", title = "Frequency of RT Data")

# Create density plot with histogram and a regression line for Block_type
ggplot(Testing_Phase, aes(x = RT, fill = Block_type)) +
  geom_density(alpha = 0.5) +
  geom_histogram(aes(y = ..density..), binwidth = 50, color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(Testing_Phase$RT), sd = sd(Testing_Phase$RT)), color = "red", size = 1) +
  xlab("Stimulus RT (ms)") +
  ylab("Density") +
  ggtitle("Distribution of Stimulus RT by Block_type") +
  theme_bw()


# Create density plot with histogram and a regression line for Block_number
ggplot(Testing_Phase, aes(x = RT, fill = Block_number)) +
  geom_density(alpha = 0.5) +
  geom_histogram(aes(y = ..density..), binwidth = 50, color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(Testing_Phase$RT), sd = sd(Testing_Phase$RT)), color = "red", size = 1) +
  xlab("Stimulus RT (ms)") +
  ylab("Density") +
  ggtitle("Distribution of Stimulus RT by Block_number") +
  theme_bw()

# Check distribution using Shapiro-Wilk test

# shapiro.test(Testing_Phase$RT)

# Number of observations is > 5000, so: Check distribution using Anderson-Darling test

ad.test(Testing_Phase$RT)

# Strongly significant AD test implies that the data is not normally distributed


###############################################################################
# Let's design an LMM for this dataset and see how it fits the data by plotting the residuals
###############################################################################


# Fit the model
LMER_model_Testing_Phase <- lmer(RT ~ Block_type*Block_number + (1|Subject), data = Testing_Phase)
# View the model
LMER_model_Testing_Phase
# Perform ANOVA on the LMER model, Type 3 Wald Chi Square Test
car::Anova(LMER_model_Testing_Phase, type=3)


# Extract the residuals
resid <- residuals(LMER_model_Testing_Phase)

# Plot the histogram of residuals
ggplot(data.frame(resid), aes(x = resid)) +
  geom_histogram(binwidth = 10, color = "black", fill = "white") +
  labs(x = "Residuals", y = "Frequency", title = "Histogram of Residuals")

# Check distribution using Shapiro-Wilk test
# shapiro.test(resid)

# Number of observations is > 5000, so: Check distribution using Anderson-Darling test
ad.test(resid)

# Strongly significant AD test implies that the residuals are not normally distributed



###############################################################################
# Build GLMMs with whichever family and link function that make the most sense, theoretically.
###############################################################################

# Example dataset: continuous, positive response variable with skewness (long right tail); log-normal distribution
# Gamma and Inverse Gaussian families with log link function should, theoretically, provide the best fit for the data

# nAGQ determines how GLMER will integrate out the random effects when fitting the model
# nAGQ = 1: Laplace approximation is used
# nAGQ = 0: Less interaction with random effects and therefore less precise. Only use in case of highly complex models with convergence issues
# actual difference between nAGQ = 1 and nAGQ = 0 is often quite small
# https://stats.stackexchange.com/questions/544937/when-is-it-appropriate-to-set-nagq-0-in-glmer


# Effect of Block type

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
GLMER_model_Testing_Phase_gamma_identity_1 <- glmer(RT ~ Block_type + (1|Subject), data = Testing_Phase, family = Gamma(link = "identity"))
GLMER_model_Testing_Phase_gamma_identity_1
car::Anova(GLMER_model_Testing_Phase_gamma_identity_1, type=3)

#ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
#GLMER_model_Testing_Phase_invgauss_identity_1 <- glmer(RT ~ Block_type + (1|Subject), data = Testing_Phase, family = inverse.gaussian(link = "identity"))
#GLMER_model_Testing_Phase_invgauss_identity_1
#car::Anova(GLMER_model_Testing_Phase_invgauss_identity_1, type=3)

# # Random Intercept
# 
# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Testing_Phase_gamma_identity_1a <- glmer(RT ~ Block_type + (1 + Block_type|Subject), data = Testing_Phase, family = Gamma(link = "identity"))
# GLMER_model_Testing_Phase_gamma_identity_1a
# car::Anova(GLMER_model_Testing_Phase_gamma_identity_1a, type=3)


# Effect of Block number centered

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
GLMER_model_Testing_Phase_gamma_identity_2 <- glmer(RT ~ Block_number  + (1|Subject), data = Testing_Phase, family = Gamma(link = "identity"))
GLMER_model_Testing_Phase_gamma_identity_2
car::Anova(GLMER_model_Testing_Phase_gamma_identity_2, type=3)

# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Testing_Phase_invgauss_identity_2 <- glmer(RT ~ Block_number_centered  + (1|Subject), data = Testing_Phase, family = inverse.gaussian(link = "identity"))
# GLMER_model_Testing_Phase_invgauss_identity_2
# car::Anova(GLMER_model_Testing_Phase_invgauss_identity_2, type=3)

# Random intercept

# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Testing_Phase_gamma_identity_2a <- glmer(RT ~ Block_number_centered  + (1 + Block_number_centered |Subject), data = Testing_Phase, family = Gamma(link = "identity"))
# GLMER_model_Testing_Phase_gamma_identity_2a
# car::Anova(GLMER_model_Testing_Phase_gamma_identity_2a, type=3)


#Effect of violation type vs regular blocks

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
GLMER_model_Testing_Phase_gamma_identity_3 <- glmer(RT ~ TrialType + (1|Subject), data = Testing_Phase, family = Gamma(link = "identity"))
GLMER_model_Testing_Phase_gamma_identity_3
car::Anova(GLMER_model_Testing_Phase_gamma_identity_3, type=3)

# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Testing_Phase_invgauss_identity_3 <- glmer(RT ~ TrialType + (1|Subject), data = Testing_Phase, family = inverse.gaussian(link = "identity"))
# GLMER_model_Testing_Phase_invgauss_identity_3
# car::Anova(GLMER_model_Testing_Phase_invgauss_identity_3, type=3)

# Random intercept

# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Testing_Phase_gamma_identity_3a <- glmer(RT ~ TrialType + (1 + TrialType|Subject), data = Testing_Phase, family = Gamma(link = "identity"))
# GLMER_model_Testing_Phase_gamma_identity_3a
# car::Anova(GLMER_model_Testing_Phase_gamma_identity_3a, type=3)

# Communicability

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
GLMER_model_Testing_Phase_gamma_identity_4 <- glmer(RT ~ Communicability + (1 |Subject), data = Testing_Phase, family = Gamma(link = "identity"))
GLMER_model_Testing_Phase_gamma_identity_4
car::Anova(GLMER_model_Testing_Phase_gamma_identity_4, type=3)

# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Testing_Phase_invgauss_identity_4 <- glmer(RT ~ Communicability + (1|Subject), data = Testing_Phase, family = inverse.gaussian(link = "identity"))
# GLMER_model_Testing_Phase_invgauss_identity_4
# car::Anova(GLMER_model_Testing_Phase_invgauss_identity_4, type=3)

# Random intercept

# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Testing_Phase_gamma_identity_4a <- glmer(RT ~ Communicability + (1 + Communicability|Subject), data = Testing_Phase, family = Gamma(link = "identity"))
# GLMER_model_Testing_Phase_gamma_identity_4a
# car::Anova(GLMER_model_Testing_Phase_gamma_identity_4a, type=3)

### Interactions

# Block Number vs Block Type

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
GLMER_model_Testing_Phase_gamma_identity_5 <- glmer(RT ~ Block_number*Block_type + (1|Subject), data = Testing_Phase, family = Gamma(link = "identity"))
GLMER_model_Testing_Phase_gamma_identity_5
car::Anova(GLMER_model_Testing_Phase_gamma_identity_5, type=3)

# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Testing_Phase_invgauss_identity_5 <- glmer(RT ~ Block_number_centered*Block_type + (1|Subject), data = Testing_Phase, family = inverse.gaussian(link = "identity"))
# GLMER_model_Testing_Phase_invgauss_identity_5
# car::Anova(GLMER_model_Testing_Phase_invgauss_identity_5, type=3)

# Random intercept
# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Testing_Phase_gamma_identity_5a <- glmer(RT ~ Block_number_centered*Block_type + (1 + Block_number_centered+Block_type|Subject), data = Testing_Phase, family = Gamma(link = "identity"))
# GLMER_model_Testing_Phase_gamma_identity_5a
# car::Anova(GLMER_model_Testing_Phase_gamma_identity_5a, type=3)

# Random slope
# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Testing_Phase_gamma_identity_5b <- glmer(RT ~ Block_number_centered*Block_type + (1 + Block_number_centered*Block_type|Subject), data = Testing_Phase, family = Gamma(link = "identity"))
# GLMER_model_Testing_Phase_gamma_identity_5b
# car::Anova(GLMER_model_Testing_Phase_gamma_identity_5b, type=3)

# Block Type vs Trial type

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
GLMER_model_Testing_Phase_gamma_identity_6 <- glmer(RT ~ Block_type*TrialType + (1|Subject), data = Testing_Phase, family = Gamma(link = "identity"))
GLMER_model_Testing_Phase_gamma_identity_6
car::Anova(GLMER_model_Testing_Phase_gamma_identity_6, type=3)

# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Testing_Phase_invgauss_identity_6 <- glmer(RT ~ Block_type*TrialType + (1|Subject), data = Testing_Phase, family = inverse.gaussian(link = "identity"))
# GLMER_model_Testing_Phase_invgauss_identity_6
# car::Anova(GLMER_model_Testing_Phase_invgauss_identity_6, type=3)

# Random intercept
# 
# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Testing_Phase_gamma_identity_6a <- glmer(RT ~ Block_type*TrialType + (1 + Block_type + TrialType|Subject), data = Testing_Phase, family = Gamma(link = "identity"))
# GLMER_model_Testing_Phase_gamma_identity_6a
# car::Anova(GLMER_model_Testing_Phase_gamma_identity_6a, type=3)

# Random slope

# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Testing_Phase_gamma_identity_6b <- glmer(RT ~ Block_type*TrialType + (1 + Block_type*TrialType|Subject), data = Testing_Phase, family = Gamma(link = "identity"))
# GLMER_model_Testing_Phase_gamma_identity_6b
# car::Anova(GLMER_model_Testing_Phase_gamma_identity_6b, type=3)

# Block Number vs Block Type vs Trial Type

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
GLMER_model_Testing_Phase_gamma_identity_7 <- glmer(RT ~ Block_number*Block_type*TrialType + (1|Subject), data = Testing_Phase, family = Gamma(link = "identity"))
GLMER_model_Testing_Phase_gamma_identity_7
car::Anova(GLMER_model_Testing_Phase_gamma_identity_7, type=3)

# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Testing_Phase_invgauss_identity_7 <- glmer(RT ~ Block_number_centered*Block_type + (1|Subject), data = Testing_Phase, family = inverse.gaussian(link = "identity"))
# GLMER_model_Testing_Phase_invgauss_identity_7
# car::Anova(GLMER_model_Testing_Phase_invgauss_identity_7, type=3)

# Random intercept

# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Testing_Phase_gamma_identity_7a <- glmer(RT ~ Block_number_centered*Block_type*TrialType + (1 + Block_number_centered + Block_type + TrialType|Subject), data = Testing_Phase, family = Gamma(link = "identity"))
# GLMER_model_Testing_Phase_gamma_identity_7a
# car::Anova(GLMER_model_Testing_Phase_gamma_identity_7a, type=3)
# 
# # Random slope
# 
# ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# GLMER_model_Testing_Phase_gamma_identity_7b <- glmer(RT ~ Block_number_centered*Block_type*TrialType + (1 + Block_number_centered*Block_type*TrialType|Subject), data = Testing_Phase, family = Gamma(link = "identity"))
# GLMER_model_Testing_Phase_gamma_identity_7b
# car::Anova(GLMER_model_Testing_Phase_gamma_identity_7b, type=3)

# ANOVAs

# ANOVAs for both gamma and inverse gaussians  
#anova(GLMER_model_Testing_Phase_gamma_identity_1,
      # GLMER_model_Testing_Phase_invgauss_identity_1,
      # GLMER_model_Testing_Phase_gamma_identity_2,
      # GLMER_model_Testing_Phase_invgauss_identity_2,
      # GLMER_model_Testing_Phase_gamma_identity_3,
      # GLMER_model_Testing_Phase_invgauss_identity_3,
      # GLMER_model_Testing_Phase_gamma_identity_4,
      # GLMER_model_Testing_Phase_invgauss_identity_4,
      # GLMER_model_Testing_Phase_gamma_identity_5,
      # GLMER_model_Testing_Phase_invgauss_identity_5,
      # GLMER_model_Testing_Phase_gamma_identity_6,
      # GLMER_model_Testing_Phase_invgauss_identity_6,
      # GLMER_model_Testing_Phase_gamma_identity_7,
      # GLMER_model_Testing_Phase_invgauss_identity_7)

anova(GLMER_model_Testing_Phase_gamma_identity_1,
      GLMER_model_Testing_Phase_gamma_identity_2,
      GLMER_model_Testing_Phase_gamma_identity_3,
      GLMER_model_Testing_Phase_gamma_identity_4,
      GLMER_model_Testing_Phase_gamma_identity_5,
      GLMER_model_Testing_Phase_gamma_identity_6,
      GLMER_model_Testing_Phase_gamma_identity_7)


emm_options(opt.digits = FALSE)
# emmeans automatically adjusts p-values using the Tukey method (!)
# The Tukey correction method controls the overall error rate for all pairwise comparisons, so it is not necessary to apply additional corrections for follow-up contrasts (!)
# emmeans can back-transform the transformed emmeans to the response (RT) type, by using type = "response"

options(max.print=10000)

GLMER_model_Testing_Phase_gamma_identity_2_emm1 <- emmeans(GLMER_model_Testing_Phase_gamma_identity_2, pairwise ~ Block_number, type = "response")
GLMER_model_Testing_Phase_gamma_identity_2_emm1


GLMER_model_Testing_Phase_gamma_identity_3_emm2 <- emmeans(GLMER_model_Testing_Phase_gamma_identity_3, pairwise ~ TrialType, type = "response")
GLMER_model_Testing_Phase_gamma_identity_3_emm2

GLMER_model_Testing_Phase_gamma_identity_4_emm3 <- emmeans(GLMER_model_Testing_Phase_gamma_identity_4, pairwise ~ Communicability, type = "response")
GLMER_model_Testing_Phase_gamma_identity_4_emm3

GLMER_model_Testing_Phase_gamma_identity_1_emm4 <- emmeans(GLMER_model_Testing_Phase_gamma_identity_1, pairwise ~ Block_type, type = "response")
GLMER_model_Testing_Phase_gamma_identity_1_emm4 

GLMER_model_Testing_Phase_gamma_identity_5_emm5 <- emmeans(GLMER_model_Testing_Phase_gamma_identity_5, pairwise ~ Block_type*Block_number, type = "response")
GLMER_model_Testing_Phase_gamma_identity_5_emm5 


###################################################

# Data seems to be consistently better represented by gamma rather than inverse gaussian distribution

#xxxxxx = best fitting model

# Add random effects to the models


# GLMER_model_Testing_Phase_gamma_log_2 <- glmer(RT ~ Block_number_centered*Block_type + (1 + Block_number_centered*Trial_type|Subject), data = Testing_Phase, family = inverse.gaussian(link = "identity"))
# GLMER_model_Testing_Phase_gamma_log_2
# car::Anova(GLMER_model_Testing_Phase_gamma_log_2, type=3)

# anova(GLMER_model_Testing_Phase_gamma_log_2)

# xxxxxxx = best fitting model

# Add more complex random effects to the models

# GLMER_model_Testing_Phase_gamma_log_2 <- glmer(RT ~ Block_number_centered*Block_type + (1 + Block_cluster + Block_type|Subject), data = Testing_Phase, family = Gamma(link = "log"))
# GLMER_model_Testing_Phase_gamma_log_2
# car::Anova(GLMER_model_Testing_Phase_gamma_log_2, type=3)


# GLMER_model_Testing_Phase_gamma_log_2 <- glmer(RT ~ Block_number_centered*Block_type + (1 + Block_cluster*Block_type|Subject), data = Testing_Phase, family = Gamma(link = "log"))
# GLMER_model_Testing_Phase_gamma_log_2
# car::Anova(GLMER_model_Testing_Phase_gamma_log_2, type=3)


# The model with the lowest AIC/BIC provides the best fit
# anova(...)

# ... = best fit


###############################################################################
# Investigate interactions
###############################################################################

# GLMER_model_Testing_Phase_gamma_log_8 = best fit
# GLMER_model_Testing_Phase_gamma_log_8
# car::Anova(GLMER_model_Testing_Phase_gamma_log_8, type=3)


# GLMER_model_Testing_Phase_gamma_log_1_emm1 <- emmeans(GLMER_model_Testing_Phase_gamma_log_1, pairwise ~ Block_type, type = "response")
# GLMER_model_Testing_Phase_gamma_log_1_emm1
# 
# GLMER_model_Testing_Phase_gamma_log_2_emm2 <- emmeans(GLMER_model_Testing_Phase_gamma_log_2, pairwise ~ TrialType, type = "response")
# GLMER_model_Testing_Phase_gamma_log_2_emm2
# 
# GLMER_model_Testing_Phase_gamma_log_3_emm3 <- emmeans(GLMER_model_Testing_Phase_gamma_log_3, pairwise ~ Communicability, type = "response")
# GLMER_model_Testing_Phase_gamma_log_3_emm3
# 


##Add whatever other effects are significant 
# GLMER_model_Testing_Phase_gamma_log_8_emm2 <- emmeans(GLMER_model_Testing_Phase_gamma_log_8, pairwise ~ TrialType, type = "response")
# GLMER_model_Testing_Phase_gamma_log_8_emm2
# 
# GLMER_model_Testing_Phase_gamma_log_3_emm3 <- emmeans(GLMER_model_Testing_Phase_gamma_log_3, pairwise ~ Communicability, type = "response")
# GLMER_model_Testing_Phase_gamma_log_3_emm3
# 
# GLMER_model_Testing_Phase_gamma_log_2_emm4 <- emmeans(GLMER_model_Testing_Phase_gamma_log_2, pairwise ~ Block_type, type = "response")
# GLMER_model_Testing_Phase_gamma_log_2_emm4


# # # Create graph for Trial Type # # #

emmip(GLMER_model_Testing_Phase_gamma_identity_3_emm2, ~ TrialType, type = "response", CIs = TRUE)


# Extract estimated marginal means and confidence intervals for TrialType
emm_data_TrialType <- as_tibble(GLMER_model_Testing_Phase_gamma_identity_3_emm2)
emm_pred_TrialType <- predict(GLMER_model_Testing_Phase_gamma_identity_3_emm2, type = "response", interval = "confidence")

plot_data <- as.data.frame(emm_pred_TrialType)
plot_data


# Define data frame

plot_data <- data.frame(
  TrialType = c("Randomblock_non-violation", "Randomblock_violation", "RegularBlock"),
  emmean = c(683.4225, 680.1400, 668.1251),
  SE = c(1.539083, 2.253742, 2.122725),
  df = c(Inf, Inf, Inf),
  asymp.LCL = c(680.4060, 675.7228, 663.9647),
  asymp.UCL = c(686.4391, 684.5573, 672.2856)
)

# Convert TrialType to a factor to control the order and labels in the legend
plot_data$TrialType <- factor(plot_data$TrialType,
                              levels = c("Randomblock_non-violation", "Randomblock_violation", "RegularBlock"))

# Create the bar graph
plot <- ggplot(data = plot_data, aes(x = TrialType, y = emmean, fill = TrialType)) +
  geom_bar(stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(x = "Trial Type", y = "Reaction Time (ms)") +
  coord_cartesian(ylim = c(660, 690)) +
  scale_fill_manual(values = c("Randomblock_non-violation" = "lightblue", 
                               "Randomblock_violation" = "lightgreen", 
                               "RegularBlock" = "salmon"),
                    labels = c("Random Blocks (Non-violations)", 
                               "Random Blocks (Violations)", 
                               "Regular Blocks")) +  # Renaming legend labels
  scale_x_discrete(labels = c("Randomblock_non-violation" = "Non-violations",
                              "Randomblock_violation" = "Violations",
                              "RegularBlock" = "Regular Blocks")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12), # Adjust the font size and angle of x-axis labels
        axis.text.y = element_text(size = 12), # Adjust the font size of y-axis labels
        axis.title.x = element_text(size = 14),  # Customize x-axis label
        axis.title.y = element_text(size = 14),   # Customize y-axis label
        legend.title = element_blank(), # Removes legend title
        panel.grid.major = element_blank(),       # Remove major gridlines
        panel.grid.minor = element_blank())       # Remove minor gridlines



# Display the plot
print(plot)



# # # Create graph for Communicability # # # 

emmip(GLMER_model_Testing_Phase_gamma_identity_4_emm3, ~ Communicability, type = "response", CIs = TRUE)

# Extract estimated marginal means and confidence intervals for Communicability

emm_data_Communicability <- as_tibble(GLMER_model_Testing_Phase_gamma_identity_4_emm3)
emm_pred_Communicability <- predict(GLMER_model_Testing_Phase_gamma_identity_4_emm3, type = "response", interval = "confidence")

plot_data <- as.data.frame(emm_pred_Communicability)
plot_data

# Your data frame
plot_data <- data.frame(
  Communicability = c(1.5547420000000001, 2.0130409999999999, 2.2303980000000001, 3.2427649999999999, 3.8736190000000001,
                      4.2135179999999997, 4.6002840000000003, 4.7092530000000004, 4.8220650000000003, 4.9106959999999997,
                      5.389583, 5.5574750000000002, 5.6260120000000002, 6.3595449999999998, 6.4879480000000003,
                      6.6086999999999998, 7.5646139999999997, 7.7580439999999999, 8.5557160000000003, 8.7128019999999999),
  emmean = c(678.8659, 686.0034, 683.4117, 656.9753, 694.2947, 669.8815, 677.2807, 686.0794, 661.9146, 690.1699,
             652.9542, 675.9696, 698.4849, 687.1318, 667.2032, 663.9028, 682.6625, 666.9769, 648.1292, 669.4516),
  asymp.LCL = c(670.5517, 677.5380, 672.2661, 648.8969, 685.7229, 661.6459, 669.0125, 677.6111, 653.9087, 681.6271,
                644.9600, 667.6567, 687.1541, 680.0140, 658.9022, 654.8672, 675.5965, 658.3511, 639.6104, 662.6280),
  asymp.UCL = c(687.1800, 694.4689, 694.5573, 665.0537, 702.8664, 678.1171, 685.5488, 694.5476, 669.9205, 698.7127,
                660.9485, 684.2826, 709.8158, 694.2495, 675.5041, 672.9385, 689.7286, 675.6026, 656.6480, 676.2753)
)


# Create scatterplot
plot <- ggplot(data = plot_data, aes(x = Communicability, y = emmean)) +
  geom_point() +  # Scatter points
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1) +  # Error bars
  geom_smooth(method = "lm", se = FALSE) +  # Add slope
  labs(x = "Communicability", y = "Estimated Marginal Mean (emmean)", 
       title = "Estimated Marginal Means by Communicability") +  # Title and axis labels
  scale_x_continuous(breaks = seq(1, 9, 1)) +  # Set x-axis from 1 to 9
  theme_minimal() +  # Minimal theme for clean look
  theme(axis.title = element_text(size = 14),  # Customize axis title font size
        axis.text = element_text(size = 12),   # Customize axis text font size
        plot.title = element_text(size = 16, face = "bold", hjust = 0),  # Left-align title
        panel.grid.major = element_blank(),     # Remove major gridlines
        panel.grid.minor = element_blank())     # Remove minor gridlines

# Print the plot to visualize it
print(plot)


# Save the plot as a PNG file
ggsave("Communicability_Scatterplot.png", 
       plot = plot,  # Save the plot object you created
       width = 8,    # Width of the plot in inches
       height = 6,   # Height of the plot in inches
       dpi = 300)    # Resolution of the plot


# # # Create a graph for Block number and block type # # #


# Extract estimated marginal means and confidence intervals
emm_data <- as_tibble(GLMER_model_Testing_Phase_gamma_identity_5_emm5)
emm_pred <- predict(GLMER_model_Testing_Phase_gamma_identity_5_emm5, type = "response", interval = "confidence")

plot_data <- as.data.frame(emm_pred)
plot_data


# Define data frame

plot_data_ordered <- data.frame(
  Block_number_centered = c(-3.5, -1.5, 0.5, 2.5, 4.5, -4.5, -2.5, -0.5, 1.5, 3.5),
  emmean = c(685.2011, 684.1145, 679.7682, 684.8487, 672.4502, 669.8852, 676.2481, 667.1935, 666.9517, 660.3155),
  asymp.LCL = c(680.5022, 679.2140, 675.0794, 680.1049, 667.7018, 666.4774, 671.5880, 662.6787, 662.3910, 655.5669),
  asymp.UCL = c(689.9000, 689.0151, 684.4569, 689.5926, 677.1986, 673.2931, 680.9082, 671.7082, 671.5125, 665.0641),
  Block_type = c('RandomBlock', 'RandomBlock', 'RandomBlock', 'RandomBlock', 'RandomBlock',
                 'RegularBlock', 'RegularBlock', 'RegularBlock', 'RegularBlock', 'RegularBlock')
)

# Convert Block_number_centered to a factor with specified levels
plot_data_ordered$Block_number_centered <- factor(plot_data_ordered$Block_number_centered,
                                                  levels = c("-4.5", "-3.5", "-2.5", "-1.5", "-0.5", "0.5", "1.5", "2.5", "3.5", "4.5"))

# Create bar graph with different linetypes for RandomBlock and RegularBlock
plot <- ggplot(data = plot_data_ordered, aes(x = Block_number_centered, y = emmean, fill = Block_type)) +
  
  # Bars with colors for different Block types
  geom_bar(stat = "identity") +
  
  # Error bars
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, color = "black") +
  
  # Add trend lines: RandomBlock = dotted, RegularBlock = solid
  geom_smooth(aes(group = Block_type, color = Block_type, linetype = Block_type), method = "lm", se = FALSE) +
  
  
  # Customize labels and axis
  labs(title = "Testing Phase", x = "Block Number", y = "Estimated Marginal Mean", fill = "Block type") +
  
  # Adjust y-axis limits
  coord_cartesian(ylim = c(640, 690)) +
  
  # Customize fill colors for bars
  scale_fill_manual(values = c("RandomBlock" = "lightblue", "RegularBlock" = "salmon"),
                    labels = c("Random Block", "Regular Block")) +
  
  # Customize line colors and linetypes for slopes
  scale_color_manual(values = c("RandomBlock" = "black", "RegularBlock" = "black")) +
  scale_linetype_manual(values = c("RandomBlock" = "dotted", "RegularBlock" = "solid")) +  # Different line styles
  
  # Customize x-axis labels
  scale_x_discrete(labels = c("-4.5" = "9", "-3.5" = "10", "-2.5" = "11", "-1.5" = "12",
                              "-0.5" = "13", "0.5" = "14", "1.5" = "15", "2.5" = "16",
                              "3.5" = "17", "4.5" = "18")) +
  
  # APA-style theme adjustments
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Font size for x-axis labels, tilted
        axis.text.y = element_text(size = 12),  # Font size for y-axis labels
        axis.title.x = element_text(size = 14, family = "sans", face = "plain"),  # Customize x-axis label
        axis.title.y = element_text(size = 14, family = "sans", face = "plain"),  # Customize y-axis label
        panel.grid.major = element_blank(),       # Remove major gridlines
        panel.grid.minor = element_blank())       # Remove minor gridlines

# Display the plot
print(plot)

# Save plot in .png
ggsave("my_plot.png", plot = plot, width = 8, height = 6, dpi = 300)


#######################  Left off here ##############

# Create a line plot
plot <- ggplot(data = plot_data_ordered, aes(x = Block_number_centered, y = emmean, group = Block_number_centered)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, size = 1) +  # Increase line thickness
  geom_point(size = 1.5) +
  geom_line(aes(linetype = Block_number_centered), size = 1) +  # Connect the dots within each level of Block_number_centered
  labs(x = "Block", y = "Reaction Time (ms)") +
  scale_y_continuous(breaks = seq(0, max(plot_data_ordered$emmean), by = 20)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),  # Remove default vertical gridlines
        panel.grid.major.y = element_line(color = "gray", linetype = "solid"))  # Add horizontal gridlines

# Add vertical gridlines per level of Block
# plot <- plot + geom_vline(xintercept = seq_along(unique(plot_data_ordered$Block_number_centered)), color = "gray", linetype = "solid")

# Display the plot
plot



###################################

# Convert Block_number to numeric for trend line
plot_data <- plot_data %>%
  mutate(Block_number = as.numeric(as.character(Block_number)))  # Ensure Block_number is numeric

# Create the bar plot with error bars and a trend line for each Block_type
ggplot(data = plot_data, aes(x = Block_number, y = emmean, fill = Block_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +  # Bars, grouped by Block_type
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),   # Error bars, grouped by Block_type
                position = position_dodge(0.9), width = 0.2) +
  geom_smooth(aes(group = Block_type, color = Block_type), method = "lm", linetype = "dotted", se = FALSE) +  # Trend line per Block_type
  labs(x = "Block Number", y = "Estimated Marginal Mean (emmean)", 
       title = "Estimated Marginal Means of Reaction Time by Block Type and Number") +  # Labels
  theme_minimal() +  # Use a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank()) +  # Remove minor gridlines
  coord_cartesian(ylim = c(600, 750)) +  # Set y-axis limits from 600 to 750 without removing data
  scale_fill_manual(values = c("salmon", "skyblue")) +  # Custom colors for different Block_type
  scale_color_manual(values = c("black", "darkblue"))  # Custom colors for trend lines of different Block_type



