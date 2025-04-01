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

# Create a new variable 'TrialType' to distinguish between trial types
Selected_Columns$TrialType <- with(Selected_Columns, ifelse(Block_type == "RegularBlock", "RegularBlock",
                                                            ifelse(Obey_sequence == "Yes", "Randomblock_non-violation", "Randomblock_violation")))

head(Selected_Columns)  # View the first few rows to see the new column


Testing_Phase <- Selected_Columns %>%
  filter(Block_number > 8 & Block_number != 19) %>%
#  filter(Block_type == "RandomBlock") %>%
  
  mutate(TrialType = ifelse(Block_type == "RegularBlock", "RegularBlock",
                            ifelse(Obey_sequence == "Yes", "Randomblock_non-violation", 
                                   "Randomblock_violation"))) %>%
  mutate(TrialType = as.character(TrialType))  # Convert to factor

Testing_Phase <- Testing_Phase %>%
  filter(Block_type != "TestExplicit") %>%
  droplevels()  # Drop unused levels


########################################## ADD AVERAGE COMMUNICABILITY

# Step 1: Calculate average communicability per trial type
average_communicability <- Testing_Phase %>%
  group_by(TrialType) %>%
  summarise(Average_Communicability = mean(Communicability, na.rm = TRUE))

# View the average communicability
print(average_communicability)


# Step 3: Join the average communicability back to the Testing_Phase dataset
Testing_Phase <- Testing_Phase %>%
  left_join(average_communicability, by = "TrialType")

# Check the resulting dataset to ensure Average_Communicability is included
head(Testing_Phase)


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





# Communicability

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
GLMER_model_Testing_Phase_gamma_identity_4 <- glmer(RT ~ Communicability + (1 |Subject), data = Testing_Phase, family = Gamma(link = "identity"))
GLMER_model_Testing_Phase_gamma_identity_4
car::Anova(GLMER_model_Testing_Phase_gamma_identity_4, type=3)

# Trial Type 

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
GLMER_model_Testing_Phase_gamma_identity_6 <- glmer(RT ~ TrialType + (1 |Subject), data = Testing_Phase, family = Gamma(link = "identity"))
GLMER_model_Testing_Phase_gamma_identity_6
car::Anova(GLMER_model_Testing_Phase_gamma_identity_6, type=3)

# Communicability vs Trial Type

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
GLMER_model_Testing_Phase_gamma_identity_5 <- glmer(RT ~ Average_Communicability*TrialType + (1 |Subject), data = Testing_Phase, family = Gamma(link = "identity"))
GLMER_model_Testing_Phase_gamma_identity_5
car::Anova(GLMER_model_Testing_Phase_gamma_identity_5, type=3)

# Communicability vs block type

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
GLMER_model_Testing_Phase_gamma_identity_1 <- glmer(RT ~ Communicability*Block_type + (1 |Subject), data = Testing_Phase, family = Gamma(link = "identity"))
GLMER_model_Testing_Phase_gamma_identity_1
car::Anova(GLMER_model_Testing_Phase_gamma_identity_1, type=3)




################## 

emm_options(opt.digits = FALSE)
# emmeans automatically adjusts p-values using the Tukey method (!)
# The Tukey correction method controls the overall error rate for all pairwise comparisons, so it is not necessary to apply additional corrections for follow-up contrasts (!)
# emmeans can back-transform the transformed emmeans to the response (RT) type, by using type = "response"

options(max.print=10000)

GLMER_model_Testing_Phase_gamma_identity_1_emm1 <- emmeans(GLMER_model_Testing_Phase_gamma_identity_1, pairwise ~ Communicability*Block_type, type = "response")
GLMER_model_Testing_Phase_gamma_identity_1_emm1

# Calculate estimated marginal means for the interaction term
interaction_emms <- emmeans(GLMER_model_Testing_Phase_gamma_identity_1, 
                            ~ Communicability * Block_type)

# View the estimated marginal means for the interaction
print(interaction_emms)

# Perform pairwise contrasts for the interaction term
interaction_contrasts <- contrast(interaction_emms, 
                                  interaction = "pairwise", 
                                  by = "Communicability")

# View contrasts to interpret the significant interaction
summary(interaction_contrasts)

# Convert emmeans results to a dataframe for ggplot
interaction_df <- as.data.frame(interaction_emms)

# Plot the interaction effect
ggplot(interaction_df, aes(x = Communicability, y = emmean, color = Block_type)) +
  geom_line() +
  geom_point() +
  labs(x = "Communicability", y = "Estimated Marginal Mean RT", 
       title = "Interaction between Block_type and Communicability") +
  theme_minimal()

########## emTrends


install.packages("margins")
install.packages("ggplot2")
install.packages("effects")
library(margins)
library(ggplot2)
library(effects)


marginal_effects_interaction <- margins(GLMER_model_Testing_Phase_gamma_identity_1, 
                                        variables = c("Communicability", "Block_type"), 
                                        at = list(Block_type = levels(Testing_Phase$Block_type), 
                                                  Communicability = unique(Testing_Phase$Communicability)))
summary(marginal_effects_interaction)

# Compute the effects for the interaction
interaction_effects <- allEffects(GLMER_model_Testing_Phase_gamma_identity_1)
interaction_effects

# Plot the interaction between Communicability and Block_type
plot(interaction_effects, "Communicability:Block_type")

# Get the interaction effect data as a data frame
interaction_df <- as.data.frame(effect("Communicability:Block_type", GLMER_model_Testing_Phase_gamma_identity_1))

# # Plot with APA style elements
# 
# plot <- ggplot(interaction_df, aes(x = Communicability, y = fit, color = Block_type)) +
#   geom_line(size = 1.2) +  # Thicker lines
#   geom_ribbon(aes(ymin = lower, ymax = upper, fill = Block_type), alpha = 0.2) +  # Shaded confidence intervals
#   scale_color_manual(values = c("#0073C2FF", "#EFC000FF")) +  # Customize colors for APA style
#   scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +  # Same color for the fill
#   labs(
#     title = "Marginal Effects of Communicability by Block Type",  # Descriptive title
#     x = "Communicability",  # X-axis label
#     y = "Predicted RT (ms)",  # Y-axis label
#     color = "Block Type",  # Legend label
#     fill = "Block Type"    # Fill label for ribbon
#   ) +
#   theme_minimal() +  # Minimal theme for a clean APA style look
#   theme(
#     text = element_text(family = "sans", size = 12),  # Use sans-serif fonts
#     plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Centered and bold title
#     axis.title = element_text(size = 12, face = "bold"),  # Bold axis titles
#     axis.text = element_text(size = 11),  # Axis labels size
#     legend.position = "top",  # Place the legend at the top
#     legend.title = element_text(size = 12),  # Bold legend title
#     legend.text = element_text(size = 11),  # Legend text size
#     panel.grid.major = element_blank(),  # Remove major grid lines
#     panel.grid.minor = element_blank(),  # Remove minor grid lines
#     axis.line = element_line(size = 0.5)  # Thin axis lines
#   ) +
#   guides(fill = FALSE)  # Remove redundant fill legend (we only need the color legend)
# 
# ggsave("plot.png", plot, width = 8, height = 6, dpi = 300)


# Calculate marginal effects, specifying values for Block_type and Communicability
marginal_effects_interaction <- margins(
  GLMER_model_Testing_Phase_gamma_identity_1,
  at = list(
    Block_type = levels(Testing_Phase$Block_type), 
    Communicability = seq(min(Testing_Phase$Communicability), max(Testing_Phase$Communicability), length.out = 5)  # Adjust the number of points as needed
  )
)

# Summarize the marginal effects
summary(marginal_effects_interaction)

# Optional: Visualize the marginal effects
plot(marginal_effects_interaction)

# Calculate marginal effects specifically for each level of Block_type
marginal_effects_all <- margins(GLMER_model_Testing_Phase_gamma_identity_1, 
                                  +                                 at = list(Block_type = levels(Testing_Phase$Block_type)))
summary(marginal_effects_all)
##################

mtrends <- margins(GLMER_model_Testing_Phase_gamma_identity_1, variables = c("Communicability", "Block_type"))
summary(mtrends)

# Get the marginal effects for the interaction term
effects_data <- allEffects(GLMER_model_Testing_Phase_gamma_identity_1)

# Plot the interaction term Communicability*Block_type
plot(effects_data, 'Communicability:Block_type')

# Extract the effects data
interaction_effect <- as.data.frame(effects_data$`Communicability:Block_type`)

# Use ggplot2 to create a custom plot

ggplot(interaction_effect, aes(x = Communicability, y = fit, color = Block_type, fill = Block_type)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  scale_fill_manual(
    values = c("lightgreen", "salmon"),
    labels = c("Random Blocks", "Regular Blocks")  # Custom legend labels for fill
  ) +
  scale_color_manual(
    values = c("lightgreen", "salmon"),
    labels = c("Random Blocks", "Regular Blocks")  # Custom legend labels for color
  ) +
  labs(
    x = "Communicability",
    y = "Estimated RT (ms)",
    color = "Block type",  # Custom label for the legend title
    fill = "Block type"    # Custom label for the fill legend title
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),      # Remove grid lines
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    plot.title = element_blank()        # Remove the title
  )

ggsave("Communicability_improved.png", plot = plot, width = 8, height = 6)
##########

# Obtain estimated marginal means for Block_type at levels of Communicability
emms_interaction <- emmeans(GLMER_model_Testing_Phase_gamma_identity_1, 
                            ~ Block_type * Communicability)

# View the estimated marginal means for interaction
print(emms_interaction)

# Perform pairwise contrasts specifically for interactions
interaction_contrasts <- pairs(emms_interaction, adjust = "tukey")
summary(interaction_contrasts)


