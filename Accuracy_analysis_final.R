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
# install.packages("ggpubr")

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
         Block_type = as.factor(Block_type), # change later
         Block_number = as.numeric(Block_number),
         Trial = as.numeric(Trial),
         Obey_sequence = as.character(Obey_sequence),
         RT = as.numeric(RT),
         Acc = as.numeric(Acc), #Change to character after Acc analysis is done
         Explicit_knowledge_acc = as.character(Explicit_knowledge_acc),
         Communicability = as.factor(Communicability))





###################### Accuracy ########################

### Filter out unwanted participants (in this case 2 older pps for age effects)
# Inspect the structure of the dataset
str(Selected_Columns)
# Define the subjects to remove
subjects_to_remove <- c("104", "150")
# Remove the specified subjects
Selected_Columns <- Selected_Columns %>%
  filter(!Subject %in% subjects_to_remove)
# Verify the subjects have been removed
unique(Selected_Columns$Subject)


# Remove rows where Block_number is "Text_explicit"
Selected_Columns <- Selected_Columns %>%
  filter(Block_number != 19)

# Overall count and percentage of accuracy
overall_stats <- table(Selected_Columns$Acc)
overall_percent <- prop.table(overall_stats) * 100

# Display results
overall_stats
overall_percent


# Convert Acc to numeric if it's still character
Selected_Columns$Acc <- as.numeric(Selected_Columns$Acc)


# Convert Block_number to numeric
Selected_Columns$Block_number <- as.numeric(Selected_Columns$Block_number)

# Re-run the summarization after converting to numeric
block_stats <- Selected_Columns %>%
  group_by(Block_number, Block_type) %>%
  summarise(
    Total_Trials = n(),
    Accurate_Trials = sum(Acc == 1, na.rm = TRUE),    # Count accurate trials
    Inaccurate_Trials = sum(Acc == 0, na.rm = TRUE),  # Count inaccurate trials
    Accuracy_Percentage = mean(Acc == 1, na.rm = TRUE) * 100,  # Percentage of accurate trials
    .groups = 'drop'
  ) %>%
  arrange(Block_number)  # Arrange the output by Block_number

# Display the block-wise statistics
block_stats

# Convert Acc to numeric if it's still character
Selected_Columns$Accuracy_Percentage <- as.numeric(Selected_Columns$Acc)

# Convert Block_number to numeric if it isn't already
Accuracy_data <- Accuracy_data %>%
  mutate(Block_number = as.numeric(as.character(Block_number)))

# Now calculate the average accuracy for Blocks 9 to 18
average_accuracy_9_to_18 <- Accuracy_data %>%
  summarise(Average_Accuracy_9_to_18 = mean(Accuracy_Percentage[Block_number >= 9 & Block_number <= 18], na.rm = TRUE))

# Display the result
average_accuracy_9_to_18

###################################################


# Step 1: Prepare the Data
Accuracy_data <- Selected_Columns %>%
  mutate(Block_number = as.factor(Block_number),
         Block_type = fct_recode(Block_type, 
                                 `Random Block` = "RandomBlock",
                                 `Regular Block` = "RegularBlock")) %>% 
  group_by(Block_number, Block_type) %>%
  summarise(
    Total_Trials = n(),
    Accurate_Trials = sum(Acc == 1, na.rm = TRUE),
    Inaccurate_Trials = sum(Acc == 0, na.rm = TRUE),
    Accuracy_Percentage = (Accurate_Trials / Total_Trials) * 100,
    SE_Accuracy = sqrt((Accuracy_Percentage * (100 - Accuracy_Percentage)) / Total_Trials),
    .groups = 'drop'
  ) %>%
  # Step 2: Specify the order of phases
  mutate(Phase = factor(ifelse(as.numeric(Block_number) <= 8, "Training Phase", "Testing Phase"),
                        levels = c("Training Phase", "Testing Phase")))  # Set the order

# Step 3: Create the Bar Plot with Error Bars
ggplot(Accuracy_data, aes(x = factor(Block_number), y = Accuracy_Percentage, fill = Block_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +  # Bars
  geom_errorbar(aes(ymin = Accuracy_Percentage - SE_Accuracy, ymax = Accuracy_Percentage + SE_Accuracy),
                position = position_dodge(0.9), width = 0.2) +  # Error bars
  coord_cartesian(ylim = c(50, 100)) +  # Y-axis from 50% to 100%
  facet_grid(~ Phase, scales = "free_x", space = "free") +  # Separate phases
  labs(x = "Block Number", y = "Accuracy Percentage", fill = "Block Type") +  # Labels
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank())  # Remove minor gridlines


# Step 4: Create the Bar Plot with Error Bars in APA style
ggplot(Accuracy_data, aes(x = factor(Block_number), y = Accuracy_Percentage, fill = Block_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +  # Bars
  geom_errorbar(aes(ymin = Accuracy_Percentage - SE_Accuracy, ymax = Accuracy_Percentage + SE_Accuracy),
                position = position_dodge(0.9), width = 0.2) +  # Error bars
  coord_cartesian(ylim = c(50, 100)) +  # Y-axis from 50% to 100%
  facet_grid(~ Phase, scales = "free_x", space = "free") +  # Separate phases
  labs(x = "Block Number", y = "Accuracy Percentage", fill = "Block Type", title = "Accuracy Percentage by Block Type") +  # Labels
  theme_minimal(base_size = 12, base_family = "Arial") +  # APA font and size
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        legend.position = "right") +  # Place the legend on the right
  scale_fill_manual(values = c("Regular Block" = "salmon", "Random Block" = "lightblue"),  # Custom colors
                    labels = c("Regular Block", "Random Block"),  # Renaming legend labels
                    name = "Block Type")  # Legend title



############# To save as .png (copy/paste code from above, but change the first and last lines) 

png("my_plot_apa.png", width = 800, height = 600, res = 100)  # Set file name and dimensions
ggplot(Accuracy_data, aes(x = factor(Block_number), y = Accuracy_Percentage, fill = Block_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +  # Bars
  geom_errorbar(aes(ymin = Accuracy_Percentage - SE_Accuracy, ymax = Accuracy_Percentage + SE_Accuracy),
                position = position_dodge(0.9), width = 0.2) +  # Error bars
  coord_cartesian(ylim = c(50, 100)) +  # Y-axis from 50% to 100%
  facet_grid(~ Phase, scales = "free_x", space = "free") +  # Separate phases
  labs(x = "Block Number", y = "Accuracy Percentage", fill = "Block Type", title = "Accuracy Percentage by Block Type") +  # Labels
  theme_minimal(base_size = 12, base_family = "Arial") +  # APA font and size
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        legend.position = "right") +  # Place the legend on the right
  scale_fill_manual(values = c("Regular Block" = "salmon", "Random Block" = "lightblue"),  # Custom colors
                    labels = c("Regular Block", "Random Block"),  # Renaming legend labels
                    name = "Block Type")  # Legend title
dev.off()  # Close the PNG device