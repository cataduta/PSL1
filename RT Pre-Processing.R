###############################################################################
###############################################################################
###############################################################################
###############################################################################
# TEMPLATE FOR REACTION TIME DATA PRE-PROCESSING: SEQUENCE-SPECIFIC LEARNING
###############################################################################
###############################################################################
###############################################################################
###############################################################################


###############################################################################
# Additional dataset pre-processing
# in case of very large datasets, you may want to eliminate certain (levels of) variables to avoid unworkable complexity in the models
###############################################################################


# Standardize the RT variable
# Testing_Phase$RT_standardized <- scale(Testing_Phase$RT)

#Standardize the communicability variable
# Testing_Phase$Communicability_standardized <- scale(Testing_Phase$Communicability)

# Ensure Block_type is treated as a factor
#Testing_Phase$Block_type <- as.factor(Testing_Phase$Block_type)
#Testing_Phase$Block_number_centered <- as.factor(Testing_Phase$Block_number_centered)

# Center and scale numeric predictors
# Testing_Phase$Block_number_centered_scaled <- scale(Testing_Phase$Block_number_centered)

### To create a variable that distinguishes between transition types (e.g. violations vs non-violations) in the random blocks in the Testing Phase and takes into account that Non-violations occur in Regular Blocks as well, use code below. 

# Create a new variable 'TrialType' to distinguish between trial types
Testing_Phase$TrialType <- with(Testing_Phase, ifelse(Block_type == "RegularBlock", "RegularBlock",
                                                      ifelse(Obey_sequence == "Yes", "Randomblock_non-violation", "Randomblock_violation")))


# Check unique values of 'TrialType'
unique(Testing_Phase$TrialType)

Testing_Phase <- Testing_Phase %>%
  mutate(Block_number = as.factor(Block_number),
         TrialType = as.factor(TrialType))

