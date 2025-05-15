### Diagnostic tests ###

##### Sensitivity and Specificity ####
# install.packages("caret")
# install.packages("pROC")
# Be sure to have the reference and the data with same levels
library(caret)
library(readxl)
library(pROC)

protein_data <- read_xlsx("protein_data.xlsx") # change your path
protein_data$Test <- as.factor(protein_data$Test)
levels(protein_data$Test) # check your categories

protein_data$Gold_Standard <- as.factor(protein_data$Gold_Standard)
levels(protein_data$Gold_Standard) # check your categories

confusionMatrix(protein_data$Test, # the test you are running
                protein_data$Gold_Standard, # the reference
                positive = "Malignant") # indicate the positive result

# Create the ROC object
roc_obj <- roc(response = protein_data$Gold_Standard, 
               predictor = protein_data$Protein_Expression)

auc(roc_obj)
plot(roc_obj)

# Create a customized plot
roc_df <- data.frame(
  specificity = roc_obj$specificities,
  sensitivity = roc_obj$sensitivities
)

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "red", size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  xlab("1 - Specificity") + 
  ylab("Sensitivity")


##### Predict a diagnosis ####
# install.packages("mlbench")
# install.packages("randomForest")
library(mlbench)
library(caret)
library(randomForest)

data("BreastCancer") # from mlbench

# First, remove observations with missing values with "na.omit"
bc_data <- na.omit(BreastCancer)

# Remove the Id (no need)
bc_data$Id <- NULL

# Convert to factor
bc_data$Class <- factor(bc_data$Class, levels = c("benign", "malignant"))

# Divide you original data into training and validation/test 
set.seed(123) # makes reproducible results when using random algorithms
trainIndex <- createDataPartition(bc_data$Class, p = 0.7, list = FALSE)
trainData <- bc_data[trainIndex, ]
testData  <- bc_data[-trainIndex, ]

# Train your model
# rpart is a decision tree algorithm
# There are multiple methods inclduing glm, lm, neural networks...
model <- train(Class ~ ., data = trainData, method = "rpart")

# Predictions
predictions <- predict(model, newdata = testData)

# Confusion matrix of your predictions
result <- confusionMatrix(predictions, testData$Class, positive = "malignant")
result



##### Inter rater variability ####
# Install the irr package and download the diagnoses data
# install.packages("irr")
# install.packages('vcd')
library(vcd)
library(irr)
library(tidyverse)
data("diagnoses")
str(diagnoses)

# Calculate the % of agreement between raters
agree(diagnoses)

# If you only have two raters
kappa2(diagnoses) # it fails

# More than 2 raters
kappam.fleiss(diagnoses)

# Make a subset of diagnoses
diagnoses_2 <- diagnoses %>% select(rater1, rater2)
# diagnoses_3 <- subset(diagnoses, select=c("rater1", "rater2")) # another way
kappa2(diagnoses_2)

# Prepare to plot
table <- table(diagnoses_2$rater1, diagnoses_2$rater2)
# agreementplot(table)

# Use the full dataset
diagnoses <- diagnoses %>% mutate(Patient_ID = 1:nrow(.))

# Transformation for plotting
diagnoses_long <- diagnoses %>% 
  pivot_longer(cols = starts_with("rater"),
               names_to = "rater",
               values_to = "diagnosis")

# Actually plotting
ggplot(diagnoses_long, aes(x = Patient_ID, y = rater, fill = diagnosis)) +
  geom_tile(color = "black") 
