#===========================================
## Titanic Survival (Logistic Regression) ##
#===========================================

#==================
# Install Package #
#==================

# Install Package
install.packages("titanic")
library(titanic)
library(tidyverse)

#===============
# Prepare Data #
#===============

## View Data
head(titanic_train)
nrow(titanic_train)

#----------
## Drop NA (Missing Values)
titanic_train <- na.omit(titanic_train)
nrow(titanic_train)

#----------
## Split data
set.seed(42)
n <- nrow(titanic_train)
id <- sample(1:n, size = n*0.7)
train_data <- titanic_train[id, ]
test_data <- titanic_train[-id, ]

#==============
# Fit Model #
#==============

# Fit Logistic Regression Full Dataset
survived_model <- glm(Survived ~ Sex + Fare + Pclass, 
                   data = train_data, 
                   family = "binomial")

survived_model
summary(survived_model)

#==============
# Train Data #
#==============

# Predict Model
train_data$prob_survived <- predict(survived_model, type = "response")
train_data$pred_survived <- ifelse(train_data$prob_survived >= 0.5, 1, 0)

train_data %>%
  select(PassengerId, Survived, Sex, Fare, Pclass, prob_survived, pred_survived)

#------------------
## Model Evaluation

# Confusion Matrix
conM_survivedTrain <- table(train_data$pred_survived, train_data$Survived,
                            dnn = c("Predicted", "Actual"))
conM_survivedTrain

# Accuracy
accuracy_survivedTrain <- (conM_survivedTrain[1, 1] + conM_survivedTrain[2, 2]) / sum(conM_survivedTrain)
precision_survivedTrain <- conM_survivedTrain[2, 2] / (conM_survivedTrain[2, 1] + conM_survivedTrain[2, 2])
recall_survivedTrain <- conM_survivedTrain[2, 2] / (conM_survivedTrain[1, 2] + conM_survivedTrain[2, 2])

f1_survivedTrain <- 2 * ((precision_survivedTrain * recall_survivedTrain) / (precision_survivedTrain + recall_survivedTrain))

cat("Accuracy :", accuracy_survivedTrain, "\n")
cat("Precision :", precision_survivedTrain, "\n")
cat("Recall :", recall_survivedTrain, "\n")
cat("F1 :", f1_survivedTrain, "\n")

#==============
# Test Data #
#==============

# Predict Model
test_data$prob_survived <- predict(survived_model, newdata = test_data, type = "response")
test_data$pred_survived <- ifelse(test_data$prob_survived >= 0.5, 1, 0)

test_data %>%
  select(PassengerId, Survived, Sex, Fare, Pclass, prob_survived, pred_survived)

#------------------
## Model Evaluation

# Confusion Matrix
conM_survivedTest <- table(test_data$pred_survived, test_data$Survived,
                           dnn = c("Predicted", "Actual"))
conM_survivedTest

# Accuracy
accuracy_survivedTest <- (conM_survivedTest[1, 1] + conM_survivedTest[2, 2]) / sum(conM_survivedTest)
precision_survivedTest <- conM_survivedTest[2, 2] / (conM_survivedTest[2, 1] + conM_survivedTest[2, 2])
recall_survivedTest <- conM_survivedTest[2, 2] / (conM_survivedTest[1, 2] + conM_survivedTest[2, 2])

f1_survivedTest <- 2 * ((precision_survivedTest * recall_survivedTest) / (precision_survivedTest + recall_survivedTest))

cat("Accuracy :", accuracy_survivedTest, "\n")
cat("Precision :", precision_survivedTest, "\n")
cat("Recall :", recall_survivedTest, "\n")
cat("F1 :", f1_survivedTest, "\n")

#=================
# Compare Result #
#=================

df_result <- data.frame(data = c("Train", "Test"),
                        Accuracy = c(accuracy_survivedTrain, accuracy_survivedTest),
                        Precision = c(precision_survivedTrain, precision_survivedTest),
                        Recall = c(recall_survivedTrain, recall_survivedTest),
                        F1 = c(f1_survivedTrain, f1_survivedTest)
                        )
df_result
