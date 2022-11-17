# Load the packages 
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(caret)
library(MASS)
library(randomForest)

# Read the csv
df <- read.csv('Telco-Customer-Churn.csv')
# The dimension and structure of the data frame
dim(df)
str(df)

# See how many NAs in each column
sapply(df, function(x) sum(is.na(x)))
# Omit the rows with NA
df <- na.omit(df)
dim(df)

# See unique values
unique(df['OnlineSecurity'])

# Change 'No internet service' to 'No' for six columns which have it.
for (i in 1:ncol(df[, c(10:15)])) {
  df[, c(10:15)][, i] <- as.factor(mapvalues(df[, c(10:15)][, i], from = c("No internet service"), to = c("No")))
}

# Change 'No phone service' to 'No' for column 'MultipleLines'
df$MultipleLines <- as.factor(mapvalues(df$MultipleLines, from = c("No phone service"), to = c("No")))

# Change 'SeniorCitizen' column to factors from integers. 
df$SeniorCitizen <- as.factor(mapvalues(df$SeniorCitizen, from = c("0", "1"), to = c("No", "Yes")))

# Remove the 'cutomerID' column, as it is not useful in our case
df$customerID <- NULL

# Convert all columns with character variables to factors.
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], 
                                       as.factor)


# Correlation between the 3 numerical columns
numeric_var <- sapply(df, is.numeric)
corr_matrix <- cor(df[, numeric_var])
corrplot(corr_matrix, main = "\n\nCorrelation Plot for Numerical Variables", method = "number")
# Delete total charges
df$TotalCharges <- NULL


# Split to train and test
set.seed(1)
train <- sample(1:nrow(df), 0.7*nrow(df))
Churn.test <- df$Churn[-train]
training <- df[train,]
testing <- df[-train,]

# Fit Random Forest by default settings
rf <- randomForest(Churn ~., data = training)
print(rf)

# predict 
pred_rf <- predict(rf, testing)
table(Predicted = pred_rf, Actual = testing$Churn)
plot(rf)

# Tune the model using tuneRF()
tune <- tuneRF(training[, -19], training[, 19], stepFactor = 0.5, plot = TRUE,
            ntreeTry = 500, trace = TRUE, improve = 0.05)

# Use mtry = 2 and ntree = 500
rf_new <- randomForest(Churn ~., data = training, ntree = 500,
                                   mtry = 2, importance = TRUE, proximity = TRUE)
print(rf_new)

# Predict using the new forest
pred_rf_new <- predict(rf_new, testing)
# Confusion matrix of the new prediction 
table(Predicted = pred_rf_new, Actual = testing$Churn)

# Use mtry =8 just to see the TPR
rf_newnew <- randomForest(Churn ~., data = training, ntree = 500,
                                    mtry = 8, importance = TRUE, proximity = TRUE)
pred_rf_newnew <- predict(rf_newnew, testing)
print(rf_newnew)
table(Predicted = pred_rf_newnew, Actual = testing$Churn)

# Feature importance of the tuned model
varImpPlot(rf_new, sort=T, n.var = 10, main = 'Top 10 Feature Importance')
