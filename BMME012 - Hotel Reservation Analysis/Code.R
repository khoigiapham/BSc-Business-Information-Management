setwd("C:/Users/pkhoi/OneDrive - Erasmus University Rotterdam/2 Big Data/AS 2")

#1----
library(readr)
df <- read.csv("data_hotel_reservations.csv", stringsAsFactors = TRUE)
df$arrival_year <- as.factor(df$arrival_year)
df$arrival_month <- as.factor(df$arrival_month)
df$arrival_date <- as.factor(df$arrival_date)
df$repeated_guest <- as.factor(df$repeated_guest)
df$required_car_parking_space <- as.factor(df$required_car_parking_space)
str(df)
summary(df)
head(df)
tail(df)
colSums(is.na(df))
sapply(df, class)

# create column booking_canceled
df$booking_canceled <- ifelse(df$booking_status == "Canceled", 1, 0)
# replace NA with 0 in column no_of_special_requests
df$no_of_special_requests[is.na(df$no_of_special_requests)] <- 0
# remove columns Booking_ID and booking_status
df <- df[, !(names(df) %in% c("Booking_ID", "booking_status"))]
# print summary of the updated data
summary(df)

#2----
mdlLPM <- booking_canceled ~ .
rsltLPM <- lm(mdlLPM, data = df)
stargazer(rsltLPM, type = "text")
# Scale the quantitative data columns
colTypes   <- sapply(df, class) 
colNumeric <- which(colTypes == "numeric" |colTypes == "integer")
df[, colNumeric] <- scale(df[, colNumeric])
#Rename the formula to make it nice
mdlLAS <- booking_canceled ~ .
# Call the glmnetUtils library
library(glmnet)
library(stargazer)
X <- model.matrix(mdlLAS, data=df)
Y <- df$booking_canceled
# Fit the model and store the results
rsltLAS <- glmnet(X, Y, lambda =0.01)
# Display the coefficients assigned by LASSO
coefLAS <- as.matrix(coef(rsltLAS))
stargazer(coefLAS, type = "text")


#3----
library(dplyr)
library(caret)
df$booking_canceled<- factor(df$booking_canceled, levels = c(0,1))
df_sample <- df %>% slice(1:10000)
set.seed(123)
# Randomize the order of the observations
df_sample <- df_sample[sample(1:nrow(df_sample)),]
# Create K folds with equal size. This folds vector is
# not added to the data frame (as there is need to do so)
nFolds  <- 5
myFolds <- cut(seq(1, nrow(df_sample)), 
               breaks = nFolds, 
               labels=FALSE)
table(myFolds)
# Initialize empty vectors to collect results
accSVM <- rep(NA, nFolds)
accCT  <- rep(NA, nFolds)
accRF  <- rep(NA, nFolds)

str(df_sample)
# Define the model
mdlq3 <- booking_canceled ~ no_of_adults + no_of_children + no_of_weekend_nights +
  no_of_week_nights + required_car_parking_space + lead_time + arrival_year +
  arrival_month + arrival_date + repeated_guest + no_of_previous_cancellations +
  no_of_previous_bookings_not_canceled + avg_price_per_room + no_of_special_requests

library(rpart)
library(e1071)
library(rpart.plot)
library(randomForest) 
library(gbm) 
library(stargazer)
library(psych)

for (i in 1:nFolds) {
  cat("Analysis of fold", i, "\n")
  
  # 1: Define training and test sets
  testObs  <- which(myFolds == i, arr.ind = TRUE)
  dsTest   <- df_sample[ testObs, ]
  dsTrain  <- df_sample[-testObs, ]
  
  # 2: Train the models on the training sets
  rsltSVM   <- svm(mdlq3, data= dsTrain, type ="C-classification")
  rsltCT    <- rpart(mdlq3, data=dsTrain, 
                     method="class", 
                     parms = list(split="information"))
  rsltRF    <- randomForest(mdlq3, data=dsTrain,
               ntree = 100, mtry = round(sqrt((length(all.vars(mdlq3)) - 1))),
               importance = TRUE)
  
  # 3: Predict values for the test sets
  classSVM  <- predict(rsltSVM, dsTest)
  classCT   <- predict(rsltCT, dsTest, type="class")
  classRF   <- predict(rsltRF, dsTest, type = "class")

  # 4: Measure accuracy and store the results
  accSVM[i]   <- mean(classSVM == dsTest$booking_canceled)
  accCT[i]    <- mean(classCT  == dsTest$booking_canceled)
  accRF[i]    <- mean(classRF  == dsTest$booking_canceled)
}

# Combine the accuracies obtained with the three 
# classifiers in a single matrix
accRslt <- cbind(accSVM, accCT, accRF)

# Summarize the accuracies per technique. Function describe
# is from the psych package; function stargazer is from
# the stargazer package
describe(accRslt)
stargazer(accRslt, summary = TRUE, align = TRUE, no.space = TRUE, type="text")


#4----
library(nnet)
nn <- function(data, model, n) {
  # split data into train and test sets
  index <- 1:round(0.7*nrow(data))
  train <- data[index, ]
  test  <- data[-index, ]
  # Train neural network
  nn <- nnet(model, data = train, maxit = 300, size = n, trace = FALSE)
  # Predict on train and test sets
  train_preds <- predict(nn, train, type = "class")
  test_preds <- predict(nn, test, type = "class")
  # Calculate accuracy on train and test sets
  train_accuracy <- mean(train_preds == train$booking_canceled)
  test_accuracy <- mean(test_preds == test$booking_canceled)
  return(c(train_accuracy, test_accuracy))
}
data <- df %>% slice(1:100000)
model <- mdlq3
results <- data.frame(neurons = 1:15, train_accuracy = 0, test_accuracy = 0)
for (n in 1:15) {
  accuracies <- nn(data, model, n)
  results[n, "train_accuracy"] <- accuracies[1]
  results[n, "test_accuracy"] <- accuracies[2]
}
print(results)

library(ggplot2)

ggplot(results, aes(x = neurons)) +
  geom_line(aes(y = train_accuracy), color = "blue") +
  geom_line(aes(y = test_accuracy), color = "red") +
  labs(title = "Neural Network Accuracy as a Function of Hidden Layer Size",
       x = "Number of Neurons",
       y = "Accuracy") +
  theme_minimal()

library(ggplot2)

library(ggplot2)

# Set theme
my_theme <- theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  axis.text = element_text(size = 14),
                  axis.title = element_text(size = 16),
                  plot.title = element_text(size = 20, hjust = 0.5),
                  legend.position = "bottom",
                  legend.text = element_text(size = 14, colour = "black"))

# Create plot
ggplot(results, aes(x = neurons)) +
  geom_line(aes(y = train_accuracy, color = "Train"), size = 1.5) +
  geom_line(aes(y = test_accuracy, color = "Test"), size = 1.5) +
  scale_color_manual(values = c("#0072B2", "#E69F00")) +
  labs(title = "Neural Network Accuracy as a Function of Hidden Layer Size",
       x = "Number of Neurons",
       y = "Accuracy") +
  scale_x_continuous(breaks = 1:15) +
  scale_y_continuous(labels = scales::percent_format()) +
  my_theme



