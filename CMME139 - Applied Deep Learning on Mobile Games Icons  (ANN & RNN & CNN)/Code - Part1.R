# Load library
library(readr)
library(tidyverse)
library(caret)
library(e1071)
library(caTools)
library(ggplot2)

# Set wd
path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
rm(list=ls())

# Task 1: Importing----
data <- read_csv("appstore_games.csv")
str(data)
summary(data)

# Task 2: Data Cleaning----
## 2.1 Remove URL and name attributes----
data$URL <- NULL
data$Name <- NULL

## 2.2 Create new binary subtitle column and remove old----
data$SubtitleNew <- ifelse(is.na(data$Subtitle),0,1)
data$Subtitle <- NULL

## 2.3 Remove Icon URL (after Task 4)----

## 2.4 Keep Average User Rating, User Rating Count and Price as they are ----

## 2.5 Extract information from In-App Purchases (IAP)----
# IAP values (counts)
data$`IAP values(counts)` <- sapply(strsplit(data$`In-app Purchases`, ","), function(x) ifelse(all(is.na(as.numeric(x))), NA, sum(!is.na(as.numeric(x)))))
# Minimum IAP
data$`Minimum IAP` <- sapply(strsplit(data$`In-app Purchases`, ","), function(x) ifelse(all(is.na(as.numeric(x))), NA, min(as.numeric(x), na.rm = TRUE)))
# Maximum IAP
data$`Maximum IAP` <- sapply(strsplit(data$`In-app Purchases`, ","), function(x) ifelse(all(is.na(as.numeric(x))), NA, max(as.numeric(x), na.rm = TRUE)))
# Sum IAP
data$`Sum IAP` <- sapply(strsplit(data$`In-app Purchases`, ","), function(x) ifelse(all(is.na(as.numeric(x))), NA, sum(as.numeric(x), na.rm = TRUE)))
# Average IAP, not rounded
data$`Average IAP` <- sapply(strsplit(data$`In-app Purchases`, ","), function(x) ifelse(all(is.na(as.numeric(x))), NA, mean(as.numeric(x), na.rm = TRUE)))

## 2.6 Create new column with count of words in description----
# Apply regex \\s+ for checking for any sequence of one or more white space characters
data$`Description words` <- sapply(data$Description, function(x) length(strsplit(as.character(x), "\\s+")[[1]]))
# Remove description column
data$Description <- NULL

## 2.7 Create new column with categorized developers----
# Count games per unique developer
developer_counts <- table(data$`Developer`)
# Consider developer with less than 4 games a Newbie, else a Professional
data$`Categorical Developers` <- ifelse(developer_counts[data$`Developer`] < 4, 'Newbie', "Professional")
rm(developer_counts)

## 2.8 Create second age rating attribute with 4+ and 9+----
data$`Second Age Rating` <- ifelse(data$`Age Rating`=="4+", "4+", "9+")

## 2.9 Create language attributes----
# Number of languages
data$`Languages(counts)` <- ifelse(count.fields(textConnection(data$Languages), sep = ",", blank.lines.skip = TRUE)==1,"Single","Many")
# Availability in English, encoded 1 for yes and 0 for no
data$`Is_Available_in_English` <- ifelse(data$Languages=="EN", 1, 0)

## 2.10  Convert Size to categorical based on quintiles (quintiles)----
data$`Game Size` <- cut(data$Size, 
                        breaks = quantile(data$Size, probs = seq(0, 1, by = 0.2), na.rm = TRUE),
                        labels = c("S1", "S2", "S3", "S4", "S5"), include.lowest = TRUE)
## 2.11 Remove primary genre----
data$`Primary Genre` <- NULL

## 2.12 Create new attribute with number of genres----
data$`Genres(counts)` <- count.fields(textConnection(data$Genres), sep = ",", blank.lines.skip = TRUE)

## 2.13 Create two data related attributes----
# Release month
data$`Release Month` <- strftime(data$`Original Release Date`,"%m")
# Elapsed months
data$`Elapsed months` <- as.numeric(difftime(as.Date("2019/08/03"), as.Date(data$`Current Version Release Date`, format = "%d/%m/%Y"), units = "days")) / (365.25 / 12)

## 2.14 Additional attributes----
# Game free
data$`Game Free` <- ifelse(is.na(data$`In-app Purchases`),1,0)
# Categorical rating count, binary with 'low' below median, else 'high'
data$`Categorical Rating Count` <- ifelse(data$`User Rating Count` < median(data$`User Rating Count`, na.rm = TRUE), 'Low', "High")

# Task 3: Missing values and formatting----
## 3.1 Have a overview of data----
# First have a brief overview of our data, to check the number of missing values and the class of each column. 
summary(data)
colSums(is.na(data))

## 3.2 Evaluate the options to deal with the NAs----
# While there are around 17000 observations, some columns have more than 9000 missing values which accounts for a large proportion of the total observations.
# Therefore, if we merely drop all the observations with missing values, we would be at risk of losing too much data.
# By taking into consideration of the meaning of each columns, we chose different methods to replace the NAs.

# Follow the method the authors use in the paper:
### Replace the NAs in the 'User Rating Count' with 5, because games with NAs for this column means they have less than 5 ratings:
data$`User Rating Count`[is.na(data$`User Rating Count`)] <- 5
### Replace the NAs with 'Low' in the 'Categorical Rating Count'. 
### This column is associated with NAs in the original 'User Rating Count' column which are replaced with value 5 in the previous step, 
### and the value 5 should be considered as 'Low', according to the criteria set in the previous task:
data$`Categorical Rating Count`[is.na(data$`Categorical Rating Count`)] <- "Low"

### Replace the NAs in the 'Average User Rating' with 0, because games with NAs for this column means they did not gain more than 5 rating counts:
data$`Average User Rating`[is.na(data$`Average User Rating`)] <- 0
### Remove games with missing values for 'Price' and 'Size' attributes, because only a small number of observations will be dropped:
data<- data[complete.cases(data$Price), ]
data<- data[complete.cases(data$Size), ]
### Assign the English language for games with NAs in the 'Languages' column, because most games in our data use English as the system language:
data$Languages[is.na(data$Languages)] <- "EN"
### Assign value 1 for games with NAs in the 'Is_Available_in_English' column, since it is associated with the 'Languages' column:
data$Is_Available_in_English[is.na(data$Is_Available_in_English)] <- 1

### Replace the NAs in the 'In-app Purchases' with 0, because games with NAs for this column means no product can be purchased in the app:
data$`In-app Purchases`[is.na(data$`In-app Purchases`)] <- 0
### Do the same for other added columns related to 'In-app Purchases':
data$`IAP values(counts)`[is.na(data$`IAP values(counts)`)] <- 0
data$`Minimum IAP`[is.na(data$`Minimum IAP`)] <- 0
data$`Maximum IAP`[is.na(data$`Maximum IAP`)] <- 0
data$`Sum IAP`[is.na(data$`Sum IAP`)] <- 0
data$`Average IAP`[is.na(data$`Average IAP`)] <- 0

## 3.3 Convert anything that is categorical into categorical data----
data$`Categorical Rating Count` <- as.factor(data$`Categorical Rating Count`)
data$`Developer` <- as.factor(data$`Developer`)
data$`Age Rating` <- as.factor(data$`Age Rating`)
data$`Categorical Developers` <- as.factor(data$`Categorical Developers`)
data$`Second Age Rating` <- as.factor(data$`Second Age Rating`)
data$`Languages(counts)` <- as.factor(data$`Languages(counts)`)
data$`Genres(counts)` <- as.factor(data$`Genres(counts)`)
data$`SubtitleNew` <- as.factor(data$`SubtitleNew`)
data$`Release Month` <- as.factor(data$`Release Month`)
data$`Game Free` <- as.factor(data$`Game Free`)
data$Is_Available_in_English <- as.factor(data$Is_Available_in_English)
sapply(data, class)
# The numeric columns, such as 'average user rating' and 'price', are not converted
# because they will be scaled in the task 6. 

## 3.4 Output the overview of current data----
colSums(is.na(data))
sapply(data,class)
summary(data)

# Task 4: Prepare for the next assignment----
## 4.1 Select the 200 games with the highest and lowest user ratings----
top_games <- data[order(data$`Average User Rating`, decreasing = TRUE), ][1:202, ]
bottom_games <- data[order(data$`Average User Rating`), ][1:202, ]
selected_games <- rbind(top_games, bottom_games)
rm(top_games)
rm(bottom_games)

saveRDS(selected_games, "selected_games.RDS")

## 4.2 Create a function to download the image for a given URL----
download_image <- function(url, filepath) {
  download.file(url, destfile = filepath, mode = "wb")
}
## 4.3 Create the folder to store the downloaded images----
dir.create("game_images_3", showWarnings = FALSE)

## 4.4 Download images for the selected games----
for (i in 1:nrow(selected_games)) {
  url <- selected_games$`Icon URL`[i]
  filename <- paste0("game_images_1/", selected_games$ID[i], ".jpeg")
  download_image(url, filename)
}

## 4.5 Remove the Icon URL column----
data$`Icon URL` <- NULL


# Directory to save images, replace with your desired directory
dir_name <- "C:/Users/pkhoi/OneDrive - Erasmus University Rotterdam/2 Deep Learning/Assignment 1/game_images_3"

# Function to download and save images
save_icon <- function(ID, url) {
  file_name <- paste0(dir_name, ID, ".jpeg")
  download.file(url, file_name, mode = "wb")
}

# Apply the function to each row of the dataframe
walk2(selected_games$ID, selected_games$`Icon URL`, save_icon)


## 4.6 Codes for downloading the first 10 jpegs for the testing purpose----
#for (i in 1:10) {
#  url <- selected_games$`Icon URL`[i]
#  filename <- paste0("game_images/test_", selected_games$ID[i], ".jpeg")
#  download_image(url, filename)
#}
#rm(selected_games)

# Question 5: Dataset Partitioning----
## Column removal reasoning:
# ID: This column is uninformative, because it is just a unique identifier for each row of 
# data, which doesn't provide useful insights or features for your analysis or model.

# In-app Purchases: This column is problematic and repetitive because there 
# are other columns like Max, Min, Sum and Average IAP which provide more informative 
# numeric data about the in-app purchases.

# Genres: This column is considered repetitive because there's already a Genres(counts) 
# column that quantifies the number of genres, which is more useful for our purpose.

# Original Release Date: This column is excluded as the paper specifies that release 
# months, not exact dates, have influence. This information is captured in the Release Month column.

# Current Version Release Date: The impact of this column is unclear from the paper. 
# It's likely used to assess the elapsed time since the last update, but as it doesn't 
# seem to hold the same weight as the launch timing, it's been decided to remove this 
# column. And we created the the Elapsed Month column from this column (how many months have passed
# since the current version release date, examined at 03.08.2019, data collection date).

# Size, Languages, Developer and Age Rating: These columns are replaced by newer 
# columns with more usable information:
# - Size is replaced by Game Size, which categorizes games into 5 size categories.
# - Languages is replaced by Languages(counts) which classifies games into "many languages" 
# or "single", and Is_Available_in_English which indicates if games are available in English.
# - Developer is replaced by Categorical Developers which sorts developers into two categories.
# - Age Rating is replaced by Second Age Rating, which classifies games as either “4+” or “9+”.


## Create a vector with the names of the columns to be removed
columns_to_remove <- c("ID", "In-app Purchases", "Genres", "Original Release Date", 
                       "Current Version Release Date", "Size", "Languages", 
                       "Developer", "Age Rating")

## Remove the columns from the data
data <- data[, !(colnames(data) %in% columns_to_remove)]

## Duplicate the current data to create baseline data
data_baseline <- data
## THE DATA WE USE IS WITHOUT THE USER RATING COUNT
data$`User Rating Count`<- NULL
# Save data to use for different machine learning models
save(data, file = "data.Rda")

## Split data into training and testing set
set.seed(123)
index <- createDataPartition(data$`Categorical Rating Count`, p = 0.8, list = FALSE)
# Create the training set
train <- data[index, ]
# Create the testing set
test <- data[-index, ]

# Question 6: Scale data----
# Scale all numerical data
colTypes   <- sapply(data, class) 
colNumeric <- which(colTypes == "numeric" | colTypes == "integer")
# Create the train and test data for scaling
train_scale <- train
test_scale <- test
# Scale the data
train_scale[, colNumeric] <- scale(train_scale[, colNumeric]) 
test_scale[, colNumeric] <- scale(test_scale[, colNumeric])


# Question 7: Machine learning models----
# Create the model
mdl <- `Categorical Rating Count` ~.

## 7.1 Naive Bayes----
### Model Training
set.seed(123)
classifier_NB <- naiveBayes(mdl, data = train_scale)
### Model Testing
pred_NB <- predict(classifier_NB, newdata = test_scale)
### Confusion Matrix
cm_NB <- table(test$`Categorical Rating Count`, pred_NB)
cm_NB
### Model Evaluation and accuracy and more stats on predictions
confusionMatrix(cm_NB)
### Evaluations----
# The model predicted 'High' 441 times correctly, and 'Low' 2407 times correctly.
# It also misclassified 316 'High' instances as 'Low', 
# and 232 'Low' instances as 'High'.

# The overall accuracy of the model is 0.8386. 
# This means the model correctly predicted the class about 83.86% of the time.

# The 95% Confidence Interval for the accuracy is (0.8258, 0.8509). 
# If we repeated the process on multiple samples, 
# the accuracy would fall within this range 95% of the time.

# The No Information Rate is 0.8018. 
# This is the accuracy that could be achieved by always predicting the most frequent class.

# The p-value of the accuracy being greater than the No Information Rate is 1.992e-08, 
# indicating high statistical significance.

# The Kappa statistic is 0.515. 
# A Kappa of 1 indicates perfect agreement, while a Kappa of 0 indicates no agreement beyond chance. 
# Here, the value of 0.515 indicates a moderate level of agreement.

# Mcnemar's Test P-Value of 0.0003917 suggests a significant difference in the misclassification costs.

# The Sensitivity (true positive rate) is 0.6553. 
# When the actual class is 'High', the model correctly identifies it as 'High' 65.53% of the time.

# The Specificity (true negative rate) is 0.8840. 
# When the actual class is 'Low', the model correctly identifies it as 'Low' 88.4% of the time.

# The Positive Predictive Value (precision) for 'High' is 0.5826. 
# When the model predicts 'High', it is correct 58.26% of the time.

# The Negative Predictive Value for 'Low' is 0.9121. 
# When the model predicts 'Low', it is correct 91.21% of the time.

# The Prevalence for 'High' is 0.1982. 
# This is the proportion of instances that are 'High' in the dataset.

# The Detection Rate for 'High' is 0.1299. 
# This is the proportion of all instances that were correctly predicted as 'High'.

# The Detection Prevalence for 'High' is 0.2229. 
# This is the proportion of instances that were predicted as 'High'.

# The Balanced Accuracy is 0.7696. 
# Balanced Accuracy is the average of Sensitivity and Specificity, 
# useful when dealing with imbalanced datasets.

## 7.2 KNN----
### Model Training
set.seed(123)
classifier_KNN <- gknn(mdl, train_scale, k = 3, method = "Manhattan")
### Model Testing
pred_KNN <- predict(classifier_KNN, test_scale)
### Confusion Matrix
cm_KNN <- table(test$`Categorical Rating Count`, pred_KNN)
cm_KNN
### Model Evaluation and accuracy and more stats on predictions
confusionMatrix(cm_KNN)
### Evaluations----
# The model predicted 'High' 407 times correctly, and 'Low' 2413 times correctly.

# However, it also misclassified 350 'High' instances as 'Low', 
# and 226 'Low' instances as 'High'.

# The overall accuracy of the model is 0.8304. 
# This indicates the model correctly predicted the class about 83.01% of the time.

# The 95% Confidence Interval for the accuracy is (0.817, 0.8429). 
# If we repeated the process on multiple samples, 
# the accuracy would fall within this range 95% of the time.

# The No Information Rate is 0.8136. 
# This is the accuracy that could be achieved by always predicting the most frequent class.

# The p-value of the accuracy being greater than the No Information Rate is 0.005983, 
# indicating statistical significance.

# The Kappa statistic is 0.4794. 
# A Kappa of 1 indicates perfect agreement, while a Kappa of 0 indicates no agreement beyond chance. 
# Here, the value of 0.4794 indicates a moderate level of agreement.

# Mcnemar's Test P-Value of 2.975e-07 suggests a highly significant difference in the misclassification costs.

# The Sensitivity (true positive rate) is 0.6430. 
# When the actual class is 'High', the model correctly identifies it as 'High' 64.20% of the time.

# The Specificity (true negative rate) is 0.8733. 
# When the actual class is 'Low', the model correctly identifies it as 'Low' 87.33% of the time.

# The Positive Predictive Value (precision) for 'High' is 0.5376. 
# When the model predicts 'High', it is correct 53.76% of the time.

# The Negative Predictive Value for 'Low' is 0.9144. 
# When the model predicts 'Low', it is correct 91.44% of the time.

# The Prevalence for 'High' is 0.1867. 
# This is the proportion of instances that are 'High' in the dataset.

# The Detection Rate for 'High' is 0.1198. 
# This is the proportion of all instances that were correctly predicted as 'High'.

# The Detection Prevalence for 'High' is 0.2229. 
# This is the proportion of instances that were predicted as 'High'.

# The Balanced Accuracy is 0.7576. 
# Balanced Accuracy is the average of Sensitivity and Specificity, 
# useful when dealing with imbalanced datasets.


## 7.3 SVM----
### Model Training
set.seed(123)
classifier_SVM <- svm(mdl, data = train_scale, kernel="linear", type = "C-classification")
### Model Testing
pred_SVM <- predict(classifier_SVM, test_scale)
### Confusion Matrix
cm_SVM <- table(test$`Categorical Rating Count`, pred_SVM)
cm_SVM
### Model Evaluation and accuracy and more stats on predictions
confusionMatrix(cm_SVM)
### Evaluations----
# The model predicted 'High' 417 times correctly, and 'Low' 2483 times correctly.
# However, it also misclassified 340 'High' instances as 'Low', 
# and 156 'Low' instances as 'High'.

# The overall accuracy of the model is 0.8539. 
# This indicates the model correctly predicted the class about 85.39% of the time.

# The 95% Confidence Interval for the accuracy is (0.8416, 0.8657). 
# If we repeated the process on multiple samples, 
# the accuracy would fall within this range 95% of the time.

# The No Information Rate is 0.8313. 
# This is the accuracy that could be achieved by always predicting the most frequent class.

# The p-value of the accuracy being greater than the No Information Rate is 0.0001811, 
# indicating high statistical significance.

# The Kappa statistic is 0.5384. 
# A Kappa of 1 indicates perfect agreement, while a Kappa of 0 indicates no agreement beyond chance. 
# Here, the value of 0.5384 indicates a moderate level of agreement.

# Mcnemar's Test P-Value of < 2.2e-16 suggests a highly significant difference in the misclassification costs.

# The Sensitivity (true positive rate) is 0.7277. 
# When the actual class is 'High', the model correctly identifies it as 'High' 72.77% of the time.

# The Specificity (true negative rate) is 0.8796. 
# When the actual class is 'Low', the model correctly identifies it as 'Low' 87.96% of the time.

# The Positive Predictive Value (precision) for 'High' is 0.5509. 
# When the model predicts 'High', it is correct 55.09% of the time.

# The Negative Predictive Value for 'Low' is 0.9409. 
# When the model predicts 'Low', it is correct 94.09% of the time.

# The Prevalence for 'High' is 0.1687. 
# This is the proportion of instances that are 'High' in the dataset.

# The Detection Rate for 'High' is 0.1228. 
# This is the proportion of all instances that were correctly predicted as 'High'.

# The Detection Prevalence for 'High' is 0.2229. 
# This is the proportion of instances that were predicted as 'High'.

# The Balanced Accuracy is 0.8037. 
# Balanced Accuracy is the average of Sensitivity and Specificity, 
# useful when dealing with imbalanced datasets.

## 7.4 Model comparison----
# 1. Accuracy: The SVM model has the highest accuracy (0.8539), followed by the NB model (0.8386), 
#    and the KNN model (0.8301). Accuracy is the proportion of total correct predictions in all predictions.

# 2. Sensitivity (true positive rate): The SVM model again performs best with a sensitivity of 0.7277, 
#    followed by the NB model (0.6553), and the KNN model (0.6420). Sensitivity indicates how often 
#    the actual positive class is correctly identified.

# 3. Specificity (true negative rate): The NB model shows the highest specificity (0.8840), followed 
#    by the SVM model (0.8796), and then the KNN model (0.8733). Specificity measures how often the 
#    actual negative class is correctly identified.

# 4. Positive Predictive Value (precision): Here, the NB model performs the best (0.5826), 
#    followed by the SVM model (0.5509), and the KNN model (0.5376). This metric indicates how often 
#    the predicted positive class is actually positive.

# 5. Negative Predictive Value: The SVM model shows the highest NPV (0.9409), followed by the 
#    KNN model (0.9140), and the NB model (0.9121). This metric tells how often the predicted negative 
#    class is actually negative.

# 6. Balanced Accuracy: The SVM model again leads with a balanced accuracy of 0.8037, followed by 
#    the NB model (0.7696), and the KNN model (0.7576). Balanced accuracy is useful for imbalanced datasets 
#    and is the average of sensitivity and specificity.

# 7. Kappa Statistic: The SVM model has the highest Kappa (0.5384), followed by the NB model (0.5150), 
#    and the KNN model (0.4794). Kappa indicates agreement of prediction with the actual class, 
#    a higher Kappa means better performance.

# In summary, the SVM model shows the best overall performance according to most of the evaluated metrics. 



