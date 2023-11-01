library(spotifyr)
library(RedditExtractoR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)
library(plyr)
library(glmnet)
library(glmnetUtils)
library(stargazer)
library(caret)
library(e1071)
library(rpart)
library(randomForest)
library(nnet)
library(mice)
library(class)  
library(ROCR)
library(cluster) 
library(ape)
library(rpart.plot)
library(proxy)
library(forcats)
library(corrplot)

#Set working directory
path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
rm(list = ls())
finaldata <- readRDS("data_final/data_prepared.RDS")

#1. Descriptive analysis----
# Creating figure 1 in the report
# Correlation pplot
corr <- cor(finaldata[, c(6:16)])
corplot <- corrplot(corr, method = "circle", type = "upper")
dir.create("results")
ggsave("results/f1.pdf")
# Summary statistics for sonic characteristics variables
df_summary <- df
df_summary$num_markets <- NULL
# Generate stargazer table
table <- stargazer(df_summary, type = "text")

# Creating figure 2 in the report
# Distribution of the output variable
f2 <- ggplot(df, aes(x = release_effects, fill = release_effects)) +
  geom_bar(width = 0.6) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  labs(
    title = "Sentiment Distribution of User-Generated Content",
    x = "Sentiment",
    y = "Count"
  ) +
  scale_fill_manual(
    values = c("positive" = "#66BB6A", "negative" = "#EF5350"),
    name = "Release Effects",
    labels = c("Negative", "Positive")
  ) +
  guides(fill = guide_legend(title = "Sentiment", title.position = "top", title.theme = element_text(face = "bold", size = 12)))
ggsave("results/f2.pdf")


#2 Machine learning modelling----
#Factoring the output variable
finaldata$release_effects <- ifelse(finaldata$release_effects == "positive", 1, 0)
# finaldata$release_effects <- factor(finaldata$release_effects, levels = c("positive", "negative"))
colTypes   <- sapply(finaldata, class) 
colNumeric <- which(colTypes == "numeric" | colTypes == "integer")
colNumeric <- colNumeric[colNumeric != which(names(finaldata) == "num_markets") & 
                           colNumeric != which(names(finaldata) == "release_effects")]
finaldata[, colNumeric] <- scale(finaldata[, colNumeric]) 

# Create the model formula
mdlA <- release_effects ~ danceability + energy + key + loudness + mode + speechiness + 
  acousticness + instrumentalness + liveness + valence + tempo

# Create subset of finaldata, with just the sonic characteristics in it
sonic_vars <- c("danceability", "energy", "key", "loudness", "mode", "speechiness", 
                "acousticness", "instrumentalness", "liveness", "valence", "tempo")

finaldata_soniconly <- finaldata[, sonic_vars]

#------------------
# Clustering
#------------------

# Run k-means clustering with 2, 3, and 4 clusters
kmeans_2 <- kmeans(finaldata_soniconly, centers = 2)
kmeans_3 <- kmeans(finaldata_soniconly, centers = 3)
kmeans_4 <- kmeans(finaldata_soniconly, centers = 4)

# Create cluster plots for each k-means result
clusplot(finaldata_soniconly, kmeans_2$cluster, color = TRUE, shade = TRUE, 
         labels = 2, lines = 0, main = "K-means Clustering with 2 Clusters")
clusplot(finaldata_soniconly, kmeans_3$cluster, color = TRUE, shade = TRUE, 
         labels = 2, lines = 0, main = "K-means Clustering with 3 Clusters")
clusplot(finaldata_soniconly, kmeans_4$cluster, color = TRUE, shade = TRUE, 
         labels = 2, lines = 0, main = "K-means Clustering with 4 Clusters")

# 3 clusters is probably the best, since it becomes more specific within each cluster. 

#------------------
# Further explore k-means clustering
#------------------

# Provide a descriptive summary per assigned cluster for cluster interpretation
# with describeBy from psych package
tmp <- describeBy(finaldata_soniconly, group = kmeans_3$cluster)

# Make a table with means and standard deviations
# (columns 3 and 4 in the tmp object) of the sonic characteristics per assigned cluster
stargazer(cbind(tmp$`1`[3:4], tmp$`2`[3:4], tmp$`3`[3:4]),
          type = "text", align = TRUE, no.space = TRUE, summary = FALSE)

# Provide the decision tree for cluster interpretation
# Model
mdlA_finaldata <- cluster ~ danceability + energy + key + loudness + mode + speechiness + 
  acousticness + instrumentalness + liveness + valence + tempo

# Add cluster assignments to the finaldata
finaldata$cluster <- kmeans_3$cluster

# Train the tree
tree_finaldata <- rpart(mdlA_finaldata, data = finaldata, 
                        method = "class", parms = list(split = "information"))

# Plot the decision tree
rpart.plot(tree_finaldata,
           main = "Decision tree for finaldata dataset, K = 3",
           box.col = c("yellow", "orange", "green")[tree_finaldata$frame$yval],
           extra = 104)

# Split the dataset into groups based on the cluster labels
cluster_groups <- split(finaldata, finaldata$cluster)

# Function to print the first five song names in each cluster
print_first_five_song_names <- function(cluster_groups) {
  for (i in 1:length(cluster_groups)) {
    cat("\n\nCluster", i, ":\n")
    cat(paste(head(cluster_groups[[i]]$album_name, n = 5), collapse = "\n"))
  }
}

# Call the function to print the first five song names in each cluster
print_first_five_song_names(cluster_groups)

# Cluster 1: Upbeat Party Albums
#   
# High danceability
# High energy
# Loud
# Low acousticness
# High valence (positive mood)
# 
# This cluster seems to represent albums with high energy, danceable, and positive 
# songs that are generally louder and less acoustic. The albums in this cluster might 
# be more suitable for parties or upbeat environments.
# 
# Cluster 2: Relaxed & Introspective Albums
#   
# Lower danceability
# Low energy
# Quieter (compared to Cluster 1)
# High acousticness
# Low valence (negative mood)
# 
# This cluster represents albums with songs that have lower danceability, lower energy, 
# and a more negative mood. These songs are quieter and more acoustic in nature, 
# suggesting a more relaxed and introspective listening experience. The albums in 
# this cluster might be more suitable for calm environments or background music.
# 
# Cluster 3: High-Energy Live Experience Albums
#   
# Very low danceability
# High energy
# Moderately loud
# Low acousticness
# Very low valence (negative mood)
# High liveness
# 
# This cluster represents albums with songs that have very low danceability, high 
# energy, and a very negative mood. These songs are moderately loud and less acoustic, 
# with a notably high level of liveness. The high liveness suggests that the albums 
# in this cluster might contain more live recordings, or the songs might have a more 
# authentic or raw feel to them. These albums could be more suitable for a focused 
# listening experience or for fans of live music.


#------------------
# Logit, SVM, Tree, RF, and KNN
#------------------

finaldata$release_effects <- factor(finaldata$release_effects, levels = c(0, 1))

# Set seed and create the folds
set.seed(123)

nFolds <- 5
myFolds <- cut(seq(1, nrow(finaldata)), 
               breaks = nFolds, 
               labels = FALSE)
#table(myFolds)

# Initialize accuracies in empty vectors
logit_accuracy <- rep(NA, nFolds)
svm_accuracy <- rep(NA, nFolds)
tree_accuracy <- rep(NA, nFolds)
rf_accuracy <- rep(NA, nFolds)
knn_accuracy <- rep(NA, nFolds)

# Initialize lists to store confusion matrices for each model
logit_tables <- vector("list", nFolds)
svm_tables <- vector("list", nFolds)
tree_tables <- vector("list", nFolds)
rf_tables <- vector("list", nFolds)
knn_tables <- vector("list", nFolds)


vars_per_split <- round(sqrt((length(all.vars(mdlA))-1)))

# Cross-validation
for (i in 1:nFolds) {
  #cat("Working on fold", i, "\n") # solely to keep track of process in console
  
  # Training and testing data for each fold
  train_data <- finaldata[myFolds != i,]
  test_data <- finaldata[myFolds == i,]

  # Logistic Regression
  logit_model <- glm(mdlA, data = train_data, family = "binomial")
  logit_probs <- predict(logit_model, test_data, type = "response")
  logit_preds <- ifelse(logit_probs > 0.5, 1, 0)
  logit_accuracy[i] <- mean(logit_preds == test_data$release_effects)
  logit_tables[[i]] <- table(Predicted = logit_preds, Actual = test_data$release_effects)
  
  
  # SVM
  svm_model <- svm(mdlA, data = train_data, type = "C-classification")
  svm_pred <- predict(svm_model, test_data)
  svm_accuracy[i] <- mean(svm_pred == test_data$release_effects)
  svm_tables[[i]] <- table(Predicted = svm_pred, Actual = test_data$release_effects)
  
  
  # Classification Tree
  tree_model <- rpart(mdlA, data = train_data, method = "class", 
                      parms = list(split = "information"))
  tree_pred <- predict(tree_model, test_data, type = "class")
  tree_accuracy[i] <- mean(tree_pred == test_data$release_effects)
  tree_tables[[i]] <- table(Predicted = tree_pred, Actual = test_data$release_effects)
  
  
  # Random Forest
  rf_model <- randomForest(mdlA, data = train_data, ntree = 100, mtry = vars_per_split, 
                           importance = TRUE)
  rf_pred <- predict(rf_model, test_data, type = "class")
  rf_accuracy[i] <- mean(rf_pred == test_data$release_effects)
  rf_tables[[i]] <- table(Predicted = rf_pred, Actual = test_data$release_effects)
  
  
  # KNN
  knn_preds <- knn(train = train_data[, sonic_vars], test = test_data[, sonic_vars], cl = train_data$release_effects, k = 5)
  knn_accuracy[i] <- mean(knn_preds == test_data$release_effects)
  knn_tables[[i]] <- table(Predicted = knn_preds, Actual = test_data$release_effects)
  
}

accuracies <- cbind(logit_accuracy, svm_accuracy, tree_accuracy, rf_accuracy, knn_accuracy)

describe(accuracies)
stargazer(accuracies, summary = TRUE, align = TRUE, no.space = TRUE, type = "text")

# Create a named vector with just the mean accuracies of the models
mean_accuracies <- c(Logistic_Regression = mean(logit_accuracy),
                     SVM = mean(svm_accuracy), 
                     Classification_Tree = mean(tree_accuracy), 
                     Random_Forest = mean(rf_accuracy),
                     KNN = mean(knn_accuracy))

# Find the name of the model with the highest mean accuracy
best_model <- names(which.max(mean_accuracies))

# Print a nicely formatted sentence that explains which model performed the best
cat("The model that performed best is the", best_model, 
    "model, because it has an \n accuracy of",
    round(mean_accuracies[best_model], 4) * 100, "%.",
    "\n\nHowever, accuracy is not the only important performance metric.",
    "\nOther metrics are important, and it's rather context dependent.")

#------------------
# Neural Network
#------------------

get_nn_accuracies <- function(data, model, n) {
  train_index <- seq_len(floor(0.7 * nrow(data)))
  train_data <- data[train_index,]
  test_data <- data[-train_index,]
  
  nn_model <- nnet(model, data = train_data, maxit = 300, size = n, trace = FALSE)
  
  train_pred <- predict(nn_model, train_data, type = "class")
  train_accuracy <- mean(train_pred == train_data$release_effects)
  
  test_pred <- predict(nn_model, test_data, type = "class")
  test_accuracy <- mean(test_pred == test_data$release_effects)
  
  # Calculate the confusion matrix
  confusion_matrix <- table(Predicted = factor(test_pred, levels = 0:1),
                            Actual = factor(test_data$release_effects, levels = 0:1))

  
  return(list(train_accuracy = train_accuracy, test_accuracy = test_accuracy, confusion_matrix = confusion_matrix))
}

# Set seed once more
set.seed(123)

# Loop over different sizes of hidden layers
max_layers <- 15
nn_results <- list()

for (n in 1:max_layers) {
  #cat("Working on neural network with", n, "hidden layers\n") #to keep track of process
  nn_results[[n]] <- get_nn_accuracies(finaldata, mdlA, n)
}

# Create a data frame to store the results
nn_results_df <- data.frame(n = 1:max_layers,
                            Train_Accuracy = sapply(nn_results, "[[", "train_accuracy"),
                            Test_Accuracy = sapply(nn_results, "[[", "test_accuracy"))

# Find the index of the best test accuracy
best_test_accuracy_index <- which.max(nn_results_df$Test_Accuracy)

# Train the neural network model with the best number of hidden layers
best_n_hidden_layers <- nn_results_df$n[best_test_accuracy_index]
nn_model <- nnet(mdlA, data = finaldata, maxit = 300, size = best_n_hidden_layers, trace = FALSE)

# Calculate probabilities for the neural network model
nn_probs <- predict(nn_model, finaldata, type = "raw")
nn_preds <- ifelse(nn_probs > 0.5, 1, 0)

# Get the confusion matrix for the best model
best_confusion_matrix_NN <- nn_results[[best_test_accuracy_index]]$confusion_matrix

# Plot the graph
ggplot(nn_results_df, aes(x = n)) +
  geom_line(aes(y = Train_Accuracy, color = "Train")) +
  geom_line(aes(y = Test_Accuracy, color = "Test")) +
  labs(title = "Neural Network Performance",
       x = "Hidden Layers",
       y = "Accuracy") +
  scale_color_manual(values = c("Train" = "blue", "Test" = "red"),
                     labels = c("Test", "Train")) +
  theme_minimal()

#------------------
# Confusion matrices
#------------------

# Function to calculate the mean confusion matrix table
mean_confusion_matrix_table <- function(tables) {
  sum_table <- Reduce("+", tables)
  mean_table <- sum_table / length(tables)
  return(mean_table)
}

# Calculate mean confusion matrix tables for each model
mean_logit_table <- mean_confusion_matrix_table(logit_tables)
mean_svm_table <- mean_confusion_matrix_table(svm_tables)
mean_tree_table <- mean_confusion_matrix_table(tree_tables)
mean_rf_table <- mean_confusion_matrix_table(rf_tables)
mean_knn_table <- mean_confusion_matrix_table(knn_tables)

# Print mean confusion matrix tables
cat("Mean Confusion Matrix Table for Logistic Regression:\n")
print(mean_logit_table)
cat("\nMean Confusion Matrix Table for SVM:\n")
print(mean_svm_table)
cat("\nMean Confusion Matrix Table for Classification Tree:\n")
print(mean_tree_table)
cat("\nMean Confusion Matrix Table for Random Forest:\n")
print(mean_rf_table)
cat("\nMean Confusion Matrix Table for KNN:\n")
print(mean_knn_table)

# The NN one:
# Print confusion matrices
cat("Best Neural Network Confusion Matrices:\n")
print(best_confusion_matrix_NN)

#------------------
# Sensitivity, specificity, and precision
#------------------

# Logistic Regression
logit_sensitivity <- mean_logit_table[2, 2] / (mean_logit_table[2, 2] + 
                                                 mean_logit_table[1, 2])
logit_specificity <- mean_logit_table[1, 1] / (mean_logit_table[1, 1] + 
                                                 mean_logit_table[2, 1])
logit_precision <- mean_logit_table[2, 2] / (mean_logit_table[2, 2] + 
                                               mean_logit_table[2, 1])

# SVM
svm_sensitivity <- mean_svm_table[2, 2] / (mean_svm_table[2, 2] + mean_svm_table[1, 2])
svm_specificity <- mean_svm_table[1, 1] / (mean_svm_table[1, 1] + mean_svm_table[2, 1])
svm_precision <- mean_svm_table[2, 2] / (mean_svm_table[2, 2] + mean_svm_table[2, 1])

# Classification Tree
tree_sensitivity <- mean_tree_table[2, 2] / (mean_tree_table[2, 2] + mean_tree_table[1, 2])
tree_specificity <- mean_tree_table[1, 1] / (mean_tree_table[1, 1] + mean_tree_table[2, 1])
tree_precision <- mean_tree_table[2, 2] / (mean_tree_table[2, 2] + mean_tree_table[2, 1])

# Random Forest
rf_sensitivity <- mean_rf_table[2, 2] / (mean_rf_table[2, 2] + mean_rf_table[1, 2])
rf_specificity <- mean_rf_table[1, 1] / (mean_rf_table[1, 1] + mean_rf_table[2, 1])
rf_precision <- mean_rf_table[2, 2] / (mean_rf_table[2, 2] + mean_rf_table[2, 1])

# KNN
knn_sensitivity <- mean_knn_table[2, 2] / (mean_knn_table[2, 2] + mean_knn_table[1, 2])
knn_specificity <- mean_knn_table[1, 1] / (mean_knn_table[1, 1] + mean_knn_table[2, 1])
knn_precision <- mean_knn_table[2, 2] / (mean_knn_table[2, 2] + mean_knn_table[2, 1])

# NN
nn_sensitivity <- best_confusion_matrix_NN[2, 2] / (best_confusion_matrix_NN[2, 2] + best_confusion_matrix_NN[1, 2])
nn_specificity <- best_confusion_matrix_NN[1, 1] / (best_confusion_matrix_NN[1, 1] + best_confusion_matrix_NN[2, 1])
nn_precision <- best_confusion_matrix_NN[2, 2] / (best_confusion_matrix_NN[2, 2] + best_confusion_matrix_NN[2, 1])

# Display the results
cat("Logistic Regression\n",
    "Sensitivity:", logit_sensitivity, "\n",
    "Specificity:", logit_specificity, "\n",
    "Precision:  ", logit_precision, "\n\n")

# Display the results for SVM
cat("SVM\n",
    "Sensitivity:", svm_sensitivity, "\n",
    "Specificity:", svm_specificity, "\n",
    "Precision:  ", svm_precision, "\n\n")

# Display the results for Classification Tree
cat("Classification Tree\n",
    "Sensitivity:", tree_sensitivity, "\n",
    "Specificity:", tree_specificity, "\n",
    "Precision:  ", tree_precision, "\n\n")

cat("Random Forest\n",
    "Sensitivity:", rf_sensitivity, "\n",
    "Specificity:", rf_specificity, "\n",
    "Precision:  ", rf_precision, "\n\n")

cat("KNN\n",
    "Sensitivity:", knn_sensitivity, "\n",
    "Specificity:", knn_specificity, "\n",
    "Precision:  ", knn_precision, "\n\n")

cat("Neural Network\n",
    "Sensitivity:", nn_sensitivity, "\n",
    "Specificity:", nn_specificity, "\n",
    "Precision:  ", nn_precision, "\n\n")

#------------------
# ROC curves and AUC
#------------------

# Function to calculate performance and AUC for models
get_model_perf_auc <- function(predictions, actuals) {
  pred <- prediction(predictions, actuals)
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  auc <- performance(pred, measure = "auc")@y.values[[1]]
  return(list(performance = perf, AUC = auc))
}

# Logistic Regression
logit_probs <- predict(logit_model, finaldata, type = "response")
logit_roc <- get_model_perf_auc(logit_probs, finaldata$release_effects)

# SVM
svm_probs <- attr(predict(svm_model, finaldata, decision.values = TRUE), "decision.values")
svm_roc <- get_model_perf_auc(svm_probs, finaldata$release_effects)

# Classification Tree
tree_probs <- predict(tree_model, finaldata, type = "prob")[,2]
tree_roc <- get_model_perf_auc(tree_probs, finaldata$release_effects)

# Random Forest
rf_probs <- predict(rf_model, finaldata, type = "prob")[,2]
rf_roc <- get_model_perf_auc(rf_probs, finaldata$release_effects)

# KNN
knn_probs <- knn(train = train_data[, sonic_vars], test = finaldata[, sonic_vars], cl = train_data$release_effects, k = 5, prob = TRUE)
knn_roc <- get_model_perf_auc(attr(knn_probs, "prob"), finaldata$release_effects)


# Check the number of columns in nn_probs
if (ncol(nn_probs) == 1) {
  nn_probs_positive <- nn_probs
} else {
  nn_probs_positive <- nn_probs[, 2]
}

# Get predictions for the neural network
nn_pred_rocr <- prediction(nn_probs_positive, finaldata$release_effects)
nn_perf <- performance(nn_pred_rocr, "tpr", "fpr")
nn_auc <- performance(nn_pred_rocr, "auc")@y.values[[1]]


# Neural Network: Get predictions for the neural network
nn_pred_rocr <- prediction(nn_probs[, 2], finaldata$release_effects)
nn_perf <- performance(nn_pred_rocr, "tpr", "fpr")
nn_auc <- performance(nn_pred_rocr, "auc")@y.values[[1]]

#Creating figure 2 in report
# Plot ROC curves
plot(logit_roc$performance, main = "ROC Curves", col = "darkblue", lty = 1, lwd = 2.0)
plot(svm_roc$performance, col = "red", lty = 1, lwd = 2.0, add = TRUE)
plot(tree_roc$performance, col = "orange", lty = 1, lwd = 2.0, add = TRUE)
plot(rf_roc$performance, col = "darkgreen", lty = 1, lwd = 2.0, add = TRUE)
plot(knn_roc$performance, col = "purple", lty = 1, lwd = 2.0, add = TRUE)
plot(nn_perf, col = "darkcyan", lty = 1, lwd = 2.0, add = TRUE)
abline(a = 0, b = 1, lty = 3, lwd = 1.5)

# Add legend
legend("bottomright", legend = c("Logistic Regression", "SVM", "Classification Tree", "Random Forest", "KNN", "Neural Network"), 
       col = c("darkblue", "red", "orange", "darkgreen", "purple", "darkcyan"), lty = 1, lwd = 2.0)
ggsave("results/f2.pdf")

# Print AUCs
cat("Logistic Regression AUC:", logit_roc$AUC, "\n")
cat("SVM AUC:", svm_roc$AUC, "\n")
cat("Classification Tree AUC:", tree_roc$AUC, "\n")
cat("Random Forest AUC:", rf_roc$AUC, "\n")
cat("KNN AUC:", knn_roc$AUC, "\n")
cat("Neural Network AUC:", nn_auc, "\n")






