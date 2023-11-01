# Load required library
library(tidyverse)
library(caret)
library(caTools)
library(keras)
library(mlbench)

# Set working directory
path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
rm(list=ls())

# Task 1: Importing-------------------------------------------------------------
load(file="data.Rda")

# Task 2: Data preparation------------------------------------------------------
## 2.1 Convert all columns to numeric other than the objective column
data <- data %>% 
  mutate_at(vars(-`Categorical Rating Count`), as.numeric)

## 2.2 One-hot encoding of 'Categorical Rating Count'
data <- cbind(data, model.matrix(~`Categorical Rating Count` - 1, data))
## Removing original 'Categorical Rating Count' column
data$`Categorical Rating Count` <- NULL

## 2.3 Format data to matrix
data <- as.matrix(data)
dimnames(data) <- NULL 

## 2.4 Partition
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob=c(0.7, 0.3))
training <- data[ind==1, 1:18]
test <- data[ind==2, 1:18]
trainingtarget <- data[ind==1, 19:20]
testtarget <- data[ind==2, 19:20]

## 2.5 Feature Scaling
m    <- colMeans(training)
sd   <- apply(training, 2, sd)
training <- scale(training, center = m, scale = sd)
test <- scale(test, center = m, scale = sd)

# Task 3: Build, compile the neural network, and fit training data--------------
## 3.1 Create the model
model <- keras_model_sequential()
model %>%
  layer_dense(units = 32, activation = 'relu', input_shape = c(18)) %>%    
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dense(units = 2, activation = 'softmax')
summary(model)

## 3.2 Compile the model
model %>% compile(
  optimizer = 'rmsprop',
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)
## 3.3 Train the model
mymodel <- model %>%   
  fit(training, 
      trainingtarget, 
      epochs = 50,
      batch_size = 32, #samples per gradient update default 32
      validation_split = 0.2)
plot(mymodel)
## 3.4 Evaluate the model
result <- model %>%  evaluate(test, testtarget)


# Task 4: Parameter Tuning------------------------------------------------------
## 4a Add one layer and add dropout rate----
## 4a.1 Create the model
model2 <- keras_model_sequential()
model2 %>%
  layer_dense(units = 32, activation = 'relu', input_shape = c(18)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 8, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 2, activation = 'softmax')
summary(model2)
## 4a.2 Compile the model
model2 %>% compile(
  optimizer = 'rmsprop',
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)
## 4a.3 Train the model
mymodel2 <- model2 %>%   
  fit(training, 
      trainingtarget, 
      epochs = 50,
      batch_size = 32, #samples per gradient update default 32
      validation_split = 0.2)
plot(mymodel2)
## 4a.4 valuate the model
result2 <- model2 %>%  evaluate(test, testtarget)

## 4b Keep dropout rate & Increase number of neurons----
## 4b.1 Create the model
model3 <- keras_model_sequential()
model3 %>%
  layer_dense(units = 64, activation = 'relu', input_shape = c(18)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 2, activation = 'softmax')
summary(model3)
## 4b.2 Compile the model
model3 %>% compile(
  optimizer = 'rmsprop',
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)
## 4b.3 Train the model
mymodel3 <- model3 %>%   
  fit(training, 
      trainingtarget, 
      epochs = 50,
      batch_size = 32, #samples per gradient update default 32
      validation_split = 0.2)
plot(mymodel3)
## 4b.4 valuate the model
result3 <- model3 %>%  evaluate(test, testtarget)

## 4c Keep number of neurons and Increase dropout rate----
## 4c.1 Create the model
model4 <- keras_model_sequential()
model4 %>%
  layer_dense(units = 32, activation = 'relu', input_shape = c(18)) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 8, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'softmax')
summary(model4)
## 4c.2 Compile the model
model4 %>% compile(
  optimizer = 'rmsprop',
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)
## 4c.3 Train the model
mymodel4 <- model4 %>%   
  fit(training, 
      trainingtarget, 
      epochs = 50,
      batch_size = 32, #samples per gradient update default 32
      validation_split = 0.2)
plot(mymodel4)
## 4c.4 Evaluate the model
result4 <- model4 %>%  evaluate(test, testtarget)

## 4d Increase number of neurons and increase dropout rate----
## 4d.1 Create the model
model5 <- keras_model_sequential()
model5 %>%
  layer_dense(units = 64, activation = 'relu', input_shape = c(18)) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'softmax')
summary(model5)
## 4d.2 Compile the model
model5 %>% compile(
  optimizer = 'rmsprop',
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)
## 4c.3 Train the model
mymodel5 <- model5 %>%   
  fit(training, 
      trainingtarget, 
      epochs = 50,
      batch_size = 32, #samples per gradient update default 32
      validation_split = 0.2)
plot(mymodel5)
## 4c.4 Evaluate the model
result5 <- model5 %>%  evaluate(test, testtarget)

# Task 5: Present results-------------------------------------------------------
# Put results in a list
results <- list("Model 1" = result, "Model 2" = result2, "Model 3" = result3, 
                "Model 4" = result4, "Model 5" = result5)
# Create a data frame for results
results_df <- map_dfr(results, ~as.data.frame(t(.x)), .id = "model")
print(results_df)

# Plot the models
plot(mymodel)
plot(mymodel2)
plot(mymodel3)
plot(mymodel4)
plot(mymodel5)





