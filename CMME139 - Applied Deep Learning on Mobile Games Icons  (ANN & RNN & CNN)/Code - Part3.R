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

str(data)

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

# Measure time for the entire code block
time <- system.time({
  # Task 3: Create baseline model-------------------------------------------------
  ## 1 Create the model
  model <- keras_model_sequential()
  model %>%
    layer_dense(units = 32, activation = 'relu', input_shape = c(18)) %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 16, activation = 'relu') %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 8, activation = 'relu') %>%
    layer_dropout(rate = 0.1) %>%
    layer_dense(units = 2, activation = 'softmax')
  summary(model)
  ## 2 Compile the model
  model %>% compile(
    optimizer = 'rmsprop',
    loss = 'categorical_crossentropy',
    metrics = c('accuracy')
  )
  ## 3 Train the model
  mymodel <- model %>%   
    fit(training, 
        trainingtarget,  
        epochs = 50,
        batch_size = 32, #samples per gradient update default 32
        validation_split = 0.2)
  plot(mymodel)
}) # end of system.time()
print(time)
## 4 Evaluate the model
result <- model %>%  evaluate(test, testtarget)



## Structure of the neural network----
time2 <- system.time({
  ## 1 Create the model
  model2 <- keras_model_sequential()
  model2 %>%
    layer_dense(units = 20, activation = 'relu', input_shape = c(18)) %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 20, activation = 'relu') %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 20, activation = 'relu') %>%
    layer_dropout(rate = 0.1) %>%
    layer_dense(units = 2, activation = 'softmax')
  summary(model2)
  ## 2 Compile the model
  model2 %>% compile(
    optimizer = 'rmsprop',
    loss = 'categorical_crossentropy',
    metrics = c('accuracy')
  )
  ## 3 Train the model
  mymodel2 <- model2 %>%   
    fit(training, 
        trainingtarget,  
        epochs = 50,
        batch_size = 32, #samples per gradient update default 32
        validation_split = 0.2)
  plot(mymodel2)
}) # end of system.time()
print(time2)
## 4 Evaluate the model
result2 <- model2 %>%  evaluate(test, testtarget)


## Class imbalance:
## Structure of the neural network----
time3 <- system.time({
  ## 1 Create the model
  model3 <- keras_model_sequential()
  model3 %>%
    layer_dense(units = 32, activation = 'relu', input_shape = c(18)) %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 16, activation = 'relu') %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 8, activation = 'relu') %>%
    layer_dropout(rate = 0.1) %>%
    layer_dense(units = 2, activation = 'softmax')
  summary(model3)
  ## 2 Compile the model
  model3 %>% compile(
    optimizer = 'rmsprop',
    loss = 'categorical_crossentropy',
    metrics = c('accuracy')
  )
  ## 3 Train the model
  mymodel3 <- model3 %>%   
    fit(training, 
        trainingtarget,  
        epochs = 50,
        batch_size = 32, #samples per gradient update default 32
        validation_split = 0.2,
        class_weight = list("0" = 1, "1" = 3)
    )
  plot(mymodel3)
}) # end of system.time()
print(time3)
## 4 Evaluate the model
result3 <- model3 %>%  evaluate(test, testtarget)

