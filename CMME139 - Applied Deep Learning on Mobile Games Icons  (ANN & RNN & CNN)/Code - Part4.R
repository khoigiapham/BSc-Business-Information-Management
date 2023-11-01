# Load the required libraries
library(tidyverse)
library(imager)
library(keras)
library(EBImage)


# Set wd
path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
rm(list=ls())

#Load select games
selected_games  <- readRDS("selected_games.RDS")
selected_games <- selected_games %>% 
  select(ID) %>%
  distinct(ID, .keep_all = TRUE)
# Add a 'User Rating Count' column to your selected_games dataframe
selected_games$User_Rating_Count <- c(rep(1, 200), rep(0, 200))  # High: 1, Low: 0

#3. Image preporcessing----
# Define the directory where images are stored
image_dir <- "C:/Users/pkhoi/OneDrive - Erasmus University Rotterdam/2 Deep Learning/Assignment 4/game_images_1"
# List all files in the directory
filenames <- list.files(path = image_dir, full.names = TRUE)
# Initialize an empty array to store image data
image_data <- array(0, dim = c(length(filenames), 128, 128, 3))  # For color images
# Initialize an empty vector to store the labels
image_labels <- numeric(length(filenames))
# Lop over all files
for(i in 1:length(filenames)) {
  # Extract ID from the filename
  id <- as.numeric(str_extract(basename(filenames[i]), "^\\d+"))
    # Find the corresponding label in the selected_games dataframe
  label <- selected_games$User_Rating_Count[selected_games$ID == id]
    # If the ID was not found in selected_games, label as NA
    # Assign the label
  image_labels[i] <- label
    # Read the image
  img <- readImage(filenames[i])
  # Resize the image
  img <- resize(img, w = 128, h = 128)
    # Add image data to our array
  image_data[i, , , ] <- img
}

#Create train and test:
# Split the image_data into training and testing sets
train_ratio <- 0.7
test_ratio <- 0.3
# Determine the number of samples for each set
num_samples <- dim(image_data)[1]
num_train <- round(train_ratio * num_samples)
num_test <- num_samples - num_train
# Create a vector of indices for shuffling
indices <- sample(num_samples)
# Split the image_data and image_label using the shuffled indices
x_train <- image_data[indices[1:num_train], , , ]
y_train <- image_labels[indices[1:num_train]]
x_test <- image_data[indices[(num_train + 1):num_samples], , , ]
y_test <- image_labels[indices[(num_train + 1):num_samples]]
#restructure the objective
y_train <- to_categorical(y_train, 2)
y_test <- to_categorical(y_test, 2)


#Build the model----
# Initialize the model
model <- keras_model_sequential()
# Add layers to the model
model %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = c(128, 128, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%

  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate = 0.25) %>%
  
  layer_flatten() %>%
  
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 2, activation = 'softmax')  

# Compile the model
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Train the model
set.seed(123)
history <- model %>% fit(
  x = x_train,
  y = y_train,
  epochs = 10,  # You might want to adjust this
  batch_size = 32,  # And this
  validation_split = 0.2  # And this
)
# Evaluate the model on the test data
scores <- model %>% evaluate(x_test, y_test)
print(scores)


#Model 2----
# Initialize the model2
model2 <- keras_model_sequential()
# Add layers to the model2
model2 %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = c(128, 128, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate = 0.25) %>%
  
  layer_flatten() %>%
  
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 2, activation = 'softmax')  

# Compile the model2
model2 %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Train the model2
set.seed(123)
history <- model2 %>% fit(
  x = x_train,
  y = y_train,
  epochs = 10,  # You might want to adjust this
  batch_size = 32,  # And this
  validation_split = 0.2  # And this
)
# Evaluate the model2 on the test data
scores2 <- model2 %>% evaluate(x_test, y_test)
print(scores2)

#Model 3----
# Initialize the model
model3 <- keras_model_sequential()

# Add layers to the model
model3 %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = c(128, 128, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate = 0.25) %>% 
  
  # Additional convolutional layer
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate = 0.25) %>% 
  
  layer_flatten() %>%
  
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 2, activation = 'softmax')  

# Compile the model
model3 %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)
# Train the model2
set.seed(123)
history <- model3 %>% fit(
  x = x_train,
  y = y_train,
  epochs = 10,  # You might want to adjust this
  batch_size = 32,  # And this
  validation_split = 0.2  # And this
)
# Evaluate the model2 on the test data
scores3 <- model3 %>% evaluate(x_test, y_test)
print(scores3)

#model 3a:
#Build the model----
# Initialize the model
model3a <- keras_model_sequential()
# Add layers to the model3a
model3a %>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu', input_shape = c(128, 128, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate = 0.25) %>%
  
  layer_flatten() %>%
  
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 2, activation = 'softmax')  

# Compile the model3a
model3a %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Train the model3a
set.seed(123)
history <- model3a %>% fit(
  x = x_train,
  y = y_train,
  epochs = 10,  # You might want to adjust this
  batch_size = 32,  # And this
  validation_split = 0.2  # And this
)
# Evaluate the model3a on the test data
scores <- model3a %>% evaluate(x_test, y_test)
print(scores)

#model 3b----
#Build the model3b----
# Initialize the model3b
model3b <- keras_model_sequential()
# Add layers to the model3b
model3b %>%
  layer_conv_2d(filters = 16, kernel_size = c(3,3), activation = 'relu', input_shape = c(128, 128, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate = 0.25) %>%
  
  layer_flatten() %>%
  
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 2, activation = 'softmax')  

# Compile the model3b
model3b %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Train the model3b
set.seed(123)
history <- model3b %>% fit(
  x = x_train,
  y = y_train,
  epochs = 10,  # You might want to adjust this
  batch_size = 32,  # And this
  validation_split = 0.2  # And this
)
# Evaluate the model3b on the test data
scores <- model3b %>% evaluate(x_test, y_test)
print(scores)

##model 4a----
#Build the model4a----
# Initialize the model4a
model4a <- keras_model_sequential()
# Add layers to the model4a
model4a %>%
  layer_conv_2d(filters = 32, kernel_size = c(5,5), activation = 'relu', input_shape = c(128, 128, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  
  layer_conv_2d(filters = 64, kernel_size = c(5,5), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate = 0.25) %>%
  
  layer_flatten() %>%
  
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 2, activation = 'softmax')  

# Compile the model4a
model4a %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Train the model4a
set.seed(123)
history <- model4a %>% fit(
  x = x_train,
  y = y_train,
  epochs = 10,  # You might want to adjust this
  batch_size = 32,  # And this
  validation_split = 0.2  # And this
)
# Evaluate the model4a on the test data
scores <- model4a %>% evaluate(x_test, y_test)
print(scores)

#Model 4b----
#Build the model4b----
# Initialize the model4b
model4b <- keras_model_sequential()
# Add layers to the model4b
model4b %>%
  layer_conv_2d(filters = 32, kernel_size = c(2,2), activation = 'relu', input_shape = c(128, 128, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  
  layer_conv_2d(filters = 64, kernel_size = c(2,2), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate = 0.25) %>%
  
  layer_flatten() %>%
  
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 2, activation = 'softmax')  

# Compile the model4b
model4b %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Train the model4b
set.seed(123)
history <- model4b %>% fit(
  x = x_train,
  y = y_train,
  epochs = 10,  # You might want to adjust this
  batch_size = 32,  # And this
  validation_split = 0.2  # And this
)
# Evaluate the model4b on the test data
scores <- model4b %>% evaluate(x_test, y_test)
print(scores)

#Model 5a----
#Build the model5a----
# Initialize the model5a
model5a <- keras_model_sequential()
# Add layers to the model5a
model5a %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = c(128, 128, 3)) %>%
  layer_max_pooling_2d(pool_size = c(3,3)) %>%
  
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(3,3)) %>%
  layer_dropout(rate = 0.25) %>%
  
  layer_flatten() %>%
  
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 2, activation = 'softmax')  

# Compile the model5a
model5a %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Train the model5a
set.seed(123)
history <- model5a %>% fit(
  x = x_train,
  y = y_train,
  epochs = 10,  # You might want to adjust this
  batch_size = 32,  # And this
  validation_split = 0.2  # And this
)
# Evaluate the model5a on the test data
scores <- model5a %>% evaluate(x_test, y_test)
print(scores)

#model5b 5b----
#Build the model5b----
# Initialize the model5b
model5b <- keras_model_sequential()
# Add layers to the model5b
model5b %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = c(128, 128, 3)) %>%
  layer_max_pooling_2d(pool_size = c(1,1)) %>%
  
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(1,1)) %>%
  layer_dropout(rate = 0.25) %>%
  
  layer_flatten() %>%
  
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 2, activation = 'softmax')  

# Compile the model5b
model5b %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Train the model5b
set.seed(123)
history <- model5b %>% fit(
  x = x_train,
  y = y_train,
  epochs = 100,  # You might want to adjust this
  batch_size = 32,  # And this
  validation_split = 0.2  # And this
)
# Evaluate the model5b on the test data
scores <- model5b %>% evaluate(x_test, y_test)
print(scores)
loss = 'categorical_crossentropy',
optimizer = optimizer_rmsprop(),
metrics = c('accuracy')
)

# Train the model3b
set.seed(123)
history <- model3b %>% fit(
  x = x_train,
  y = y_train,
  epochs = 10,  # You might want to adjust this
  batch_size = 32,  # And this
  validation_split = 0.2  # And this
)
# Evaluate the model3b on the test data
scores <- model3b %>% evaluate(x_test, y_test)
print(scores)

##model 4a----
#Build the model4a----
# Initialize the model4a
model4a <- keras_model_sequential()
# Add layers to the model4a
model4a %>%
  layer_conv_2d(filters = 32, kernel_size = c(5,5), activation = 'relu', input_shape = c(128, 128, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  
  layer_conv_2d(filters = 64, kernel_size = c(5,5), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate = 0.25) %>%
  
  layer_flatten() %>%
  
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 2, activation = 'softmax')  

# Compile the model4a
model4a %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Train the model4a
set.seed(123)
history <- model4a %>% fit(
  x = x_train,
  y = y_train,
  epochs = 10,  # You might want to adjust this
  batch_size = 32,  # And this
  validation_split = 0.2  # And this
)
# Evaluate the model4a on the test data
scores <- model4a %>% evaluate(x_test, y_test)
print(scores)

#Model 4b----
#Build the model4b----
# Initialize the model4b
model4b <- keras_model_sequential()
# Add layers to the model4b
model4b %>%
  layer_conv_2d(filters = 32, kernel_size = c(2,2), activation = 'relu', input_shape = c(128, 128, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  
  layer_conv_2d(filters = 64, kernel_size = c(2,2), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate = 0.25) %>%
  
  layer_flatten() %>%
  
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 2, activation = 'softmax')  

# Compile the model4b
model4b %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Train the model4b
set.seed(123)
history <- model4b %>% fit(
  x = x_train,
  y = y_train,
  epochs = 10,  # You might want to adjust this
  batch_size = 32,  # And this
  validation_split = 0.2  # And this
)
# Evaluate the model4b on the test data
scores <- model4b %>% evaluate(x_test, y_test)
print(scores)

#Model 5a----
#Build the model5a----
# Initialize the model5a
model5a <- keras_model_sequential()
# Add layers to the model5a
model5a %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = c(128, 128, 3)) %>%
  layer_max_pooling_2d(pool_size = c(3,3)) %>%
  
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(3,3)) %>%
  layer_dropout(rate = 0.25) %>%
  
  layer_flatten() %>%
  
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 2, activation = 'softmax')  

# Compile the model5a
model5a %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Train the model5a
set.seed(123)
history <- model5a %>% fit(
  x = x_train,
  y = y_train,
  epochs = 10,  # You might want to adjust this
  batch_size = 32,  # And this
  validation_split = 0.2  # And this
)
# Evaluate the model5a on the test data
scores <- model5a %>% evaluate(x_test, y_test)
print(scores)

#model5b 5b----
#Build the model5b----
# Initialize the model5b
model5b <- keras_model_sequential()
# Add layers to the model5b
model5b %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = c(128, 128, 3)) %>%
  layer_max_pooling_2d(pool_size = c(1,1)) %>%
  
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(1,1)) %>%
  layer_dropout(rate = 0.25) %>%
  
  layer_flatten() %>%
  
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  
  layer_dense(units = 2, activation = 'softmax')  

# Compile the model5b
model5b %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Train the model5b
set.seed(123)
history <- model5b %>% fit(
  x = x_train,
  y = y_train,
  epochs = 100,  # You might want to adjust this
  batch_size = 32,  # And this
  validation_split = 0.2  # And this
)
# Evaluate the model5b on the test data
scores <- model5b %>% evaluate(x_test, y_test)
print(scores)
