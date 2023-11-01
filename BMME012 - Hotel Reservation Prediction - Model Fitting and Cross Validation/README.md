# Hotel Reservations Prediction 🏨 Model Fitting, Overfitting, and Cross Validation

## Overview

This assignment focuses on analyzing a dataset of hotel reservations with the help of multiple machine learning models. The objective is to predict the likelihood of a booking being canceled based on various attributes. The dataset consists of 36,275 hotel reservations with multiple features like the number of adults, type of meal plan, room type, lead time, and many others.

## Data Preprocessing

**Data set**: `data_hotel_reservations.csv`

- Load the dataset and inspect it.
- Create a new column `booking_canceled`, which will be 1 if the `booking_status` is 'Canceled', and 0 otherwise.
- Replace NA values in `no_of_special_requests` with 0.
- Remove `Booking_ID` and `booking_status` columns.

## Models and Techniques Used

1. **Linear Probability Model & LASSO Regression**
    - Predict the cancellations using a linear probability model.
    - Apply LASSO regression with lambda = 0.01 and display the output.

2. **Cross-Validation using Multiple Models**
    - Use the first 10,000 observations.
    - Create 5 folds for cross-validation.
    - Build models using Support Vector Machines (SVM), Classification Trees, and Random Forest.
    - Calculate average accuracy for each model.

3. **Neural Networks**
    - Write a function to create a neural network model with varying hidden layers (1 to 15).
    - Train the model on 70% of the data and test on the remaining 30%.
    - Plot a graph depicting the performance of each model configuration.


## Evaluation Metrics

- Coefficients from Linear and LASSO regression.
- Average accuracy for SVM, Classification Tree, and Random Forest.
- Training and Test set accuracy for Neural Networks.

