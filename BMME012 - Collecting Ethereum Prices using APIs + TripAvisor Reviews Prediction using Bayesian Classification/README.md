#  Collecting Ethereum Prices using APIs 💸📈🔗 + TripAvisor Reviews Prediction using Bayesian Classification 🌴🏨⭐ 

## Overview

The project comprises two main sections. The first focuses on collecting and analyzing data from APIs related to Ethereum prices. The second part deals with Bayesian Classification applied to a dataset containing TripAdvisor reviews of hotels. 

## Data Sources

1. **Ethereum Price Data**: To be collected using CoinGecko API
2. **TripAdvisor Reviews**: `data_tripadvisor.csv`
---

## Part 1: Collecting Data using APIs (Ethereum Prices)

- **Q1**: Use CoinGecko API to fetch the historical USD prices of Ether for the past 365 days.
- **Q2**: Plot the price data considering specific date ranges and visual properties.
- **Q3**: Interpret the effect of the Ethereum merge on Ether prices.

## Part 2: Applied - Bayesian Classification (TripAdvisor Reviews)

- **Q4**: Load the data and create a 'helpful' indicator variable.
- **Q5**: Text processing and sentiment analysis on the `review_body` column.
- **Q6**: Calculate conditional probabilities based on the `sentiment` column.
- **Q7**: Apply Bayes’ theorem to evaluate probabilities for reviews mentioning 'hotel' or 'beach'.
- **Q8**: Co-occurrence analysis for identifying combinations of common words in helpful reviews.
- **Q9**: Run Naive Bayes and Random Forest models to predict the 'helpful' indicator, and compare the performance.

---

## Required Packages

- `jsonlite`: For JSON data manipulation
- `ggplot2`: For data visualization
- `syuzhet`: For sentiment analysis
- `arules`: For association rule learning
- `arulesViz`: For visualizing association rules
- `e1071`: For Naive Bayes Classifier
- `ROCR`: For ROC curves
- `randomForest`: For Random Forest Classifier
- `stargazer`: For well-formatted regression tables

---

## Instructions to Run Code

1. Download the required dataset (`data_tripadvisor.csv`) and save it in your working directory.
2. Install the necessary R packages.
3. Open the R Markdown file and execute the code blocks sequentially.

---

## Evaluation Criteria

- Code Quality and Efficiency
- Accuracy of Calculations
- Quality of Visualizations
- Interpretation and Explanation
