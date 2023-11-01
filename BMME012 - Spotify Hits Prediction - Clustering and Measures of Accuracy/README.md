# Spotify Hits Prediction 🟢🎧🎵 Clustering and Measures of Accuracy  

## Overview

The project focuses on clustering and predictive analysis of top-performing songs on Spotify. We will use two datasets, `SpotifyTop10s.csv` and `SpotifyTop50country_prepared.csv`, for various tasks including k-means clustering, logit models, random forests, and K-nearest neighbors algorithm.

## Data Preprocessing

**Datasets**:
- `SpotifyTop10s.csv`
- `SpotifyTop50country_prepared.csv`

**Tasks**:
- Load and examine both datasets.
- Handle missing values appropriately.
  
## Questions to Answer

1. **k-means Clustering for Song Characteristics**
    - Run k-means clustering with 2, 3, and 4 clusters on `SpotifyTop10s.csv`.
    - Examine overlap between clusters using Cluster plots and explain the optimal number of clusters. (1.5 points)

2. **Cluster Interpretation**
    - Run k-means with three clusters and assign a meaningful name for each cluster based on their characteristics. Explain your choice. (2 points)

3. **Predicting International Hits**
    - Use logit model, random forest, and K-nearest neighbors (K=5) to predict international hits based on nine sonic characteristics.
    - Compute the accuracy of each model on a test set comprising 30% of the original data. (2 points)

4. **Model Comparison**
    - Examine the confusion matrix for all models and explain why a naïve "model" might perform similarly in terms of accuracy. (1 point)

5. **Alternative Measures**
    - Discuss whether specificity, sensitivity, or precision might be a preferable measure in this case. (1 point)

6. **ROC Curve Analysis**
    - Plot the ROC curve for logit and random forest models.
    - Compute the area below the ROC curve for both. (1 point)

7. **Advisory to Record Label**
    - Discuss the financial implications of using a random forest model for deciding if a song is worthy of production. Use expected value calculations. (1.5 points)


## Evaluation Metrics

- Cluster Interpretation
- Model Accuracy
- ROC Curve Analysis
- Financial Implications
