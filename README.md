# Anomaly Detection in Automated Control Systems

## Overview
This project focuses on cybersecurity in automated control systems, particularly employing anomaly-detection-based intrusion detection methods. It utilizes Principal Component Analysis (PCA) for feature selection, Hidden Markov Models (HMMs) for model training, and reinforcement learning for investment decision-making. The aim is to enhance cybersecurity awareness and improve anomaly detection methodologies within critical infrastructure.

## Project Structure
- **Feature Engineering**: Data preprocessing, interpolation of missing values, and PCA for feature selection.
- **HMM Training and Testing**: Model training on selected features, evaluation using log-likelihood, Bayesian Information Criterion (BIC), and Akaike Information Criterion (AIC).
- **Anomaly Detection**: Application of trained HMMs on datasets with injected anomalies and comparison with normal data log-likelihoods.
- **Reinforcement Learning**: Implementation of Q-learning for investment decisions with hyperparameter tuning (alpha, gamma, epsilon) and policy computation.

## Technologies Used
- **Programming Language**: R
- **Libraries**:
  - `depmixS4` for Hidden Markov Models
  - `ggplot2` for data visualization
  - `tidyverse` for data manipulation
  - `dplyr` for dataset transformations
  - `caret` for preprocessing and validation
  - `numpy` (for reinforcement learning analysis, if applicable)

## Setup and Installation
### Prerequisites
Ensure you have R installed along with the required libraries. Install missing packages using:
```r
install.packages(c("depmixS4", "ggplot2", "tidyverse", "dplyr", "caret"))
```

### Running the Code
1. Load the dataset and preprocess it.
2. Run PCA to extract key features.
3. Train HMMs using various state configurations and evaluate using BIC and log-likelihood values.
4. Apply the trained model on new datasets and identify anomalies based on log-likelihood differences.
5. Execute reinforcement learning for investment optimization.

To execute the R script:
```r
source("AnomalyDetection.R")
```

## Results and Insights
- The best HMM model was determined by selecting the lowest BIC and highest log-likelihood.
- Anomalies were detected when log-likelihood deviated beyond a threshold of 0.5.
- Reinforcement learning demonstrated optimal investment strategies through Q-learning with fine-tuned parameters.

## Challenges & Lessons Learned
- **Feature Selection**: PCA significantly reduced dimensionality while preserving essential variance.
- **Computational Complexity**: Training HMMs across multiple states was time-intensive.
- **Threshold Selection**: A predefined threshold was used for anomaly detection; future improvements could involve statistical validation.
- **Investment Strategy Optimization**: Fine-tuning hyperparameters improved reinforcement learning outcomes.

## Contributors
- **Daniel Šurina**
- **Arya Singh**
- **Oliver Ng-Young-Lim**
- **Steven Dai Chuy**

## References
- Principal Component Analysis (PCA) Documentation
- Hidden Markov Models for Anomaly Detection
- Reinforcement Learning: Q-Learning Basics

## Future Work
- Implement alternative anomaly detection techniques (e.g., LSTMs, Isolation Forests).
- Optimize threshold determination using statistical methods.
- Expand reinforcement learning applications to real-world financial datasets.

## License
This project is for educational purposes. Contact contributors for reuse permissions.

---
### Questions?
Feel free to reach out via email or GitHub for further discussions on anomaly detection and cybersecurity in automated control systems!

