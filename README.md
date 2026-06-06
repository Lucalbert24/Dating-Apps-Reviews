# Dating App Review

This repository contains a text analysis of user reviews from three dating apps: Tinder, Bumble and Hinge.

The aim of the project is to compare user perceptions across platforms by analysing review sentiment and identifying the main topics discussed by users in 2021.

## Repository Content

- `code/`: R code used for the analysis
- `report/`: final report in PDF format

## Data

The dataset is not included in this repository.

The data used in the project come from Kaggle, and the download link is provided inside the report. To reproduce the analysis, download the dataset and update the file path in the R script if necessary.

## Methods

The analysis is performed in R and includes:

- text preprocessing
- word clouds
- sentiment analysis using the Bing lexicon
- topic modeling with the Mixture of Unigrams model
- topic number selection using AIC
- manual aggregation of topics into interpretable macro-topics

## Personal Contribution

My contribution focused on the overall structure of the study and on Chapter 6 of the report, dedicated to topic detection.

In particular, I worked on organizing the methodological workflow and applying the Mixture of Unigrams model to identify the main recurring topics in the reviews.

## Main Findings

The analysis shows a prevalence of negative sentiment across the three applications.

The main sources of dissatisfaction are related to payments, premium features, fake profiles, banned or blocked accounts, technical problems and match quality.

## Authors

Luca Alberti  
Matteo Canton  
Miriam Strepparava
