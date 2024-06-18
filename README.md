# topics
[![DOI](https://zenodo.org/badge/785738351.svg)](https://zenodo.org/doi/10.5281/zenodo.11165377)

topics is an R-package based on mallet for LDA (Latent Dirichlet Allocation) Topic Modeling.

When using this code, please reference:

Ackermann L. & Kjell O.N.E. (2024). An R-package for visualizing text in topics. https://github.com/theharmonylab/topics. DOI:https://zenodo.org/doi/10.5281/zenodo.11165377. 

## Installation
``` r
# install.packages("devtools")
devtools::install_github("theharmonylab/ldatext", auth_token = " .... ")

# see how to make your private auth token here: https://github.com/settings/tokens



# Before open the library, consider setting this option (can increase 5000);  without it the code may ran out of memory
options(java.parameters = "-Xmx5000m")


```

## Table of Contents
1. [Overview](#overview)
2. [Installation](#installation)
3. [Usage](#usage)

## Overview
The pipeline is composed of the following steps:

**1. Data Preprocessing**<br>
The data preprocessing converts the data into a document term matrix (DTM) and removes stopwords, punctuation, etc. which is the data format needed for the LDA model.

**2. Model Training**<br>
The model training step trains the LDA model on the DTM with a number of iterations and predefined amount of topics.

**3. Model Inference**<br>
The model inference step uses the trained LDA model to infer the topic term distribution of the documents.

**4. Statistical Analysis**<br>
The analysis includes the methods like linear regression, binary regression, ridge regression or correlation to analyze the relationship between the topics and the prediction variable. It is possible to control for a number of variables and to adjust the p-value for multiple comparisons.

**5. Visualization**<br>
The visualization step creates wordclouds of the significant topics found by the statistical analysis.


## Usage
In an example where the topics are used to predict the PHQ-9 score, the pipeline can be run as follows:


**1. Data Preprocessing**<br>
To preprocess the data, run the following command:
```R
data <- read.csv("data.csv")
dtm <- topicsDtm(data = data)

# Checking the results from the dtm
length(Matrix::colSums(dtm$train_dtm))
Matrix::colSums(dtm$train_dtm)[1:20]
Matrix::colSums(dtm$train_dtm)[(length(Matrix::colSums(dtm$train_dtm)) - 100):length(Matrix::colSums(dtm$train_dtm))]

```
The function takes the following arguments:
- `data` (list): a list containing the data
- `ngram_window` (list): the minimum and maximum n-gram length, e.g. `c(1,3)`
- `stopwords` (stopwords): the stopwords to remove, e.g. `stopwords::stopwords("en", source = "snowball")`
- `removalword` (string): the word to remove
- `occ_rate` (integer): the rate of occurence of a word to be removed
- `removal_mode` (string): the mode of removal ("most" or "least")
- `removal_rate_most` (integer): the rate of most frequent words to be removed
- `removal_rate_least` (integer): the rate of least frequent words to be removed
- `split` (float): the proportion of the data to be used for training
- `seed` (integer): the random seed for reproducibility
- `save_dir` (string): the directory to save the results, default is "./results", if NULL, no results are saved


**2. Model Training**<br>
To train the LDA model, run the following command:
```R
model <- topicsModel(dtm = dtm,
                  num_topics = 20,
                  num_iterations = 1000)
                  
```
The function takes the following arguments:
- `dtm` (R_obj): The document term matrix.
- `num_topics` (integer): The number of topics to be created.
- `num_top_words` (integer): The number of top words to be displayed per topic.
- `num_iterations` (integer): The number of iterations to run the model for.
- `seed` (integer): The seed to set for reproducibility.
- `save_dir` (string): The directory to save the model. If NULL, the model will not be saved.
- `load_dir` (string): The directory to load the model from. If NULL, the model will not be loaded.

**3. Model Inference**<br>
To infer the topic term distribution of the documents, run the following command:
```R
preds <- topicsPreds(model = model,
                  data = data)
```
The function takes the following arguments:
- `model` (list): The trained model.
- `data` (list): The list containing the text data to infer the topic document distribution from.
- `num_iterations` (integer): The number of iterations to run the model.
- `seed` (integer): The seed to set for reproducibility.
- `save_dir` (string): The directory to save the predictions. If NULL, the predictions will not be saved.
- `load_dir` (string): The directory to load the model from. If NULL, the predictions will not be loaded.

**4. Statistical Analysis**<br>
To analyze the relationship between the topics and the prediction variable, run the following command:
```R
test <- topicsTest(model = model,
                data = data,
                preds = preds,
                pred_var = "phq9",
                control_vars = c("age",..),
                test_method = "linear_regression")
```
The function takes the following arguments:
- `model` (list): The trained LDA model.
- `data` (tibble): The data frame containing the text data.
- `preds` (tibble): The predictions made by the model.
- `pred_var` (string): The variable to be predicted, necessary only for regression or correlation tests.
- `group_var` (string): The variable used to group data, necessary only for t-test.
- `control_vars` (vector): Control variables to adjust for in the testing process.
- `test_method` (string): The test method to use. Options include "correlation", "t-test", "linear_regression", "logistic_regression", or "ridge_regression".
- `seed` (integer): The seed to set for reproducibility.
- `load_dir` (string): The directory to load the test from. If NULL, the test will not be loaded.
- `save_dir` (string): The directory to save the test results. If NULL, the test will not be saved.

**5. Visualization**<br>
To visualize the significant topics as wordclouds, run the following command:
```R
topicsWordclouds(model = model,
              test = test)
```
The function takes the following arguments:
- `model` (list): The trained model.
- `test` (list): The test results, typically containing topic distributions and associated metrics.
- `color_negative_cor` (R_obj): The color gradient for negative correlations, specifying colors from low to high, default: `scale_color_gradient(low = "darkgreen", high = "green")`
- `color_positive_cor` (R_obj): The color gradient for positive correlations, specifying colors from low to high, default: `scale_color_gradient(low = "darkred", high = "red")`
- `scale_size` (logical): Whether to scale the size of the words in the wordcloud based on frequency or importance.
- `plot_topics_idx` (vector): Indices of the topics to be plotted.
- `p_threshold` (integer): The p-value threshold for determining significance in topic correlations.
- `save_dir` (string): The directory where the wordclouds will be saved.
- `figure_format` (string): Set the figure format, e.g., ".svg" or ".png".
- `seed` (integer): The seed for reproducibility.









