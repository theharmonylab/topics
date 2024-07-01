# topics
[![DOI](https://zenodo.org/badge/785738351.svg)](https://zenodo.org/doi/10.5281/zenodo.11165377)

topics is an R-package based on mallet for LDA (Latent Dirichlet Allocation) Topic Modeling.

When using this code, please reference:

Ackermann L., Zhuojun Gu. & Kjell O.N.E. (2024). An R-package for visualizing text in topics. https://github.com/theharmonylab/topics. DOI:https://zenodo.org/doi/10.5281/zenodo.11165377. 

## Installation
``` r
# install.packages("devtools")
devtools::install_github("theharmonylab/topics")


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
dtm <- topicsDtm(data = data$text)

# Checking the results from the dtm
length(Matrix::colSums(dtm$train_dtm))
Matrix::colSums(dtm$train_dtm)[1:20]
Matrix::colSums(dtm$train_dtm)[(length(Matrix::colSums(dtm$train_dtm)) - 100):length(Matrix::colSums(dtm$train_dtm))]

```



**2. Model Training**<br>
To train the LDA model, run the following command:
```R
model <- topicsModel(dtm = dtm,
                  num_topics = 20,
                  num_iterations = 1000)
                  
```

**3. Model Inference**<br>
To infer the topic term distribution of the documents, run the following command:
```R
preds <- topicsPreds(model = model,
                    data = data$text)
```


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

**5. Visualization**<br>
To visualize the significant topics as wordclouds, run the following command:
```R
topicsPlots(model = model,
            test = test)
```








