
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![DOI](https://zenodo.org/badge/785738351.svg)](https://zenodo.org/doi/10.5281/zenodo.11165377)
[![Github build
status](https://github.com/theharmonylab/topics/workflows/R-CMD-check/badge.svg)](https://github.com/theharmonylab/topics/actions)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing-1)
[![codecov](https://codecov.io/gh/theharmonylab/topics/graph/badge.svg?token=7ZTWBNIVCX)](https://app.codecov.io/gh/theharmonylab/topics/)
<!-- 

[![CRAN  Downloads](https://cranlogs.r-pkg.org/badges/grand-total/text)](https://CRAN.R-project.org/package=text)

<!-- badges: end -->

# Topics

`topics` is an R-package enabling Differential Language Analysis using
topics. It can produce LDA topics, but also support the text-package
(www.r-text.org) in analyzing BERTtopics.

When using this package, please cite:

Ackermann L., Zhuojun G. & Kjell O.N.E. (2024). An R-package for
visualizing text in topics. <https://github.com/theharmonylab/topics>.
`DOI:zenodo.org/records/11165378`.

## Installation

The topics package uses <b>JAVA</b>, which is another programming
language. Please start by downloading and installing it from
`www.java.com/en/download/`. Then open R and run:

``` r
install.packages("devtools")
devtools::install_github("theharmonylab/topics")

# if you run in to any installation problem, try installing rJava first.

# Before open the library, consider setting this option (can increase 5000);  without it the code may ran out of memory
options(java.parameters = "-Xmx5000m")

```

## Table of Contents

1.  [Overview](#overview)
2.  [Installation](#installation)
3.  [Usage](#usage)

## Overview

The pipeline is composed of the following steps:

**1. Data Preprocessing**<br> The data preprocessing converts the data
into a document term matrix (DTM) and removes stopwords, punctuation,
etc. which is the data format needed for the LDA model.

**2. Model Training**<br> The model training step trains the LDA model
on the DTM with a number of iterations and predefined amount of topics.

**3. Model Inference**<br> The model inference step uses the trained LDA
model to infer the topic term distribution of the documents.

**4. Statistical Analysis**<br> The analysis includes the methods like
linear regression, binary regression, ridge regression or correlation to
analyze the relationship between the topics and the prediction variable.
It is possible to control for a number of variables and to adjust the
p-value for multiple comparisons.

**5. Visualization**<br> The visualization step creates wordclouds of
the significant topics found by the statistical analysis.

## Usage

In an example where the topics are used to predict the PHQ-9 score, the
pipeline can be run as follows:

**1. Data Preprocessing**<br> To preprocess the data, run the following
command:

``` r
data <- read.csv("data.csv")
dtm <- topicsDtm(data = data$text)

# Check the results from the dtm and refine stopwords and removal rates if necessary
topicsDtmEval(dtm)
```

**2. Model Training**<br> To train the LDA model, run the following
command:

``` r
model <- topicsModel(dtm = dtm,
                  num_topics = 20,
                  num_iterations = 1000)
                  
```

**3. Model Inference**<br> To infer the topic term distribution of the
documents, run the following command:

``` r
preds <- topicsPreds(model = model,
                    data = data$text)
```

**4. Statistical Analysis**<br> To analyze the relationship between the
topics and the prediction variable, run the following command:

``` r
test <- topicsTest(model = model,
                  data = data,
                  preds = preds,
                  pred_var = "phq9",
                  control_vars = c("age",..),
                  test_method = "linear_regression")
```

**5. Visualization**<br> To visualize the significant topics as
wordclouds, run the following command:

``` r
topicsPlots(model = model,
            test = test)
```
