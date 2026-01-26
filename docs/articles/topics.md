# Getting started

The *topics*-package enables Differential Language Analysis using words,
phrases and topics

Please reference our tutorial article when using the package: **Language
visualisation methods for psychological assessments** and Ackermann L.,
Zhuojun G. & Kjell O.N.E. (2024). An R-package for visualizing text in
topics. <https://github.com/theharmonylab/topics>.
`DOI:zenodo.org/records/11165378`..

This Getting Started tutorial is going through the most central *topics*
functions.

## Usage

In an example where the topics are used to predict the PHQ-9 score, the
pipeline can be run as follows:

**1. Data Preprocessing**  
To preprocess the data, run the following command:

``` r

library(topics)

dtm <- topicsDtm(
  data = dep_wor_data$Depword)

# Check the results from the dtm and refine stopwords and removal rates if necessary
dtm_evaluation <- topicsDtmEval(
  dtm)
dtm_evaluation$frequency_plot
```

**2. Model Training**  
To train the LDA model, run the following command:

``` r

model <- topicsModel(
  dtm = dtm,
  num_topics = 20,
  num_iterations = 1000)
```

**3. Model Inference**  
To infer the topic term distribution of the documents, run the following
command:

``` r

preds <- topicsPreds(
  model = model,
  data = dep_wor_data$Depword)
```

**4. Statistical Analysis**  
To analyze the relationship between the topics and the prediction
variable, run the following command:

``` r

test <- topicsTest(
  data = dep_wor_data,
  model = model,
  preds = preds,
  x_variable = "PHQ9tot",
  controls = c("Age"),
  test_method = "linear_regression")
```

**5. Visualization**  
To visualize the significant topics as wordclouds, run the following
command:

``` r

plot_list <- topicsPlot(
  model = model,
  test = test,
  figure_format = "png")

# showing some of the plots
plot_list$square1
```

### Articles using the topics-package

Differentiating balance and harmony through natural language analysis: A
cross-national exploration of two understudied wellbeing-related
concepts

### Other relevant references

The below list consists of papers analyzing human language in a similar
fashion that is possible in *topics*.

***Methods Articles***

[Gaining insights from social media language: Methodologies and
challenges.](http://www.peggykern.org/uploads/5/6/6/7/56678211/kern_2016_-_gaining_insights_from_social_media_language-_methodologies_and_challenges.pdf).  
*Kern et al., (2016). Psychological Methods.*

***Computer Science: Python Software***

[DLATK: Differential language analysis
toolkit.](https://aclanthology.org/D17-2010/) *Schwartz, H. A., Giorgi,
et al., (2017). In Proceedings of the 2017 Conference on Empirical
Methods in Natural Language Processing: System Demonstrations*

[DLATK](https://github.com/dlatk/dlatk)
