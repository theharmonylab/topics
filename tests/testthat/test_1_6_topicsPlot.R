# tests/test-topicsWordclouds.R
library(testthat)
library(topics)  # Replace with your package name
#library(text)

library(here)


test_that("topicsPlot with test", {
  dtm <- topicsDtm(data = dep_wor_data$Deptext)
  model <- topicsModel(dtm = dtm)
  preds <- topicsPreds(model = model, data = dep_wor_data$Deptext)
  result <- topicsTest(model=model, preds=preds, data=dep_wor_data, pred_var_x = "Age")
  topicsPlot(model, result, p_threshold=1, figure_format = "png")
  
  # Check if the wordcloud directory exists
  testthat::expect_true(dir.exists("./results/seed_42/wordclouds"))
})

test_that("topicsPlot without test and preds", {
  dtm <- topicsDtm(data = dep_wor_data$Deptext)
  model <- topicsModel(dtm = dtm)
  
  topicsPlot(model, figure_format = "png")
  
  # Check if the wordcloud directory exists
  testthat::expect_true(dir.exists("./results/seed_42/wordclouds"))
})

test_that("topicsPlot with topicsGrams",{
  data <- dep_wor_data$Worphrase
  ngrams <- topicsGrams(data = data, top_n = 10, n=3, pmi_threshold = 3)
  topics::topicsPlot(ngrams = ngrams, figure_format = "png" )
  testthat::expect_true(file.exists("./results/seed_42/wordclouds/ngrams.png"))
})

