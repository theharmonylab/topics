# tests/test-topicsWordclouds.R
library(testthat)
library(topics)  # Replace with your package name
library(text)

test_that("topicsPlot with test", {
  data <- Language_based_assessment_data_8
  dtm <- topicsDtm(data = data$harmonytexts)
  model <- topicsModel(dtm = dtm)
  preds <- topicsPreds(model = model, data = data$harmonytexts)
  
  result <- topicsTest(model, preds, data, pred_var_x = "hilstotal")
  
  topicsPlot(model, result)
  
  # Check if the wordcloud directory exists
  testthat::expect_true(dir.exists("./results/seed_42/wordclouds"))
})

test_that("topicsPlot without test and preds", {
  data <- Language_based_assessment_data_8
  dtm <- topicsDtm(data = data$harmonytexts)
  model <- topicsModel(dtm = dtm)
  
  topicsPlot(model)
  
  # Check if the wordcloud directory exists
  testthat::expect_true(dir.exists("./results/seed_42/wordclouds"))
})

test_that("topicsPlot with topicsGrams",{
  data <- Language_based_assessment_data_8$harmonytexts
  ngrams <- topicsGrams(data = data, top_n = 20)
  topics::topicsPlot(ngrams = ngrams, figure_format = "png" )
  testthat::expect_true(file.exists("./results/seed_42/wordclouds/ngrams.png"))
})
