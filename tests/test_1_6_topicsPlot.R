# tests/test-topicsWordclouds.R
library(testthat)
library(topics)  # Replace with your package name
#library(text)

library(here)

file_path <- here::here("data", "Language_based_assessment_data_8.rda")
load(file=file_path)


test_that("topicsPlot with test", {
  dtm <- topicsDtm(data = data$harmonytexts)
  model <- topicsModel(dtm = dtm)
  preds <- topicsPreds(model = model, data = data$harmonytexts)
  result <- topicsTest(model=model, preds=preds, data=data, pred_var_x = "hilstotal")
  topicsPlot(model, result, p_threshold=1)
  
  # Check if the wordcloud directory exists
  testthat::expect_true(dir.exists("./results/seed_42/wordclouds"))
})

test_that("topicsPlot without test and preds", {
  dtm <- topicsDtm(data = data$harmonytexts)
  model <- topicsModel(dtm = dtm)
  
  topicsPlot(model)
  
  # Check if the wordcloud directory exists
  testthat::expect_true(dir.exists("./results/seed_42/wordclouds"))
})

test_that("topicsPlot with topicsGrams",{
  data <- data$harmonytexts
  ngrams <- topicsGrams(data = data, top_n = 20)
  topics::topicsPlot(ngrams = ngrams, figure_format = "png" )
  testthat::expect_true(file.exists("./results/seed_42/wordclouds/ngrams.png"))
})

