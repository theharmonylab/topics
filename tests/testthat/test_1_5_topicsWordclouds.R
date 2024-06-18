# tests/test-topicsWordclouds.R
library(testthat)
library(topics)  # Replace with your package name
library(text)

test_that("topicsWordclouds", {
  model <- topicsModel(load_dir = "./results")
  test <- topicsTest(load_dir = "./results")
  
  topicsWordclouds(model, test, save_dir = save_dir)
  
  # Check if the wordcloud directory exists
  testthat::expect_true(dir.exists("./results/seed42/wordclouds"))
})