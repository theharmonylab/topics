# tests/test-topicsPreds.R
library(testthat)
library(topics)  # Replace with your package name
#library(text)
library(tibble)

library(here)


test_that("topicsPreds generates predictions with default parameters", {
  data <- Language_based_assessment_data_8$harmonytexts
  dtm <- topicsDtm(data = data)
  model <- topicsModel(dtm = dtm)
  
  result <- topicsPreds(model = model, data = data)
  
  testthat::expect_true(is_tibble(result))
  testthat::expect_equal(nrow(result), length(data))
  testthat::expect_equal(ncol(result), 20)  # Assuming 5 topics
})

test_that("topicsPreds handles different numbers of iterations", {
  data <- Language_based_assessment_data_8$harmonytexts
  dtm <- topicsDtm(data = data)
  model <- topicsModel(dtm = dtm)
  result <- topicsPreds(model, data, num_iterations = 200)
  
  testthat::expect_true(is_tibble(result))
  testthat::expect_equal(nrow(result), length(data))
})

test_that("topicsPreds sets seed for reproducibility", {
  data <- Language_based_assessment_data_8$harmonytexts
  dtm <- topicsDtm(data = data)
  model <- topicsModel(dtm = dtm)
  
  result1 <- topicsPreds(model, data, seed = 123)
  result2 <- topicsPreds(model, data, seed = 123)
  
  testthat::expect_equal(result1, result2)
})

test_that("topicsPreds saves predictions to the specified directory", {
  data <- Language_based_assessment_data_8$harmonytexts
  dtm <- topicsDtm(data = data)
  model <- topicsModel(dtm = dtm)
  save_dir <- tempfile()
  
  result <- topicsPreds(model, data)
  
  testthat::expect_true(file.exists(file.path("results", "seed_42", "preds.rds")))
})


test_that("topicsPreds loads predictions from the specified directory", {
  data <- Language_based_assessment_data_8$harmonytexts
  dtm <- topicsDtm(data = data)
  model <- topicsModel(dtm = dtm)
  # Load predictions
  topicsPreds(model, data)
  result <- topicsPreds(load_dir = "./results")
  
  testthat::expect_true(is_tibble(result))
  testthat::expect_equal(nrow(result), length(data))
  testthat::expect_equal(ncol(result), 20)  # Assuming 5 topics
})

