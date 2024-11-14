# tests/test-topicsPreds.R
library(testthat)
library(topics)  # Replace with your package name
#library(text)
library(tibble)


test_that("topicsPreds generates predictions with default parameters", {
  data <- dep_wor_data$Depphrase
  dtm <- topics::topicsDtm(data = data)
  model <- topics::topicsModel(dtm = dtm)
  
  result <- topics::topicsPreds(model = model, data = data)
  
  testthat::expect_true(is_tibble(result))
  testthat::expect_equal(result$t_1[[1]], 0.006824058, tolerance = 0.00001)
  testthat::expect_equal(nrow(result), length(data))
  testthat::expect_equal(ncol(result), 20)  # Assuming 5 topics
  unlink("./results/", recursive = TRUE)
  
})

test_that("topicsPreds handles different numbers of iterations", {
  data <- dep_wor_data$Depphrase
  dtm <- topics::topicsDtm(data = data)
  model <- topics::topicsModel(dtm = dtm)
  result <- topics::topicsPreds(model, data, num_iterations = 200)
  
  testthat::expect_true(is_tibble(result))
  testthat::expect_equal(nrow(result), length(data))
  unlink("./results/", recursive = TRUE)
})

test_that("topicsPreds sets seed for reproducibility", {
  data <- dep_wor_data$Depphrase
  dtm <- topics::topicsDtm(data = data)
  model <- topics::topicsModel(dtm = dtm)
  
  result1 <- topics::topicsPreds(model, data, seed = 123)
  result2 <- topics::topicsPreds(model, data, seed = 123)
  
  testthat::expect_equal(result1, result2)
  unlink("./results/", recursive = TRUE)
})

test_that("topicsPreds saves predictions to the specified directory", {
  data <- dep_wor_data$Depphrase
  dtm <- topics::topicsDtm(data = data)
  model <- topics::topicsModel(dtm = dtm)
  save_dir <- tempfile()
  
  result <- topics::topicsPreds(model, data)
  
  testthat::expect_true(file.exists(file.path("results", "seed_42", "preds.rds")))
  unlink("./results/", recursive = TRUE)
})


test_that("topicsPreds loads predictions from the specified directory", {
  data <- dep_wor_data$Depphrase
  dtm <- topics::topicsDtm(data = data)
  model <- topics::topicsModel(dtm = dtm)
  # Load predictions
  topics::topicsPreds(model, data)
  result <- topics::topicsPreds(load_dir = "./results")
  
  testthat::expect_true(is_tibble(result))
  testthat::expect_equal(nrow(result), length(data))
  testthat::expect_equal(ncol(result), 20)  # Assuming 5 topics
  unlink("./results/", recursive = TRUE)
})

