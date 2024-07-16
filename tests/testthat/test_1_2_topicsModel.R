# tests/test-topicsModel.R
library(testthat)
library(topics)  # Replace with your package name
library(text)



test_that("topicsModel creates an LDA model correctly with default parameters", {
  dtm <- topicsDtm(c("This is a test document.", "This is another test."))
  result <- topicsModel(dtm)
  
  testthat::expect_true(is.list(result))
  testthat::expect_true("summary" %in% names(result))
  testthat::expect_true("top_terms" %in% names(result))
  testthat::expect_true("labels" %in% names(result))
  testthat::expect_true("coherence" %in% names(result))
  testthat::expect_true("prevalence" %in% names(result))
})

test_that("topicsModel handles different numbers of topics", {
  dtm <- topicsDtm(load_dir = "./results")
  result <- topicsModel(dtm, num_topics = 10)
  
  testthat::expect_true(is.list(result))
  testthat::expect_equal(nrow(result$summary), 10)
})

test_that("topicsModel handles different numbers of top words", {
  dtm <- topicsDtm(load_dir = "./results")
  result <- topicsModel(dtm, num_top_words = 5)
  
  testthat::expect_true(is.list(result))
  testthat::expect_true(all(sapply(result$summary$top_terms, function(x) length(strsplit(x, ", ")[[1]]) == 5)))
})

test_that("topicsModel handles different numbers of iterations", {
  dtm <- topicsDtm(load_dir = "./results")
  result <- topicsModel(dtm, num_iterations = 500)
  
  testthat::expect_true(is.list(result))
})

test_that("topicsModel sets seed for reproducibility", {
  dtm <- topicsDtm(load_dir = "./results")
  result1 <- topicsModel(dtm, seed = 123)
  result2 <- topicsModel(dtm, seed = 123)
  
  testthat::expect_equal(result1$summary, result2$summary)
})

test_that("topicsModel saves the model to the specified directory", {
  dtm <- topicsDtm(load_dir = "./results")
  result <- topicsModel(dtm)
  
  testthat::expect_true(file.exists(file.path("results", "seed_42", "model.rds")))
})

test_that("topicsModel loads the model from the specified directory", {
  dtm <- topicsDtm(load_dir = "./results")
  result1 <- topicsModel(dtm)
  result2 <- topicsModel(load_dir = "./results")
  
  testthat::expect_equal(result1$summary, result2$summary)
})
