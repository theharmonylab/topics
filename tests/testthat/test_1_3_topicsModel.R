# tests/test-topicsModel.R
library(testthat)
library(topics)  # Replace with your package name
#library(text)


test_that("topicsModel creates an LDA model correctly with default parameters", {
  
  testthat::skip_on_cran()
  
  dtm <- topicsDtm(c("This is a test document.", "This is another test."))
  result <- topicsModel(dtm)
  
  testthat::expect_true(is.list(result))
  testthat::expect_true("summary" %in% names(result))
  testthat::expect_true("top_terms" %in% names(result))
  testthat::expect_true("labels" %in% names(result))
  testthat::expect_true("coherence" %in% names(result))
  testthat::expect_true("prevalence" %in% names(result))
  #unlink("./results/", recursive = TRUE)
})

test_that("topicsModel handles different numbers of topics", {
  
  testthat::skip_on_cran()
  
  dtm <- topics::topicsDtm(load_dir = "./results")
  result <- topics::topicsModel(dtm, num_topics = 10)
  
  testthat::expect_true(is.list(result))
  testthat::expect_equal(nrow(result$summary), 10)
 # unlink("./results/", recursive = TRUE)
})

test_that("topicsModel handles different numbers of top words", {
  
  testthat::skip_on_cran()
  
  dtm <- topics::topicsDtm(load_dir = "./results")
  result <- topics::topicsModel(dtm, num_top_words = 5)
  
  testthat::expect_true(is.list(result))
  testthat::expect_true(all(sapply(result$summary$top_terms, function(x) length(strsplit(x, ", ")[[1]]) == 5)))
 # unlink("./results/", recursive = TRUE)
})

test_that("topicsModel handles different numbers of iterations", {
  
  testthat::skip_on_cran()
  
  dtm <- topics::topicsDtm(load_dir = "./results")
  result <- topics::topicsModel(dtm, num_iterations = 500)
  
  testthat::expect_true(is.list(result))
 # unlink("./results/", recursive = TRUE)
})

test_that("topicsModel sets seed for reproducibility", {
  
  testthat::skip_on_cran()
  
  dtm <- topics::topicsDtm(load_dir = "./results")
  result1 <- topics::topicsModel(dtm, seed = 123)
  result2 <- topics::topicsModel(dtm, seed = 123)
  
  testthat::expect_equal(result1$summary, result2$summary)
 # unlink("./results/", recursive = TRUE)
})

test_that("topicsModel saves the model to the specified directory", {
  
  testthat::skip_on_cran()
  
  
  dtm <- topics::topicsDtm(load_dir = "./results")
  result <- topics::topicsModel(dtm)
  
  testthat::expect_true(file.exists(file.path("results", "seed_42", "model.rds")))
#  unlink("./results/", recursive = TRUE)
})

test_that("topicsModel loads the model from the specified directory", {
  
  testthat::skip_on_cran()
  
  dtm <- topics::topicsDtm(load_dir = "./results")
  result1 <- topics::topicsModel(dtm)
  result2 <- topics::topicsModel(load_dir = "./results")
  
  testthat::expect_equal(result1$summary, result2$summary)
#  unlink("./results/", recursive = TRUE)
})

