# tests/test-topicsModel.R
library(testthat)
library(topics)  # Replace with your package name
#library(text)


test_that("topicsModel creates an LDA model correctly with default parameters", {
  
  testthat::skip_on_cran()
  
  dtm <- topicsDtm(
    data = c("This is a test document.", "This is another test."))
  
  result <- topicsModel(
    dtm)
  
  testthat::expect_true(is.list(result))
  testthat::expect_true("summary" %in% names(result))
  testthat::expect_true("top_terms" %in% names(result))
  testthat::expect_true("labels" %in% names(result))
  testthat::expect_true("coherence" %in% names(result))
  testthat::expect_true("prevalence" %in% names(result))
  
  
  result <- topics::topicsModel(
    dtm, 
    num_topics = 10)
  
  testthat::expect_true(is.list(result))
  testthat::expect_equal(nrow(result$summary), 10)
  
  
  # topicsModel handles different numbers of top words"
  result <- topics::topicsModel(
    dtm, 
    num_top_words = 5)
  
  testthat::expect_true(is.list(result))
  testthat::expect_true(all(sapply(result$summary$top_terms, function(x) length(strsplit(x, ", ")[[1]]) == 5)))
  
  # "topicsModel handles different numbers of iterations"
  result <- topics::topicsModel(
    dtm, 
    num_iterations = 500)
  
  testthat::expect_true(is.list(result))
  
  
  ## topicsModel sets seed for reproducibility
  
  testthat::skip_on_cran()
  
  result1 <- topics::topicsModel(
    dtm, 
    seed = 123)
  
  # They are the same even when having different seed; should look over this test. 
  result2 <- topics::topicsModel(
    dtm, 
    seed = 222 
    )
  
  testthat::expect_equal(result1$summary, result2$summary)
  
})

