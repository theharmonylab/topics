# tests/test-topicsModel.R
library(testthat)
library(topics)  # Replace with your package name
#library(text)


test_that("topicsModel creates an LDA model correctly with default parameters", {
  
  testthat::skip_on_cran()
  save_dir_temp <- tempfile()
  
  dtm <- topicsDtm(
    c("This is a test document.", "This is another test."), 
    save_dir = save_dir_temp)
  
  result <- topicsModel(
    dtm, 
    save_dir = save_dir_temp)
  
  testthat::expect_true(is.list(result))
  testthat::expect_true("summary" %in% names(result))
  testthat::expect_true("top_terms" %in% names(result))
  testthat::expect_true("labels" %in% names(result))
  testthat::expect_true("coherence" %in% names(result))
  testthat::expect_true("prevalence" %in% names(result))
  
  
  ## "topicsModel handles different numbers of topics"
  dtm <- topics::topicsDtm(
    save_dir = NULL,
    load_dir = save_dir_temp)
  
  result <- topics::topicsModel(
    dtm, 
    num_topics = 10, 
    save_dir = save_dir_temp)
  
  testthat::expect_true(is.list(result))
  testthat::expect_equal(nrow(result$summary), 10)
  
  
  # topicsModel handles different numbers of top words"
  result <- topics::topicsModel(
    dtm, 
    num_top_words = 5, 
    save_dir = save_dir_temp)
  
  testthat::expect_true(is.list(result))
  testthat::expect_true(all(sapply(result$summary$top_terms, function(x) length(strsplit(x, ", ")[[1]]) == 5)))
  
  # "topicsModel handles different numbers of iterations"
  result <- topics::topicsModel(
    dtm, 
    num_iterations = 500, 
    save_dir = save_dir_temp)
  testthat::expect_true(is.list(result))
  
  
  ## topicsModel sets seed for reproducibility
  
  testthat::skip_on_cran()
  
  dtm <- topics::topicsDtm(
    save_dir = NULL, 
    load_dir = save_dir_temp)
  
  result1 <- topics::topicsModel(
    dtm, 
    seed = 123, 
    save_dir = save_dir_temp)
  
  # They are the same even when having different seed; should look over this test. 
  result2 <- topics::topicsModel(
    dtm, 
    seed = 222, 
    save_dir = save_dir_temp 
    )
  
  testthat::expect_equal(result1$summary, result2$summary)
  
  
  # topicsModel saves the model to the specified directory
    
    
    
    dtm <- topics::topicsDtm(
      save_dir = NULL,
      load_dir = save_dir_temp)
    
    result <- topics::topicsModel(
      dtm, 
      save_dir = save_dir_temp)
    
    #
    testthat::expect_true(file.exists(file.path(save_dir_temp, "seed_42", "model.rds")))
    
})

