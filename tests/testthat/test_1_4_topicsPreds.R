library(testthat)
library(topics)
library(tibble)


test_that("topicsPreds generates predictions with default parameters", {
  
  testthat::skip_on_cran()
  save_dir_temp <- tempfile()
  
  dtm <- topics::topicsDtm(
    data = dep_wor_data$Depphrase, 
    save_dir = save_dir_temp)
  
  model <- topics::topicsModel(
    dtm = dtm, 
    save_dir = save_dir_temp)
  
  result <- topics::topicsPreds(
    model = model, 
    data = dep_wor_data$Depphrase, 
    save_dir = save_dir_temp)
  
  testthat::expect_true(is_tibble(result))
  testthat::expect_equal(result$t_1[[1]], 0.006824058, tolerance = 0.00001)
  testthat::expect_equal(nrow(result), length(dep_wor_data$Depphrase))
  testthat::expect_equal(ncol(result), 20)  # Assuming 5 topics
  
  
  # topicsPreds handles different numbers of iterations
  result <- topics::topicsPreds(
    model, 
    data = dep_wor_data$Depphrase, 
    num_iterations = 200, 
    save_dir = save_dir_temp)
  
  testthat::expect_true(is_tibble(result))
  testthat::expect_equal(nrow(result), length(dep_wor_data$Depphrase))
  
  # topicsPreds sets seed for reproducibility
  result1 <- topics::topicsPreds(
    model, 
    data = dep_wor_data$Depphrase, 
    seed = 123, 
    save_dir = save_dir_temp)
  
  result2 <- topics::topicsPreds(
    model, 
    data = dep_wor_data$Depphrase, 
    seed = 123, 
    save_dir = save_dir_temp)
  
  testthat::expect_equal(result1, result2)
  
  
  # topicsPreds saves predictions to the specified directory
  
  result <- topics::topicsPreds(
    model, 
    data = dep_wor_data$Depphrase, 
    save_dir = save_dir_temp)
  
  testthat::expect_true(file.exists(file.path(save_dir_temp, "seed_42", "preds.rds")))
  
  # "topicsPreds loads predictions from the specified directory"
  topics::topicsPreds(
    model, 
    data = dep_wor_data$Depphrase, 
    save_dir = save_dir_temp)
  
  result <- topics::topicsPreds(
    save_dir = NULL, 
    load_dir = save_dir_temp)
  
  testthat::expect_true(is_tibble(result))
  testthat::expect_equal(nrow(result), length(dep_wor_data$Depphrase))
  testthat::expect_equal(ncol(result), 20)  # Assuming 5 topics
  
})


