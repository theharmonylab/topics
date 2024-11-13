library(testthat)
library(topics)  # Replace with your package name
library(tibble)
library(dplyr)
library(text)
options(mc.cores = 1)




test_that("topicsDtm creates a DTM correctly with default parameters", {

  data <- dep_wor_data$Deptext
  result <- topics::topicsDtm(data = data)
  
  testthat::expect_true(is.list(result))
  testthat::expect_true("train_dtm" %in% names(result))
  testthat::expect_true("test_dtm" %in% names(result))
  testthat::expect_true("train_data" %in% names(result))
  testthat::expect_true("test_data" %in% names(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  unlink("./results/", recursive = TRUE)
})

test_that("topicsDtm handles different ngram_window values", {
  data <- dep_wor_data$Deptext
  result <- topics::topicsDtm(data, ngram_window = c(1, 2), threads=1)
  
  testthat::expect_true(is.list(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  unlink("./results/", recursive = TRUE)
})

test_that("topicsDtm removes a specified word", {
  data <- dep_wor_data$Deptext
  result <- topics::topicsDtm(data, removalword = "test")
  
  testthat::expect_true(is.list(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  testthat::expect_false("test" %in% colnames(result$train_dtm))
  unlink("./results/", recursive = TRUE)
})

test_that("topicsDtm handles different occurrence rates", {
  data <- dep_wor_data$Deptext
  result <-topics:: topicsDtm(data, occ_rate = 1)
  
  testthat::expect_true(is.list(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  unlink("./results/", recursive = TRUE)
})

test_that("topicsDtm handles different removal modes", {
  data <- dep_wor_data$Deptext
  result <- topics::topicsDtm(data, removal_mode = "absolute")
  
  testthat::expect_true(is.list(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  unlink("./results/", recursive = TRUE)
})

test_that("topicsDtm handles different split proportions", {
  data <- dep_wor_data$Deptext
  result <- topics::topicsDtm(data, split = 0.5)
  
  testthat::expect_true(is.list(result))
#  testthat::expect_true(nrow(result$train_data) <= nrow(data)*0.5)
#  testthat::expect_true(nrow(result$test_data) <= nrow(data)*0.5)
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  
})

test_that("topicsDtm handles different seeds for reproducibility", {
  data <- dep_wor_data$Deptext
  result1 <- topics::topicsDtm(data, seed = 123)
  result2 <- topics::topicsDtm(data, seed = 123)
  
  testthat::expect_equal(result1$train_dtm, result2$train_dtm)
  unlink("./results/", recursive = TRUE)
})

test_that("topicsDtm saves results to the specified directory", {
  data <- dep_wor_data$Deptext
  save_dir <- tempfile()
  result <- topics::topicsDtm(data, save_dir = save_dir)
  
  testthat::expect_true(file.exists(file.path(save_dir, "seed_42", "dtms.rds")))
  
})

test_that("topicsDtm loads results from the specified directory", {
  data <- dep_wor_data$Deptext
  result1 <- topics::topicsDtm(data)
  result2 <- topics::topicsDtm(load_dir = "./results")
  
  testthat::expect_equal(result1$train_dtm, result2$train_dtm)
  unlink("./results/", recursive = TRUE)
})

test_that("topicsDtm removes least frequent words based on a threshold", {
  data <- dep_wor_data$Deptext
  result <- topics::topicsDtm(data, removal_mode = "frequency", removal_rate_least = 2)
  
  testthat::expect_true(is.list(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  unlink("./results/", recursive = TRUE)
})

test_that("topicsDtm removes most frequent words based on a threshold", {
  data <- dep_wor_data$Deptext
  result <- topics::topicsDtm(data, removal_mode = "frequency", removal_rate_most = 50)
  
  testthat::expect_true(is.list(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  unlink("./results/", recursive = TRUE)
})

#test_that("topicsDtm removes least frequent words in percent mode", {
#  data <- dep_wor_data$Wortext
#  result <- topicsDtm(data, removal_mode = "percent", removal_rate_least = 50)
  
#  testthat::expect_true(is.list(result))
#  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
#})

test_that("topicsDtm removes most frequent words in percent mode", {
  data <- dep_wor_data$Deptext[1:20]
  result <- topics::topicsDtm(data, 
                              removal_mode = "percentage", 
                              removal_rate_most = 5)
  
  testthat::expect_true(is.list(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  
})
