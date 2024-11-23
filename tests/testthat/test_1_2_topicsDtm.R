library(testthat)
library(topics)  # Replace with your package name
library(tibble)
library(dplyr)
options(mc.cores = 1)




test_that("topicsDtm creates a DTM correctly with default parameters", {

  testthat::skip_on_cran()
  save_dir_temp <- tempdir()

  result <- topics::topicsDtm(
    data =  dep_wor_data$Deptext, 
    save_dir = save_dir_temp)
  
  testthat::expect_true(is.list(result))
  testthat::expect_true("train_dtm" %in% names(result))
  testthat::expect_true("test_dtm" %in% names(result))
  testthat::expect_true("train_data" %in% names(result))
  testthat::expect_true("test_data" %in% names(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")

})

test_that("topicsDtm handles different ngram_window values", {
  
  testthat::skip_on_cran()
  save_dir_temp <- tempdir()
  
  result <- topics::topicsDtm(
    data = dep_wor_data$Deptext, 
    ngram_window = c(1, 2), 
    threads=1, 
    save_dir = save_dir_temp)
  
  testthat::expect_true(is.list(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  
})

test_that("topicsDtm removes a specified word", {
  
  testthat::skip_on_cran()
  save_dir_temp <- tempdir()
  
  result <- topics::topicsDtm(
    data = dep_wor_data$Deptext, 
    removalword = "test", 
    save_dir =   save_dir_temp)
  
  testthat::expect_true(is.list(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  testthat::expect_false("test" %in% colnames(result$train_dtm))
  
})

test_that("topicsDtm handles different occurrence rates", {
  
  testthat::skip_on_cran()
  save_dir_temp <- tempdir()
  
  result <-topics:: topicsDtm(
    data = dep_wor_data$Deptext,
    occ_rate = 1, 
    save_dir =   save_dir_temp)
  
  testthat::expect_true(is.list(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  
})

test_that("topicsDtm handles different removal modes", {
  
  testthat::skip_on_cran()
  save_dir_temp <- tempdir()
 
  result <- topics::topicsDtm(
    data = dep_wor_data$Deptext, 
    removal_mode = "absolute", 
    save_dir = save_dir_temp)
  
  testthat::expect_true(is.list(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  
})

test_that("topicsDtm handles different split proportions", {
  
  testthat::skip_on_cran()
  save_dir_temp <- tempdir()
  
  result <- topics::topicsDtm(
    data = dep_wor_data$Deptext, 
    split = 0.5, 
    save_dir = save_dir_temp)
  
  testthat::expect_true(is.list(result))
#  testthat::expect_true(nrow(result$train_data) <= nrow(data)*0.5)
#  testthat::expect_true(nrow(result$test_data) <= nrow(data)*0.5)
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  
})

#test_that("topicsDtm handles different seeds for reproducibility", {
#  
#  testthat::skip_on_cran()
#  save_dir_temp <- tempdir()
#  
#  
#  result1 <- topics::topicsDtm(
#    data =  dep_wor_data$Deptext, 
#    seed = 123, 
#    save_dir = save_dir_temp)
#  
#  result2 <- topics::topicsDtm(
#    data =  dep_wor_data$Deptext, 
#    seed = 124, 
#    save_dir = save_dir_temp)
#  
#  testthat::expect_equal(result1$train_dtm, result2$train_dtm)
#  
#})

test_that("topicsDtm saves results to the specified directory", {
  
  testthat::skip_on_cran()
  
  save_dir <- tempfile()
  
  result <- topics::topicsDtm(
    data = dep_wor_data$Deptext, 
    save_dir = save_dir)
  
  testthat::expect_true(file.exists(file.path(save_dir, "seed_42", "dtms.rds")))
  
})

test_that("topicsDtm loads results from the specified directory", {
  
  testthat::skip_on_cran()
  
  save_dir_temp <- tempfile()
  
  result1 <- topics::topicsDtm(
    data = dep_wor_data$Deptext, 
    save_dir = save_dir_temp)
  
  result2 <- topics::topicsDtm(
    load_dir = save_dir_temp, 
    save_dir = NULL)
  
  testthat::expect_equal(result1$train_dtm, result2$train_dtm)

})

test_that("topicsDtm removes least frequent words based on a threshold", {
  
  testthat::skip_on_cran()
  save_dir_temp <- tempfile()
  
  result <- topics::topicsDtm(
    data = dep_wor_data$Deptext, 
    removal_mode = "frequency", 
    removal_rate_least = 2, 
    save_dir = save_dir_temp)
  
  testthat::expect_true(is.list(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  
})

test_that("topicsDtm removes most frequent words based on a threshold", {
  
  testthat::skip_on_cran()
  save_dir_temp <- tempfile()
  
  result <- topics::topicsDtm(
    data = dep_wor_data$Deptext, 
    removal_mode = "frequency", 
    removal_rate_most = 50, 
    save_dir = save_dir_temp)
  
  testthat::expect_true(is.list(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")

})

#test_that("topicsDtm removes least frequent words in percent mode", {
#  testthat::skip_on_cran()
# data <- dep_wor_data$Wortext
#  result <- topicsDtm(data, removal_mode = "percent", removal_rate_least = 50)
  
#  testthat::expect_true(is.list(result))
#  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
#})

test_that("topicsDtm removes most frequent words in percent mode", {
  
  testthat::skip_on_cran()
  save_dir_temp <- tempfile()
  
  result <- topics::topicsDtm(
    data = dep_wor_data$Deptext[1:20], 
    removal_mode = "percentage", 
    removal_rate_most = 5, 
    save_dir = save_dir_temp)
  
  testthat::expect_true(is.list(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  
})
