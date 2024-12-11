library(testthat)
library(topics)  # Replace with your package name
library(tibble)
library(dplyr)
options(mc.cores = 1)




test_that("topicsDtm creates a DTM correctly with default parameters", {

  testthat::skip_on_cran()

  dtm_result <- topics::topicsDtm(
    data =  dep_wor_data$Deptext, 
    ngram_window = c(1,1), 
    removal_mode = "frequency",
    removal_rate_most = 30,
    removal_rate_least = 25
    )
  
  testthat::expect_true(is.list(dtm_result))
  testthat::expect_true("train_dtm" %in% names(dtm_result))
  #testthat::expect_true("test_dtm" %in% names(dtm_result))
  #testthat::expect_true("train_data" %in% names(dtm_result))
  #testthat::expect_true("test_data" %in% names(dtm_result))
  testthat::expect_s4_class(dtm_result$train_dtm, "dgCMatrix")
  
  # Testing with less than 30 words
  dtmeval <- topicsDtmEval(dtm_result)
  dtmeval$frequency_plot

})

test_that("topicsDtm handles different ngram_window values", {
  
  testthat::skip_on_cran()
  
  result <- topics::topicsDtm(
    data = dep_wor_data$Deptext, 
    ngram_window = c(1, 2), 
    threads = 1)
  
  testthat::expect_true(is.list(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  
})

test_that("topicsDtm removes a specified word", {
  
  testthat::skip_on_cran()
  
  result <- topics::topicsDtm(
    data = dep_wor_data$Deptext, 
    removalword = "test")
  
  testthat::expect_true(is.list(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  testthat::expect_false("test" %in% colnames(result$train_dtm))
  
})

test_that("topicsDtm handles different occurrence rates", {
  
  testthat::skip_on_cran()
  
  result <-topics:: topicsDtm(
    data = dep_wor_data$Deptext,
    occurance_rate = 1)
  
  testthat::expect_true(is.list(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  
})

test_that("topicsDtm handles different removal modes", {
  
  testthat::skip_on_cran()
 
  result <- topics::topicsDtm(
    data = dep_wor_data$Deptext, 
    removal_mode = "absolute")
  
  testthat::expect_true(is.list(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  
})

test_that("topicsDtm handles different split proportions", {
  
  testthat::skip_on_cran()
  
  result <- topics::topicsDtm(
    data = dep_wor_data$Deptext)
  
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



test_that("topicsDtm removes least frequent words based on a threshold", {
  
  testthat::skip_on_cran()
  
  result <- topics::topicsDtm(
    data = dep_wor_data$Deptext, 
    removal_mode = "frequency", 
    removal_rate_least = 2)
  
  testthat::expect_true(is.list(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  
})

test_that("topicsDtm removes most frequent words based on a threshold", {
  
  testthat::skip_on_cran()
  
  result <- topics::topicsDtm(
    data = dep_wor_data$Deptext, 
    removal_mode = "frequency", 
    removal_rate_most = 50)
  
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
  
  result <- topics::topicsDtm(
    data = dep_wor_data$Deptext[1:20], 
    removal_mode = "percentage", 
    removal_rate_most = 5)
  
  testthat::expect_true(is.list(result))
  testthat::expect_s4_class(result$train_dtm, "dgCMatrix")
  
})


test_that("topicsDtm PMI thresholhd ", {
  
  testthat::skip_on_cran()
  
  result_pmi0 <- topics::topicsDtm(
    data = dep_wor_data$Deptext,
    pmi_threshold = NULL)
  
  result_pmi1 <- topics::topicsDtm(
    data = dep_wor_data$Deptext,
    pmi_threshold = 1)
  
  # Check the n-grams arranged according to pmi
  result_pmi1$n_grams_pmi %>% 
    arrange(desc(pmi_value))
  
  result_pmi1$n_grams_pmi %>% 
    arrange(-desc(pmi_value))
  
  testthat::expect_true(ncol(result_pmi1$train_dtm) == 21692)
  
})


