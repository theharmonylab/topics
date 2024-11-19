# tests/test-topicsModel.R
library(testthat)
library(topics)  # Replace with your package name
#library(text)
library(tibble)

test_that("topicsGrams with default parameters", {

  testthat::skip_on_cran()
  
  data <- dep_wor_data$Worphrase
  
  ngrams <- topics::topicsGrams(
    data = data, 
    top_n = 2, 
    n = 3)
  
  testthat::expect_true(is.list(ngrams))
  testthat::expect_true(is_tibble(ngrams$ngrams))
  testthat::expect_true(is_tibble(ngrams$freq_per_user))
})

test_that("topicsGrams with default parameters", {
  
  testthat::skip_on_cran()
  data <- dep_wor_data$Worphrase
  
  ngrams <- topics::topicsGrams(
    data = data, 
    top_n = 10, 
    n = 3, 
    pmi_threshold = 3)
  
  testthat::expect_true(is.list(ngrams))
  testthat::expect_true(is_tibble(ngrams$ngrams))
  testthat::expect_true(is_tibble(ngrams$freq_per_user))
})
