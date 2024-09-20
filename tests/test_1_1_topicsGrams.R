# tests/test-topicsModel.R
library(testthat)
library(topics)  # Replace with your package name
#library(text)
library(tibble)



test_that("topicsGrams with default parameters", {
  
  data <- Language_based_assessment_data_8$harmonytexts
  ngrams <- topicsGrams(data = data)
  testthat::expect_true(is.list(ngrams))
  testthat::expect_true(is.tibble(ngrams$ngrams))
  testthat::expect_true(is.tibble(ngrams$freq_per_user))
})
