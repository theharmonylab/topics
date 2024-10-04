# tests/test-topicsModel.R
library(testthat)
library(topics)  # Replace with your package name
#library(text)
library(tibble)

load(file='./data/Language_based_assessment_data_8.rda')

test_that("topicsGrams with default parameters", {

  data <- Language_based_assessment_data_8$harmonytexts
  data <- gsub("[()]", "", data)
  ngrams <- topicsGrams(data = data)
  testthat::expect_true(is.list(ngrams))
  testthat::expect_true(is_tibble(ngrams$ngrams))
  testthat::expect_true(is_tibble(ngrams$freq_per_user))
})
