# tests/test-topicsModel.R
library(testthat)
library(topics)  # Replace with your package name
#library(text)
library(tibble)
library(here)

file_path <- here::here("data", "Language_based_assessment_data_8.rda")
load(file=file_path)

test_that("topicsGrams with default parameters", {

  data <- data$harmonytexts
  data <- gsub("[()]", "", data)
  ngrams <- topicsGrams(data = data)
  testthat::expect_true(is.list(ngrams))
  testthat::expect_true(is_tibble(ngrams$ngrams))
  testthat::expect_true(is_tibble(ngrams$freq_per_user))
})
