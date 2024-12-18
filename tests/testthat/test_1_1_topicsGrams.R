library(testthat)
library(topics)
library(tibble)

test_that("topicsGrams with default parameters", {

  testthat::skip_on_cran()
  
  ngrams <- topics::topicsGrams(
    data = dep_wor_data$Worphrase, 
    ngram_window = c(1,3),
    stopwords = stopwords::stopwords("en", source = "snowball"),
    pmi_threshold = 0)
  
  testthat::expect_true(is.list(ngrams))
  testthat::expect_true(is_tibble(ngrams$ngrams))
  testthat::expect_true(is_tibble(ngrams$freq_per_user))
  
  testthat::expect_equal(unname(ngrams$ngrams$ngrams[1:5]),
    unname(c("anxious", "worried", "scared", "nervous", "concerned")))
  
  testthat::expect_equal(ngrams$ngrams$freq[1:5], c(174, 125, 63, 57, 50))
  testthat::expect_equal(ngrams$ngrams$prop[1:5], c(0.04763208, 0.03421845, 0.01724610, 0.01560361, 0.01368738), tolerance = 0.0001)
  testthat::expect_equal(ngrams$ngrams$prevalence[1:5], c(0.027588394, 0.019819248, 0.009988901, 0.009037577, 0.007927699), tolerance = 0.0001)
  testthat::expect_equal(ngrams$ngrams$pmi[3000:3005], c(12.916076, 17.390007, 10.709120, 20.596543,  7.938796, 17.048970), tolerance = 0.0001)
  testthat::expect_equal(ngrams$freq_per_user$worried[1:5], c(0.000, 0.000, 0.000, 0.000, 0.008), tolerance = 0.0001)
  testthat::expect_equal(ngrams$freq_per_user$concerned[1:5], c(0.00, 0.00, 0.00, 0.02, 0.00), tolerance = 0.0001)
  
  testthat::expect_equal(ngrams$stats$initial_count[[1]], 1225)
  testthat::expect_equal(ngrams$stats$initial_count[[2]], 2636)
  testthat::expect_equal(ngrams$stats$pmi_removed[[1]], 0)
  testthat::expect_equal(ngrams$stats$final_count[[1]], 1225)
  
})

test_that("topicsGrams with default parameters", {
  
  testthat::skip_on_cran()
  
  ngrams <- topics::topicsGrams(
    data = dep_wor_data$Worphrase, 
    ngram_window = c(1,3),
    stopwords = NULL,
    pmi_threshold = NULL)
  
  testthat::expect_true(is.list(ngrams))
  testthat::expect_true(is_tibble(ngrams$ngrams))
  testthat::expect_true(is_tibble(ngrams$freq_per_user))
})
