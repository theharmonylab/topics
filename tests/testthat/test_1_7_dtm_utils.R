library(testthat)
library(topics)

test_that("create_dtm_internal builds n-grams, removes stopwords and keeps doc names", {

  create_dtm_internal <- getFromNamespace("create_dtm_internal", "topics")

  dtm <- create_dtm_internal(
    doc_vec = c("The cat sat on the mat.", "Dogs bark! 42 dogs", ""),
    doc_names = c(10, 20, 30),
    ngram_window = c(1, 2),
    stopword_vec = c("the", "on"))

  testthat::expect_s4_class(dtm, "dgCMatrix")
  testthat::expect_equal(rownames(dtm), c("10", "20", "30"))
  testthat::expect_setequal(
    colnames(dtm),
    c("cat", "sat", "mat", "dogs", "bark",
      "cat_sat", "sat_mat", "dogs_bark", "bark_dogs"))
  testthat::expect_equal(dtm["20", "dogs"], 2)
  testthat::expect_equal(sum(dtm["30", ]), 0)
  # Terms ordered by total count, then alphabetically
  testthat::expect_equal(colnames(dtm)[ncol(dtm)], "dogs")
})

test_that("create_dtm_internal respects lower, punctuation and number settings", {

  create_dtm_internal <- getFromNamespace("create_dtm_internal", "topics")

  dtm <- create_dtm_internal(
    doc_vec = c("Self-care 2day"),
    doc_names = 1,
    ngram_window = c(1, 1),
    stopword_vec = character(0),
    lower = FALSE,
    remove_punctuation = TRUE,
    remove_numbers = TRUE)
  testthat::expect_setequal(colnames(dtm), c("Self", "care", "day"))

  dtm <- create_dtm_internal(
    doc_vec = c("Self-care 2day"),
    doc_names = 1,
    ngram_window = c(1, 1),
    stopword_vec = character(0),
    lower = TRUE,
    remove_punctuation = FALSE,
    remove_numbers = FALSE)
  testthat::expect_setequal(colnames(dtm), c("self", "care", "2day"))
})

test_that("dtm_to_docs repeats each term by its count", {

  dtm_to_docs <- getFromNamespace("dtm_to_docs", "topics")

  dtm <- Matrix::sparseMatrix(
    i = c(1, 1, 2), j = c(1, 2, 3), x = c(1, 2, 2), dims = c(3, 3),
    dimnames = list(c("a", "b", "c"), c("sat", "cat", "dog")))

  docs <- dtm_to_docs(dtm)
  testthat::expect_equal(names(docs), c("a", "b", "c"))
  testthat::expect_equal(unname(docs), c(" sat  cat  cat ", " dog  dog ", ""))
})

test_that("calc_gamma gives P(topic | word)", {

  calc_gamma <- getFromNamespace("calc_gamma", "topics")

  phi <- matrix(c(0.5, 0.5, 0,
                  0.1, 0.3, 0), nrow = 2, byrow = TRUE,
                dimnames = list(c("t_1", "t_2"), c("a", "b", "c")))
  theta <- matrix(c(0.75, 0.25,
                    0.25, 0.75), nrow = 2, byrow = TRUE)

  gamma <- calc_gamma(phi, theta)
  # Equal topic weights: gamma = phi normalised per word
  testthat::expect_equal(gamma[, "a"], c(t_1 = 0.5 / 0.6, t_2 = 0.1 / 0.6))
  testthat::expect_equal(gamma[, "b"], c(t_1 = 0.5 / 0.8, t_2 = 0.3 / 0.8))
  testthat::expect_equal(unname(gamma[, "c"]), c(0, 0))
})
