# tests/test-topicsTest.R
library(testthat)
library(topics)  # Replace with your package name
library(text)


test_that("topicsTest performs linear regression correctly", {
  data <- Language_based_assessment_data_8$harmonytexts
  model <- topicsModel(load_dir = "./results")
  preds <- topicsPreds(load_dir = "./results")
  data$
  result <- topicsTest(model = model, 
                       preds = preds, 
                       data = data, 
                       pred_var = "hilstotal", 
                       test_method = "linear_regression")
  
  testthat::expect_true(is.list(result))
  testthat::expect_equal(result$test_method, "linear_regression")
  testthat::expect_true("estimate" %in% names(result$test))
  testthat::expect_true("t" %in% names(result$test))
  testthat::expect_true("p" %in% names(result$test))
  testthat::expect_true("p_adjusted" %in% names(result$test))
  
})

test_that("topicsTest performs t-test correctly", {
  data <- Language_based_assessment_data_8$harmonytexts
  model <- topicsModel(load_dir = "./results")
  preds <- topicsPreds(load_dir = "./results")
  result <- topicsTest(model = model, 
                       pres = preds, 
                       data = data, 
                       group_var = "gender", 
                       test_method = "t-test")
  
  testthat::expect_true(is.list(result))
  testthat::expect_equal(result$test_method, "t-test")
  testthat::expect_true("topic_name" %in% names(result$test))
  testthat::expect_true("cohen_d" %in% names(result$test))
  testthat::expect_true("p.value" %in% names(result$test))
})

test_that("topicsTest handles missing pred_var for non t-test methods", {
  data <- Language_based_assessment_data_8$harmonytexts
  model <- topicsModel(load_dir = "./results")
  preds <- topicsPreds(load_dir = "./results")
  
  testthat::result <- topicsTest(model, preds, data, pred_var = NULL)
  testthat::expect_null(result)
})

test_that("topicsTest adjusts p-values for multiple comparisons", {
  data <- Language_based_assessment_data_8$harmonytexts
  model <- topicsModel(load_dir = "./results")
  preds <- topicsPreds(load_dir = "./results")
  
  result <- topicsTest(model, preds, data, pred_var = "hilstotal", p_adjust_method = "bonferroni")
  
  expect_true(is.list(result))
  expect_equal(result$test_method, "linear_regression")
  expect_true("hilstotal.estimate" %in% names(result$test))
  expect_true("hilstotal.t" %in% names(result$test))
  expect_true("hilstotal.p" %in% names(result$test))
})

test_that("topicsTest saves test results to the specified directory", {
  data <- Language_based_assessment_data_8$harmonytexts
  model <- topicsModel(load_dir = "./results")
  preds <- topicsPreds(load_dir = "./results")
  
  result <- topicsTest(model, preds, data, pred_var = "hilstotal", save_dir = save_dir)
  
  testthat::expect_true(file.exists(file.path("results", "seed_42", paste0("test_", result$test_method, ".rds"))))
})

test_that("topicsTest loads test results from the specified directory", {
  data <- Language_based_assessment_data_8$harmonytexts
  model <- topicsModel(load_dir = "./results")
  preds <- topicsPreds(load_dir = "./results")
  

  # Generate and save test results
  topicsTest(model, preds, data, pred_var = "hilstotal")
  
  # Load test results
  result <- topicsTest(load_dir = "./results")
  
  testthat::xpect_true(is.list(result))
  testthat::expect_equal(result$test_method, "linear_regression")
  testthat::expect_true("hilstotal.estimate" %in% names(result$test))
  testthat::expect_true("hilstotal.t" %in% names(result$test))
  testthat::expect_true("hilstotal.p" %in% names(result$test))
})



test_that("topicsTest performs logistic regression correctly", {
  data <- Language_based_assessment_data_8$harmonytexts
  model <- topicsModel(load_dir = "./results")
  preds <- topicsPreds(load_dir = "./results")
  
  result <- topicsTest(model, preds, data, pred_var = "gender", test_method = "logistic_regression")
  
  expect_true(is.list(result))
  expect_equal(result$test_method, "logistic_regression")
  expect_true("gender.estimate" %in% names(result$test))
  expect_true("gender.t" %in% names(result$test))
  expect_true("gender.p" %in% names(result$test))
})

test_that("topicsTest performs ridge regression correctly", {
  data <- Language_based_assessment_data_8$harmonytexts
  model <- topicsModel(load_dir = "./results")
  preds <- topicsPreds(load_dir = "./results")
  
  result <- topicsTest(model, preds, data, pred_var = "age", test_method = "ridge_regression")
  
  expect_true(is.list(result))
  expect_equal(result$test_method, "ridge_regression")
  expect_true("estimate" %in% names(result$test))
  expect_true("statistic" %in% names(result$test))
  expect_true("p.value" %in% names(result$test))
})