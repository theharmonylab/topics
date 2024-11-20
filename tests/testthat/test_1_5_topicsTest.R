# tests/test-topicsTest.R
library(testthat)
library(topics)  # Replace with your packAge name
#library(text)
library(dplyr)

test_that("topicsTest performs linear regression correctly", {
  
  testthat::skip_on_cran()
  
  dtm <- topics::topicsDtm(data = dep_wor_data$Deptext)
  model <- topics::topicsModel(dtm = dtm)
  preds <- topics::topicsPreds(model = model, data = dep_wor_data$Deptext)
  
  result <- topics::topicsTest(
    model = model,
    preds = preds,
    data = dep_wor_data,
    pred_var_x = "Age",
    test_method = "linear_regression"
    )
  
  testthat::expect_true(is.list(result[[1]]))
  testthat::expect_equal(result[[1]]$test_method, "linear_regression")
  testthat::expect_true(any(grepl("estimate", names(result[[1]]$test))))
  testthat::expect_true(any(grepl("t", names(result[[1]]$test))))
  testthat::expect_true(any(grepl("p", names(result[[1]]$test))))
  testthat::expect_true(any(grepl("p_adjusted", names(result[[1]]$test))))
  unlink("./results/", recursive = TRUE)  
})


test_that("topicsTest handles missing pred_var for non t-test methods", {
  
  testthat::skip_on_cran()
  
  dtm <- topics::topicsDtm(data = dep_wor_data$Deptext)
  model <- topics::topicsModel(dtm = dtm)
  preds <- topics::topicsPreds(model = model, data = dep_wor_data$Deptext)
  
  result <- topics::topicsTest(model = model, 
                               preds = preds, 
                               data = dep_wor_data, 
                               pred_var_x = NULL)
  testthat::expect_null(result)
  unlink("./results/", recursive = TRUE)
})

test_that("topicsTest adjusts p-values for multiple comparisons", {
  
  testthat::skip_on_cran()
  
  dtm <- topics::topicsDtm(data = dep_wor_data$Deptext)
  model <- topics::topicsModel(dtm = dtm)
  preds <- topics::topicsPreds(model = model, data = dep_wor_data$Deptext)
  
  result <- topics::topicsTest(model = model, preds = preds, data = dep_wor_data, pred_var_x = "Age", p_adjust_method = "bonferroni")
  
  expect_true(is.list(result))
  expect_equal(result[[1]]$test_method, "linear_regression")
  expect_true(any(grepl("Age.estimate", names(result[[1]]$test))))
  expect_true(any(grepl("Age.t", names(result[[1]]$test))))
  expect_true(any(grepl("Age.p", names(result[[1]]$test))))
  unlink("./results/", recursive = TRUE)
})


test_that("topicsTest saves test results to the specified directory", {
  
  testthat::skip_on_cran()
  
  dtm <- topics::topicsDtm(data = dep_wor_data$Deptext)
  model <- topics::topicsModel(dtm = dtm)
  preds <- topics::topicsPreds(model = model, data = dep_wor_data$Deptext)
  
  result <- topics::topicsTest(model = model, preds = preds, data = dep_wor_data, pred_var_x = "Age")
  
  testthat::expect_true(file.exists(file.path("results", "seed_42", paste0("test_", result[[1]]$test_method, "_Age.rds"))))
  unlink("./results/", recursive = TRUE)
  })


# this feature is currently not working
#test_that("topicsTest loads test results from the specified directory", {
#  testthat::skip_on_cran()
# data <- dep_wor_data
#  dtm <- topicsDtm(data = data$Deptext)
#  model <- topicsModel(dtm = dtm)
#  preds <- topicsPreds(model = model, data = data$Deptext)
#  
#
#  # Generate and save test results
#  topicsTest(model, preds, data, pred_var_x = "hilstotal")
#  
#  # Load test results
#  result <- topicsTest(load_dir = "./results")
#  result
#  
#  testthat::expect_true(is.list(result[[1]]))
#  testthat::expect_equal(result[[1]]$test_methodtopicsTest performs logistic regression correctly, "linear_regression")
#  testthat::expect_true("hilstotal.estimate" %in% names(result[[1]]$test))
#  testthat::expect_true("hilstotal.t" %in% names(result[[1]]$test))
#  testthat::expect_true("hilstotal.p" %in% names(result[[1]]$test))
#})


test_that("topicsTest performs logistic regression correctly", {
  
  testthat::skip_on_cran()
  
  data <- dep_wor_data %>% dplyr::mutate(Gender = ifelse(Gender == "male", 1, 0))
  data
  dtm <- topics::topicsDtm(data = dep_wor_data$Deptext)
  model <- topics::topicsModel(dtm = dtm)
  preds <- topics::topicsPreds(model = model, data = dep_wor_data$Deptext)
  
  result <- topics::topicsTest(model = model, preds = preds, data = dep_wor_data, pred_var_x = "Age", test_method = "logistic_regression")
  
  expect_true(is.list(result[[1]]))
  expect_equal(result[[1]]$test_method, "logistic_regression")
  expect_true(any(grepl("estimate", names(result[[1]]$test))))
  expect_true(any(grepl(".t", names(result[[1]]$test))))
  expect_true(any(grepl(".p", names(result[[1]]$test))))
  unlink("./results/", recursive = TRUE)
})

#test_that("topicsTest performs ridge regression correctly", {
#  testthat::skip_on_cran()
# dtm <- topicsDtm(data = data$Deptext)
#  model <- topicsModel(dtm = dtm)
#  preds <- topicsPreds(model = model, data = data$Deptext)
#  
#  result <- topicsTest(model = model, preds = preds, data = data, pred_var_x = "Age", test_method = "ridge_regression")
#  result[[1]]
#  expect_true(is.list(result[[1]]))
#  expect_equal(result[[1]]$test_method, "ridge_regression")
#  expect_true(any(grepl("estimate", names(result[[1]]$test))))
#  expect_true(any(grepl("statistic", names(result[[1]]$test))))
#  expect_true(any(grepl("p.value", names(result[[1]]$test))))
#})
