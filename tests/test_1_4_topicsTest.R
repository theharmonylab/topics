# tests/test-topicsTest.R
library(testthat)
library(topics)  # Replace with your package name
library(text)

data <- Language_based_assessment_data_8
dtm <- topicsDtm(data = data$harmonytexts)
model <- topicsModel(dtm = dtm)
preds <- topicsPreds(model = model, data = data$harmonytexts)
result <- topicsTest(model = model, 
                     preds = preds, 
                     data = data, 
                     pred_var = "hilstotal", 
                     test_method = "linear_regression")
names(result$test)


test_that("topicsTest performs linear regression correctly", {
  data <- Language_based_assessment_data_8
  dtm <- topicsDtm(data = data$harmonytexts)
  model <- topicsModel(dtm = dtm)
  preds <- topicsPreds(model = model, data = data$harmonytexts)
  result <- topicsTest(model = model, 
                       preds = preds, 
                       data = data, 
                       pred_var = "hilstotal", 
                       test_method = "linear_regression")
  
  testthat::expect_true(is.list(result))
  testthat::expect_equal(result$test_method, "linear_regression")
  testthat::expect_true(any(grepl("estimate", names(result$test))))
  testthat::expect_true(any(grepl("t", names(result$test))))
  testthat::expect_true(any(grepl("p", names(result$test))))
  testthat::expect_true(any(grepl("p_adjusted", names(result$test))))
  
})

data <- Language_based_assessment_data_8
dtm <- topicsDtm(data = data$harmonytexts)
model <- topicsModel(dtm = dtm)
preds <- topicsPreds(model = model, data = data$harmonytexts)
result <- topicsTest(model = model, 
                     preds = preds, 
                     data = data, 
                     group_var = "gender", 
                     test_method = "t-test")

result$test

test_that("topicsTest performs t-test correctly", {
  data <- Language_based_assessment_data_8
  dtm <- topicsDtm(data = data$harmonytexts)
  model <- topicsModel(dtm = dtm)
  preds <- topicsPreds(model = model, data = data$harmonytexts)
  result <- topicsTest(model = model, 
                       preds = preds, 
                       data = data, 
                       group_var = "gender", 
                       test_method = "t-test")
  
  testthat::expect_true(is.list(result))
  testthat::expect_equal(result$test_method, "t-test")
  testthat::expect_true(any(grepl("topic_name", names(result$test$male_female))))
  testthat::expect_true(any(grepl("cohen_d", names(result$test$male_female))))
  testthat::expect_true(any(grepl("p.value", names(result$test$male_female))))
})

test_that("topicsTest handles missing pred_var for non t-test methods", {
  data <- Language_based_assessment_data_8
  dtm <- topicsDtm(data = data$harmonytexts)
  model <- topicsModel(dtm = dtm)
  preds <- topicsPreds(model = model, data = data$harmonytexts)
  
  result <- topicsTest(model, preds, data, pred_var = NULL)
  testthat::expect_null(result)
})

test_that("topicsTest adjusts p-values for multiple comparisons", {
  data <- Language_based_assessment_data_8
  dtm <- topicsDtm(data = data$harmonytexts)
  model <- topicsModel(dtm = dtm)
  preds <- topicsPreds(model = model, data = data$harmonytexts)
  
  result <- topicsTest(model, preds, data, pred_var = "hilstotal", p_adjust_method = "bonferroni")
  
  expect_true(is.list(result))
  expect_equal(result$test_method, "linear_regression")
  expect_true(any(grepl("hilstotal.estimate", names(result$test))))
  expect_true(any(grepl("hilstotal.t", names(result$test))))
  expect_true(any(grepl("hilstotal.p", names(result$test))))
})


test_that("topicsTest saves test results to the specified directory", {
  data <- Language_based_assessment_data_8
  dtm <- topicsDtm(data = data$harmonytexts)
  model <- topicsModel(dtm = dtm)
  preds <- topicsPreds(model = model, data = data$harmonytexts)
  
  result <- topicsTest(model, preds, data, pred_var = "hilstotal", save_dir = save_dir)
  
  testthat::expect_true(file.exists(file.path("results", "seed_42", paste0("test_", result$test_method, ".rds"))))
})

test_that("topicsTest loads test results from the specified directory", {
  data <- Language_based_assessment_data_8
  dtm <- topicsDtm(data = data$harmonytexts)
  model <- topicsModel(dtm = dtm)
  preds <- topicsPreds(model = model, data = data$harmonytexts)
  

  # Generate and save test results
  topicsTest(model, preds, data, pred_var = "hilstotal")
  
  # Load test results
  result <- topicsTest(load_dir = "./results")
  result
  
  testthat::expect_true(is.list(result))
  testthat::expect_equal(result$test_method, "linear_regression")
  testthat::expect_true("hilstotal.estimate" %in% names(result$test))
  testthat::expect_true("hilstotal.t" %in% names(result$test))
  testthat::expect_true("hilstotal.p" %in% names(result$test))
})

data

test_that("topicsTest performs logistic regression correctly", {
  data <- Language_based_assessment_data_8
  data <- data %>% mutate(gender = ifelse(gender == "male", 1, 0))
  data
  dtm <- topicsDtm(data = data$harmonytexts)
  model <- topicsModel(dtm = dtm)
  preds <- topicsPreds(model = model, data = data$harmonytexts)
  
  result <- topicsTest(model, preds, data, pred_var = "gender", test_method = "logistic_regression")
  
  expect_true(is.list(result))
  expect_equal(result$test_method, "logistic_regression")
  expect_true(any(grepl("estimate", names(result$test))))
  expect_true(any(grepl(".t", names(result$test))))
  expect_true(any(grepl(".p", names(result$test))))
})

test_that("topicsTest performs ridge regression correctly", {
  data <- Language_based_assessment_data_8
  dtm <- topicsDtm(data = data$harmonytexts)
  model <- topicsModel(dtm = dtm)
  preds <- topicsPreds(model = model, data = data$harmonytexts)
  
  result <- topicsTest(model, preds, data, pred_var = "age", test_method = "ridge_regression")
  result
  expect_true(is.list(result))
  expect_equal(result$test_method, "ridge_regression")
  expect_true(any(grepl("estimate", names(result$test))))
  expect_true(any(grepl("statistic", names(result$test))))
  expect_true(any(grepl("p.value", names(result$test))))
})
