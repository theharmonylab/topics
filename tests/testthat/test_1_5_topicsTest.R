library(testthat)
library(topics)
library(dplyr)

test_that("topicsTest performs linear regression correctly", {
  
  testthat::skip_on_cran()
  save_dir_temp <- tempfile()
  save_dir_temp <-  "./results"
  
  dtm <- topics::topicsDtm(
    data = dep_wor_data$Deptext, 
    save_dir = save_dir_temp)
  
  model <- topics::topicsModel(
    dtm = dtm, 
    save_dir = save_dir_temp)
  
  preds <- topics::topicsPreds(
    model = model, 
    data = dep_wor_data$Deptext, 
    save_dir = save_dir_temp)
  
  result <- topics::topicsTest(
    model = model,
    preds = preds,
    data = dep_wor_data,
    x_variable = "Age",
    y_variable = "PHQ9tot",
    test_method = "linear_regression", 
    save_dir = save_dir_temp
    )
#  test = result
  testthat::expect_true(is.list(result))
  testthat::expect_equal(result$test_method, "linear_regression")
  testthat::expect_true(any(grepl("estimate", names(result$test))))
  testthat::expect_true(any(grepl("t", names(result$test))))
  testthat::expect_true(any(grepl("p", names(result$test))))
  testthat::expect_true(any(grepl("p_adjusted", names(result$test))))
  
  
  testthat::expect_equal(result$test_method, "linear_regression")
  testthat::expect_equal(result$test$x.Age.estimate[1:5], 
           c(-0.02968493, -0.02739334,  0.03571515, -0.01120996, -0.08525859), tolerance = 0.0001)
  testthat::expect_equal(result$test$x.Age.t[1:5], 
                         c(-0.6627383, -0.6115369,  0.7975251, -0.2501762, -1.9095765), tolerance = 0.0001)
  testthat::expect_equal(result$test$x.Age.p[1:5], 
                         c(0.50780478, 0.54112328, 0.42552623, 0.80255430, 0.05676206), tolerance = 0.0001)
  testthat::expect_equal(result$test$x.Age.p_adjusted[1:5], 
                         c(0.7459746, 0.7459746, 0.7459746, 0.8917270, 0.3784137), tolerance = 0.0001)
  testthat::expect_equal(result$test$y.PHQ9tot.estimate[1:5], 
                         c(-0.028913708,  0.063841350, -0.012663082, -0.048725894,  0.004654584), tolerance = 0.0001)
  
  
#  result <- topics::topicsTest(
#    model = model,
#    preds = preds,
#    data = dep_wor_data,
#    x_variable = "Age",
#    y_variable = "PHQ9tot",
#    test_method = "correlation", 
#    save_dir = save_dir_temp
#  )
#  
#  result <- topics::topicsTest(
#    model = model,
#    preds = preds,
#    data = dep_wor_data,
#    group_var = "PHQ9tot",
#    test_method = "t-test", 
#    save_dir = save_dir_temp
#  )
  
  
  
})

test_that("topicsTest performs logistic regression correctly", {
  
  testthat::skip_on_cran()
  save_dir_temp <- tempfile()
  
  #  data <- dep_wor_data %>% dplyr::mutate(Gender = ifelse(Gender == "male", 1, 0))
  
  dtm <- topics::topicsDtm(
    data = dep_wor_data$Deptext, 
    save_dir = save_dir_temp) 
  
  model <- topics::topicsModel(
    dtm = dtm, 
    save_dir = save_dir_temp)
  
  preds <- topics::topicsPreds(
    model = model, 
    data = dep_wor_data$Deptext, 
    save_dir = save_dir_temp)
  
  result <- topics::topicsTest(
    model = model, 
    preds = preds, 
    data = dep_wor_data, 
    x_variable = "Age", 
    test_method = "logistic_regression", 
    save_dir = save_dir_temp)
  
  testthat::expect_true(is.list(result))
  testthat::expect_equal(result$test_method, "logistic_regression")
  testthat::expect_true(any(grepl("estimate", names(result$test))))
  testthat::expect_true(any(grepl(".t", names(result$test))))
  testthat::expect_true(any(grepl(".p", names(result$test))))
  
})

#### Test t-test here! 




#test_that("topicsTest handles missing pred_var for non t-test methods", {
#  
#  testthat::skip_on_cran()
#  save_dir_temp <- tempfile()
#  
#  dtm <- topics::topicsDtm(
#    data = dep_wor_data$Deptext, 
#    save_dir = save_dir_temp)
#  
#  model <- topics::topicsModel(
#    dtm = dtm, 
#    save_dir = save_dir_temp)
#  
#  preds <- topics::topicsPreds(
#    model = model, 
#    data = dep_wor_data$Deptext, 
#    save_dir = save_dir_temp)
#  
#  result <- topics::topicsTest(
#    model = model, 
#    preds = preds, 
#    data = dep_wor_data, 
#    x_variable = NULL, 
#    save_dir = save_dir_temp)
#  
#  testthat::expect_null(result)
#  
#})

test_that("topicsTest adjusts p-values for multiple comparisons", {
  
  testthat::skip_on_cran()
  save_dir_temp <- tempfile()
  
  dtm <- topics::topicsDtm(
    data = dep_wor_data$Deptext, 
    save_dir = save_dir_temp)
  
  model <- topics::topicsModel(
    dtm = dtm, 
    save_dir = save_dir_temp)
  
  preds <- topics::topicsPreds(
    model = model, 
    data = dep_wor_data$Deptext, 
    save_dir = save_dir_temp)
  
  result <- topics::topicsTest(
    model = model, 
    preds = preds, 
    data = dep_wor_data, 
    x_variable = "Age", 
    p_adjust_method = "bonferroni", 
    save_dir = save_dir_temp)
  
  testthat::expect_true(is.list(result))
  testthat::expect_equal(result$test_method, "linear_regression")
  testthat::expect_true(any(grepl("Age.estimate", names(result$test))))
  testthat::expect_true(any(grepl("Age.t", names(result$test))))
  testthat::expect_true(any(grepl("Age.p", names(result$test))))
  
})


test_that("topicsTest saves test results to the specified directory", {
  
  testthat::skip_on_cran()
  save_dir_temp <- tempfile()
  dtm <- topics::topicsDtm(
    data = dep_wor_data$Deptext, 
    save_dir = save_dir_temp)
  
  model <- topics::topicsModel(
    dtm = dtm, 
    save_dir = save_dir_temp)
  
  preds <- topics::topicsPreds(
    model = model, 
    data = dep_wor_data$Deptext, 
    save_dir = save_dir_temp)
  
  result <- topics::topicsTest(
    model = model, 
    preds = preds, 
    data = dep_wor_data, 
    x_variable = "Age", 
    save_dir = save_dir_temp)
  
  testthat::expect_true(file.exists(file.path(save_dir_temp, "seed_42", paste0("test_", result$test_method, "_Age.rds"))))
  
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
#  topicsTest(model, preds, data, x_variable = "hilstotal")
#  
#  # Load test results
#  result <- topicsTest(load_dir = "./results")
#  result
#  
#  testthat::expect_true(is.list(result))
#  testthat::expect_equal(result$test_methodtopicsTest performs logistic regression correctly, "linear_regression")
#  testthat::expect_true("hilstotal.estimate" %in% names(result$test))
#  testthat::expect_true("hilstotal.t" %in% names(result$test))
#  testthat::expect_true("hilstotal.p" %in% names(result$test))
#})




#test_that("topicsTest performs ridge regression correctly", {
#  testthat::skip_on_cran()
# dtm <- topicsDtm(data = data$Deptext)
#  model <- topicsModel(dtm = dtm)
#  preds <- topicsPreds(model = model, data = data$Deptext)
#  
#  result <- topicsTest(model = model, preds = preds, data = data, x_variable = "Age", test_method = "ridge_regression")
#  result
#  expect_true(is.list(result))
#  expect_equal(result$test_method, "ridge_regression")
#  expect_true(any(grepl("estimate", names(result$test))))
#  expect_true(any(grepl("statistic", names(result$test))))
#  expect_true(any(grepl("p.value", names(result$test))))
#})
