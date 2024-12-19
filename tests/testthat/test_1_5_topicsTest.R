library(testthat)
library(topics)
library(dplyr)


#remotes::install_github("theharmonylab/topics@12cd5b9ad33ef30d012b4aa65790a9be81afb5dd")
# .rs.restartR()

test_that("topicsTest performs linear regression correctly", {
  
  testthat::skip_on_cran()
  
  dtm <- topics::topicsDtm(
    data = dep_wor_data$Deptext)
  
  model <- topics::topicsModel(
    dtm = dtm)
  
  #help(topicsPreds)
  preds <- topics::topicsPreds(
    model = model, 
    data = dep_wor_data$Deptext)
  
  ############################
  #### linear_regression #####
  ############################
  result <- topics::topicsTest(
    model = model,
    preds = preds,
    data = dep_wor_data,
    x_variable = "Age",
    y_variable = "PHQ9tot",
    controls = NULL,
    test_method = "linear_regression"
    )
  result

  testthat::expect_true(is.list(result))
  testthat::expect_equal(result$test_method, "linear_regression")
  testthat::expect_true(any(grepl("estimate", names(result$test))))
  testthat::expect_true(any(grepl("t", names(result$test))))
  testthat::expect_true(any(grepl("p", names(result$test))))
  testthat::expect_true(any(grepl("p_adjusted", names(result$test))))
  
  testthat::expect_equal(result$test_method, "linear_regression")
  testthat::expect_equal(result$test$topic[1], 
                         "t_1")
  
  testthat::expect_equal(result$test$prevalence[1:4], 
                         c(3.389,  2.601,  3.733,  2.864), tolerance = 0.001)
  testthat::expect_equal(result$test$coherence[1:4], 
                         c(1.453, 0.803, 1.236, 0.361), tolerance = 0.001)
  
  
  testthat::expect_equal(result$test$x.z_Age.estimate_beta[1:5], 
           c(-0.02968493, -0.02739334,  0.03571515, -0.01120996, -0.08525859), tolerance = 0.0001)
  testthat::expect_equal(result$test$x.z_Age.t[1:5], 
                         c(-0.6627383, -0.6115369,  0.7975251, -0.2501762, -1.9095765), tolerance = 0.0001)
  testthat::expect_equal(result$test$x.z_Age.p[1:5], 
                         c(0.50780478, 0.54112328, 0.42552623, 0.80255430, 0.05676206), tolerance = 0.0001)
  testthat::expect_equal(result$test$x.z_Age.p_adjusted[1:5], 
                         c(0.7459746, 0.7459746, 0.7459746, 0.8917270, 0.3784137), tolerance = 0.0001)

  testthat::expect_equal(result$test$y.z_PHQ9tot.estimate[1:5], 
                         c(-0.028913708,  0.063841350, -0.012663082, -0.048725894,  0.004654584), tolerance = 0.0001)
  
  
  ####### Controlling for variables ####
  
  result_ctrl <- topics::topicsTest(
    model = model,
    preds = preds,
    data = dep_wor_data,
    x_variable = "Age",
    y_variable = "PHQ9tot",
    controls = c("Gender",  "GAD7tot"), #,
    test_method = "linear_regression"
  )
  result_ctrl
  
  # these are with the previous code; where controls where scaled in manua calcuation
  # -0.03121410, -0.02527967,  0.03445701, -0.02278217) the (average diff: 0.000265)
  testthat::expect_equal(result_ctrl$test$x.z_Age.estimate_beta[1:4], 
                         c(-0.03089316, -0.02503072,  0.03423101, -0.02287568), tolerance = 0.0001)
  
  testthat::expect_equal(result_ctrl$test$x.z_Age.t[1:4], 
                         c(-0.6919216, -0.5604032,  0.7653247, -0.5085558), tolerance = 0.0001)
  testthat::expect_equal(result_ctrl$test$x.z_Age.p[1:4], 
                         c(0.48931035, 0.57545745, 0.44444219, 0.61128977), tolerance = 0.0001)
  testthat::expect_equal(result_ctrl$test$x.z_Age.p_adjusted[1:4], 
                         c(0.7527852, 0.7858261, 0.7527852, 0.7858261), tolerance = 0.0001)
  
  # When not z-scoring control in scale:-0.02416600, 0.10753802, -0.07512578, 0.17245684; the average diff: 0.0653)
  testthat::expect_equal(result_ctrl$test$y.z_PHQ9tot.estimate_beta[1:4], 
                         c(-0.007475498,  0.033280355, -0.023326785,  0.054123169), tolerance = 0.0001)
  
  
  ##############################
  #### logistic_regression #####
  ##############################
  
  result_log <- topics::topicsTest(
    model = model,
    preds = preds,
    data = dep_wor_data,
    x_variable = "Gender",
   # y_variable = "PHQ9tot",
    test_method = "logistic_regression"
  )
  result_log
  
  
  testthat::expect_equal(result_log$test$x.Gender.estimate_beta[1:4], 
                         c(-0.07507502,  0.05734802,  0.15556099, -0.03501527), tolerance = 0.0001)
  testthat::expect_equal(result_log$test$x.Gender.z[1:4], 
                         c(-0.7553748,  0.6360858,  1.4940836, -0.3581462), tolerance = 0.0001)
  testthat::expect_equal(result_log$test$x.Gender.p[1:4], 
                         c(0.4500241, 0.5247205, 0.1351538, 0.7202339), tolerance = 0.0001)
  testthat::expect_equal(result_log$test$x.Gender.p_adjusted[1:4], 
                         c(0.6173182, 0.6173182, 0.6173182, 0.7581409), tolerance = 0.0001)
  
  # Should be able to have different test methods for x and y. 
  # Or could make it so that is is using logistic when it is a dichotomouse factor, 
  # otherwise it uses linear regression. 
  
})

test_that("topicsTest adjusts p-values for multiple comparisons", {
  
  testthat::skip_on_cran()
  
  dtm <- topics::topicsDtm(
    data = dep_wor_data$Deptext)
  
  model <- topics::topicsModel(
    dtm = dtm)
  
  preds <- topics::topicsPreds(
    model = model, 
    data = dep_wor_data$Deptext)
  
  result <- topics::topicsTest(
    model = model, 
    preds = preds, 
    data = dep_wor_data, 
    x_variable = "Age", 
    p_adjust_method = "bonferroni")
  
  testthat::expect_true(is.list(result))
  testthat::expect_equal(result$test_method[[1]], "linear_regression")
  testthat::expect_true(any(grepl("Age.estimate", names(result$test))))
  testthat::expect_true(any(grepl("Age.t", names(result$test))))
  testthat::expect_true(any(grepl("Age.p", names(result$test))))
  
})
