

library(testthat)
library(topics)
library(text)

test_that("Testing to training topics distributions using textTrainRegression",{
  
  testthat::skip_on_cran()
  if (Sys.getenv("SKIP_GITHUB_ACTIONS") == "true") {
    testthat::skip("Skipping this test in GitHub Actions R CMD check.")
  }
  
  dtm <- topics::topicsDtm(
    data = dep_wor_data$Deptext, 
    removal_mode = "frequency",
    removal_rate_most = 50,
    removal_rate_least = 5)
  
  dtmeval <- topicsDtmEval(dtm)
  dtmeval$frequency_plot
  dtmeval$frequency_plot_30_least
  dtmeval$frequency_plot_30_most
  
  model <- topics::topicsModel(
    dtm = dtm, 
    num_topics = 50,
    num_top_words = 10,
    num_iterations = 1500)
  
  #### Same data predictions  ####
  same_data_preds <- topicsPreds(
    model = model,
    data = dep_wor_data$Deptext,
    num_iterations = 50
  )
  
  colnames(same_data_preds) <- paste0("Dim", 1:20, "_", colnames(same_data_preds)) 
  model_same_data <- text::textTrainRegression(
    x = same_data_preds, 
    y = dep_wor_data["PHQ9tot"], 
    multi_cores = FALSE
  )
  #model_same_data
  testthat::expect_equal(model_same_data$results$estimate[[1]], .3906754, tolerance = .00001)
  
  ##### Testing on new data #####
  new_data_preds <- topics::topicsPreds(
    model = model,
    data = dep_wor_data$Worphrase,
    num_iterations = 50,
    create_new_dtm = FALSE
  )
  colnames(new_data_preds) <- paste0("Dim", 1:20, "_", colnames(new_data_preds)) 
  model_new_data <- text::textTrainRegression(
    x = new_data_preds, 
    y = dep_wor_data["PHQ9tot"], 
    multi_cores = FALSE
  )
  #model_new_data
  testthat::expect_equal(model_new_data$results$estimate[[1]], .1779766, tolerance = .00001)
  
  ##### Testing on new data with new dtm ####
  new_data_newdtm_preds <- topics::topicsPreds(
    model = model,
    data = dep_wor_data$Worphrase,
    num_iterations = 100,
    create_new_dtm = TRUE
  )
  colnames(new_data_newdtm_preds) <- paste0("Dim", 1:20, "_", colnames(new_data_newdtm_preds)) 
  model_new_data_pewnew <- text::textTrainRegression(
    x = new_data_newdtm_preds, 
    y = dep_wor_data["PHQ9tot"], 
    multi_cores = FALSE
  )
  model_new_data_pewnew$results
  
  testthat::expect_equal(model_new_data_pewnew$results$estimate[[1]], .2480465, tolerance = .00001)
  
})


