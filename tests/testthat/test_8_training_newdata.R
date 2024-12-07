

library(testthat)
library(topics)
library(text)

test_that("N-Grams: topicsPlot with topicsGrams (without and with test",{
  
  testthat::skip_on_cran()
  save_dir_temp <- tempfile()
  save_dir_temp = "./results1"
  

  dtm <- topics::topicsDtm(
    data = dep_wor_data$Deptext, 
    removal_mode = "frequency",
    removal_rate_most = 50,
    removal_rate_least = 5,
    save_dir = save_dir_temp)
  
  dtmeval <- topicsDtmEval(dtm)
  dtmeval$frequency_plot
  dtmeval$frequency_plot_30_least
  dtmeval$frequency_plot_30_most
  
  model <- topics::topicsModel(
    dtm = dtm, 
    num_topics = 50,
    num_top_words = 10,
    num_iterations = 1500,
    save_dir = save_dir_temp)
  
  #### Same data predictions  ####
  same_data_preds <- topicsPreds(
    model = model,
    data = dep_wor_data$Deptext,
    num_iterations = 1000,
    save_dir = save_dir_temp,
    load_dir = NULL
  )
  
  colnames(same_data_preds) <- paste0("Dim", 1:20, "_", colnames(same_data_preds)) 
  model_same_data <- textTrainRegression(
    x = same_data_preds, 
    y = dep_wor_data["PHQ9tot"]
  )
  model_same_data
  testthat::expect_equal(model_same_data$results$estimate[[1]], .4031594, tolerance = .00001)
  
  ##### Testing on new data #####
  new_data_preds <- topics::topicsPreds(
    model = model,
    data = dep_wor_data$Worphrase,
    num_iterations = 1000,
    create_new_dtm = FALSE,
    save_dir = save_dir_temp,
    load_dir = NULL
  )
  colnames(new_data_preds) <- paste0("Dim", 1:20, "_", colnames(new_data_preds)) 
  model_new_data <- textTrainRegression(
    x = new_data_preds, 
    y = dep_wor_data["PHQ9tot"]
  )
  model_new_data
  testthat::expect_equal(model_new_data$results$estimate[[1]], .305296, tolerance = .00001)
  
  ##### Testing on new data with new dtm ####
  new_data_newdtm_preds <- topics::topicsPreds(
    model = model,
    data = dep_wor_data$Worphrase,
    num_iterations = 1000,
    create_new_dtm = TRUE,
    save_dir = save_dir_temp,
    load_dir = NULL
  )
  colnames(new_data_newdtm_preds) <- paste0("Dim", 1:20, "_", colnames(new_data_newdtm_preds)) 
  model_new_data_pewnew <- textTrainRegression(
    x = new_data_newdtm_preds, 
    y = dep_wor_data["PHQ9tot"]
  )
  model_new_data_pewnew$results
  
  testthat::expect_equal(model_new_data_pewnew$results$estimate[[1]], .3081117, tolerance = .00001)
  
})


