
library(testthat)
library(topics)

test_that("N-Grams: topicsPlot with topicsGrams (without and with test",{
  
  testthat::skip_on_cran()
  save_dir_temp <- tempfile()
  #save_dir_temp = "./results3"
  
  # No test (i.e., no dimension) help(topicsGrams)
  ngrams <- topics::topicsGrams(
    data = dep_wor_data$Worphrase, 
    top_n = 50, 
    n = 3, 
    pmi_threshold = 6)
  
  topics::topicsPlot(
    ngrams = ngrams, 
    figure_format = "png", 
    save_dir = save_dir_temp)
  
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_42/wordclouds/ngrams.png")))
  
  
  # With test (i.e., 1 dimenstion = two plots)
  ngrams <- topics::topicsGrams(
    data = dep_wor_data$Worphrase, 
    top_n = 10, 
    n=3, 
    pmi_threshold = 3)
  
  # help(topicsTest)
  test <- topics::topicsTest(
    data = dep_wor_data,
    ngrams = ngrams, 
    x_variable = "Age", 
    save_dir = save_dir_temp)
  
  #help(topicsPlot)
  topics::topicsPlot(
    ngrams = ngrams, 
    test = test,
    figure_format = "png", 
    p_alpha = 1, 
    save_dir = save_dir_temp)
  
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_42/wordclouds/ngrams_negative.png")))
  
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_42/wordclouds/ngrams_positive.png")))
  
})


test_that("topicsPlot WITHOUT test and preds", {
  
  testthat::skip_on_cran()
  save_dir_temp <- tempfile()
  save_dir_temp <- "./Oscar"
  
  dtm <- topics::topicsDtm(
    data = dep_wor_data$Deptext, 
    save_dir = save_dir_temp)
  
  model <- topics::topicsModel(
    dtm = dtm, 
    save_dir = save_dir_temp)
  
  # names(model)
  # cor.test(model$coherence, model$summary$coherence)
  # cor.test(model$prevalence, model$summary$prevalence)
  
  colnames(model$summary)
  #help(topicsPlot)
  topics::topicsPlot(
    model = model,
    plot_topics_idx = c(1,3),
    figure_format = "png", 
    save_dir = save_dir_temp)
  
  # Check if the wordcloud directory exists
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_42/wordclouds/t_1.png")))
  
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_42/wordclouds/t_3.png")))
  
})


test_that("topicsPlot WITH test", {
  
  testthat::skip_on_cran()
  save_dir_temp <- tempfile()
  #save_dir_temp = "./results"
  
  ## 1-Dimension
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
  
  test1 <- topics::topicsTest(
    model= model,
    preds = preds,
    data = dep_wor_data,
    x_variable = "Age", 
    save_dir = save_dir_temp)

  topics::topicsPlot(
    model = model, 
    test = test1, 
    p_alpha = 1,
    figure_format = "png",
    seed = 11, 
    save_dir = save_dir_temp)
  
  # Check if the wordcloud directory exists
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_11/wordclouds/dot_legend_corvar_Age.png")))
  
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_11/wordclouds/grid_legend_corvar_Age.png")))
  

  ## 2-Dimension  
  
  test2 <- topics::topicsTest(
    model = model, 
    preds = preds, 
    data = dep_wor_data, 
    x_variable = "PHQ9tot",
    y_variable = "Age", 
    save_dir = save_dir_temp
    )
  
  topics::topicsPlot(
    model = model, 
    test = test2, 
    p_alpha = 1, 
    color_scheme =  c(
      "yellow", "#398CF9",  # quadrant 1 (upper left corner)
      "yellow", "#60A1F7",  # quadrant 2 
      "yellow", "#5dc688",  # quadrant 3 (upper right corner)
      "yellow", "#e07f6a",  # quadrant 4
      "yellow", "darkgray", # quadrant 5 (middle square)
      "yellow", "#40DD52",  # quadrant 6 
      "yellow", "#FF0000",  # quadrant 7 (bottom left corner)
      "yellow", "#EA7467",  # quadrant 8 
      "yellow", "#85DB8E"),
    figure_format = "png",
    seed = 12, 
    save_dir = save_dir_temp
    )
  
  
  # Check if the wordcloud directory exists
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_12/wordclouds/dot_legend_corvar_PHQ9tot__Age.png")))
  
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_12/wordclouds/grid_legend_corvar_PHQ9tot__Age.png")))
  
})




test_that("topicsPlot WITH underscores in names", {
  # strsplit(cor_var,
  testthat::skip_on_cran()
  save_dir_temp <- tempfile()
  #save_dir_temp <- "./res_under"
  # Testing with _ 
  data_test <- dep_wor_data
  data_test$Dep_text <- data_test$Deptext
  data_test$Age_test <- data_test$Age
  ## 1-Dimension
  dtm <- topics::topicsDtm(
    data = data_test$Dep_text, 
    save_dir = save_dir_temp)
  
  model <- topics::topicsModel(
    dtm = dtm, 
    save_dir = save_dir_temp)
  
  preds <- topics::topicsPreds(
    model = model, 
    data = data_test$Dep_text, 
    save_dir = save_dir_temp)
  
  test1 <- topics::topicsTest(
    model= model,
    preds = preds,
    data = data_test,
    x_variable = "Age_test",
    y_variable = "Age",
    save_dir = save_dir_temp)
  
  topics::topicsPlot(
    model = model, 
    test = test1, 
    p_alpha = 1,
    figure_format = "png",
    seed = 11, 
    save_dir = save_dir_temp)
  
  # Check if the wordcloud directory exists
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_11/wordclouds/dot_legend_corvar_Age_test__Age.png")))
  
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_11/wordclouds/grid_legend_corvar_Age_test__Age.png")))
  
  
  ## 2-Dimension  
  
  test2 <- topics::topicsTest(
    model = model, 
    preds = preds, 
    data = dep_wor_data, 
    x_variable = "PHQ9tot",
    y_variable = "Age_test", 
    save_dir = save_dir_temp
  )
  
  topics::topicsPlot(
    model = model, 
    test = test2, 
    p_alpha = 1, 
    color_scheme =  c(
      "yellow", "#398CF9",  # quadrant 1 (upper left corner)
      "yellow", "#60A1F7",  # quadrant 2 
      "yellow", "#5dc688",  # quadrant 3 (upper right corner)
      "yellow", "#e07f6a",  # quadrant 4
      "yellow", "darkgray", # quadrant 5 (middle square)
      "yellow", "#40DD52",  # quadrant 6 
      "yellow", "#FF0000",  # quadrant 7 (bottom left corner)
      "yellow", "#EA7467",  # quadrant 8 
      "yellow", "#85DB8E"),
    figure_format = "png",
    seed = 12, 
    save_dir = save_dir_temp
  )
  
  
  # Check if the wordcloud directory exists
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_12/wordclouds/dot_legend_corvar_PHQ9tot__Age_test.png")))
  
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_12/wordclouds/grid_legend_corvar_PHQ9tot__Age_test.png")))
  
})





test_that("topicsPlot WITH PMI", {
  
  testthat::skip_on_cran()
  save_dir_temp <- tempfile()
  save_dir_temp <- "./res_under"
  
 ## dtm_1 <- topics::topicsDtm(
 ##   data = dep_wor_data$Dep_text, 
 ##   save_dir = save_dir_temp, 
 ##   pmi_threshold = 1
 ## )
 ## dtm_1$train_dtm
 ## dtm_null <- topics::topicsDtm(
 ##   data = dep_wor_data$Dep_text, 
 ##   save_dir = save_dir_temp, 
 ##   pmi_threshold = NULL
 ## )
 ## dtm_null$train_dtm
 ## 
 ## model_1 <- topics::topicsModel(
 ##   dtm = dtm_1, 
 ##   save_dir = save_dir_temp)
 ## model_1
 ## 
 ## model_null <- topics::topicsModel(
 ##   dtm = dtm_null, 
 ##   save_dir = save_dir_temp)
 ## model_null
 ## 
##
 ## preds_1 <- topics::topicsPreds(
 ##   model = model_1, 
 ##   data = dep_wor_data$Dep_text, 
 ##   save_dir = save_dir_temp)
 ## preds_1
 ## 
 ## preds_null <- topics::topicsPreds(
 ##   model = model_null, 
 ##   data = dep_wor_data$Dep_text, 
 ##   save_dir = save_dir_temp)
 ## preds_null
 ## 
 ## # # # # #
##
 ## # testing a model on new data = Same effect
 ## 
 ## preds_1_new <- topics::topicsPreds(
 ##   model = model_1, 
 ##   data = dep_wor_data$Wortext, 
 ##   save_dir = save_dir_temp)
 ## preds_1_new
 ## 
 ## preds_null_new <- topics::topicsPreds(
 ##   model = model_null, 
 ##   data = dep_wor_data$Wortext, 
 ##   save_dir = save_dir_temp)
 ## preds_null_new
 ## 
 ## preds_null_new <- topics::topicsPreds(
 ##   model = model_null, 
 ##   data = dep_wor_data$Wortext[1:2], 
 ##   save_dir = save_dir_temp)
 ## preds_null_new
 ## 
 ## # # # # #
  
  ## 1-Dimension
  dtm <- topics::topicsDtm(
    data = dep_wor_data$Dep_text, 
    save_dir = save_dir_temp, 
    pmi_threshold = 1
    )
  
  model <- topics::topicsModel(
    dtm = dtm, 
    save_dir = save_dir_temp)
  
  preds <- topics::topicsPreds(
    model = model, 
    data = dep_wor_data$Dep_text, 
    save_dir = save_dir_temp)
  
  test1 <- topics::topicsTest(
    model= model,
    preds = preds,
    data = dep_wor_data,
    x_variable = "Age",
    save_dir = save_dir_temp)
  
  testthat::expect_equal(test1$test$x.Age.estimate[1:4], 
                         c(0.012425389,  0.034527241,  0.039768994,  0.007723522), 
                         tolerance = 0.00001)
  
  
  
})
