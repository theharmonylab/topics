
library(testthat)
library(topics)

test_that("N-Grams: topicsPlot with topicsGrams (without and with test",{
  
  testthat::skip_on_cran()
  save_dir_temp <- tempfile()
  
  # No test (i.e., no dimension) help(topicsGrams)
  ngrams <- topics::topicsGrams(
    data = dep_wor_data$Worphrase, 
    top_n = 10, 
    n = 3, 
    pmi_threshold = 3)
  
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
    pred_var_x = "Age", 
    save_dir = save_dir_temp)
  
  #help(topicsPlot)
  topics::topicsPlot(
    ngrams = ngrams, 
    test = test,
    figure_format = "png", 
    p_threshold = 1, 
    save_dir = save_dir_temp)
  
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_42/wordclouds/ngrams_negative.png")))
  
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_42/wordclouds/ngrams_positive.png")))
  
})


test_that("topicsPlot WITHOUT test and preds", {
  
  testthat::skip_on_cran()
  save_dir_temp <- tempfile()
  
  dtm <- topics::topicsDtm(
    data = dep_wor_data$Deptext, 
    save_dir = save_dir_temp)
  
  model <- topics::topicsModel(
    dtm = dtm, 
    save_dir = save_dir_temp)
  
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
    pred_var_x = "Age", 
    save_dir = save_dir_temp)

  topics::topicsPlot(
    model = model, 
    test = test1, 
    p_threshold = 1,
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
    pred_var_x = "PHQ9tot",
    pred_var_y = "Age", 
    save_dir = save_dir_temp
    )
  
  topics::topicsPlot(
    model = model, 
    test = test2, 
    p_threshold = 1, 
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
    save_dir_temp, "/seed_12/wordclouds/dot_legend_corvar_PHQ9tot_Age.png")))
  
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_12/wordclouds/grid_legend_corvar_PHQ9tot_Age.png")))
  
})


