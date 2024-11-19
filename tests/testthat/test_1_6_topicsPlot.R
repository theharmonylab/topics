# tests/test-topicsWordclouds.R
library(testthat)
library(topics)  # Replace with your package name
#library(text)


test_that("N-Grams: topicsPlot with topicsGrams (without and with test",{
  
  testthat::skip_on_cran()
  
  
  # No test (i.e., no dimension) 
  data <- dep_wor_data$Worphrase
  ngrams <- topics::topicsGrams(
    data = data, 
    top_n = 10, 
    n = 3, 
    pmi_threshold = 3)
  
  topics::topicsPlot(
    ngrams = ngrams, 
    figure_format = "png" )
  
  testthat::expect_true(file.exists("./results/seed_42/wordclouds/ngrams.png"))
  file.remove("./results/seed_42/wordclouds/ngrams.png")
  
  
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
    pred_var_x = "Age")
  
  #help(topicsPlot)
  topics::topicsPlot(
    ngrams = ngrams, 
    test = test,
    figure_format = "png", 
    p_threshold = 1)
  
  testthat::expect_true(file.exists("./results/seed_42/wordclouds/ngrams_negative.png"))
  testthat::expect_true(file.exists("./results/seed_42/wordclouds/ngrams_positive.png"))
  
  #unlink("./results/", recursive = TRUE)
  
  
})


test_that("topicsPlot WITHOUT test and preds", {
  
  testthat::skip_on_cran()
  
  dtm <- topics::topicsDtm(data = dep_wor_data$Deptext)
  model <- topics::topicsModel(dtm = dtm)
  
  #help(topicsPlot)
  topics::topicsPlot(
    model = model,
    plot_topics_idx = c(1,3),
    figure_format = "png")
  
  # Check if the wordcloud directory exists
  testthat::expect_true(file.exists("./results/seed_42/wordclouds/t_1.png"))
  testthat::expect_true(file.exists("./results/seed_42/wordclouds/t_3.png"))
  
  unlink("./results/", recursive = TRUE)
})


test_that("topicsPlot WITH test", {
  
  testthat::skip_on_cran()
  
  ## 1-Dimension
  dtm <- topics::topicsDtm(data = dep_wor_data$Deptext)
  model <- topics::topicsModel(dtm = dtm)
  preds <- topics::topicsPreds(model = model, data = dep_wor_data$Deptext)
  
  test1 <- topics::topicsTest(
    model= model,
    preds = preds,
    data = dep_wor_data,
    pred_var_x = "Age")

  topics::topicsPlot(
    model = model, 
    test = test1, 
    p_threshold = 1,
    figure_format = "png",
    seed = 11)
  
  # Check if the wordcloud directory exists
  testthat::expect_true(file.exists("./results/seed_11/wordclouds/dot_legend_corvar_Age.png"))
  testthat::expect_true(file.exists("./results/seed_11/wordclouds/grid_legend_corvar_Age.png"))
  
#  file.remove("./results/seed_01/wordclouds/dot_legend_corvar_Age.png")
#  file.remove("./results/seed_01/wordclouds/grid_legend_corvar_Age.png")

  ## 2-Dimension  
  
  test2 <- topics::topicsTest(
    model = model, 
    preds = preds, 
    data = dep_wor_data, 
    pred_var_x = "PHQ9tot",
    pred_var_y = "Age"
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
    seed = 12
    )
  
  
  # Check if the wordcloud directory exists
  testthat::expect_true(file.exists("./results/seed_12/wordclouds/dot_legend_corvar_PHQ9tot_Age.png"))
  testthat::expect_true(file.exists("./results/seed_12/wordclouds/grid_legend_corvar_PHQ9tot_Age.png"))
  
 # unlink("./results/", recursive = TRUE)
})


