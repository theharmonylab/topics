
library(testthat)
library(topics)

test_that("N-Grams: topicsPlot with topicsGrams (without and with test",{
  
  testthat::skip_on_cran()
  
  # No test (i.e., no dimension) help(topicsGrams)
  ngrams <- topics::topicsGrams(
    data = dep_wor_data$Worphrase, 
    top_n = 50, 
    n = 3, 
    pmi_threshold = 6)
  
  save_dir_temp <- tempfile()
  save_dir_temp = "./results3"
  
  plots1 <- topics::topicsPlot(
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
    x_variable = "Age")
  
  #help(topicsPlot)
  plot2 <- topics::topicsPlot(
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
  
  dtm <- topics::topicsDtm(
    data = dep_wor_data$Deptext)
  
  model <- topics::topicsModel(
    dtm = dtm)
  
  save_dir_temp <- tempfile()
  save_dir_temp <- "./Oscar"
  
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
  
  #### Plot most prevalent topics in model ####  
  
  plots_prevalence <- topics::topicsPlot(
    model = model,
    plot_n_most_prevalent_topics = 5,
    figure_format = "png", 
    save_dir = save_dir_temp)
  
  testthat::expect_error(topics::topicsPlot(
    model = model,
    plot_topics_idx = c("t1"),
    plot_n_most_prevalent_topics = 5,
    figure_format = "png", 
    save_dir = save_dir_temp))
  
})


test_that("topicsPlot WITH test", {
  
  testthat::skip_on_cran()
  save_dir_temp <- tempfile()
  #save_dir_temp = "./results"
  
  ## 1-Dimension
  dtm <- topics::topicsDtm(
    data = dep_wor_data$Deptext)
  
  model <- topics::topicsModel(
    dtm = dtm)
  
  #### Plots one-dimensional plot ####
  preds <- topics::topicsPreds(
    model = model, 
    data = dep_wor_data$Deptext)
  
  test1 <- topics::topicsTest(
    model = model,
    preds = preds,
    data = dep_wor_data,
    x_variable = "Age")

  plots3 <- topics::topicsPlot(
    model = model, 
    test = test1, 
    p_alpha = 1,
    figure_format = "png",
    seed = 11, 
    save_dir = save_dir_temp)
  
  plots3$legend
  plots3$distribution
  
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
    y_variable = "Age"
    )
  
  plot4 <- topics::topicsPlot(
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
  
  plot4$legend
  plot4$distribution
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
    data = data_test$Dep_text)
  
  model <- topics::topicsModel(
    dtm = dtm)
  
  preds <- topics::topicsPreds(
    model = model, 
    data = data_test$Dep_text)
  
  test1 <- topics::topicsTest(
    model= model,
    preds = preds,
    data = data_test,
    x_variable = "Age_test",
    y_variable = "Age")
  
  plot5 <- topics::topicsPlot(
    model = model, 
    test = test1, 
    p_alpha = 1,
    figure_format = "png",
    seed = 11, 
    save_dir = save_dir_temp)
  plot5
  # Check if the wordcloud directory exists
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_11/wordclouds/dot_legend_corvar_Age_test__Age.png")))
  
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_11/wordclouds/grid_legend_corvar_Age_test__Age.png")))
  
  
  ## 2-Dimension  
  
  test2 <- topics::topicsTest(
    model = model, 
    preds = preds, 
    data = data_test, 
    x_variable = "PHQ9tot",
    y_variable = "Age_test"
  )
  
  plot6 <- topics::topicsPlot(
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

  
  ## 1-Dimension
  dtm <- topics::topicsDtm(
    data = dep_wor_data$Deptext,
    pmi_threshold = 1
    )
  
  model <- topics::topicsModel(
    dtm = dtm)
  
  preds <- topics::topicsPreds(
    model = model, 
    data = dep_wor_data$Deptext)
  
  test1 <- topics::topicsTest(
    model= model,
    preds = preds,
    data = dep_wor_data,
    x_variable = "Age")
  
  testthat::expect_equal(test1$test$x.z_Age.estimate[1:4], 
                         c(0.012425389,  0.034527241,  0.039768994,  0.007723522), 
                         tolerance = 0.00001)
  
  
  
})
