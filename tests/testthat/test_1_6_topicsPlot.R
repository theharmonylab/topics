
library(testthat)
library(vdiffr)
library(topics)

test_that("N-Grams: topicsPlot with topicsGrams (without and with test",{
  
  testthat::skip_on_os(c("linux", "windows"))  # Skip on Ubuntu (Linux) and Windows
  testthat::skip_on_cran()
  
  # No test (i.e., no dimension) help(topicsGrams)
  ngrams <- topics::topicsGrams(
    data = dep_wor_data$Worphrase[1:100], 
    ngram_window = c(1:2), 
    stopwords = NULL,
    pmi_threshold = NULL)
  
  save_dir_temp <- tempfile()
  #save_dir_temp = "./results3"
  
  # Create a plot function
  ngram <- function() {
    topics::topicsPlot(
      ngrams = ngrams, 
      ngrams_max = 40,
      ngram_select = "proportion",
      figure_format = "png", 
      save_dir = save_dir_temp)
  }
  
  # If it fails you should review changes <-  if they are intentionally you accept them: 
  # vdiffr::manage_cases()
  expect_doppelganger("1_ngram", ngram())
  
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_42/wordclouds/ngrams.png")))
  
  
  # With test (i.e., 1 dimension = two plots)
  ngrams <- topics::topicsGrams(
    data = dep_wor_data$Worphrase,
    pmi_threshold = 3)
  
  # help(topicsTest)
  test <- topics::topicsTest(
    data = dep_wor_data,
    ngrams = ngrams, 
    x_variable = "Age")
  
  
  
  pl <- topics::topicsPlot(
    ngrams = ngrams, 
    test = test,
    ngrams_max = 10,
    ngram_select = "prevalence",
    figure_format = "png", 
    p_alpha = 1, 
    save_dir = save_dir_temp)
  
  ngram_negative <- function() {
    pl$negative
  }
  expect_doppelganger("2_ngram_negative", ngram_negative())
  
  ngram_positive <- function() {
    pl$positive
  }
  expect_doppelganger("3_ngram_positive", ngram_positive())
  
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_42/wordclouds/ngrams_negative.png")))
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_42/wordclouds/ngrams_positive.png")))
  
})


test_that("topicsPlot WITHOUT test and preds", {
  testthat::skip_on_os(c("linux", "windows"))  # Skip on Ubuntu (Linux) and Windows
  testthat::skip_on_cran()
  
  dtm <- topics::topicsDtm(
    data = dep_wor_data$Deptext)
  
  #help(topicsModel)
  model <- topics::topicsModel(
    dtm = dtm, 
    num_topics = 50,
    num_top_words = 20, 
    num_iterations = 100)
  
  save_dir_temp <- tempfile()
  
  topics_1 <- topics::topicsPlot(
    model = model,
    plot_topics_idx = c(1,3),
    figure_format = "png", 
    save_dir = save_dir_temp)
  
  topics_t1 <- function() {
    topics_1$t1
  }
  expect_doppelganger("topics_t1", topics_t1())
  
  topics_t3 <- function() {
    topics_1$t3
  }
  expect_doppelganger("topics_t3", topics_t1())
  
  # Check if the word cloud directory exists
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_42/wordclouds/t_1.png")))
  
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_42/wordclouds/t_3.png")))
  
  #### Plot most prevalent topics in model ####  
  
  topics2 <- topics::topicsPlot(
    model = model,
    plot_n_most_prevalent_topics = 5,
    figure_format = "png", 
    save_dir = save_dir_temp)
  
  testthat::expect_equal(
    names(topics2), 
    c("t_t_2",  "t_t_29", "t_t_46", "t_t_36", "t_t_35")
  )
  
  topics2_t_t_2 <- function() {
    topics2$t1
  }
  expect_doppelganger("topics2_t_t_2", topics2_t_t_2())
  
  
  topics3 <- topics::topicsPlot(
    model = model,
    plot_n_most_prevalent_topics = 5,
    allowed_word_overlap = 2,
    figure_format = "png", 
    save_dir = save_dir_temp)
  
  testthat::expect_equal(
    names(topics3), 
    c("t_t_2", "t_t_46", "t_t_31", "t_t_18", "t_t_19")
  )
  
  topics3_t3 <- function() {
    topics3$t3
  }
  expect_doppelganger("topics3_t3", topics3_t3())
  
  
  testthat::expect_error(topics::topicsPlot(
    model = model,
    plot_topics_idx = c("t1"),
    plot_n_most_prevalent_topics = 5,
    figure_format = "png", 
    save_dir = save_dir_temp))
  
})


test_that("topicsPlot WITH test", {
  
  testthat::skip_on_os(c("linux", "windows"))  # Skip on Ubuntu (Linux) and Windows
  testthat::skip_on_cran()
  
  ## 1-Dimension
  dtm <- topics::topicsDtm(
    data = dep_wor_data$Deptext)
  
  model <- topics::topicsModel(
    dtm = dtm, 
    num_topics = 50,
    num_top_words = 20, 
    num_iterations = 100)
  
  #### Plots one-dimensional regression type plot ####
  preds <- topics::topicsPreds(
    model = model, 
    data = dep_wor_data$Deptext)
  
  test1 <- topics::topicsTest(
    model = model,
    preds = preds,
    data = dep_wor_data,
    x_variable = "Age")

  save_dir_temp <- tempdir()
  
  topics4 <- topics::topicsPlot(
    model = model, 
    test = test1, 
    p_alpha = .1,
    scatter_legend_dot_size = c(1, 10),
    scatter_legend_bg_dot_size = c(10, 20),
    scatter_legend_dots_alpha = 0.3, 
    scatter_legend_bg_dots_alpha = 0.1,
    figure_format = "png",
    seed = 11, 
    allowed_word_overlap = 3,
    save_dir = save_dir_temp, 
    grid_legend_number_color = "white")
  
  topics4$distribution
  topics4$legend
  

  topics4_legend <- function() {
    topics4$legend
  }
  expect_doppelganger("topics4_legend", topics4_legend())
  
  topics4_distribution <- function() {
    topics4$distribution
  }
  expect_doppelganger("topics4_distribution", topics4_distribution())
  
  topics4square1t_48 <- function() {
    topics4$square1$t_48
  }
  expect_doppelganger("topics4square1t_48", topics4square1t_48())
  
#  topics4square2t_1 <- function() {
#    topics4$square2$t_1
#  }
#  expect_doppelganger("topics4square2t_1", topics4square2t_1())
  
  topics4square3t_34 <- function() {
    topics4$square3$t_34
  }
  expect_doppelganger("topics4square2t_1", topics4square3t_34())
  
  # Check if the wordcloud directory exists
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_11/wordclouds/dot_legend_corvar_Age.png")))
  
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_11/wordclouds/grid_legend_corvar_Age.png")))
  
  
  
  #### Plots one-dimensional LOGISTIC regression type plot ####
  preds <- topics::topicsPreds(
    model = model, 
    data = dep_wor_data$Deptext)
  
  test1 <- topics::topicsTest(
    model = model,
    preds = preds,
    data = dep_wor_data,
    x_variable = "Gender")
  
  save_dir_temp <- tempdir()
  
  topics4_Logistic <- topicsPlot(
    model = model, 
    test = test1, 
    p_alpha = .1,
    figure_format = "png",
    seed = 11)
  
  topics4_Logistic$legend
  topics4_Logistic$distribution
  
  topics4_legend_Logistic <- function() {
    topics4_Logistic$legend
  }
  expect_doppelganger("topics4_legend_Logistic", topics4_legend_Logistic())
  
  topics4_distribution_Logistic <- function() {
    topics4_Logistic$distribution
  }
  expect_doppelganger("topics4_distribution_Logistic", topics4_distribution_Logistic())
  
#  topics4square2t_1 <- function() {
#    topics4_Logistic$square2$t_1
#  }
#  expect_doppelganger("topics4square2t_1", topics4square2t_1())
  
  
  ###########################
  #### 2-Dimension  #########
  ###########################
  
  test2 <- topics::topicsTest(
    model = model, 
    preds = preds, 
    data = dep_wor_data, 
    x_variable = "PHQ9tot",
    y_variable = "Age"
    )
  
  topics5 <- topics::topicsPlot(
    model = model, 
    test = test2, 
    p_alpha = 0.5, 
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
  

  # Legend
  topics5legend <- function() {
    topics5$legend
  }
  expect_doppelganger("topics5legend", topics5legend())
  
  # Distribution
  topics5distribution <- function() {
    topics5$distribution
  }
  expect_doppelganger("topics5distribution", topics5distribution())
  
  # Square 1
  topics5square1 <- function() {
    topics5$square1$t_6
  }
  expect_doppelganger("topics5square1", topics5square1())
  
  # Square 2
  topics5square2 <- function() {
    topics5$square2$t_34
  }
  expect_doppelganger("topics5square2", topics5square2())
  
#  # Square 3
#  topics5square3 <- function() {
#    topics5$square3$t_1
#  }
#  expect_doppelganger("topics5square3", topics5square3())
  
  # Square 4
  topics5square4 <- function() {
    topics5$square4$t_2
  }
  expect_doppelganger("topics5square4", topics5square4())
  
#  # Square 5
#  topics5square5 <- function() {
#    topics5$square5
#  }
#  expect_doppelganger("topics5square5", topics5square5())
  
  # Square 6
  topics5square6 <- function() {
    topics5$square6$t_7
  }
  expect_doppelganger("topics5square6", topics5square6())
  
#  # Square 7
#  topics5square7 <- function() {
#    topics5$square7
#  }
#  expect_doppelganger("topics5square7", topics5square7())
#  
#  # Square 8
#  topics5square8 <- function() {
#    topics5$square8
#  }
#  expect_doppelganger("topics5square8", topics5square8())
  
  # Square 9
  topics5square9 <- function() {
    topics5$square9$t_23
  }
  expect_doppelganger("topics5square9", topics5square9())
  
  
  # Check if the wordcloud directory exists
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_12/wordclouds/dot_legend_corvar_PHQ9tot__Age.png")))
  
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_12/wordclouds/grid_legend_corvar_PHQ9tot__Age.png")))
  
  
  #### Setting specific topics to plot ######
  topics_specific <- topics::topicsPlot(
    model = model,
    test = test2,
    p_alpha = 0.99,
    scatter_legend_specified_topics = c('t_1', 't_2'),
    seed = 8, 
    save_dir = save_dir_temp, 
    figure_format = "png")
  
  # Checking legend
  topics_specific_t1 <- function() {
    topics_specific$distribution
  }
  expect_doppelganger("topics_specific_t1", topics_specific_t1())
  
  
  ##### Using Control Variables 
  
  tests2D <- topics::topicsTest(
    model = model,
    preds = preds,
    data =  dep_wor_data,
    x_variable = 'PHQ9tot',
    y_variable = 'GAD7tot',
    controls = c('Age','Gender')
  )
  
  #random_sequence <- sample(1:9, size = nrow(tests2D[[3]]$test), replace = TRUE)
  #tests2D[[3]]$test$color_categories <- random_sequence
  
  # Why do we get a warning here
  plots_controlled <- topics::topicsPlot(
    model = model,
    test = tests2D,
    p_alpha = 0.99,
    seed = 1,
    figure_format = "png",
    save_dir = save_dir_temp
  )
  
  # Checking legend
  plots_controlled_distribution <- function() {
    plots_controlled$distribution
  }
  expect_doppelganger("plots_controlled_distribution", plots_controlled_distribution())
  
  # Checking legend
  plots_controlled_legend <- function() {
    plots_controlled$legend
  }
  expect_doppelganger("plots_controlled_legend", plots_controlled_legend())
  
  # Checking legend
  plots_controlled_square1t_3 <- function() {
    plots_controlled$square1$t_3
  }
  expect_doppelganger("plots_controlled_square1t_3", plots_controlled_square1t_3())
  
  
})


test_that("topicsPlot WITH underscores in names", {
  
  testthat::skip_on_os(c("linux", "windows"))  # Skip on Ubuntu (Linux) and Windows
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
  
  testthat::skip_on_os(c("linux", "windows"))  # Skip on Ubuntu (Linux) and Windows
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

