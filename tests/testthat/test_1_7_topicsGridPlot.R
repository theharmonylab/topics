library(testthat)
library(tibble)
library(dplyr)
library(topics)

test_that('Case 1: Save all topics without the topic grid plot by using the topicsPlot function.',{
  
  testthat::skip_on_cran()
  save_dir_temp <- tempfile()
  
  dtmtest <- topics::topicsDtm(
    data = topics::dep_wor_data$Wortext
  )
  
  model <- topics::topicsModel(
    dtmtest)
  
  preds <- topics::topicsPreds(
    model = model, 
    data = topics::dep_wor_data$Wortext
  )

#  dat1 <- dplyr::mutate(dep_wor_data, gender = ifelse(gender == "male", 0, 1))
  #help(topicsTest)
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
  plots <- topics::topicsPlot(
    model = model,
    test = tests2D,
    p_alpha = 0.99,
    seed = 1,
    figure_format = "png",
    save_dir = save_dir_temp
    )
#  testthat::expect_true(dir.exists("./results/seed42/wordclouds"))

})

test_that('Case 2: Save the scatter legend and grid legend for topic grids using default parameters.',{
  
  testthat::skip_on_cran()

  
  dtmtest <- topics::topicsDtm(
    data = dep_wor_data$Wortext
  )
  
  model <- topics::topicsModel(
    dtmtest)
  
  preds <- topics::topicsPreds(
    model = model, 
    data = dep_wor_data$Wortext
  )
  
  tests2D <- topics::topicsTest(
    model = model,
    preds = preds,
    data =  dep_wor_data,
    x_variable = 'PHQ9tot',
    y_variable = 'GAD7tot',
    controls = c('Age','Gender')
  )
  
  save_dir_temp <- tempfile()
  
  topics::topicsPlot(
    model = model,
    test = tests2D,
    p_alpha = 0.99,
    seed = 2, 
    save_dir = save_dir_temp, 
    figure_format = "png")
  
  testthat::expect_true(dir.exists(paste0(
    save_dir_temp, "/seed_2/wordclouds"))
    )
  
})

test_that('Case 3: Setting dimension = 1 or 3 for 2 dimensional plots shall return nothing.',{
  
  testthat::skip_on_cran()

  save_dir_temp <- "./res"
  dtmtest <- topics::topicsDtm(
    data = dep_wor_data$Wortext
  )
  
  model <- topics::topicsModel(
    dtmtest)
  
  preds <- topics::topicsPreds(
    model = model, 
    data = dep_wor_data$Wortext
  )
  
#  dat1 <- dplyr::mutate(topics::data, gender = ifelse(gender == "male", 0, 1))
  
  tests2D <- topics::topicsTest(
    model = model,
    preds = preds,
    data =  dep_wor_data,
    x_variable = 'PHQ9tot',
    y_variable = 'GAD7tot',
    controls = c('Age','Gender')
  )
#  random_sequence <- sample(1:9, size = nrow(tests2D[[3]]$test), replace = TRUE)
#  tests2D[[3]]$test$color_categories <- random_sequence
  
  save_dir_temp <- tempfile()
  
  out1 <- topics::topicsPlot(
    model = model,
    test = tests2D,
    p_alpha = 0.99,
    seed = 3, 
    save_dir = save_dir_temp, 
    figure_format = "png")
 
  # After save_dir update this is now NULL 
  # testthat::expect_true(!is.null(out1))
  
  out1 <- topics::topicsPlot(
    model = model,
    test = tests2D,
    p_alpha = 0.99,
    seed = 4, 
    save_dir = save_dir_temp, 
    figure_format = "png")
  
  # After save_dir update this is now NULL
#  testthat::expect_true(!is.null(out1))
  
})

test_that('Case 4: Set dimension = 2 for successfully saving the legends.',{
  
  testthat::skip_on_cran()
  
  dtmtest <- topics::topicsDtm(
    data = dep_wor_data$Wortext
  )
  
  model <- topics::topicsModel(
    dtmtest)
  
  preds <- topics::topicsPreds(
    model = model, 
    data = dep_wor_data$Deptext
  )
  
  # dat1 <- dplyr::mutate(topics::data,gender = ifelse(gender == "male", 0, 1))
  
  tests2D <- topics::topicsTest(
    model = model,
    preds = preds,
    data = dep_wor_data,
    x_variable = 'PHQ9tot',
    y_variable = 'GAD7tot',
    controls = c('Age','Gender')
  )
  
  save_dir_temp <- tempfile()
  
  topics::topicsPlot(
    model = model,
    test = tests2D,
    p_alpha = 0.99,
    seed = 5, 
    save_dir = save_dir_temp, 
    figure_format = "png")
  
#  testthat::expect_true(
#    file.exists(
#      './results/seed_42/wordclouds/dot_legend_corvar_hilstotal_swlstotal.svg'))
#  
#  testthat::expect_true(
#    file.exists(
#      './results/seed_42/wordclouds/grid_legend_corvar_hilstotal_swlstotal.svg'))
  
})

test_that('Case 5: Change the popout method to "max_x", and "max_y".',{
  
  testthat::skip_on_cran()

  dtmtest <- topics::topicsDtm(
    data = dep_wor_data$Wortext
  )
  
  model <- topics::topicsModel(
    dtmtest)
  
  preds <- topics::topicsPreds(
    model = model, 
    data = dep_wor_data$Deptext
  )
  
#  dat1 <- dplyr::mutate(topics::data,gender = ifelse(gender == "male", 0, 1))
  tests2D <- topics::topicsTest(
    model = model,
    preds = preds,
    data = dep_wor_data,
    x_variable = 'PHQ9tot',
    y_variable = 'GAD7tot',
    controls = c('Age','Gender')
  )
  #random_sequence <- sample(1:9, size = nrow(tests2D[[3]]$test), replace = TRUE)
  #tests2D[[3]]$test$color_categories <- random_sequence
  
  save_dir_temp <- tempfile()
  
  topics::topicsPlot(
    model = model,
    test = tests2D,
    p_alpha = 0.99,
    scatter_legend_method = 'max_x',
    seed = 6, 
    save_dir = save_dir_temp, 
    figure_format = "png")
  
#  testthat::escatter_legend_method = testthat::expect_true(
#    file.exists(
#      './results/seed_42/wordclouds/dot_legend_corvar_hilstotal_swlstotal.svg'))
#  testthat::expect_true(
#    file.exists(
#      './results/seed_42/wordclouds/grid_legend_corvar_hilstotal_swlstotal.svg'))
  
  topics::topicsPlot(
    model = model,
    test = tests2D,
    p_alpha = 0.99,
    scatter_legend_method = 'max_y',
    seed = 7, 
    save_dir = save_dir_temp, 
    figure_format = "png")
  
#  testthat::expect_true(
#    file.exists(
#      './results/seed_42/wordclouds/dot_legend_corvar_hilstotal_swlstotal.svg'))
#  testthat::expect_true(
#    file.exists(
#      './results/seed_42/wordclouds/grid_legend_corvar_hilstotal_swlstotal.svg'))
  
})


test_that('Case 6: Manually set the topic numbers to save topics',{
  
  testthat::skip_on_cran()
  
  dtmtest <- topics::topicsDtm(
    data = dep_wor_data$Wortext
  )
  
  model <- topics::topicsModel(
    dtmtest)
  
  preds <- topics::topicsPreds(
    model = model, 
    data = dep_wor_data$Wortext
  )
  
  tests <- topics::topicsTest(
    model = model,
    preds = preds,
    data = dep_wor_data,
    x_variable = 'PHQ9tot',
    y_variable = 'GAD7tot'
  )
  
#  dat1 <- dplyr::mutate(topics::data,gender = ifelse(gender == "male", 0, 1))

  tests2D <- topics::topicsTest(
    model = model,
    preds = preds,
    data = dep_wor_data,
    x_variable = 'PHQ9tot',
    y_variable = 'GAD7tot',
    controls = c('Age','Gender')
  )
  
  save_dir_temp <- tempfile()
  
  topics::topicsPlot(
    model = model,
    test = tests2D,
    p_alpha = 0.99,
    scatter_legend_specified_topics = c('t_1', 't_2'),
    seed = 8, 
    save_dir = save_dir_temp, 
    figure_format = "png")
  
  #random_sequence <- sample(1:9, size = nrow(tests2D[[3]]$test), replace = TRUE)
  #tests2D[[3]]$test$color_categories <- random_sequence
  
#  testthat::expect_true(
#    file.exists(
#      './results/seed_42/wordclouds/dot_legend_corvar_hilstotal_swlstotal.svg'))
#  testthat::expect_true(
#    file.exists(
#      './results/seed_42/wordclouds/grid_legend_corvar_hilstotal_swlstotal.svg'))
  
})

test_that('Case 7: Set dimension = 1 for successfully saving the legends',{

  testthat::skip_on_cran()
  
  dtmtest <- topics::topicsDtm(
    data = dep_wor_data$Wortext
  )
  
  model <- topics::topicsModel(
    dtmtest)
  
  preds <- topics::topicsPreds(
    model = model, 
    data = dep_wor_data$Deptext
  )
  
  tests1D <- topics::topicsTest(
    model = model,
    preds = preds,
    data =  dep_wor_data,
    x_variable = 'PHQ9tot'
  )
  
  save_dir_temp <- tempfile()
  
  topics::topicsPlot(
    model = model,
    test = tests1D,
    p_alpha = 0.99,
    scatter_legend_specified_topics = c('t_1', 't_2'),
    seed = 9, 
    save_dir = save_dir_temp, 
    figure_format = "png")
  
#  testthat::expect_true(
#    file.exists(
#      './results/seed_42/wordclouds/dot_legend_corvar_hilstotal.svg'))
#  testthat::expect_true(
#    file.exists(
#      './results/seed_42/wordclouds/grid_legend_corvar_hilstotal.svg'))
  
})
