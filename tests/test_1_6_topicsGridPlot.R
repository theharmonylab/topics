rm(list=ls())
gc()

library(testthat)
library(topics)  # Replace with your package name
library(tibble)
library(dplyr)
library(topics)


test_that('Save all topics without the topic grid plot by using the topicsPlot function.',{
  
  if (dir.exists("./results")){
    unlink("./results", recursive = TRUE)
  }
  
  dtmtest <- topicsDtm(
    data = topics::data$harmonytexts
  )
  
  model <- topicsModel(dtmtest)
  
  preds <- topicsPreds(
    model = model, 
    data = topics::data$harmonywords
  )

  tests2D <- topicsTest(
    model = model,
    preds = preds,
    data =  topics::data,
    pred_var_x = 'hilstotal',
    pred_var_y = 'swlstotal',
    control_vars = c('age','gender')
  )
  random_sequence <- sample(1:9, size = nrow(tests2D[[3]]$test), replace = TRUE)
  tests2D[[3]]$test$color_categories <- random_sequence
  
  topicsPlot(model = model,
             test = tests2D,
             grid_plot = FALSE,
             p_threshold = 0.99
             ,seed = 42)
  
  testthat::expect_true(dir.exists("./results/seed42/wordclouds"))

})

test_that('Save the scatter legend and grid legend for topic grids using default parameters.',{
  
  if (dir.exists("./results")){
    unlink("./results", recursive = TRUE)
  }
  
  dtmtest <- topicsDtm(
    data = topics::data$harmonytexts
  )
  
  model <- topicsModel(dtmtest)
  
  preds <- topicsPreds(
    model = model, 
    data = topics::data$harmonywords
  )
  
  tests2D <- topicsTest(
    model = model,
    preds = preds,
    data =  topics::data,
    pred_var_x = 'hilstotal',
    pred_var_y = 'swlstotal',
    control_vars = c('age','gender')
  )
  
  topicsPlot(model = model,
             test = tests2D,
             grid_plot = FALSE,
             p_threshold = 0.99
             ,seed = 42)
  
  testthat::expect_true(dir.exists("./results/seed42/wordclouds"))
  
})

test_that('Setting dimension = 1 or 3 for 2 dimensional plots shall return nothing.',{
  
  if (dir.exists("./results")){
    unlink("./results", recursive = TRUE)
  }
  
  dtmtest <- topicsDtm(
    data = topics::data$harmonytexts
  )
  
  model <- topicsModel(dtmtest)
  
  preds <- topicsPreds(
    model = model, 
    data = topics::data$harmonywords
  )
  
  tests2D <- topicsTest(
    model = model,
    preds = preds,
    data =  topics::data,
    pred_var_x = 'hilstotal',
    pred_var_y = 'swlstotal',
    control_vars = c('age','gender')
  )
  random_sequence <- sample(1:9, size = nrow(tests2D[[3]]$test), replace = TRUE)
  tests2D[[3]]$test$color_categories <- random_sequence
  
  out1 <- topicsPlot(model = model,
                     test = tests2D,
                     grid_plot = TRUE,
                     p_threshold = 0.99,
                     dim = 1
                     ,seed = 42)
  
  testthat::expect_true(is.null(out1))
  
  out1 <- topicsPlot(model = model,
                     test = tests2D,
                     grid_plot = TRUE,
                     p_threshold = 0.99,
                     dim = 3
                     ,seed = 42)
  
  testthat::expect_true(is.null(out1))
  
})

test_that('Set dimension = 2 for successfully saving the legends.',{
  
  if (dir.exists("./results")){
    unlink("./results", recursive = TRUE)
  }
  
  dtmtest <- topicsDtm(
    data = topics::data$harmonytexts
  )
  
  model <- topicsModel(dtmtest)
  
  preds <- topicsPreds(
    model = model, 
    data = topics::data$harmonywords
  )
  
  tests2D <- topicsTest(
    model = model,
    preds = preds,
    data =  topics::data,
    pred_var_x = 'hilstotal',
    pred_var_y = 'swlstotal',
    control_vars = c('age','gender')
  )
  
  
  topicsPlot(model = model,
             test = tests2D,
             grid_plot = TRUE,
             p_threshold = 0.99,
             dim = 2
             ,seed = 42)
  
  testthat::expect_true(
    file.exists(
      './results/seed_42/wordclouds/dot_legend_corvar_hilstotal_swlstotal.svg'))
  testthat::expect_true(
    file.exists(
      './results/seed_42/wordclouds/grid_legend_corvar_hilstotal_swlstotal.svg'))
  
})

test_that('Change the popout method to "max_x", and "max_y".',{
  
  if (dir.exists("./results")){
    unlink("./results", recursive = TRUE)
  }
  
  dtmtest <- topicsDtm(
    data = topics::data$harmonytexts
  )
  
  model <- topicsModel(dtmtest)
  
  preds <- topicsPreds(
    model = model, 
    data = topics::data$harmonywords
  )
  
  tests2D <- topicsTest(
    model = model,
    preds = preds,
    data =  topics::data,
    pred_var_x = 'hilstotal',
    pred_var_y = 'swlstotal',
    control_vars = c('age','gender')
  )
  random_sequence <- sample(1:9, size = nrow(tests2D[[3]]$test), replace = TRUE)
  tests2D[[3]]$test$color_categories <- random_sequence
  
  topicsPlot(model = model,
             test = tests2D,
             grid_plot = TRUE,
             p_threshold = 0.99,
             dim = 2,
             scatter_legend_way_popout_topics = 'max_x',
             seed = 42)
  
  testthat::expect_true(
    file.exists(
      './results/seed_42/wordclouds/dot_legend_corvar_hilstotal_swlstotal.svg'))
  testthat::expect_true(
    file.exists(
      './results/seed_42/wordclouds/grid_legend_corvar_hilstotal_swlstotal.svg'))
  
  topicsPlot(model = model,
             test = tests2D,
             grid_plot = TRUE,
             p_threshold = 0.99,
             dim = 2,
             scatter_legend_way_popout_topics = 'max_y',
             seed = 42)
  
  testthat::expect_true(
    file.exists(
      './results/seed_42/wordclouds/dot_legend_corvar_hilstotal_swlstotal.svg'))
  testthat::expect_true(
    file.exists(
      './results/seed_42/wordclouds/grid_legend_corvar_hilstotal_swlstotal.svg'))
  
})


test_that('Manually set the topic numbers to save topics',{
  
  if (dir.exists("./results")){
    unlink("./results", recursive = TRUE)
  }
  
  dtmtest <- topicsDtm(
    data = topics::data$harmonytexts
  )
  
  model <- topicsModel(dtmtest)
  
  preds <- topicsPreds(
    model = model, 
    data = topics::data$harmonywords
  )
  
  tests <- topicsTest(
    model = model,
    preds = preds,
    data =  topics::data,
    pred_var_x = 'hilstotal',
    pred_var_y = 'swlstotal'
  )
  
  tests2D <- topicsTest(
    model = model,
    preds = preds,
    data =  topics::data,
    pred_var_x = 'hilstotal',
    pred_var_y = 'swlstotal',
    control_vars = c('age','gender')
  )
  
  
  topicsPlot(model = model,
             test = tests2D,
             grid_plot = TRUE,
             p_threshold = 0.99,
             dim = 2,
             scatter_legend_user_spec_topics = c('t_1', 't_2'),
             seed = 42)
  random_sequence <- sample(1:9, size = nrow(tests2D[[3]]$test), replace = TRUE)
  tests2D[[3]]$test$color_categories <- random_sequence
  
  testthat::expect_true(
    file.exists(
      './results/seed_42/wordclouds/dot_legend_corvar_hilstotal_swlstotal.svg'))
  testthat::expect_true(
    file.exists(
      './results/seed_42/wordclouds/grid_legend_corvar_hilstotal_swlstotal.svg'))
  
})

test_that('Set dimension = 1 for successfully saving the legends',{
  if (dir.exists("./results")){
    unlink("./results", recursive = TRUE)
  }
  
  dtmtest <- topicsDtm(
    data = topics::data$harmonytexts
  )
  
  model <- topicsModel(dtmtest)
  
  preds <- topicsPreds(
    model = model, 
    data = topics::data$harmonywords
  )
  
  tests1D <- topicsTest(
    model = model,
    preds = preds,
    data =  topics::data,
    pred_var_x = 'hilstotal',
    #pred_var_y = NULL,
    #control_vars = c('age','gender')
  )
  
  topicsPlot(model = model,
             test = tests1D,
             grid_plot = TRUE,
             p_threshold = 0.99,
             dim = 1,
             scatter_legend_user_spec_topics = c('t_1', 't_2'),
             seed = 42)
  
  testthat::expect_true(
    file.exists(
      './results/seed_42/wordclouds/dot_legend_corvar_hilstotal.svg'))
  testthat::expect_true(
    file.exists(
      './results/seed_42/wordclouds/grid_legend_corvar_hilstotal.svg'))
  
})




