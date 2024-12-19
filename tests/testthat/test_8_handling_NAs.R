

library(testthat)
library(topics)
library(text)
library(glmnet)
test_that("Handling NAs",{
  
  testthat::skip_on_cran()
  
  NA_data <- dep_wor_data %>% 
    dplyr::select(c(Deptext, Gender, Age, PHQ9tot))
  
  colnames(NA_data)
  
  set.seed(123) # Set seed for reproducibility
  
  # Introduce NAs in the 'Deptext' column
  NA_data$Deptext[sample(1:nrow(NA_data), size = 5)] <- NA
  table(is.na(NA_data$Deptext))
  # Introduce NAs in the 'Gender' column
  NA_data$Gender[sample(1:nrow(NA_data), size = 5)] <- NA
  table(is.na(NA_data$Gender))
  # Introduce NAs in the 'Age' column
  NA_data$Age[sample(1:nrow(NA_data), size = 5)] <- NA
  
  # Introduce NAs in the 'PHQ9tot' column
  NA_data$PHQ9tot[sample(1:nrow(NA_data), size = 5)] <- NA
  
  # Introduce an entire row of NAs
  row_to_na <- sample(1:nrow(NA_data), size = 1) # Randomly select one row
  NA_data[row_to_na, ] <- NA
  
  
  # Introducing rows with few words
  NA_data$Deptext[[2]] <- "hello"
  NA_data$Deptext[[3]] <- "hello you"
  NA_data$Deptext[[4]] <- "hello how are"
  ##### Testing dataset on our functions ####
  
  # Testing with _ 
  data_test <- NA_data

  ## 1-Dimension
  dtm <- topics::topicsDtm(
    data = data_test$Deptext)
  
  model <- topics::topicsModel(
    dtm = dtm)
  
  preds <- topics::topicsPreds(
    model = model, 
    data = data_test$Deptext)
  
  test1 <- topics::topicsTest(
    model= model,
    preds = preds,
    data = data_test,
    x_variable = "PHQ9tot",
    y_variable = "Age")
  
  save_dir_temp <- tempfile()
  
  topics::topicsPlot(
    model = model, 
    test = test1, 
    p_alpha = 1,
    figure_format = "png",
    seed = 11, 
    save_dir = save_dir_temp)
  
  # Check if the wordcloud directory exists
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_11/wordclouds/dot_legend_corvar_PHQ9tot__Age.png")))
  
  testthat::expect_true(file.exists(paste0(
    save_dir_temp, "/seed_11/wordclouds/grid_legend_corvar_PHQ9tot__Age.png")))
  
 
})


