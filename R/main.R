#' the function for creating a document term matrix
#' @param data (list) the list containing the text data with each entry belonging to a unique id
#' @param ngram_window (list) the minimum and maximum n-gram length, e.g. c(1,3)
#' @param stopwords (stopwords) the stopwords to remove, e.g. stopwords::stopwords("en", source = "snowball")
#' @param removalword (string) the word to remove
#' @param occ_rate (integer) the rate of occurence of a word to be removed
#' @param removal_mode (string) the mode of removal -> "none", "threshold", "absolute" or "percent", Threshold removes all words under a certain frequency or over a certain frequency as indicated by removal_rate_least and removal_rate_most, Absolute removes an absolute amount of terms that are most frequent and least frequent, Percent the amount of words indicated by removal_rate_least and removal_rate_most relative to the amount of terms in the matrix
#' @param removal_rate_most (integer) the rate of most frequent words to be removed, functionality depends on removal_mode
#' @param removal_rate_least (integer) the rate of least frequent words to be removed, functionality depends on removal_mode
#' @param split (float) the proportion of the data to be used for training
#' @param seed (integer) the random seed for reproducibility
#' @param save_dir (string) the directory to save the results, default is "./results", if NULL, no results are saved
#' @return the document term matrix
#' @importFrom textmineR CreateDtm
#' @importFrom stats complete.cases
#' @importFrom stopwords stopwords
#' @importFrom Matrix colSums
#' @importFrom tibble as_tibble
#' @export
topicsDtm <- function(data, # 
                   ngram_window=c(1,3),
                   stopwords=stopwords::stopwords("en", source = "snowball"),
                   removalword="",
                   occ_rate=0,
                   removal_mode="none",
                   removal_rate_most=0,
                   removal_rate_least=0,
                   split=1,
                   seed=42L,
                   save_dir="./results",
                   load_dir=NULL){
  


  if (!is.null(load_dir)){
    dtms <- readRDS(paste0(load_dir, 
                            "/seed_",
                            seed, 
                            "/dtms.rds"))
    
  } else {
    
    if (length(data) == 0){
      print("The data provided is empty. Please provide a list of text data.")
      return(NULL)
    }
    
    set.seed(seed)
    
    id_col = "id"
    data_col = "text"
    if (is.data.frame(data)){
      data <- data[,1]
    } 
    text_cols <- data.frame(text = data)
    text_cols[[id_col]] <- 1:nrow(text_cols) # create unique id
    text_cols <- as_tibble(text_cols)
    text_cols <- text_cols[stats::complete.cases(text_cols), ] # remove rows without values
    text_cols = text_cols[sample(1:nrow(text_cols)), ] # shuffle
    split_index <- round(nrow(text_cols) * split) 
    train <- text_cols[1:split_index, ] # split training set
    if (split<1){
      test <- text_cols[split_index:nrow(text_cols), ] # split test set
    } else {
      test <- train
    }
    
    if (removalword != ""){
      train[[data_col]] <- gsub(paste0("\\b", removalword, "\\b"), "", train[[data_col]]) 
    }
    
    # create a document term matrix for training set help(CreateDtm)
    train_dtm <- textmineR::CreateDtm(
      doc_vec = train[["text"]], # character vector of documents
      doc_names = train[["id"]], # document names
      ngram_window = ngram_window, # minimum and maximum n-gram length
      stopword_vec = stopwords, #::stopwords("en", source = "snowball"),
      lower = TRUE, # lowercase - this is the default value
      remove_punctuation = TRUE, # punctuation - this is the default
      remove_numbers = TRUE, # numbers - this is the default
      verbose = FALSE, # Turn off status bar for this demo
      cpus = 4) # default is all available cpus on the system
  
    
    if (occ_rate>0){
      removal_frequency <- round(nrow(train)*occ_rate) -1
      train_dtm <- train_dtm[,Matrix::colSums(train_dtm) > removal_frequency]
    }
    if (removal_mode != "threshold"){
      if (removal_rate_least > 0){
        removal_columns <- get_removal_columns(train_dtm, removal_rate_least, "least", removal_mode)
        if (removal_rate_most > 0){
          removal_columns_most <- get_removal_columns(train_dtm, removal_rate_most, "most", removal_mode)
          removal_columns <- c(removal_columns, removal_columns_most)
        }
        train_dtm <- train_dtm[,-removal_columns]
      } else if (removal_rate_most > 0){
        removal_columns <- get_removal_columns(train_dtm, removal_rate_most, "most", removal_mode)
        train_dtm <- train_dtm[,-removal_columns]
      }
    } else if (removal_mode == "threshold"){
      if (!is.null(removal_rate_least)){
        train_dtm <- train_dtm[,Matrix::colSums(train_dtm) > removal_rate_least]
      }
      if (!is.null(removal_rate_most)){
        train_dtm <- train_dtm[,Matrix::colSums(train_dtm) < removal_rate_most]
      }
    }
    
    
    # create a document term matrix for test set
    test_dtm <- textmineR::CreateDtm(
      doc_vec = test[[data_col]], # character vector of documents
      doc_names = test[[id_col]], # document names
      ngram_window = ngram_window, # minimum and maximum n-gram length
      stopword_vec = stopwords::stopwords("en", source = "snowball"),
      lower = TRUE, # lowercase - this is the default value
      remove_punctuation = TRUE, # punctuation - this is the default
      remove_numbers = TRUE, # numbers - this is the default
      verbose = FALSE, # Turn off status bar for this demo
      cpus = 4) # default is all available cpus on the system
    
    if (occ_rate>0){
      
      removal_frequency <- round(nrow(test)*occ_rate) -1
      
      test_dtm <- test_dtm[, Matrix::colSums(test_dtm) > removal_frequency]
    }
    #removal_frequency <- get_occ_frequency(test_dtm, occ_rate)
    #test_dtm <- test_dtm[,Matrix::colSums(test_dtm) > removal_frequency]
    
    if (removal_mode != "threshold"){
      if (removal_rate_least > 0){
        removal_columns <- get_removal_columns(test_dtm, removal_rate_least, "least", removal_mode)
        if (removal_rate_most > 0){
          removal_columns_most <- get_removal_columns(test_dtm, removal_rate_most, "most", removal_mode)
          removal_columns <- c(removal_columns, removal_columns_most)
        }
        test_dtm <- test_dtm[,-removal_columns]
      } else if (removal_rate_most > 0){
        removal_columns <- get_removal_columns(test_dtm, removal_rate_most, "most", removal_mode)
        test_dtm <- test_dtm[,-removal_columns]
      }
    } else if (removal_mode == "threshold"){
      if (!is.null(removal_rate_least)){
        test_dtm <- test_dtm[,Matrix::colSums(test_dtm) > removal_rate_least]
      }
      if (!is.null(removal_rate_most)){
        test_dtm <- test_dtm[,Matrix::colSums(test_dtm) < removal_rate_most]
      }
    }
    
    dtms <- list(train_dtm = train_dtm, 
                 test_dtm = test_dtm, 
                 train_data = train, 
                 test_data = test)
  }
  
  if (!is.null(save_dir)){
    if (!dir.exists(save_dir)) {
      # Create the directory
      dir.create(save_dir)
      cat("Directory created successfully.\n")
    } 
    if(!dir.exists(paste0(save_dir, "/seed_", seed))){
      dir.create(paste0(save_dir, "/seed_", seed))
    }
    print(paste0("The Dtm, data, and summary are saved in", save_dir,"/seed_", seed,"/dtms.rds"))
    saveRDS(dtms, paste0(save_dir, "/seed_", seed, "/dtms.rds"))
  }
  
  return(dtms)
}

#' The function to create and train and lda Model
#' @param dtm (R_obj) The document term matrix
#' @param num_topics (integer) The number of topics to be created
#' @param num_top_words (integer) The number of top words to be displayed
#' @param num_iterations (integer) The number of iterations to run the model
#' @param seed (integer) The seed to set for reproducibility
#' @param save_dir (string) The directory to save the model, if NULL, the model will not be saved
#' @param load_dir (string) The directory to load the model from, if NULL, the model will not be loaded
#' @return A list of the model, the top terms, the labels, the coherence, and the prevalence
#' @export
topicsModel <- function(dtm,
                    num_topics = 20,
                    num_top_words = 10,
                    num_iterations = 1000,
                    seed = 42,
                    save_dir = "./results",
                    load_dir = NULL){

  
  if (!is.null(load_dir)){
    model <- readRDS(paste0(load_dir, 
                            "/seed_",
                            seed, 
                            "/model.rds"))
  } else {
    
    dtm <- dtm$train_dtm
    
    if (length(Matrix::colSums(dtm)) == 0) {
      print("The document term matrix is empty. Please provide a valid document term matrix.")
      return(NULL)
    }
    
    set.seed(seed)
    
    model <- get_mallet_model(
      dtm = dtm,
      num_topics = num_topics,
      num_top_words = num_top_words,
      num_iterations = num_iterations, 
      seed = seed)
    
    
    model$summary <- data.frame(
      topic = rownames(model$labels),
      label = model$labels,
      coherence = round(model$coherence, 3),
      prevalence = round(model$prevalence,3),
      top_terms = apply(model$top_terms,
                        2, 
                          function(x){paste(x, collapse = ", ")}),
                                stringsAsFactors = FALSE)
    model$summary[order(model$summary$prevalence, decreasing = TRUE) , ][ 1:10 , ]
  }
  
  if (!is.null(save_dir)){
    if (!dir.exists(save_dir)) {
      # Create the directory
      dir.create(save_dir)
      cat("Directory created successfully.\n")
    } 
    if(!dir.exists(paste0(save_dir, "/seed_", seed))){
      dir.create(paste0(save_dir, "/seed_", seed))
    }
    print(paste0("The Model is saved in", save_dir,"/seed_", seed,"/model.rds"))
    saveRDS(model, paste0(save_dir, "/seed_", seed, "/model.rds"))
  }
  
  return(model)
}



#' The function to predict the topics of a new document with the trained model
#' @param model (list) The trained model
#' @param data (tibble) The new data
#' @param num_iterations (integer) The number of iterations to run the model
#' @param seed (integer) The seed to set for reproducibility
#' @param save_dir (string) The directory to save the model, if NULL, the predictions will not be saved
#' @param load_dir (string) The directory to load the model from, if NULL, the predictions will not be loaded
#' @return A tibble of the predictions
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr %>%
#' @export
topicsPreds <- function(model, # only needed if load_dir==NULL 
                     data, # data vector to infer distribution for
                     num_iterations=100, # only needed if load_dir==NULL,
                     seed=42,
                     save_dir="./results",
                     load_dir=NULL){
  set.seed(seed)
  

  
  if (!is.null(load_dir)){
    preds <- readRDS(paste0(load_dir, "/seed_", seed, "/preds.rds"))
  } else {
    
    if (length(data) == 0){
      print("The data provided is empty. Please provide a list of text data.")
      return(NULL)
    }
    # create an id column for the data
    
    if (is.data.frame(data)){
      pred_text <- data[, 1]
    } else {
      pred_text <- data
    }
    
    pred_ids <- as.character(1:length(data))
    #pred_ids <- as.character(data[[id_col]])
    
    new_instances <- compatible_instances(ids=pred_ids,
                                          texts=pred_text,
                                          instances=model$instances)
    
    inf_model <- model$inferencer
    #print(inf_model)
    preds <- infer_topics(
      inferencer = inf_model,
      #instances = model$instances,
      instances = new_instances,
      n_iterations= 200,
      sampling_interval = 10, # aka "thinning"
      burn_in = 10,
      random_seed = seed
    )
  
    preds <- tibble::as_tibble(preds)
    colnames(preds) <- paste("t_", 1:ncol(preds), sep="")
    #if (!is.null(group_var)){
    #  categories <- data[group_var]
    #  preds <- bind_cols(categories, preds)
    #}
    preds <- preds %>% tibble::tibble()
    
  }
  
  if (!is.null(save_dir)){
    if (!dir.exists(save_dir)) {
      dir.create(save_dir)
      cat("Directory created successfully.\n")
    } 
    if(!dir.exists(paste0(save_dir, "/seed_", seed))){
      dir.create(paste0(save_dir, "/seed_", seed))
    }
    print(paste0("Predictions are saved in", save_dir,"/seed_", seed,"/preds.rds"))
    saveRDS(preds, paste0(save_dir, "/seed_", seed, "/preds.rds"))
  }
  
  return(preds)
}

#' The function to test the lda model
#' @param model (list) The trained model
#' @param data (tibble) The data to test on
#' @param preds (tibble) The predictions
#' @param pred_var (string) The variable to be predicted (only needed for regression or correlation)
#' @param group_var (string) The variable to group by (only needed for t-test)
#' @param control_vars (vector) The control variables
#' @param test_method (string) The test method to use, either "correlation","t-test", "linear_regression","logistic_regression", or "ridge_regression"
#' @param p_adjust_method (character) Method to adjust/correct p-values for multiple comparisons
#' (default = "none"; see also "holm", "hochberg", "hommel", "bonferroni", "BH", "BY",  "fdr").
#' @param seed (integer) The seed to set for reproducibility
#' @param load_dir (string) The directory to load the test from, if NULL, the test will not be loaded
#' @param save_dir (string) The directory to save the test, if NULL, the test will not be saved
#' @return A list of the test results, test method, and prediction variable
#' @importFrom dplyr bind_cols
#' @importFrom readr write_csv
#' @export
topicsTest <- function(model,
                    preds, 
                    data,
                    pred_var=NULL, # for all test types except t-test
                    group_var=NULL, # only one in the case of t-test
                    control_vars=c(),
                    test_method="linear_regression",
                    p_adjust_method = "fdr",
                    seed=42,
                    load_dir=NULL,
                    save_dir="./results"){
  
  
  
  if (!is.null(load_dir)){
    if (!file.exists(test_path)) {
      print(paste0("Test file not found at: ", paste0(load_dir, "/seed_", seed, "/test.rds"), ". Exiting function."))
      return(NULL)
    }
    test <- readRDS(paste0(load_dir, "/seed_", seed, "/test.rds"))
  } else {
    
    if (is.null(pred_var) && test_method != "t-test") {
      print("Prediction variable is missing. Please input a prediction variable for all test types except t-test.")
      return(NULL)
    }
    
    if (test_method == "t-test" && is.null(group_var)){
      print("Group variable is missing. Please input a group variable for t-test.")
      return(NULL)
    }
    
    if (!is.list(model)){
      print("Input a model created with topicsModel")
      return(NULL)
    }
    
    if (length(data) == 0){
      print("The data provided is empty. Please provide a list of text data.")
      return(NULL)
    }
    
    if (nrow(preds) == 0){
      print("The predictions provided are empty. Please provide a list of predictions.")
      return(NULL)
    }
    
    if (nrow(data) != nrow(preds)){
      print("The number of data points and predictions do not match. Please provide predictions that were created from the same data.")
      return(NULL)
    }
    
    control_vars <- c(pred_var, control_vars)
    
    
    if (!is.null(group_var)){
      if (!(group_var %in% names(preds))){
        preds <- dplyr::bind_cols(data[group_var], preds)
      }
    }
    for (control_var in control_vars){
      if (!(control_var %in% names(preds))){
        preds <- dplyr::bind_cols(data[control_var], preds)
      }
    }
    if (test_method=="ridge_regression"){
      group_var <- pred_var
    }
    
    preds <- preds %>% tibble::tibble()
    
    test <- topic_test(topic_terms = model$summary,
                       topics_loadings = preds,
                       grouping_variable = preds[group_var],
                       control_vars = control_vars,
                       test_method = test_method,
                       split = "median",
                       n_min_max = 20,
                       multiple_comparison = p_adjust_method)
  }
  
  if (!is.null(save_dir)){
    if (!dir.exists(save_dir)) {
      # Create the directory
      dir.create(save_dir)
      cat("Directory created successfully.\n")
    } else {
      cat("Directory already exists.\n")
    }
    
    if(!dir.exists(paste0(save_dir, "/seed_", seed))){
      dir.create(paste0(save_dir, "/seed_", seed))
    }
    
    if (test_method == "ridge_regression"){
      df <- list(variable = group_var,
                 estimate = test$estimate,
                 t_value = test$statistic,
                 p_value = test$p.value)
      readr::write_csv(data.frame(df), paste0(save_dir, "/seed_", seed, "/textTrain_regression.csv"))
    }
    saveRDS(test, paste0(save_dir, "/seed_", seed, "/test_",test_method, ".rds"))
    print(paste0("The test was saved in: ", save_dir,"/seed_", seed, "/test_",test_method, ".rds"))
  }
  
  return(list(test = test, 
              test_method = test_method, 
              pred_var = pred_var))
}

#' The function to create lda wordclouds
#' @param model (list) The trained model
#' @param test (list) The test results
#' @param color_negative_cor (R_obj) The color gradient for negative correlations
#' @param color_positive_cor (R_obj) The color gradient for positive correlations
#' @param scale_size (logical) Whether to scale the size of the words
#' @param plot_topics_idx (vector) The topics to plot determined by index
#' @param p_threshold (integer) The p-value threshold to use for significance
#' @param save_dir (string) The directory to save the wordclouds
#' @param figure_format (string) Set the figure format, e.g., svg, or png.
#' @param width (integer) The width of the topic (units = "in"). 
#' @param height (integer) The width of the topic (units = "in").
#' @param seed (integer) The seed to set for reproducibility
#' @return nothing is returned, the wordclouds are saved in the save_dir
#' @export
topicsPlot <- function(model,
                       test,
                       color_negative_cor = scale_color_gradient(low = "darkgreen", high = "green"),
                       color_positive_cor = scale_color_gradient(low = "darkred", high = "red"),
                       scale_size = FALSE,
                       plot_topics_idx = NULL,
                       p_threshold = 0.05,
                       save_dir = "./results",
                       figure_format = "svg",
                       width = 10, 
                       height = 8,
                       seed = 42){
  
  model <- name_cols_with_vocab(model, "phi", model$vocabulary)
  test_type = test$test_method
  pred_var = test$pred_var
  df_list <- create_topic_words_dfs(model$summary)
  df_list <- assign_phi_to_words(df_list, model$phi, "mallet")
  

  create_plots(df_list = df_list, 
               summary = model$summary,
               test = test$test, 
               test_type = test$test_method,
               cor_var = pred_var,
               color_negative_cor = color_negative_cor,
               color_positive_cor = color_positive_cor,
               scale_size = scale_size,
               plot_topics_idx = plot_topics_idx,
               p_threshold = p_threshold,
               figure_format = figure_format,
               width = width, 
               height = height,
               save_dir = save_dir,
               seed = seed)
  print(paste0("The plots are saved in ", save_dir, "/seed", seed, "/wordclouds"))
}