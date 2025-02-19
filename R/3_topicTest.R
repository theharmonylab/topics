#### func topic testing ####
# p.adjust https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/p.adjust

#' The function for topic testing
#' @param topic_loadings (tibble) The predicted loadings of topics including the grouping variable.
#' @param grouping_variable (tibble) The variable for grouping
#' @param topic_terms (R_obj) The object from model$summary in textmineR package vignette topic_modeling
#' @param split (string) How to split the CONTINUOUS test_values for testing
#' @param n_min_max (integer) If split = "min_max", the number of records to test per group.
#' @param multiple_comparison (string) The p-correction method
#' @return Results
#' @importFrom dplyr contains select everything bind_cols bind_rows right_join join_by rename_with mutate across
#' @importFrom tibble is_tibble as_tibble
#' @importFrom stats  complete.cases sd lm glm as.formula
#' @noRd
topic_test <- function(
    topic_terms,
    topics_loadings,
    x_y_axis1,
    controls,
    test_method,
    split,
    n_min_max = 20,
    multiple_comparison
) {
  
  
  # Format Checker
  if (!tibble::is_tibble(topics_loadings)) {
    stop("Parameter `topics_loadings` must be a tibble.")
  }

  if (test_method %in% c("linear_regression", "logistic_regression")) {
    
    # Get number of topics automatically
    num_topics <- ncol(topics_loadings)
    lda_topics <- paste0("t_", 1:num_topics)
    z_lda_topics <- paste0("z_", lda_topics)
    preds <- topics_loadings
    
    # Standardize topic loadings; harmonize how scaling are made. 
    for (topic in lda_topics) {
      mean_value <- mean(preds[[topic]], na.rm = TRUE)
      std_dev <- stats::sd(preds[[topic]], na.rm = TRUE)
      preds[[paste0("z_", topic)]] <- (preds[[topic]] - mean_value) / std_dev
    }
    
    # Standardize control variables
    if(ncol(controls) != 0){
       # Rename columns to start with "z_"
       controls <- controls %>%
         dplyr::rename_with(~ paste0("z_", .), dplyr::everything())
       
       # Scale each column and convert the result to a numeric vector
       controls <- controls %>%
         dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.numeric(base::scale(.))))
       
     }
    
    column_name <- colnames(x_y_axis1)
   # z_outcome <- tibble(!!paste0("z_", column_name) := base::scale(x_y_axis1)[1:nrow(x_y_axis1)])
    z_outcome <- tibble::tibble(x_y_axis1) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ base::scale(.))) %>%
      dplyr::rename_with(~ paste0("z_", column_name))
    
    colnames(z_outcome) <- (paste0("z_", column_name))
    
    # Replace NA values with 0 #### Should be a flag
    #preds[is.na(preds)] <- 0
    if (any(is.na(preds))) {
      message("There are NA values in the predictions (preds).")
      
      #preds[is.na(preds)]
      
      #which(is.na(preds), arr.ind = TRUE)
      #rows_with_na <- which(apply(preds, 1, anyNA))
      
      message("Note that all NAs are set to 0.")
      #preds[is.na(preds)] <- 0
    } 
    
    
    # For logistic regression, ensure the outcome variable is binary
    if (test_method == "logistic_regression") {
      if (!all(x_y_axis1[[1]] %in% c(0, 1))) {
        stop(paste("The outcome variable", colnames(x_y_axis1), "must be binary (0 or 1) for logistic regression."))
      }
    }
    
    # Initialize an empty list to store models
    multi_models <- list()
    
    # Making the regression formulas
    if(!ncol(controls) == 0){
      formula_tail <- paste0("+", paste(paste0(colnames(controls)), collapse = " + "))
    } else {
      formula_tail <- NULL
    }
    
    regression_data <-  dplyr::bind_cols(
      preds, x_y_axis1, z_outcome, controls)
    
    if (test_method == "linear_regression" | test_method == "logistic_regression"){
      
      if (test_method == "linear_regression") {
        target_name <- colnames(z_outcome)
      }
      if (test_method == "logistic_regression") {
        target_name <- colnames(x_y_axis1)
      }
      
      for (i in 1:length(lda_topics)) {
        topic <- paste0("z_", lda_topics[i])
          
        #formula <- stats::as.formula(paste0(topic, formula_tail))
        formula <- stats::as.formula(paste0(target_name, "~", topic, formula_tail))
        
        message(colourise(
            paste0(i, ": fitting model formula: ", 
                   paste(deparse(formula), collapse = " "), "\n"), 
            "green"))
          
        if (test_method == "linear_regression") {
          multi_models[[i]] <- stats::lm(formula, data = regression_data)
        }
        if (test_method == "logistic_regression") {
          multi_models[[topic]] <- stats::glm(formula, family = binomial, data = regression_data)
        }
      }
    }
    
    #### Extract statistics from models ####
    summary_statistics <- list()
    
    for (i in seq_along(multi_models)) {
      
      model_summary <- summary(multi_models[[i]])$coefficients
      
      if (test_method == "linear_regression") {
        
        estimate_values <- model_summary[, "Estimate"][z_lda_topics[i]][[1]]
        t_values <- model_summary[, "t value"][z_lda_topics[i]][[1]]
        p_values <- model_summary[, "Pr(>|t|)"][z_lda_topics[i]][[1]]
        
      } else if (test_method == "logistic_regression") {
        
        estimate_values <- model_summary[, "Estimate"][z_lda_topics[i]][[1]]
        t_values <- model_summary[, "z value"][z_lda_topics[i]][[1]]
        p_values <- model_summary[, "Pr(>|z|)"][z_lda_topics[i]][[1]]
        
      }
      res <- tibble::tibble(
        estimate_values = estimate_values,
        t_values = t_values,
        p_values = p_values 
      )
      
      colnames(res) <- c(
        paste0(target_name, ".estimate_beta"),
        if(test_method == "linear_regression") paste0(target_name, ".t"),
        if(test_method == "logistic_regression") paste0(target_name, ".z"), 
        paste0(target_name, ".p")
      )
      summary_statistics[[i]] <- res
    }
    summary_statistics <- dplyr::bind_rows(summary_statistics)
  
    ### Adjust p-value here
    p_variable <- summary_statistics[, grepl("\\.p", colnames(summary_statistics))]
    summary_statistics[paste0(target_name, ".p_adjusted")] <- stats::p.adjust(
      p_variable[[1]], multiple_comparison)
    
    # Merge with topic_terms for additional metadata
    output <- dplyr::bind_cols(
      topic_terms[c("topic", "top_terms", "prevalence", "coherence")], 
      summary_statistics)
    
    return(output)
  }
}


#' Test topics or n-grams
#' 
#' Statistically test topics or n-grams in relation to one or two other variables using 
#' regression or t-test.  
#' @param model (list) A trained model LDA-model from the topicsModel() function.
#' @param data (tibble) The tibble containing the variables to be tested.
#' @param preds (tibble) The predictions from the topicsPred() function.
#' @param ngrams (list) Output of the n-gram function.
#' @param x_variable (string) The x variable name to be predicted, and to be plotted (only needed for regression or correlation).
#' @param y_variable (string) The y variable name to be predicted, and to be plotted (only needed for regression or correlation).
# @param group_var (string) The variable to group by (only needed for t-test)
#' @param controls (vector) The control variables (not supported yet).
#' @param test_method (string) The test method to use. "default" checks if x_variable and y_variable
#' only contain 0s and 1s, for which it applies logistic regression; otherwise it applies linear regression. 
#' Alternatively, the user may manually specify either "linear_regression" or "logistic_regression".  
# @param p_alpha (numeric) Threshold of p value set by the user for visualising significant topics 
#' @param p_adjust_method (character) Method to adjust/correct p-values for multiple comparisons
#' (default = "fdr"; see also "holm", "hochberg", "hommel", "bonferroni", "BH", "BY",  "none").
#' @param seed (integer) The seed to set for reproducibility
#' @return A list of the test results, test method, and prediction variable.
#' @examples
#' \donttest{
#' # Test the topic document distribution in respect to a variable
#' 
#' dtm <- topicsDtm(
#'   data = dep_wor_data$Depphrase)
#' 
#' model <- topicsModel(
#'   dtm = dtm, # output of topicsDtm()
#'   num_topics = 20,
#'   num_top_words = 10,
#'   num_iterations = 1000,
#'   seed = 42)
#'                      
#' preds <- topicsPreds(
#'  model = model, # output of topicsModel()
#'  data = dep_wor_data$Depphrase)
#'                      
#' test <- topicsTest(
#'   model = model, # output of topicsModel()
#'   data=dep_wor_data,
#'   preds = preds, # output of topicsPreds()
#'   test_method = "linear_regression",
#'   x_variable = "Age")
#' }                 
#' @importFrom dplyr bind_cols
#' @importFrom readr write_csv
#' @export
topicsTest <- function(
    data,
    model = NULL,
    preds = NULL,
    ngrams = NULL,
    x_variable = NULL,
    y_variable = NULL,
    controls = c(),
    test_method = "default",
    p_adjust_method = "fdr",
    seed = 42){
  
  
  ##### Getting complete.cases #####
  
#  # Select relevant columns from data
#  relevant_columns <- c(x_variable, y_variable, controls)
#  
#  # Ensure relevant columns exist in data
#  if (!all(relevant_columns %in% colnames(data))) {
#    stop("One or more relevant columns are missing in the data.")
#  }
#  
#  # Create a subset with relevant columns
#  data_subset <- data %>% dplyr::select(all_of(relevant_columns))
#  
#  # Merge preds with the data subset
#  merged_data <- dplyr::bind_cols(preds, data_subset)
#  
#  # Check initial number of rows
#  initial_rows <- nrow(merged_data)
#  
#  # Remove rows with any NA values
#  cleaned_data <- merged_data[complete.cases(merged_data), ]
#  
#  # Check number of rows after removing NAs
#  final_rows <- nrow(cleaned_data)
#  
#  # Compare and report the number of rows
#  message(sprintf("Rows before NA removal: %d", initial_rows))
#  message(sprintf("Rows after NA removal: %d", final_rows))
#  
#  # Optionally stop if too many rows are removed
#  if (final_rows == 0) {
#    stop("All rows have been removed due to missing values. Check your data for NAs.")
#  }
#  
#  # Update preds and data for subsequent analysis
#  preds <- cleaned_data %>% dplyr::select(names(preds))
#  data <- cleaned_data %>% dplyr::select(all_of(relevant_columns))
#  
  ###### End of handling NAs ####
  
  if (is.null(x_variable) & is.null(y_variable)){
    msg <- 'Please input the x_variable, and/or y_variable.'
    message(colourise(msg, "brown"))
    # return (NULL)
  }
  
  #### Warnings and instructions ####
  if(!is.null(y_variable)){
    if(grepl("__", y_variable)){
      stop("The x_variable, y_variable or controls cannot have names containing 2 underscores in a row ('__'). 
           Please rename the variable in the dataset.")
    }
  }
  if(!is.null(x_variable)){
    if(grepl("__", x_variable)){
      stop("The x_variable, y_variable or controls cannot have names containing 2 underscores in a row ('__'). 
           Please rename the variable in the dataset.")
    }
  }
  
  if (length(controls) > 0){
    for (control_var in controls){
      if (!is.numeric(data[[control_var]])){
        
        msg <- paste0("The controls variable '", 
                      control_var, 
                      "' should be numeric!\n")
        
        message(
          colourise(msg, "brown"))
        
        return (NULL)
      }}
  }
  
  if (is.null(x_variable) & is.null(y_variable)) {
    msg <- "Prediction variable is missing. Please input a prediction variable."
    message(colourise(msg, "brown"))
    return(NULL)
  }
  
  if (!is.list(model) & !is.list(ngrams)){
    msg <- "Input a model from the topicsModel() function or an ngram object from the topicsGrams() function."
    
    message(colourise(msg, "brown"))
    
    return(NULL)
  }
  
  if (length(data) == 0){
    msg <- "The data provided is empty. Please provide a list of text data."
    message(colourise(msg, "brown"))
    
    return(NULL)
  }
  
  if(!is.null(preds)){
    if (nrow(preds) == 0){
      msg <- "The predictions provided are empty. Please provide a list of predictions."
      message(colourise(msg, "brown"))
      return(NULL)
    }
    
    if (nrow(data) != nrow(preds)){
      msg <- "The number of data points and predictions do not match. Please provide predictions that were created from the same data."
      message(colourise(msg, "brown"))
      return(NULL)
    }
  }
  
  #### N-grams testing ####
  # (rearranging the data so that it fits the topics pipeline)
  if (!is.null(ngrams)){
    
    #freq_per_user <- tibble(ngrams$freq_per_user[,2:ncol(ngrams$freq_per_user)])
    freq_per_user <- ngrams$freq_per_user
    ngrams <- ngrams$ngrams
    total_n <- nrow(ngrams)
    colnames(freq_per_user) <- paste0("t_", 1:total_n)
    preds <- freq_per_user
    
    
    # 
    
    model$summary <- list(topic = paste0("t_", 1:total_n),
                          top_terms = ngrams$ngrams, 
                          prevalence = ngrams$prevalence, 
                          coherence = ngrams$coherence, 
                          pmi = ngrams$pmi)
    
    model$summary <- data.frame(model$summary)
  }
  
  
  #### Testing the elements (i.e., ngrams or topics) ####
  x_y_axis <- c(x_variable, y_variable)
  
  if(test_method == "default"){
    # Function to check if a column is binary (contains only 0 and 1)
    is_binary <- function(column) {
      all(unique(column[!is.na(column)]) %in% c(0, 1))
    }
    
    # Create a vector of test_method for each column in x_y_axis
    test_method <- sapply(data[x_y_axis], function(col) {
      if (is_binary(col)) {
        "logistic_regression"
      } else {
        "linear_regression"
      }
    })
  } else if (test_method == "linear_regression") {
    
    test_method <- rep("linear_regression", 2)
    
  } else if (test_method == "logistic_regression") {
    
    test_method <- rep("logistic_regression", 2)
    
  }
  
  topic_loadings_all <- list()
  pre <- c('x','y')
  # i = 1
  for (i in 1:length(x_y_axis)){
    
    test <- topic_test(
      topic_terms = model$summary,
      topics_loadings = preds,
      x_y_axis1 = data[x_y_axis[i]],
      controls = data[controls],
      test_method = test_method[i],
      multiple_comparison = p_adjust_method)
    
    topic_loading <- list(
      test = test,
      test_method = test_method[i],
      x_y_axis1 = x_y_axis[i])
    
    
    # Sorting output when not using ridge regression
      
    colnames(topic_loading$test) <- c("topic", "top_terms", "prevalence", "coherence",
                                        paste(pre[i], 
                                              colnames(topic_loading$test)[5:8], 
                                              sep = "."))
      
    topic_loadings_all[[i]] <- topic_loading
    
  }
  
  if (!is.null(y_variable)){
    # create the x.y.word.category
    topic_loadings_all[[3]] <- list()
    topic_loadings_all[[3]]$test <- dplyr::left_join(topic_loadings_all[[1]][[1]],#[,1:8], 
                                                     topic_loadings_all[[2]][[1]],#[,1:8],
                                                     by = c("topic", "top_terms", 
                                                            "prevalence", "coherence"))
    
    topic_loadings_all[[3]]$test_method <- topic_loadings_all[[1]]$test_method
    topic_loadings_all[[3]]$x_y_axis <- paste0(topic_loadings_all[[1]]$x_y_axis, '__',
                                               topic_loadings_all[[2]]$x_y_axis) 
    
  } else {
    
      msg <- "The parameter y_variable is not set! Output 1 dimensional results."
      message(colourise(msg, "blue"))
      
      topic_loadings_all[[2]] <- list()
      topic_loadings_all[[3]] <- list()
      topic_loadings_all[[3]]$test <- topic_loadings_all[[1]][[1]]#[,1:8]
      topic_loadings_all[[3]]$test_method <- topic_loadings_all[[1]]$test_method
      topic_loadings_all[[3]]$x_y_axis <- topic_loadings_all[[1]]$x_y_axis
      
  }
  
  topic_loadings_all <- topic_loadings_all[[length(topic_loadings_all)]]
  return(topic_loadings_all)
}