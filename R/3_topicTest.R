#### func topic testing ####
# p.adjust https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/p.adjust

#' The function to determine a continuous variable from a threshold
#' @param df (tibble) The initial tibble
#' @param column_name (string) The name of the grouping variable
#' @param threshold (integer) The threshold for determine the continous variable
#' @return boolean value
#' @noRd
is_continuous_variable <- function(
    df, 
    threshold) {
  
  numbers_unique_values <- length(unique(df[[1]]))
  
  return (numbers_unique_values > threshold)
  
}

#' The function to calculate median split on a single column
#' @param topic_loadings (tibble) The initial tibble
#' @param grouping1 (string) The name of the grouping variable
#' @param colname (string) The name of the topic variable
#' @importFrom dplyr mutate sym
#' @importFrom stats median
#' @return RObj of the testing result of a single topic
#' @noRd
median_split_test_topic_wise <- function(
    topic_loadings,
    grouping1,
    colname) {
  
  median_grouping <- stats::median(topic_loadings[[grouping1]])
  
  topic_loadings <- topic_loadings %>%
    dplyr::mutate(group = ifelse(!!dplyr::sym(grouping1) >= median_grouping, 'Group1', 'Group2'))
  
  t_test_result <- t.test(topic_loadings[[colname]] ~ group,
                          var.equal = FALSE,
                          data = topic_loadings)
  return(t_test_result)
}

#' The function to calculate median split on columns
#' @param topic_loadings (tibble) The initial tibble
#' @param grouping1 (string) The name of the grouping variable
#' @param colnames (vector) The vector of the topic names
#' @importFrom purrr pmap
#' @return a named list of testing results
#' @noRd
median_split_test_topics <- function(
    topic_loadings,
    grouping1,
    colnames) {
  
  results <- purrr::pmap(list(
    list(topic_loadings),
    grouping1,
    colnames),
    median_split_test_topic_wise)
  
  names(results) <- colnames
  
  return(results)
}

#' The function to calculate the cor on a single column
#' @param topic_loadings (tibble) The initial tibble
#' @param grouping1 (string) The name of the grouping variable
#' @param colname (string) The name of the topic variable
#' @return RObj of the testing result of a single topic
#' @importFrom dplyr mutate sym
#' @importFrom stats cor.test
#' @noRd
corr_topic_wise <- function(
    topics_loadings,
    grouping1,
    colname) {
  
  corr_results <- stats::cor.test(topics_loadings[[grouping1]],
                                  topics_loadings[[colname]])
  
  return(corr_results)
}


#' The function to calculate correlation on columns
#' @param topic_loadings (tibble) The initial tibble
#' @param grouping1 (string) The name of the grouping variable
#' @param colnames (vector) The vector of the topic names
#' @param method1 (string) The method to adjust the p value
#' @importFrom purrr pmap map
#' @importFrom stats p.adjust
#' @return a named list of testing results
#' @noRd
topics_corr_grouping <- function(
    topics_loadings,
    grouping1,
    colnames1,
    method1="bonferroni") {
  
  topics_stats <- purrr::pmap(list(
    list(topics_loadings),
    grouping1,
    colnames1),
    corr_topic_wise)
  
  names(topics_stats) <- colnames1
  
  # help(map)
  topics_stats <- purrr::map(topics_stats,
                             ~ c(.x, adjust.p.value = stats::p.adjust(
                               .x$p.value,
                               method = method1,
                               n = length(topics_loadings))))
  return(topics_stats)
}

#' The function to extract key stats for a tibble
#' @param topics_stats (list) The output from the previous topic stats
#' @param cal_cohen_d (boolean) Whether to calculate cohen's d, not applicable for correlation results
#' @importFrom purrr pmap_dfr
#' @importFrom tibble tibble
#' @importFrom effsize cohen.d
#' @return a named list of testing results
#' @noRd
extract_topic_stats_corr <- function(
    topics_stats, 
    cal_cohen_d = FALSE) {
  
  # Define a function to extract the required information from a single topic
  extract_single_topic <- function(name, topic) {
    
    df <- topic$parameter["df"]
    p_value <- topic$p.value
    estimate <- topic$estimate["cor"]
    adjust.p_value <- topic$adjust.p.value
    conf_int_lower <- topic$conf.int[1]
    conf_int_higher <- topic$conf.int[2]
    
    # Calculate Cohen's d if cal_cohen_d is TRUE help(cohen.d)
    if (cal_cohen_d) {
      effect_size <- effsize::cohen.d(topic$statistic, df)$estimate
    } else {
      effect_size <- "not_available"
    }
    
    return(tibble::tibble(
      "topic_name" = name,
      "df" = df,
      "p.value" = p_value,
      "adjust.p_value" = adjust.p_value,
      "estimate_corr" = estimate,
      "conf.int_lower" = conf_int_lower,
      "conf.int_higher" = conf_int_higher,
      "cohen_d" = effect_size))
  }
  
  # Use purrr::pmap_dfr to apply the function to each topic in the list and return a tibble
  output <- purrr::pmap_dfr(list(names(topics_stats), 
                                 topics_stats),
                            extract_single_topic)
  
  return(output)
}

#' The function to t_test on topics
#' @param topics_loadings (list) The topic loadings from input
#' @param method1 (string) Method to adjust p value.
#' @param calc_cohen_d (boolean) To calculate cohen'd else return "NA"
#' @importFrom purrr pmap map
#' @importFrom utils combn
#' @importFrom dplyr filter
#' @importFrom stats t.test p.adjust
#' @importFrom effsize cohen.d
#' @noRd
topics_t_test_grouping <- function(
    topics_loadings,
    method1,
    calc_cohen_d = TRUE) {
  
  # Get unique combinations of 'value' column
  combinations <- utils::combn(unique(topics_loadings$value), 
                               2, 
                               simplify = FALSE)
  
  # Function to perform t-test and calculate Cohen's d
  t_test_func <- function(combination) {
    df1 <- topics_loadings %>% dplyr::filter(value == combination[1])
    df2 <- topics_loadings %>% dplyr::filter(value == combination[2])
    #df1 <- df1[, 2:ncol(df1)]
    #df2 <- df2[, 2:ncol(df2)]
    
    results <- purrr::map(3:ncol(topics_loadings), ~ list(
      #view(df1[[.]]),
      t_test = stats::t.test(df1[[.]], df2[[.]]),
      cohen_d = if (calc_cohen_d) effsize::cohen.d(df1[[.]], df2[[.]]) else "NA"
    ))
    
    # Adjust p-value if more than two categories
    groupings <- unique(topics_loadings$value)
    #view(length(groupings))
    if (length(groupings) > 1) {
      results <- purrr::map(results, ~ c(.x, adjust.p.value = stats::p.adjust(
        .x$t_test$p.value,
        method = method1,
        n = length(groupings))))
    } else {
      # Return "not_available" if a group has only one observation
      results <- purrr::map(results, ~ c(.x, adjust.p.value = "not_available"))
    }
    
    names(results) <- paste0("t_", 1:(ncol(topics_loadings)-2))
    
    return(results)
  }
  
  # Apply function to each combination
  results <- purrr::map(combinations, t_test_func)
  
  # Name each element in the list with the combination it represents
  names(results) <- purrr::map(combinations, 
                               paste, 
                               collapse = "_")
  
  return(results)
}


#' The function to extract key stats for a tibble
#' @param topics_stats (list) The output from the previous topic stats
#' @param cal_cohen_d (boolean) Whether to calculate cohen's d, not applicable for correlation results
#' @importFrom purrr pmap_dfr compact
#' @importFrom tibble tibble
#' @importFrom effsize cohen.d
#' @return a named list of testing results
#' @noRd
extract_topic_stats_cate <- function(
    topics_stats,
    cal_cohen_d = TRUE) {
  
  # Define a function to extract the required information from a single topic
  extract_single_topic <- function(
    name1, 
    topic1, 
    cal_cohen_d = TRUE) {
    # Extract the non-NULL htest object / flatten
    htest <- purrr::compact(unlist(topic1, 
                                   recursive = FALSE))
    
    t_stats <- htest[["t_test.statistic"]]
    df <- htest[["t_test.parameter"]][["df"]]
    adjust_p_value <- htest[["adjust.p.value"]]
    p_value <- htest[["t_test.p.value"]]
    mean_1 <- htest[["t_test.estimate"]][["mean of x"]]
    mean_2 <- htest[["t_test.estimate"]][["mean of y"]]
    conf_int_lower <- htest[["t_test.conf.int"]][[1]]
    conf_int_higher <- htest[["t_test.conf.int"]][[2]]
    
    # Calculate Cohen's d if cal_cohen_d is TRUE and df is sufficient
    #cal_cohen_d && df > 2
    if (!cal_cohen_d) {  # Adjust this threshold as needed
      # This is problematic to have NA values. Backup, no use.
      #eff_size <- effsize::cohen.d(htest$statistic[["t"]], df)$estimate
      eff_size <- "NA"
      eff_size_conf_lower <- "NA"
      eff_size_conf_higher <- "NA"
      
    } else {
      eff_size <- htest[["cohen_d.estimate"]]
      eff_size_conf_lower <- htest[["cohen_d.conf.int"]][["lower"]]
      eff_size_conf_higher <- htest[["cohen_d.conf.int"]][["upper"]]
    }
    
    return(tibble::tibble("topic_name" = name1,
                          "p.value" = p_value,
                          "adjusted_p.value" = adjust_p_value,
                          "cohen_d" = eff_size,
                          "mean_group_1" = mean_1,
                          "mean_group_2" = mean_2,
                          "t_stats" = t_stats,
                          "df" = df,
                          "p_conf.int_lower" = conf_int_lower,
                          "p_conf.int_higher" = conf_int_higher,
                          "cohen_d_conf.int_lower" = eff_size_conf_lower,
                          "cohen_d_conf.int_higher" = eff_size_conf_higher))
  }
  
  # Use purrr::pmap_dfr to apply the function to each topic in the list and return a tibble
  output <- purrr::pmap_dfr(list(names(topics_stats),
                                 topics_stats, cal_cohen_d),
                            extract_single_topic)
  
  return(output)
}


#' The function to sort the output
#' @param df (tibble) The output from the function extract stats
#' @importFrom dplyr arrange
#' @return a sorted tibble
#' @noRd
sort_stats_tibble <- function(
    df) {
  
  # Check if 'adjusted_p.value' column exists
  if ("adjusted_p.value" %in% colnames(df)) {
    # If it exists, check if it's numeric
    if (is.numeric(df$adjusted_p.value)) {
      # Sort by 'p.value' and 'adjusted_p.value'
      df <- df %>% dplyr::arrange(p.value, adjusted_p.value)
    } else {
      # If it's not numeric, sort by 'p.value' only
      df <- df %>% dplyr::arrange(p.value)
    }
  } else {
    # If 'adjusted_p.value' column doesn't exist, sort by 'p.value' only
    df <- df %>% dplyr::arrange(p.value)
  }
  return(df)
}

#' The function for topic testing
#' @param topic_loadings (tibble) The predicted loadings of topics including the grouping variable.
#' @param grouping_variable (tibble) The variable for grouping
#' @param topic_terms (R_obj) The object from model$summary in textmineR package vignette topic_modeling
#' @param split (string) How to split the CONTINUOUS test_values for testing
#' @param n_min_max (integer) If split = "min_max", the number of records to test per group.
#' @param multiple_comparison (string) The p-correction method
#' @return Results
#' @importFrom dplyr contains select everything bind_cols right_join join_by
#' @importFrom tibble is_tibble as_tibble
# @importFrom text textTrainRegression
#' @importFrom stats  complete.cases sd lm glm as.formula
#' @noRd
topic_test <- function(
    topic_terms,
    topics_loadings,
    grouping_variable,
    controls,
    test_method = "correlation",
    split = "median",
    n_min_max = 20,
    multiple_comparison = "bonferroni"){
  
  colnames(grouping_variable) <- "value"
  topics_groupings <- dplyr::bind_cols(topics_loadings,
                                       grouping_variable)
  
  topics_loadings <- topics_loadings[stats::complete.cases(topics_groupings), ]
  grouping_variable <- grouping_variable[stats::complete.cases(topics_groupings), ]
  
  # format checker
  if (TRUE){
    
    if (!tibble::is_tibble(topics_loadings)) {
      stop("Parameter `topics_loadings` must be a tibble.")
    }
    if (!tibble::is_tibble(grouping_variable)) {
      stop("Parameter `grouping_variable` must be a tibble.")
    }
    if (nrow(topics_loadings) != nrow(grouping_variable)){
      stop("Parameters `topics_loadings` & `grouping_variable`
           should have the same length.")
    }
    if (!is.character(split)) {
      stop("Parameter `split` must be a string.")
    }
    if (!split %in% c("median", "quartile", "min_max")) {
      stop("Parameter `split` must be one of 'median', 'quartile', or 'min_max'.")
    }
    if (!is.numeric(n_min_max)) {
      stop("Parameter `n_min_max` must be numeric.")
    }
    if (!is.character(multiple_comparison)) {
      stop("Parameter `multiple_comparison` must be a string.")
    }
    if (!multiple_comparison %in% c("holm", "hochberg", "hommel", "bonferroni",
                                    "BH", "BY","fdr", "none")) {
      stop("Variable `multiple_comparison` must be one of `holm`, `hochberg`,
      `hommel`, `bonferroni`, `BH`, `BY`,`fdr`, or `none`.")
    }
  }
  
  
  #  if (FALSE){
  #    model1$prevalence <- colSums(model1$theta) / sum(model1$theta) * 100
  #    model1$top_terms <- textmineR::GetTopTerms(phi = model1$phi, M = 5)
  #    model1$summary <- data.frame(topic = rownames(model1$phi),
  #                                 label = model1$labels,
  #                                 coherence = round(model1$coherence, 3),
  #                                 prevalence = round(model1$prevalence,3),
  #                                 top_terms = apply(model1$top_terms, 2, function(x){
  #                                   paste(x, collapse = ", ")
  #                                 }),
  #                                 stringsAsFactors = FALSE)
  #  }
  
  
  if (test_method == "correlation"){
    
    if (TRUE){
      temp <- cbind(grouping_variable, topics_loadings)
      colnames(temp)[1] <- colnames(grouping_variable)[1]
      colnames(temp)[2:ncol(temp)] <- colnames(topics_loadings)
      topics_loadings <- temp
      temp <- NULL
      
      result <- topics_corr_grouping(
        topics_loadings,
        grouping1 = colnames(topics_loadings)[1],
        colnames1 = colnames(topics_loadings)[2:ncol(topics_loadings)],
        method1 = multiple_comparison)
      # Change the output of a list to a tibble. For corr only now.
      output <- extract_topic_stats_corr(result)
      names(output)[1] <- c("topic_name")
      output <- dplyr::left_join(output, 
                                 topic_terms,
                                 by = dplyr::join_by(topic_name == topic))
      
      output <- output %>%
        dplyr::select(
          topic_name,
          p.value,
          adjust.p_value,
          top_terms,
          prevalence,
          coherence,
          dplyr::everything()  # this will include the rest of the columns in their original order
        )
    }
    
    return (output %>% sort_stats_tibble())
  }
  if (test_method == "t-test"){
    temp <- cbind(grouping_variable, topics_loadings)
    colnames(temp)[1] <- colnames(grouping_variable)[1]
    colnames(temp)[2:ncol(temp)] <- colnames(topics_loadings)
    topics_loadings <- temp
    temp <- NULL
    result <- topics_t_test_grouping(topics_loadings,
                                     method1 = multiple_comparison)
    
    # Produce the topic list through the pairs of categories
    output_list <- purrr::map(names(result), function(name) {
      output <- extract_topic_stats_cate(result[[name]])
      #view(output)
      names(output)[1] <- c("topic_name")
      
      #output <- dplyr::left_join(output, topic_terms, by = join_by(topic_name == topic))
      output <- dplyr::left_join(output, 
                                 topic_terms, 
                                 by = join_by(topic_name == topic))
      #view(output)
      #output <- dplyr::left_join(output, topic_terms, by = join_by(topic))
      
      output <- output %>%
        dplyr::select(
          topic_name,
          p.value,
          adjusted_p.value,
          cohen_d,
          top_terms,
          label_1,
          #label.label_2,
          prevalence,
          coherence,
          mean_group_1,
          mean_group_2,
          dplyr::everything()  # this will include the rest of the columns in their original order
        )
      output <- sort_stats_tibble(output)
      return(output)
    })
    names(output_list) <- names(result)
    
    return (output_list)
  }
  if (test_method == "linear_regression" | test_method == "logistic_regression"){
    # still get number of topics automatically
    
    num_topics <- sum(grepl("t_", names(topics_loadings)))
    lda_topics <- character(num_topics)
    
    # Create the list of LDA topics
    for (i in 1:num_topics) {
      lda_topics[i] <- paste("t_", i, sep = "")
    }
    
    preds <- topics_loadings
    
    #preds <- topics_loadings # load topics_loading into different variable to reduce naming errors
    for (topic in lda_topics) {
      #view(preds[[topic]])
      mean_value <- mean(preds[[topic]])
      std_dev <- stats::sd(preds[[topic]])
      preds[[paste0("z_",topic)]] <- (preds[[topic]] - mean_value) / std_dev#preds[[topic]] # scale(preds[[topic]])
    }
    
    control_variables <- controls
    for (variable in control_variables){
      preds[[paste0("z_",variable)]] <- scale(preds[[variable]])
    }
    
    # Initialize an empty list to store the topic names
    z_lda_topics <- character(num_topics)
    for (i in 1:num_topics) {
      z_lda_topics[i] <- paste0("z_t_", i)
    }
    # Loop through each LDA topic and create a linear model
    multi_models <- list()
    
    preds[is.na(preds)] <- 0
    
    if (test_method == "linear_regression"){
      
      formula_tail <- "~"
      for (variable in control_variables){
        formula_tail <- paste0(formula_tail, " + z_", variable)
      }
      
      
      for (topic in z_lda_topics) {
        formula <- stats::as.formula(paste0(topic, formula_tail))
        multi_models[[paste0("t_",topic)]] <- stats::lm(formula, data = preds)
      }
    } 
    
    if (test_method == "logistic_regression"){
      for (topic in z_lda_topics){
        
        multi_models[[paste0("t_", topic)]] <- stats::glm(paste0("z_",control_variables[1], " ~ ", topic), data = preds)
      }
    }
    
    
    control_variable_summary <- list()
    topics <- c()
    if (test_method=="linear_regression"){
      for (variable in control_variables){
        control_variable_summary[[variable]] <- list()
        control_variable_summary[[variable]][["estimate"]] <- c()
        control_variable_summary[[variable]][["t"]] <- c()
        control_variable_summary[[variable]][["p"]] <- c()
        control_variable_summary[[variable]][["p_adjusted"]] <- c()
      }
    }
    if (test_method=="logistic_regression"){
      control_variable_summary[["estimate"]] <- c()
      control_variable_summary[["t"]] <- c()
      control_variable_summary[["p"]] <- c()
      control_variable_summary[["p_adjusted"]] <- c()
    }
    
    for (i in 1:length(multi_models)){
      temp <- multi_models[[i]]
      p_values <- summary(temp)$coefficients[, "Pr(>|t|)"]
      t_values <- summary(temp)$coefficients[, "t value"]
      estimate_values <- summary(temp)$coefficients[, "Estimate"]
      topics <- c(topics, paste0("t",i))
      if (test_method == "linear_regression"){
        for (variable in control_variables){
          control_variable_summary[[variable]][["estimate"]] <- c(control_variable_summary[[variable]][["estimate"]],
                                                                  estimate_values[[paste0("z_", variable)]])
          control_variable_summary[[variable]][["t"]] <- c(control_variable_summary[[variable]][["t"]],
                                                           t_values[[paste0("z_",variable)]])
          control_variable_summary[[variable]][["p"]] <- c(control_variable_summary[[variable]][["p"]],
                                                           p_values[[paste0("z_", variable)]])
        }
      }
      if (test_method == "logistic_regression"){
        control_variable_summary[["estimate"]] <- c(control_variable_summary[["estimate"]],
                                                    estimate_values[[paste0("z_t_",i )]])
        control_variable_summary[["t"]] <- c(control_variable_summary[["t"]],
                                             t_values[[paste0("z_t_",i)]])
        control_variable_summary[["p"]] <- c(control_variable_summary[["p"]],
                                             p_values[[paste0("z_t_",i )]])
      }
      
    }
    
    if (test_method == "linear_regression"){
      for (variable in control_variables){
        p_adjusted <- stats::p.adjust(control_variable_summary[[variable]][["p"]],
                                      multiple_comparison,
                                      length(multi_models))
        control_variable_summary[[variable]][[paste0("p_adjusted")]] <- c(control_variable_summary[[variable]][["p_adjusted"]],
                                                                          p_adjusted)
      }
    }
    if (test_method == "logistic_regression"){
      p_adjusted <- stats::p.adjust(control_variable_summary[["p"]],
                                    multiple_comparison,
                                    length(multi_models))
      control_variable_summary[[paste0("p_adjusted")]] <- c(control_variable_summary[["p_adjusted"]],
                                                            p_adjusted)
    }
    
    #return (control_variable_summary)
    control_variable_summary$topic <- lda_topics
    
    # since ngram does not have coherence and prevalence
    output <- dplyr::right_join(topic_terms[c("topic", "top_terms", 
                                              "prevalence", "coherence")], #, "prevalence", "coherence"
                                data.frame(control_variable_summary), 
                                by = join_by(topic))
    
    # add the adjustment for bonferroni
    return(output)
    
  }
  
  if (test_method == "ridge_regression"){
    num_topics <- nrow(topic_terms)
    preds <- topics_loadings
    
    # rename topic columns
    for (i in 1:num_topics) {
      old_column_name <- paste0("t_", i)
      new_column_name <- paste0("Dim", i, "_texts")
      
      if (old_column_name %in% colnames(preds)) {
        colnames(preds)[colnames(preds) == old_column_name] <- new_column_name
      }
    }
    
    dims <- as.data.frame(preds) %>% dplyr::select(
      dplyr::contains("Dim"))
    
    #dims <- step_zv(dims)
    dims <- tibble::as_tibble(dims)
    preds <- tibble::as_tibble(preds)
    for (col in colnames(dims)) {
      dims[[col]] <- as.numeric(dims[[col]])
    }
    
    if (requireNamespace("text", quietly = TRUE)) {
      trained_model <- text::textTrainRegression(
        x = dims,
        y = grouping_variable,
        multi_cores = FALSE # This is FALSE due to CRAN testing and Windows machines.
      )
    }
    
    return (trained_model$results)
    
  }
  
}





#' The function to test the LDA model
#' @param model (list) The trained model
#' @param data (tibble) The data to test on
#' @param preds (tibble) The predictions
#' @param pred_var (string) The variable to be predicted (only needed for regression or correlation)
# @param group_var (string) The variable to group by (only needed for t-test)
#' @param controls (vector) The control variables
#' @param test_method (string) The test method to use, either "correlation","t-test", 
#' "linear_regression","logistic_regression", or "ridge_regression"
#' @param p_adjust_method (character) Method to adjust/correct p-values for multiple comparisons
#' (default = "none"; see also "holm", "hochberg", "hommel", "bonferroni", "BH", "BY",  "fdr").
#' @param seed (integer) The seed to set for reproducibility
#' @param load_dir (string) The directory to load the test from, if NULL, the test will not be loaded
#' @param save_dir (string) The directory to save the test, if NULL, the test will not be saved
#' @return A list of the test results, test method, and prediction variable
#' @importFrom dplyr bind_cols
#' @importFrom readr write_csv
#' @noRd
topicsTest1 <- function(
    model,
    preds,
    data,
    pred_var = NULL, # for all test types except t-test
    #    group_var = NULL, # only one in the case of t-test
    controls = c(),
    test_method = "linear_regression",
    p_adjust_method = "fdr",
    seed = 42,
    load_dir = NULL,
    save_dir){
  
  group_var = NULL
  
  if (!is.null(load_dir)){
    test_path <- paste0(load_dir, "/seed_", seed, "/test.rds")
    if (!file.exists(test_path)) {
      msg <- paste0("Test file not found at: ", test_path, ". Exiting function.")
      message(colourise(msg, "brown"))
      return(NULL)
    }
    test <- readRDS(test_path)
  } else {
    #rm(controls)
    controls <- c(pred_var, controls)
    
    
    #  if (!is.null(group_var)){
    #    if (!(group_var %in% names(preds))){
    #      preds <- dplyr::bind_cols(data[group_var], preds)
    #    }
    #  }
    for (control_var in controls){
      if (!(control_var %in% names(preds))){
        preds <- dplyr::bind_cols(data[control_var], preds)
      }
    }
    
    #    if (test_method == "ridge_regression"){
    #      group_var <- pred_var
    #    }
    
    # I think this can be removed. (i.e., its already a tibble)
    preds <- preds %>% tibble::tibble()
    
    test <- topic_test(
      topic_terms = model$summary,
      topics_loadings = preds,
      grouping_variable = preds[group_var],
      controls = controls,
      test_method = test_method,
      split = "median",
      n_min_max = 20,
      multiple_comparison = p_adjust_method)
  }
  
  if (!is.null(save_dir)){
    if (!dir.exists(save_dir)) {
      # Create the directory
      dir.create(save_dir)
      
      msg <- "Directory created successfully.\n"
      message(
        colourise(msg, "green"))
      
    } else {
      
      msg <- "Directory already exists.\n"
      message(
        colourise(msg, "green"))
    }
    
    if(!dir.exists(paste0(save_dir, "/seed_", seed))){
      dir.create(paste0(save_dir, "/seed_", seed))
    }
    
    if (test_method == "ridge_regression"){
      df <- list(#variable = group_var,
        estimate = test$estimate,
        t_value = test$statistic,
        p_value = test$p.value)
      readr::write_csv(data.frame(df), paste0(save_dir, 
                                              "/seed_", 
                                              seed, 
                                              "/textTrain_regression.csv"))
    }
    
    saveRDS(test, paste0(save_dir, 
                         "/seed_", 
                         seed, 
                         "/test_",
                         test_method, 
                         "_", pred_var,".rds"))
    
    msg <- paste0("The test object of ", 
                  pred_var, 
                  " was saved in: ", 
                  save_dir,"/seed_", 
                  seed, "/test_",
                  test_method, "_", 
                  pred_var,".rds")
    
    message(colourise(msg, "green"))
  }
  
  return(list(test = test, 
              test_method = test_method, 
              pred_var = pred_var))
}


#' Test topics or n-grams
#' 
#' Statistically test topics or n-grams in relation to one or two other variables using 
#' regression or t-test.  
#' @param model (list) A trained model LDA-model from the topicsModel() function.
#' @param data (tibble) The data containing the variables to be tested.
#' @param preds (tibble) The predictions from the topicsPred() function.
#' @param ngrams (list) output of the n-gram function
#' @param x_variable (string) The x variable name to be predicted, and to be plotted (only needed for regression or correlation)
#' @param y_variable (string) The y variable name to be predicted, and to be plotted (only needed for regression or correlation)
# @param group_var (string) The variable to group by (only needed for t-test)
#' @param controls (vector) The control variables (not supported yet)
#' @param test_method (string) The test method to use, either "correlation","t-test", "linear_regression","logistic_regression", or "ridge_regression"
# @param p_alpha (numeric) Threshold of p value set by the user for visualising significant topics 
#' @param p_adjust_method (character) Method to adjust/correct p-values for multiple comparisons
#' (default = "none"; see also "holm", "hochberg", "hommel", "bonferroni", "BH", "BY",  "fdr").
#' @param seed (integer) The seed to set for reproducibility
#' @param load_dir (string) The directory to load the test from, if NULL, the test will not be loaded
#' @param save_dir (string) The directory to save the test, if NULL, the test will not be saved
#' @return A list of the test results, test method, and prediction variable
#' @examples
#' \donttest{
#' # Test the topic document distribution in respect to a variable
#' save_dir_temp <- tempfile()
#' 
#' dtm <- topicsDtm(
#'   data = dep_wor_data$Depphrase, 
#'   save_dir =  save_dir_temp)
#' 
#' model <- topicsModel(
#'   dtm = dtm, # output of topicsDtm()
#'   num_topics = 20,
#'   num_top_words = 10,
#'   num_iterations = 1000,
#'   seed = 42,
#'   save_dir = save_dir_temp)
#'                      
#' preds <- topicsPreds(
#'  model = model, # output of topicsModel()
#'  data = dep_wor_data$Depphrase,
#'  save_dir = save_dir_temp)
#'                      
#' test <- topicsTest(
#'   model = model, # output of topicsModel()
#'   data=dep_wor_data,
#'   preds = preds, # output of topicsPreds()
#'   test_method = "linear_regression",
#'   x_variable = "Age",
#'   save_dir = save_dir_temp)
#' }                 
#' @importFrom dplyr bind_cols
#' @importFrom readr write_csv
#' @export
topicsTest <- function(
    data,
    model = NULL,
    preds = NULL,
    ngrams = NULL,
    x_variable = NULL, # for all test types except t-test
    y_variable = NULL,
    controls = c(),
    test_method = "linear_regression",
    p_adjust_method = "fdr",
    seed = 42,
    load_dir = NULL,
    save_dir){
  
  # group_var = NULL
  
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
  
  if (is.null(x_variable) & is.null(y_variable) && test_method != "t-test") {
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
  
  #### Load test ####
  if (!is.null(load_dir)){
    test <- topicsTest1(load_dir = load_dir)
  }
  
  #### N-grams testing (rearranging the data so that it fits the topics pipeline) ####
  if (!is.null(ngrams)){
    
    freq_per_user <- tibble(ngrams$freq_per_user[,2:ncol(ngrams$freq_per_user)])
    ngrams <- ngrams$ngrams
    colnames(freq_per_user) <- paste0("t_", 1:ncol(freq_per_user))
    preds <- freq_per_user
    
    model$summary <- list(topic = paste0("t_", 1:ncol(freq_per_user)),
                          top_terms = ngrams$ngrams, 
                          prevalence = ngrams$prevalence, 
                          coherence = ngrams$coherence, 
                          pmi = ngrams$pmi)
    
    model$summary <- data.frame(model$summary)
  }
  
  
  #### Testing the elements (i.e., ngrams or topics) ####
  pred_vars_all <- c(x_variable, y_variable)
  
  # TBD: Change the column of pred_var into the numeric variable.
  # for (pred_var in pred_vars_all){
  #   if (any(grepl(pred_var, colnames(preds)))){
  #     if (!is.numeric(preds[[pred_var]])){
  #       msg <- paste0("Change the variable ", pred_var, ' into a numeric variable in the `pred` object.')
  #        message(colourise(msg, "brown"))
  #       return (NULL)
  #     }}
  #   if (any(grepl(pred_var, colnames(data)))){
  #     if (!is.numeric(data[[pred_var]])){
  #       msg <- paste0("Change the variable ", pred_var, ' into a numeric variable in the `data` object.')
  #        message(colourise(msg, "brown"))
  #       return (NULL)
  #     }}
  # }
  topic_loadings_all <- list()
  pre <- c('x','y')
  # i = 1
  for (i in 1:length(pred_vars_all)){
    
    topic_loading <- topicsTest1(
      model = model,
      preds = preds, 
      data = data,
      pred_var = pred_vars_all[i], # for all test types except t-test
      # group_var = group_var, # only one in the case of t-test
      controls = controls,
      test_method = test_method,
      p_adjust_method = p_adjust_method,
      seed = seed,
      load_dir = load_dir,
      save_dir = save_dir
    )
    
    # Sorting output when not using ridge regression
    if (test_method != "ridge_regression") {
      
      colnames(topic_loading$test) <- c("topic", "top_terms", "prevalence", "coherence",
                                        paste(pre[i], 
                                              colnames(topic_loading$test)[5:8], 
                                              sep = "."))
      
      topic_loadings_all[[i]] <- topic_loading
    }
  }
  
  if (!is.null(y_variable)){
    # create the x.y.word.category
    topic_loadings_all[[3]] <- list()
    topic_loadings_all[[3]]$test <- dplyr::left_join(topic_loadings_all[[1]][[1]][,1:8], 
                                                     topic_loadings_all[[2]][[1]][,1:8],
                                                     by = c("topic", "top_terms", 
                                                            "prevalence", "coherence"))
    
    topic_loadings_all[[3]]$test_method <- topic_loadings_all[[1]]$test_method
    topic_loadings_all[[3]]$pred_var <- paste0(topic_loadings_all[[1]]$pred_var, '__',
                                               topic_loadings_all[[2]]$pred_var) 
    
  } else {
    
    if (test_method == "linear_regression" | test_method == "logistic_regression"){
      
      msg <- "The parameter y_variable is not set! Output 1 dimensional results."
      message(colourise(msg, "blue"))
      
      topic_loadings_all[[2]] <- list()
      topic_loadings_all[[3]] <- list()
      topic_loadings_all[[3]]$test <- topic_loadings_all[[1]][[1]][,1:8]
      topic_loadings_all[[3]]$test_method <- topic_loadings_all[[1]]$test_method
      topic_loadings_all[[3]]$pred_var <- topic_loadings_all[[1]]$pred_var
      
    } else if (test_method == "ridge_regression"){
      topic_loadings_all[[1]] <- topic_loading
    }
  }
  
  # Not sure why we are keep all three lists - since the last list appear to have all the information.
  # so trying here to only keep the last, to see if anything else breaks. 
  topic_loadings_all <- topic_loadings_all[[length(topic_loadings_all)]]
  return(topic_loadings_all)
}