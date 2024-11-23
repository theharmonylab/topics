

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
    control_vars,
    test_method = "correlation",
    split = "median",
    n_min_max = 20,
    multiple_comparison = "bonferroni"){

  colnames(grouping_variable) <- "value"
  topics_groupings <- dplyr::bind_cols(topics_loadings,
                                       grouping_variable)
  
  topics_loadings <- topics_loadings[stats::complete.cases(topics_groupings), ]
  grouping_variable <- grouping_variable[stats::complete.cases(topics_groupings), ]
  
  
  if (TRUE){
    # format checker
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
  
  
  if (FALSE){
    model1$prevalence <- colSums(model1$theta) / sum(model1$theta) * 100
    model1$top_terms <- textmineR::GetTopTerms(phi = model1$phi, M = 5)
    model1$summary <- data.frame(topic = rownames(model1$phi),
                                 label = model1$labels,
                                 coherence = round(model1$coherence, 3),
                                 prevalence = round(model1$prevalence,3),
                                 top_terms = apply(model1$top_terms, 2, function(x){
                                   paste(x, collapse = ", ")
                                 }),
                                 stringsAsFactors = FALSE)
  }
  

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
    
    control_variables <- control_vars
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
    output <- dplyr::right_join(topic_terms[c("topic", "top_terms")], 
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



#' get_mallet_model
#' @param dtm (R_obj) A document-term matrix
#' @param num_topics (integer) The number of topics
#' @param num_top_words (string) The number of words
#' @param num_iterations (string) Number of iterations
#' @param seed Set see with L, like 42L.
#' @return Mallet model
#' @importFrom textmineR Dtm2Docs CalcGamma
#' @importFrom stats setNames
#' @importFrom mallet mallet.top.words mallet.doc.topics mallet.word.freqs mallet.topic.labels MalletLDA  mallet.import  mallet.topic.words
#' @noRd
get_mallet_model <- function(
    dtm,
    num_topics = 20,
    num_top_words = 10, 
    num_iterations = 1000, 
    seed){
  
  docs <- textmineR::Dtm2Docs(dtm)
  
  model <- mallet::MalletLDA(
    num.topics = num_topics,
    alpha.sum = 5,
    beta = 0.01)
  
  instances <- mallet::mallet.import(
    as.character(seq_along(docs)),
    docs,
    preserve.case = FALSE,
    token.regexp = "[\\p{L}\\p{N}_]+|[\\p{P}]+\ ")
  
  # set seed (it does not make the entire ldaModel() output 
  # https://stackoverflow.com/questions/37356001/using-a-random-seed-in-rmallet
  # object setqual TRUE, but many more objectis within the object becomes equal)
  model$setRandomSeed(as.integer(seed))
  model$loadDocuments(instances)

  model$train(num_iterations)
 
  topic.words <- mallet::mallet.topic.words(
    model,
    smoothed = TRUE,
    normalized = TRUE)
  
  df_top_terms <- data.frame(matrix(NA, 
                                    nrow = num_top_words, 
                                    ncol = num_topics))
  
  colnames(df_top_terms) <- paste0("t_", 
                                   1:num_topics)
  
  for(i_topic in 1:num_topics){
    top_terms <- mallet::mallet.top.words(
      model,
      word.weights = topic.words[i_topic,],
      num.top.words = num_top_words)
    
    #top_terms <- paste(top_terms$term, collapse=" ")
    #topic <- paste("t_", i_topic, sep="")
    #df <- tibble(topic, top_terms)
    #list_top_terms[[i_topic]] <- df
    top_terms <- top_terms$term
    df_top_terms[paste0("t_", i_topic)] <- top_terms
  }
  
  return_model <- list()
  #return_model$top_terms_mallet <- bind_rows(list_top_terms)
  return_model$instances <- instances #model$instances()
  return_model$inferencer <- model$getInferencer()
  #view(return_model$instances)
  return_model$top_terms_mallet <- df_top_terms
  return_model$top_terms <- return_model$top_terms_mallet
  #return_model$phi <- mallet.topic.w
  return_model$phi <- mallet::mallet.topic.words(model, 
                                                 smoothed=TRUE, 
                                                 normalized=TRUE)
  
  return_model$topic_docs <- mallet::mallet.doc.topics(model, 
                                                       smoothed=TRUE,
                                                       normalized=TRUE)
  #return_model$top_terms <- GetTopTerms(phi = return_model$phi, M = num_top_words)
  return_model$frequencies <- mallet::mallet.word.freqs(model)
  return_model$vocabulary <- model$getVocabulary()
  
  model$prevalence_mallet <- colSums(mallet::mallet.doc.topics(
    model,
    smoothed=TRUE,
    normalized=TRUE)) /
    sum(mallet::mallet.doc.topics(
      model,
      smoothed=TRUE,
      normalized=TRUE)) * 100
  
  #sum(list_top_terms$prevalence)
  return_model$labels <- mallet::mallet.topic.labels(model)
  return_model$theta <- mallet::mallet.doc.topics(
    model,
    smoothed = TRUE,
    normalized = TRUE)
  
  return_model$prevalence <- colSums(return_model$theta) / 
    sum(return_model$theta) * 100
  
  keys <- paste0("t_", 1:num_topics)
  
  return_model$prevalence <- stats::setNames(
    return_model$prevalence, 
    keys)
  
  #return_model$labels <- LabelTopics(assignments = return_model$theta > 0.05, dtm = dtm, M = 1)
  
  return_model$coherence <- return_model$prevalence
  
  # put theta into the right format
  df_theta <- data.frame(return_model$theta)
  df_theta <- stats::setNames(
    df_theta,
    keys)
  
  return_model$theta <- df_theta
  
  # take the first word as dummy label, no other solution worked
  first_row <- return_model$top_terms_mallet[1,]
  return_model$labels <- matrix(first_row, 
                                nrow = num_topics, 
                                ncol = 1)
  
  rownames(return_model$labels) <- paste0("t_", 
                                          1:num_topics)
  
  colnames(return_model$labels) <- "label_1"
  #return(c(result1 = result1, result2 = result2))
  
  pred_model <- list()
  pred_model$phi <- mallet::mallet.topic.words(
    model,
    smoothed = TRUE,
    normalized = TRUE)
  
  colnames(pred_model$phi) <- as.character(unlist(
    return_model$vocabulary))
  
  pred_model$theta <- mallet::mallet.doc.topics(
    model, 
    smoothed = TRUE, 
    normalized = TRUE)
  
  k <- ncol(pred_model$theta) # Specify the value of k  
  new_col_names <- paste("t", 
                         1:k, 
                         sep = "_") # Generate new column names
  
  colnames(pred_model$theta) <- new_col_names # Assign new column names to the dataframe
  pred_model$alpha <- model$alpha
  
  pred_model$gamma <- textmineR::CalcGamma(
    phi = pred_model$phi, 
    theta = pred_model$theta)
  
  pred_model$data <- dtm
  #names(pred_model)
  class(pred_model) <- "lda_topic_model"
  
  return_model$pred_model <- pred_model
  
  #return(list(pred_model = pred_model, return_model=return_model))
  return(return_model)
}

#' Get the most or least frequent terms in a document-term matrix
#' @param dtm (R_obj) A document-term matrix
#' @param n (integer) The number of terms to be removed
#' @param type (string) most or least frequent terms
#' @param mode (string) absolute or relative amount of terms to be removed
#' @return A list of terms to be removed
#' @noRd
get_removal_terms <- function(
    dtm, 
    n, 
    type, 
    mode="absolute"){
  
  term_frequencies <- colSums(as.matrix(dtm))
  df <- data.frame(as.matrix(dtm))
  
  # Create a data frame with term and frequency
  term_frequency_df <- data.frame(Term = colnames(dtm),
                                  Frequency = term_frequencies)
  
  # Sort the data frame in descending order of frequency
  term_frequency_df <- term_frequency_df[order(-term_frequency_df$Frequency), ]
  
  # Print the terms with the highest frequencies (e.g., top 10 terms)
  #top_terms <- head(term_frequency_df, n = 10)
  if (mode=="percentage"){
    removal_index <- nrow(term_frequency_df)*n 
    
  } else if (mode == "term"){
    removal_index <- n-1
    
  } 
  if (type=="most"){
    removal_index <- round(removal_index) 
    removal_words <- term_frequency_df[["Term"]][1:removal_index]
    
  } else {
    removal_index <- nrow(term_frequency_df) - round(removal_index) # calculates index of term that has highest freqency of all percent least frequent words
    removal_words <- term_frequency_df[["Term"]][nrow(term_frequency_df):removal_index]
  }
  
  return(removal_words)
}

#' Get the column indices of the most or least frequent terms in a document-term matrix
#' @param dtm (R_obj) A document-term matrix
#' @param n (integer) The number of terms to be removed
#' @param type (string) most or least frequent terms
#' @param mode (string) absolute or relative amount of terms to be removed
#' @return A list of column indices to be removed
#' @noRd
get_removal_columns <- function(
    dtm, 
    n, 
    type, 
    mode="absolute"){
  
  terms <- get_removal_terms(dtm, 
                             n, 
                             type, 
                             mode)
  
  filtered_terms_test <- c()
  
  for (term in terms){
    filtered_terms_test <- c(filtered_terms_test, 
                             term)
  }
  
  #filtered_terms_test <- c("sad", "tired", "happy", "low")
  column_indices_to_remove <- which(colnames(dtm) %in% filtered_terms_test)
  
  return(column_indices_to_remove)
}

#' Assgning numeric categories for further topic visualization colors.
#' @param topic_loadings_all (tibble) The tibble from topicsTest1
#' @param p_alpha (numeric) Threshold of p value set by the user for visualising significant topics 
#' @param dimNo (numeric) 1 dimension or 2 dimensions
#' @return A new tibble with the assigned numbers
#' @importFrom dplyr mutate
#' @noRd
topicsNumAssign_dim2 <- function(
    topic_loadings_all,
    p_alpha = 0.05, 
    dimNo = 2){
  
  if (dimNo == 1){
    num1 <- 1:3
    topic_loadings_all <- topic_loadings_all %>%
      dplyr::mutate(color_categories = dplyr::case_when(
        x_plotted < 0 & adjusted_p_values.x < p_alpha ~ num1[1],
        adjusted_p_values.x > p_alpha ~ num1[2],
        x_plotted > 0 & adjusted_p_values.x < p_alpha ~ num1[3]
      ))
  }else{
    num1 <- 1:9
    topic_loadings_all <- topic_loadings_all %>%
      dplyr::mutate(color_categories = dplyr::case_when(
        x_plotted < 0 & adjusted_p_values.x < p_alpha & y_plotted > 0 & adjusted_p_values.y < p_alpha ~ num1[1],
        adjusted_p_values.x > p_alpha & y_plotted > 0 & adjusted_p_values.y < p_alpha ~ num1[2],
        x_plotted > 0 & adjusted_p_values.x < p_alpha & y_plotted > 0 & adjusted_p_values.y < p_alpha ~ num1[3],
        x_plotted < 0 & adjusted_p_values.x < p_alpha & adjusted_p_values.y > p_alpha ~ num1[4],
        adjusted_p_values.x > p_alpha & adjusted_p_values.y > p_alpha ~ num1[5],
        x_plotted > 0 & adjusted_p_values.x < p_alpha & adjusted_p_values.y > p_alpha ~ num1[6],
        x_plotted < 0 & adjusted_p_values.x < p_alpha & y_plotted < 0 & adjusted_p_values.y < p_alpha ~ num1[7],
        adjusted_p_values.x > p_alpha & y_plotted < 0 & adjusted_p_values.y < p_alpha ~ num1[8],
        x_plotted > 0 & adjusted_p_values.x < p_alpha & y_plotted < 0 & adjusted_p_values.y < p_alpha ~ num1[9]
      ))
  }
  
  return (topic_loadings_all)
}
