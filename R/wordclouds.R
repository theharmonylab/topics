#' A private function renames the columns of a model with the vocabulary
#' @param model (list) the model to be modified
#' @param col (string) the column to be modified
#' @param vocabulary (list) the vocabulary to be used
#' @return (list) the modified model
#' @noRd
name_cols_with_vocab <- function(
    model, 
    col, 
    vocabulary){
  
  colnames(model[[col]]) <- as.character(unlist(vocabulary))
  return(model)
}

#' This is a private function
#' @param df_list (list) a list of data frames with each topic
#' @param phi (data.frame) data frame with topic word scores
#' @param model_type (string) "mallet"
#' @return list of data.frames with assigned phi
#' @noRd
assign_phi_to_words <- function(
    df_list, 
    phi, 
    model_type){
  
  
  for (i in 1:length(df_list)){
    df <- data.frame(df_list[[i]]) 
    colnames(df)[1] <- "Word"
    phi_vector <- c()
    for (j in 1:nrow(df)){
      word <- df[j,]
      if (model_type=="mallet"){
        phi_vector <- c(phi_vector,phi[i,][word])
      } else {
        phi_vector <- c(phi_vector,phi[paste0("t_",i),][word])
      }
    }
    df$phi <- phi_vector
    df_list[[i]] <- df
  }
  return(df_list)
}

#
#' Create list of data.frames with topics most frequent words and topic term scores
#' @param save_dir (string) directory where to get data from
#' @param num_topics (int) the number of topics
#' @return list of data.frames
#' @importFrom utils read.csv
#' @noRd
create_df_list_bert_topics <- function(
    save_dir,
    seed,
    num_topics) {
  
  df_list <- list()
  
  for (i in 1:num_topics) {
    
    df_list[[i]] <- utils::read.csv(paste0(save_dir, "/seed_", seed, "/df_list_term_phi/", i, "_top_words.csv"))
  }
  return(df_list)
}





#' This is a private function 
#' @param summary (data.frame) the models summary
#' @return a list of dataframes for each topic filled with top terms
#' @importFrom stats complete.cases
#' @noRd
create_topic_words_dfs <- function(
    summary){
  
  n <- nrow(summary)
  df_list <- vector("list", n)
  
  # Create and name the dataframes in a loop
  for (i in 1:n) {
    word_vector <- unlist(strsplit(summary[paste0("t_",i),]$top_terms, ", "))
    df <- data.frame(Word = word_vector) # Create an empty dataframe
    df <- df_cleaned <- df[stats::complete.cases(df), ]
    df_list[[i]] <- df  # Add the dataframe to the list
    
    name <- paste("t", i, sep = "_")  # Create the name for the dataframe
    assign(name, df_list[[i]])  # Assign the dataframe to a variable with the specified name
  }
  return(df_list)
}

#' This is a private function
#' @param df_list (list) list of data.frames with topics most frequent words and assigned topic term scores
#' @param test (data.frame) the test returned from textTopicTest()
#' @param test_type (string) "linear_regression", or "binary_regression"
#' @param cor_var (string) Variable for t-test, linear, binary or ridge regression
#' @param color_negative_cor (function) color of topic cloud with negative correlation
#' @param color_positive_cor (function) color of topic cloud with positive correlation
#' @param grid_pos (numeric) position of grid plots
#' @param scale_size (bool) if True, then the size of the topic cloud is scaled by the prevalence of the topic
#' @param plot_topics_idx (list) if specified, then only the specified topics are plotted
#' @param p_threshold (float) set threshold which determines which topics are plotted
#' @param save_dir (string) save plots in specified directory, if left blank, plots is not saved,
#' thus save_dir is necessary.
#' @param figure_format (string) Set the figure format, e.g., .svg, or .png.
#' @param seed (int) seed is needed for saving the plots in the correct directory
#' @importFrom ggwordcloud geom_text_wordcloud
#' @importFrom ggplot2 ggsave labs scale_size_area theme_minimal ggplot aes scale_color_gradient
#' @importFrom dplyr rename
#' @noRd
create_plots <- function(
    df_list = NULL,
    summary = NULL,
    ngrams = NULL,
    test = NULL,
    test_type = NULL,
    cor_var = NULL,
    color_negative_cor = NULL,
    color_positive_cor = NULL,
    grid_pos = "",
    scale_size = FALSE,
    plot_topics_idx = NULL,
    p_threshold = NULL,
    save_dir,
    figure_format = "svg",
    width = 10,
    height = 8,
    max_size = 10,
    seed = 42){
  
  # this code plots the wordclouds in respect to the statistical test
  # For test, topics list and summary
  if(!is.null(test) & !is.null(df_list) & !is.null(summary)){
  
    if (is.null(plot_topics_idx)){
      grid <- ""
      plot_topics_idx <- seq(1, length(df_list))
    }else{
      grid <- paste0("grid_pos_", grid_pos, "_")
      pred_var_x <- strsplit(cor_var, "_")[[1]][1]
      if (length(strsplit(cor_var, "_")[[1]]) > 1){
        pred_var_y <- strsplit(cor_var, "_")[[1]][2]
      }
    }
    for (i in paste0('t_', plot_topics_idx)){ 
      #for (i in 1:length(df_list)){
      #view(df_list[[i]])
      if (test_type == "linear_regression"){
        if (grid == ""){
          colNo.estimate <- grep(paste0(cor_var, '.estimate'),colnames(test))
          colNo.p_adjusted <- grep(paste0(cor_var, '.p_adjusted'),colnames(test))
          estimate_col <- colnames(test)[colNo.estimate] 
          p_adjusted_col <- colnames(test)[colNo.p_adjusted]
          #estimate_col <- paste0(cor_var,".estimate") # grep(partial_name, data_frame_names, value = TRUE)
          #p_adjusted_col <- paste0(cor_var,".p_adjusted")
        }else{
          colNo.estimate.x <- grep(paste0(pred_var_x, '.estimate'),colnames(test))
          colNo.p_adjusted.x <- grep(paste0(pred_var_x, '.p_adjusted'),colnames(test))
          estimate_col_x <- colnames(test)[colNo.estimate.x] 
          p_adjusted_col_x <- colnames(test)[colNo.p_adjusted.x]
          if (length(strsplit(cor_var, "_")[[1]]) > 1){
            colNo.estimate.y <- grep(paste0(pred_var_y, '.estimate'),colnames(test))
            colNo.p_adjusted.y <- grep(paste0(pred_var_y, '.p_adjusted'),colnames(test))
            estimate_col_y <- colnames(test)[colNo.estimate.y] 
            p_adjusted_col_y <- colnames(test)[colNo.p_adjusted.y]
          }}
      } else if (test_type == "t-test"){
        estimate_col <- "cohens d" # probably doesn't work yet
        
      } else if (test_type == "logistic_regression"){
        estimate_col <- "estimate"
        p_adjusted_col <- "p_adjusted"
        
      }
      if (grid == ""){
        estimate <- dplyr::filter(tibble::as_tibble(test,.name_repair='minimal'), topic==i)[[estimate_col]] # $PHQtot.estimate
        p_adjusted <- dplyr::filter(tibble::as_tibble(test,.name_repair='minimal'), topic==i)[[p_adjusted_col]] # $PHQtot.p_adjustedfdr
      }else{
        estimate_x <- dplyr::filter(tibble::as_tibble(test,.name_repair='minimal'), topic==i)[[estimate_col_x]]
        p_adjusted_x <- dplyr::filter(tibble::as_tibble(test,.name_repair='minimal'), topic==i)[[p_adjusted_col_x]]
        estimate <- estimate_x
        if (length(strsplit(cor_var, "_")[[1]]) > 1){
          estimate_y <- dplyr::filter(tibble::as_tibble(test,.name_repair='minimal'), topic==i)[[estimate_col_y]]
          p_adjusted_y <- dplyr::filter(tibble::as_tibble(test,.name_repair='minimal'), topic==i)[[p_adjusted_col_y]]
          p_adjusted <- min(p_adjusted_x, p_adjusted_y)
        }else{
          estimate <- estimate_x
          p_adjusted <- p_adjusted_x
        }
      }
      
  
      if (scale_size==TRUE){
        prevalence <- summary[paste0("t_",i),]$prevalence
      }
      
      
      # this will ensure that all topics are plotted
      if (is.null(p_threshold) ){
        if (grid == ""){
          p_threshold <- p_adjusted +1 
        }
      }
      
      #
      if (!is.nan(p_adjusted) & p_adjusted < p_threshold){
        
        
        #estimate <- test[i,][[grep(estimate_col, colnames(test), value=TRUE)]]# $PHQtot.estimate
        #p_adjusted <- test[i,][[grep("p_adjusted", colnames(test), value=TRUE)]] # $PHQtot.p_adjustedfdr
        if (estimate < 0){
          color_scheme <- color_negative_cor # scale_color_gradient(low = "darkgreen", high = "green")
        } else {
          color_scheme <- color_positive_cor # scale_color_gradient(low = "darkred", high = "red")
        }
        if (scale_size == TRUE){
          max_size <- max_size*log(prevalence)
          y <- paste0("P = ", prevalence)
        } else {
          max_size <- max_size
          y <- ""
        }

        if (grid == ""){ .
          plot <- ggplot2::ggplot(df_list[[as.numeric(sub(".*_", "", i))]], 
                                  ggplot2::aes(label = Word, 
                                               size = phi, 
                                               color = phi)) + #,x=estimate)) +
            ggwordcloud::geom_text_wordcloud() +
            ggplot2::scale_size_area(max_size = max_size) +
            ggplot2::theme_minimal() +
            #theme(plot.margin = margin(0,0,0,0, "cm")) +
            color_scheme + 
            ggplot2::labs(x = paste0("r = ", estimate),
                          y= y)
        }else{
          if (length(strsplit(cor_var, "_")[[1]]) > 1){
            x_message = paste0("r_x = ", round(estimate_x,4), "\n",
                               "r_y = ", round(estimate_y,4))
          }else{
            x_message = paste0("r_x = ", round(estimate_x,4))                    
          }
          plot <- ggplot2::ggplot(df_list[[as.numeric(sub(".*_", "", i))]], 
                                  ggplot2::aes(label = Word, 
                                               size = phi, 
                                               color = phi)) + #,x=estimate)) +
            ggwordcloud::geom_text_wordcloud() +
            ggplot2::scale_size_area(max_size = max_size) +
            ggplot2::theme_minimal() +
            #theme(plot.margin = margin(0,0,0,0, "cm")) +
            color_scheme + 
            ggplot2::labs(x = x_message,
                          y= y)
        }
        
        
        if (!dir.exists(save_dir)) {
          # Create the directory
          dir.create(save_dir)
          msg <- "Directory created successfully.\n"
          message(colourise(msg, "green"))
        } 
        if(!dir.exists(paste0(save_dir, "/seed_", seed, "/wordclouds"))){
          dir.create(paste0(save_dir, "/seed_", seed, "/wordclouds"))
        }
        p_adjusted <- sprintf("%.2e", p_adjusted)
        if (grid == ""){
          ggplot2::ggsave(paste0(save_dir,"/seed_", seed, 
                                 "/wordclouds/",
                                 grid,
                                 "corvar_", cor_var,"_",
                                 i, "_r_", 
                                 estimate, "_p_", 
                                 p_adjusted,
                                 ".",
                                 figure_format),
                          plot = plot, 
                          width = width, 
                          height = height, 
                          units = "in", 
                          create.dir = TRUE)
        }else{
          if (length(strsplit(cor_var, "_")[[1]]) > 1){
            p_adjusted_x <- sprintf("%.2e", p_adjusted_x)
            p_adjusted_y <- sprintf("%.2e", p_adjusted_y)
            fileMsg <- paste0(
              "_rx_", estimate_x,
              "_ry_", estimate_y, 
              "_px_", p_adjusted_x,
              "_py_", p_adjusted_y, ".",
              figure_format
            )
          }else{
            p_adjusted_x <- sprintf("%.2e", p_adjusted_x)
            fileMsg <- paste0(
              "_rx_", estimate_x, 
              "_px_", p_adjusted_x, ".",
              figure_format
            )
          }
          ggplot2::ggsave(paste0(save_dir,"/seed_", seed, 
                                 "/wordclouds/",
                                 grid,
                                 "corvar_", cor_var,"_",
                                 i, fileMsg),
                          plot = plot, 
                          width = width, 
                          height = height, 
                          units = "in", 
                          create.dir = TRUE)
        }
      }
    }
  }
  
  
  # For topic df_list, summary but no test
  if (!is.null(df_list) & !is.null(summary) & is.null(test)){
    if (is.null(plot_topics_idx)){
      plot_topics_idx <- seq(1, length(df_list))
    }
    if (scale_size==TRUE){
      prevalence <- summary[paste0("t_",i),]$prevalence
      max_size <- max_size*log(prevalence)
    } else {
      max_size <- max_size
    }
    if (!dir.exists(save_dir)) {
      # Create the directory
      dir.create(save_dir)
      msg <- "Directory created successfully.\n"
      message(colourise(msg, "green"))
    } 
    if(!dir.exists(paste0(save_dir, "/seed_", seed, "/wordclouds"))){
      dir.create(paste0(save_dir, "/seed_", seed, "/wordclouds"))
    }
    for (i in paste0('t_', plot_topics_idx)){
      
      plot <- ggplot2::ggplot(
        df_list[[as.numeric(sub(".*_", "", i))]],
        ggplot2::aes(label = Word,
                     size = phi,
                     color = phi)) +
        ggwordcloud::geom_text_wordcloud() +
        ggplot2::scale_size_area(max_size = max_size) +
        ggplot2::theme_minimal() +
        color_negative_cor
              #ggplot2::scale_color_gradient(low = "purple", high = "blue") # These colour does not work
    
      ggplot2::ggsave(paste0(save_dir,"/seed_", seed, 
                             "/wordclouds/",
                             i, 
                             ".",
                             figure_format),
                      plot = plot, 
                      width = width, 
                      height = height, 
                      units = "in", 
                      create.dir = TRUE)
    }
  }
  
  # For N-grams with NO test
  if (is.null(df_list) & is.null(test) & !is.null(ngrams)){
      if (!dir.exists(save_dir)) {
        # Create the directory
        dir.create(save_dir)
        msg <- "Directory created successfully.\n"
        message(colourise(msg, "green"))
      } 
      if(!dir.exists(paste0(save_dir, "/seed_", seed, "/wordclouds"))){
        dir.create(paste0(save_dir, "/seed_", seed, "/wordclouds"))
      }
      plot <- ggplot2::ggplot(ngrams, 
                              ggplot2::aes(label = ngrams, 
                                           size = prop, 
                                           color = prop)) +
        ggwordcloud::geom_text_wordcloud() +
        ggplot2::theme_minimal() +
        color_positive_cor # ggplot2::scale_color_gradient(low = "yellow", high = "red")
      
      ggplot2::ggsave(paste0(save_dir,"/seed_", seed, 
                             "/wordclouds/ngrams", 
                             ".",
                             figure_format),
                      plot = plot, 
                      width = width, 
                      height = height, 
                      units = "in", 
                      create.dir = TRUE) 
    } 
  
  # For N-gram with test  
  if (is.null(df_list) & !is.null(test) & !is.null(ngrams)){
        
        ngrams <- ngrams %>% dplyr::rename(top_terms = ngrams)
        test <- test %>% dplyr::rename(estimate = contains("estimate"))
        test <- test %>% dplyr::rename(p_adjusted = contains("p_adjusted"))
        test <- test %>% dplyr::left_join(ngrams, by = "top_terms")
        if (!is.null(p_threshold)){
          test <- test %>% dplyr::filter(p_adjusted < p_threshold)
        }
        # test for the fact that all words could be insignificant
        if (nrow(test) == 0){
          msg <- "No significant terms.\n No wordclouds generated."
          message(colourise(msg, "blue"))
        } else {
          test_positive <- test%>% dplyr::filter(estimate > 0)
          test_negative <- test%>% dplyr::filter(estimate < 0)
          if (!dir.exists(save_dir)) {
            # Create the directory
            dir.create(save_dir)
            msg <- "Directory created successfully.\n"
            message(colourise(msg, "green"))
          } 
          if(!dir.exists(paste0(save_dir, "/seed_", seed, "/wordclouds"))){
            dir.create(paste0(save_dir, "/seed_", seed, "/wordclouds"))
          }
          # Word cloud with correlation strength mapped to color gradient
          plot <- ggplot2::ggplot(test_positive, aes(label = top_terms, size = estimate, color = prop)) +
            ggwordcloud::geom_text_wordcloud() +
            scale_size_area(max_size = max_size) +  # Adjust max size
            #scale_color_gradient(low = "grey", high = "red") +  # Blue for low, red for high correlation strength
            theme_minimal() +
            color_positive_cor
          
          ggplot2::ggsave(paste0(save_dir,"/seed_", seed, 
                                 "/wordclouds/ngrams_positive", 
                                 ".",
                                 figure_format),
                          plot = plot, 
                          width = width, 
                          height = height, 
                          units = "in", 
                          create.dir = TRUE) 
          # Word cloud with correlation strength mapped to color gradient
          plot <- ggplot2::ggplot(test_negative, aes(label = top_terms, size = estimate, color = prop)) +
            ggwordcloud::geom_text_wordcloud() +
            scale_size_area(max_size = max_size) +  # Adjust max size
            #scale_color_gradient(low = "grey", high = "red") +  # Blue for low, red for high correlation strength
            theme_minimal() +
            color_negative_cor
          
          ggplot2::ggsave(paste0(save_dir,"/seed_", seed, 
                                 "/wordclouds/ngrams_negative", 
                                 ".",
                                 figure_format),
                          plot = plot, 
                          width = width, 
                          height = height, 
                          units = "in", 
                          create.dir = TRUE) 
        }
    }
}
