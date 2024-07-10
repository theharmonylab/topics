

#' A private function used by ldaWordclouds to rename the columns of a model with the vocabulary
#' @param model (list) the model to be modified
#' @param col (string) the column to be modified
#' @param vocabulary (list) the vocabulary to be used
#' @return (list) the modified model
#' @noRd
name_cols_with_vocab <- function(model, 
                                 col, 
                                 vocabulary){
  colnames(model[[col]]) <- as.character(unlist(vocabulary))
  return(model)
}

#' This is a private function and used internally by ldaWordclouds
#' @param df_list (list) a list of data frames with each topic
#' @param phi (data.frame) data frame with topic word scores
#' @param model_type (string) "mallet"
#' @return list of data.frames with assigned phi
#' @noRd
assign_phi_to_words <- function(df_list, 
                                phi, 
                                model_type){
  #view(phi)
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

#' This is a private function and used internally by ldaWordclouds
#' @param summary (data.frame) the models summary
#' @return a list of dataframes for each topic filled with top terms
#' @importFrom stats complete.cases
#' @noRd
create_topic_words_dfs <- function(summary){
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


#' This is a private function and used internally by ldaWordclouds
#' @param df_list (list) list of data.frames with topics most frequent words and assigned topic term scores
#' @param test (data.frame) the test returned from textTopicTest()
#' @param test_type (string) "linear_regression", or "binary_regression"
#' @param cor_var (string) Variable for t-test, linear, binary or ridge regression
#' @param color_negative_cor (function) color of topic cloud with negative correlation
#' @param color_positive_cor (function) color of topic cloud with positive correlation
#' @param scale_size (bool) if True, then the size of the topic cloud is scaled by the prevalence of the topic
#' @param plot_topics_idx (list) if specified, then only the specified topics are plotted
#' @param p_threshold (float) set threshold which determines which topics are plotted
#' @param save_dir (string) save plots in specified directory, if left blank, plots is not saved,
#' thus save_dir is necessary
#' @param figure_format (string) Set the figure format, e.g., .svg, or .png.
#' @param width (integer) The width of the topic (units = "in"). 
#' @param height (integer) The width of the topic (units = "in"). 
#' @param max_size (integer) The max size of the words.
#' @param seed (int) seed is needed for saving the plots in the correct directory
#' @importFrom ggwordcloud geom_text_wordcloud
#' @importFrom ggplot2 ggsave labs scale_size_area theme_minimal ggplot aes scale_color_gradient
#' @noRd
create_plots <- function(df_list, 
                         summary,
                         test, 
                         test_type,
                         cor_var,
                         color_negative_cor,
                         color_positive_cor,
                         scale_size = FALSE,
                         plot_topics_idx = NULL,
                         p_threshold = NULL,
                         save_dir = "./results",
                         figure_format = "png",
                         width = 10, 
                         height = 8,
                         max_size = 10,
                         seed = 42){
  if (is.null(plot_topics_idx)){
    plot_topics_idx <- seq(1, length(df_list))
  } 
  for (i in plot_topics_idx){
    #for (i in 1:length(df_list)){
    #view(df_list[[i]])
    if (test_type == "linear_regression"){
      estimate_col <- paste0(cor_var,".estimate") # grep(partial_name, data_frame_names, value = TRUE)
      p_adjusted_col <- paste0(cor_var,".p_adjusted")
      
    } else if (test_type == "t-test"){
      estimate_col <- "cohens d" # probably doesn't work yet
      
    } else if (test_type == "logistic_regression"){
      estimate_col <- "estimate"
      p_adjusted_col <- "p_adjusted"
      
    }
    estimate <- test[i,][[estimate_col]]# $PHQtot.estimate
    p_adjusted <- test[i,][[p_adjusted_col]] # $PHQtot.p_adjustedfdr

    if (scale_size==TRUE){
      prevalence <- summary[paste0("t_",i),]$prevalence
    }
    #  print(paste0("prevalence: ", prevalence))
    
    
    # this will ensure that all topics are plotted
    if (is.null(p_threshold) ){
      p_threshold <- p_adjusted +1 
      
    }
    
    #print(is.null(p_threshold))
    if (!is.nan(p_adjusted) & p_adjusted < p_threshold){
      
      
      #estimate <- test[i,][[grep(estimate_col, colnames(test), value=TRUE)]]# $PHQtot.estimate
      #p_adjusted <- test[i,][[grep("p_adjusted", colnames(test), value=TRUE)]] # $PHQtot.p_adjustedfdr
      if (estimate < 0){
        color_scheme <- color_negative_cor # scale_color_gradient(low = "darkgreen", high = "green")
      } else {
        color_scheme <- color_positive_cor # scale_color_gradient(low = "darkred", high = "red")
      }
      if (scale_size == TRUE){
        max_size <- max_size * log(prevalence)
        y <- paste0("P = ", prevalence)
      } else {
        max_size <- max_size
        y <- ""
      }
      #view(df_list[[i]]) help(ggplot) library(ggplot2)
      df_list[[i]]$phi_max_size <- df_list[[i]]$phi * max_size
      
      plot <- ggplot2::ggplot(df_list[[i]], 
                              ggplot2::aes(label = Word, 
                                           size = phi_max_size, 
                                           color = phi_max_size)) + #,x=estimate)) +
        ggwordcloud::geom_text_wordcloud() +
        ggplot2::scale_size_area(max_size = max_size) +
        ggplot2::theme_minimal() +
        #theme(plot.margin = margin(0,0,0,0, "cm")) +
        color_scheme + 
        ggplot2::labs(x = paste0("r = ", estimate),
             y= y)
      
      if (!dir.exists(save_dir)) {
        # Create the directory
        dir.create(save_dir)
        cat("Directory created successfully.\n")
      } 
      if(!dir.exists(paste0(save_dir, "/seed_", seed, "/wordclouds"))){
        dir.create(paste0(save_dir, "/seed_", seed, "/wordclouds"))
      }
      p_adjusted <- sprintf("%.2e", p_adjusted)
      ggplot2::ggsave(paste0(save_dir,"/seed_", seed, 
                    "/wordclouds/t_", i, "_r_", 
                    estimate, "_p_", 
                    p_adjusted,".", figure_format),
                    plot = plot, 
                    width = width, 
                    height = height, 
                    units = "in")
    }
  }
}



