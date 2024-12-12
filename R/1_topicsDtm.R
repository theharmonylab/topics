

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


#' Document Term Matrix
#' 
#' The function for creating a document term matrix
#' @param data (list) the list containing the text data with each entry belonging to a unique id
#' @param ngram_window (list) the minimum and maximum n-gram length, e.g. c(1,3)
#' @param stopwords (stopwords) the stopwords to remove, e.g. stopwords::stopwords("en", source = "snowball")
#' @param removalword (string) the word to remove
#' @param removal_mode (string) the mode of removal -> "none", "frequency", "term" or "percentage", frequency removes all words under a certain frequency or over a certain frequency as indicated by removal_rate_least and removal_rate_most, term removes an absolute amount of terms that are most frequent and least frequent, percentage the amount of terms indicated by removal_rate_least and removal_rate_most relative to the amount of terms in the matrix
#' @param removal_rate_most (integer) the rate of most frequent words to be removed, functionality depends on removal_mode
#' @param removal_rate_least (integer) the rate of least frequent words to be removed, functionality depends on removal_mode
#' @param pmi_threshold (integer; experimental) Pointwise Mutual Information (PMI) measures the association 
#' between terms by comparing their co-occurrence probability to their individual probabilities, 
#' highlighting term pairs that occur together more often than expected by chance; in this implementation,
#' terms with average PMI below the specified threshold (pmi_threshold) are removed from the document-term matrix. 
#' @param shuffle (boolean) shuffle the data before analyses
#' @param seed (integer) the random seed for reproducibility
# @param save_dir (string) the directory to save the results, if NULL, no results are saved.
# @param load_dir (string) the directory to load from.
#' @param occurance_rate (integer) the rate of occurence of a word to be removed
#' @param threads (integer) the number of threads to use
#' @return the document term matrix
#' @examples
#' \donttest{
#' 
#' # Create a Dtm and remove the terms that occur less than 4 times and more than 500 times.
#' 
#' dtm <- topicsDtm(data = dep_wor_data$Depphrase,
#'                  removal_mode = "frequency",
#'                  removal_rate_least = 4,
#'                  removal_rate_most = 500)
#' 
#' # Create Dtm and remove the 1 least and 1 most frequent terms.
#' dtm <- topicsDtm(data = dep_wor_data$Depphrase,
#'                  removal_mode = "term",
#'                  removal_rate_least = 1,
#'                  removal_rate_most = 1)
#' 
#' # Create Dtm and remove the 1% least frequent and 1% most frequent terms.
#' dtm <- topicsDtm(data = dep_wor_data$Depphrase,
#'                  removal_mode = "percentage",
#'                  removal_rate_least = 1,
#'                  removal_rate_most = 1)
#'
#' }
#' @importFrom textmineR CreateDtm 
#' @importFrom stats complete.cases
#' @importFrom stopwords stopwords
#' @importFrom Matrix colSums t
#' @importFrom tibble as_tibble
#' @export
topicsDtm <- function(
    data, #
    ngram_window = c(1,3),
    stopwords = stopwords::stopwords("en", source = "snowball"),
    removalword = "",
    pmi_threshold = NULL, 
    occurance_rate = 0,
    removal_mode = "none",
    removal_rate_most = 0,
    removal_rate_least = 0,
    shuffle = TRUE,
    seed = 42L,
    threads = 1){
  
  # Create a named list of settings
  settings <- list(
    ngram_window = ngram_window,
    stopwords = stopwords,
    removalword = removalword,
    pmi_threshold = pmi_threshold,
    occurance_rate = occurance_rate,
    removal_mode = removal_mode,
    removal_rate_most = removal_rate_most,
    removal_rate_least = removal_rate_least,
    shuffle = shuffle,
    seed = seed,
    threads = threads
  )
  
  
  pmi_tibble = NULL
  #if (!is.null(load_dir)){
  #  dtms <- readRDS(paste0(load_dir, 
  #                          "/seed_",
  #                          seed, 
  #                          "/dtms.rds"))
  #  
  #} else {
    
    if (length(data) == 0){
      msg <- "The data provided is empty. Please provide a list of text data."
      message(colourise(msg, "brown"))
      
      return(NULL)
    }
    
    set.seed(seed)
    id_col <- "id"
    data_col <- "text"
    
    if (is.data.frame(data)){
      data <- data[,1]
    }
    
    text_cols <- data.frame(text = data)
    text_cols[[id_col]] <- 1:nrow(text_cols) # create unique ID
    text_cols <- as_tibble(text_cols)
    text_cols <- text_cols[complete.cases(text_cols), ] # remove missing rows
    
    if(shuffle){
      text_cols = text_cols[sample(1:nrow(text_cols)), ] # shuffle
      split_index <- round(nrow(text_cols) * 1) 
      train <- text_cols[1:split_index, ]
    } else {
      train <- text_cols 
    }
    
    if (removalword != ""){
      train[[data_col]] <- gsub(paste0("\\b", removalword, "\\b"), "", train[[data_col]]) 
    }
    
    # Create Trigram DTM
    train_dtm <- textmineR::CreateDtm(
      doc_vec = train[["text"]], 
      doc_names = train[["id"]], 
      ngram_window = ngram_window, 
      stopword_vec = stopwords, 
      lower = TRUE, 
      remove_punctuation = TRUE, 
      remove_numbers = TRUE, 
      verbose = FALSE, 
      cpus = threads
    )
    
    # Create Unigram DTM if necessary for PMI
    if (!is.null(pmi_threshold)) {
      unigram_dtm <- textmineR::CreateDtm(
        doc_vec = train[["text"]], 
        doc_names = train[["id"]], 
        ngram_window = c(1, 1), # Unigrams only
        stopword_vec = stopwords, 
        lower = TRUE, 
        remove_punctuation = TRUE, 
        remove_numbers = TRUE, 
        verbose = FALSE, 
        cpus = threads
      )
    }
    
    # Apply Occurrence Filtering
    if (occurance_rate > 0){
      removal_frequency <- round(nrow(train) * occurance_rate) - 1
      train_dtm <- train_dtm[, Matrix::colSums(train_dtm) > removal_frequency]
    }
    
    # Removal by Frequency or Custom Modes
    if (removal_mode != "frequency"){
      if (removal_rate_least > 0){
        removal_columns <- get_removal_columns(train_dtm, removal_rate_least, "least", removal_mode)
        if (removal_rate_most > 0){
          removal_columns_most <- get_removal_columns(train_dtm, removal_rate_most, "most", removal_mode)
          removal_columns <- c(removal_columns, removal_columns_most)
        }
        train_dtm <- train_dtm[, -removal_columns, drop = FALSE]
      } else if (removal_rate_most > 0){
        removal_columns <- get_removal_columns(train_dtm, removal_rate_most, "most", removal_mode)
        train_dtm <- train_dtm[, -removal_columns, drop = FALSE]
      }
    } else if (removal_mode == "frequency"){
      if (!is.null(removal_rate_least)){
        train_dtm <- train_dtm[, Matrix::colSums(train_dtm) > removal_rate_least]
      }
      if (!is.null(removal_rate_most)){
        train_dtm <- train_dtm[, Matrix::colSums(train_dtm) < removal_rate_most]
      }
    }
    
    # PMI Filter
    train_dtm_null <- train_dtm
    if (!is.null(pmi_threshold)) {
      total_terms <- sum(train_dtm)
      ngram_probs <- Matrix::colSums(train_dtm) / total_terms
      ngram_components <- strsplit(colnames(train_dtm), "_")
      
      # Compute marginal probabilities
      component_probs <- sapply(ngram_components, function(component) {
        # Compute probabilities for all components of the n-gram
        probs <- sapply(component, function(word) {
          if (word %in% colnames(unigram_dtm)) {
            word_index <- which(colnames(unigram_dtm) == word)
            prob <- Matrix::colSums(unigram_dtm[, word_index, drop = FALSE]) / sum(unigram_dtm)
            return(prob)
          } else {
            return(1e-10)  # Small value for unmatched words
          }
        })
        # Multiply probabilities for all components
        return(prod(probs))
      })
      
      # Initialize a function to compute PMI for a single n-gram
      compute_pmi <- function(joint_prob, marg_prob) {
        # Avoid division by very small probabilities
        safe_marg_prob <- max(marg_prob, 1e-10)
        # Compute PMI
        pmi <- log2(joint_prob / safe_marg_prob)
        return(pmi)
      }
      
      # Apply the PMI computation across all n-grams
      values <- mapply(compute_pmi, ngram_probs, component_probs)
      
      # Handle invalid values
      # values[is.infinite(values) | is.nan(values) | values < 0] <- 0
      names(values) <- colnames(train_dtm)

      # Convert values into a tibble
      pmi_tibble <- tibble(
        n_gram = names(values),  # Use the names of 'values' as the word column
        pmi_value = values         # Use the PMI values as the value column
      )
      
      # Identify unigrams (terms without underscores)
      unigram_indices <- grep("^[^_]+$", colnames(train_dtm))
      
      # Apply PMI filtering only to n-grams (terms with underscores)
      pmi_indices <- which(values >= pmi_threshold)
      
      # Combine unigrams and filtered n-grams
      selected_indices <- union(unigram_indices, pmi_indices)
      
      # Filter the DTM
      train_dtm <- train_dtm[, selected_indices, drop = FALSE]
      
      }

    dtms <- list(
      n_grams_pmi = if (!is.null(pmi_tibble)) pmi_tibble else "No pmi_threshold was used",
      settings = settings,
      train_dtm = train_dtm
    )
#  }
  
#  if (!is.null(save_dir)){
#    if (!dir.exists(save_dir)) {
#      # Create the directory
#      dir.create(save_dir)
#      
#      msg <- "Directory created successfully.\n"
#      message(
#        colourise(msg, "green"))
#    } 
#    if(!dir.exists(paste0(save_dir, "/seed_", seed))){
#      dir.create(paste0(save_dir, "/seed_", seed))
#    }
#    msg <- paste0("The Dtm, data, and summary are saved in", save_dir,"/seed_", seed,"/dtms.rds")
#    message(colourise(msg, "green"))
#    
#    saveRDS(dtms, paste0(save_dir, "/seed_", seed, "/dtms.rds"))
#  }
  
  return(dtms)
}



#' Summarize and Visualize your Document Term Matrix
#' 
#' This function creates a frequency table of your DTM and generates up to four plots for visualization
#' @param dtm (R_obj) The document term matrix - output from topicsDtm
#' @importFrom Matrix colSums
#' @importFrom dplyr %>% select 
#' @importFrom ggplot2 ggplot margin element_text
#' @importFrom stats reorder median
#' @return A named list containing:
#' \describe{
#'   \item{dtm_summary}{A dataframe of terms and their frequencies.}
#'   \item{frequency_plot}{A bar plot of all term frequencies with example terms.}
#'   \item{frequency_plot_30_least}{A bar plot of the 30 least frequent terms (if numer of terms > 30).}
#'   \item{frequency_plot_30_most}{A bar plot of the 30 most frequent terms (if numer of terms > 30).}
#'   \item{histogram_of_frequencies}{A histogram of term frequencies (this is the same information as
#'   in the frequency_plot but presented differently).}
#' }
#' @export
topicsDtmEval <- function(dtm) {
  
  # create a summary dataframe of the dtm
  dtm_summary <- as.data.frame(Matrix::colSums(dtm$train_dtm))
  names(dtm_summary) <- c("freq") # rename column
  dtm_summary$term <- rownames(dtm_summary)
  rownames(dtm_summary) <- NULL
  dtm_summary$nr <- c(1:nrow(dtm_summary))
  dtm_summary <- dtm_summary %>% dplyr::select(nr, term, freq) # reorder columns
  
  # If the summary df has <= 30 words, we can use just 2 plots 
  if(nrow(dtm_summary) <= 30){
    
    
    plot_all <- ggplot2::ggplot(data = dtm_summary, ggplot2::aes(x = reorder(term, freq), y = freq, fill = nr))+
      ggplot2::geom_bar(stat = "identity", show.legend = F)+
      ggplot2::labs(title = paste0("Frequencies of all terms in the DTM (N = ", nrow(dtm_summary), ")"),
           x = "Terms", 
           y = "Frequency",
           subtitle = paste0("Min = ", min(dtm_summary$freq), ", Max = ", max(dtm_summary$freq), ", Med = ",
                             med = stats::median(dtm_summary$freq), ", SD = ", round(sd(dtm_summary$freq),2)))+
      ggplot2::scale_y_continuous(expand = c(0,0))+
      ggplot2::scale_fill_gradient(low = "#9BD7E9", high ="#15637F")+
      ggplot2::theme_minimal()+
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, size = 11),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank())
    
    # histogram of term frequencies
    plot_hist <- ggplot2::ggplot(data = dtm_summary, ggplot2::aes(x = freq)) +
      ggplot2::geom_histogram(fill = "#15637F", color = "white") +
      ggplot2::labs(x = "Term frequency",
           y = "Number of terms with this frequency",
           title = paste0("Histogram of term frequencies (N = ", nrow(dtm_summary), ")"),
           subtitle = paste0("Min = ", min(dtm_summary$freq), ", Max = ", max(dtm_summary$freq), ", Med = ",
                             med = stats::median(dtm_summary$freq), ", SD = ", round(sd(dtm_summary$freq),2)))+
      ggplot2::theme_minimal()
    
    # return output with only 2 plots
    return(list(dtm_summary = dtm_summary[,2:3],
                frequency_plot = plot_all,
                histogram_of_frequencies = plot_hist))
  }
  
  # Otherwise, split into 3 plots (first 30, last 30, overview):
  
  # subset first 30 terms (lowest frequencies) and last 30 terms (highest frequencies)
  dtm_summary_30_min <- dtm_summary[1:30,]
  dtm_summary_30_max <- dtm_summary[(nrow(dtm_summary)-30):nrow(dtm_summary),]
  
  
  # plot 30 least frequent
  plot_30_least <- ggplot2::ggplot(data = dtm_summary_30_min, ggplot2::aes(x = reorder(term, freq), y = freq))+
    ggplot2::geom_bar(stat = "identity", color = "white", fill = "#9BD7E9")+
    ggplot2::labs(title = "30 least frequent terms in the DTM",
         x = "Term", y = "Frequency")+
    ggplot2::scale_y_continuous(limits = c(0,max(dtm_summary$freq)))+
    ggplot2::theme_minimal()+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 50, hjust = 1, size = 12),
          axis.title.y = ggplot2::element_text(size = 12, face = "bold"),
          plot.title = ggplot2::element_text(size = 15),
          axis.title.x = ggplot2::element_text(margin = margin(t = 5), size = 12, face = "bold"))
  
  # plot 30 msot frequent
  plot_30_most <- ggplot2::ggplot(data = dtm_summary_30_max, ggplot2::aes(x = reorder(term, freq), y = freq))+
    ggplot2::geom_bar(stat = "identity", color = "white", fill = "#15637F")+
    ggplot2::labs(title = "30 most frequent terms in the DTM",
         x = "Term", y = "Frequency")+
    ggplot2::theme_minimal()+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 50, hjust = 1, size = 12),
          axis.title.y = ggplot2::element_text(size = 12, face = "bold"),
          plot.title = ggplot2::element_text(size = 15),
          axis.title.x = ggplot2::element_text(margin = margin(t = 5), size = 12, face = "bold"))
  
  
  # plot all terms and only show some example terms
  keep <- seq(nrow(dtm_summary), 1, -(ceiling(nrow(dtm_summary)/20))) 
  
  dtm_summary <- dtm_summary %>%
    mutate(example_terms =  ifelse(1:nrow(dtm_summary) %in% keep, term, ""))
  
  plot_all <- ggplot2::ggplot(data = dtm_summary, ggplot2::aes(x = reorder(term, freq), y = freq, fill = nr))+
    ggplot2::geom_bar(stat = "identity", show.legend = F)+
    ggplot2::labs(title = paste0("Frequencies of all terms in the DTM (N = ", nrow(dtm_summary), ")"),
         x = "Terms (only a few terms shown for illustration)", 
         y = "Frequency",
         subtitle = paste0("Min = ", min(dtm_summary$freq), ", Max = ", max(dtm_summary$freq), ", Med = ",
                           med = stats::median(dtm_summary$freq), ", SD = ", round(sd(dtm_summary$freq),2)))+
    ggplot2::scale_x_discrete(labels = dtm_summary$example_terms) +
    ggplot2::scale_y_continuous(expand = c(0,0))+
    ggplot2::scale_fill_gradient(low = "#9BD7E9", high ="#15637F")+
    ggplot2::theme_minimal()+
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, size = 11),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank())
  
  
  # histogram of term frequencies
  plot_hist <- ggplot(data = dtm_summary, ggplot2::aes(x = freq)) +
    ggplot2::geom_histogram(fill = "#15637F", color = "white") +
    ggplot2::labs(x = "Term frequency",
         y = "Number of terms with this frequency",
         title = paste0("Histogram of term frequencies (N = ", nrow(dtm_summary), ")"),
         subtitle = paste0("Min = ", min(dtm_summary$freq), ", Max = ", max(dtm_summary$freq), ", Med = ",
                           med = stats::median(dtm_summary$freq), ", SD = ", round(sd(dtm_summary$freq),2)))+
    ggplot2::theme_minimal()
  
  df_final <- dtm_summary %>% dplyr::select(term, freq)
  
  # output
  return(list(dtm_summary = df_final,
              frequency_plot = plot_all,
              frequency_plot_30_least = plot_30_least,
              frequency_plot_30_most = plot_30_most,
              histogram_of_frequencies = plot_hist))
}



