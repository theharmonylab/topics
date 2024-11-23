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
#' @param split (float) the proportion of the data to be used for training
#' @param seed (integer) the random seed for reproducibility
#' @param save_dir (string) the directory to save the results, if NULL, no results are saved.
#' @param load_dir (string) the directory to load from.
#' @param occ_rate (integer) the rate of occurence of a word to be removed
#' @param threads (integer) the number of threads to use
#' @return the document term matrix
#' @examples
#' \donttest{
#' 
#' # Create a Dtm and remove the terms that occur less than 4 times and more than 500 times.
#' save_dir_temp <- tempfile()
#' 
#' dtm <- topicsDtm(data = dep_wor_data$Depphrase,
#'                  removal_mode = "frequency",
#'                  removal_rate_least = 4,
#'                  removal_rate_most = 500,
#'                  save_dir = save_dir_temp)
#' 
#' # Create Dtm and remove the 5 least and 5 most frequent terms.
#' dtm <- topicsDtm(data = dep_wor_data$Depphrase,
#'                  removal_mode = "term",
#'                  removal_rate_least = 1,
#'                  removal_rate_most = 1,
#'                  save_dir = save_dir_temp)
#' 
#' # Create Dtm and remove the 5% least frequent and 1% most frequent terms.
#' dtm <- topicsDtm(data = dep_wor_data$Depphrase,
#'                  removal_mode = "percentage",
#'                  removal_rate_least = 1,
#'                  removal_rate_most = 1,
#'                  save_dir = save_dir_temp)
#'                  
#' # Load precomputed Dtm from directory
#' dtm <- topicsDtm(load_dir = save_dir_temp,
#'                  seed = 42,
#'                  save_dir = save_dir_temp)
#' }
#' @importFrom textmineR CreateDtm 
#' @importFrom stats complete.cases
#' @importFrom stopwords stopwords
#' @importFrom Matrix colSums
#' @importFrom tibble as_tibble
#' @export
topicsDtm <- function(
    data, #
    ngram_window=c(1,3),
    stopwords=stopwords::stopwords("en", source = "snowball"),
    removalword="",
    occ_rate=0,
    removal_mode="none",
    removal_rate_most=0,
    removal_rate_least=0,
    split=1,
    seed=42L,
    save_dir,
    load_dir = NULL,
    threads=1){
  
  if (!is.null(load_dir)){
    dtms <- readRDS(paste0(load_dir, 
                            "/seed_",
                            seed, 
                            "/dtms.rds"))
    
  } else {
    
    if (length(data) == 0){
      msg <- "The data provided is empty. Please provide a list of text data."
      message(colourise(msg, "brown"))
      
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
      cpus = threads) # default is all available cpus on the system
  
    
    if (occ_rate>0){
      removal_frequency <- round(nrow(train)*occ_rate) -1
      train_dtm <- train_dtm[,Matrix::colSums(train_dtm) > removal_frequency]
    }
    if (removal_mode != "frequency"){
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
    } else if (removal_mode == "frequency"){
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
      cpus = threads) # default is all available cpus on the system
    
    if (occ_rate>0){
      
      removal_frequency <- round(nrow(test)*occ_rate) -1
      
      test_dtm <- test_dtm[, Matrix::colSums(test_dtm) > removal_frequency]
    }
    #removal_frequency <- get_occ_frequency(test_dtm, occ_rate)
    #test_dtm <- test_dtm[,Matrix::colSums(test_dtm) > removal_frequency]
    
    if (removal_mode != "frequency"){
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
    } else if (removal_mode == "frequency"){
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
      
      msg <- "Directory created successfully.\n"
      message(
        colourise(msg, "green"))
    } 
    if(!dir.exists(paste0(save_dir, "/seed_", seed))){
      dir.create(paste0(save_dir, "/seed_", seed))
    }
    msg <- paste0("The Dtm, data, and summary are saved in", save_dir,"/seed_", seed,"/dtms.rds")
    message(colourise(msg, "green"))
    
    saveRDS(dtms, paste0(save_dir, "/seed_", seed, "/dtms.rds"))
  }
  
  return(dtms)
}


#' Summarize and Visualize your Document Term Matrix
#' 
#' This function creates a frequency table of your DTM and generates up to four plots for visualization
#' @param dtm (R_obj) The document term matrix - output from topicsDtm
#' @importFrom Matrix colSums
#' @importFrom dplyr %>% select 
#' @importFrom ggplot2 ggplot margin
#' @importFrom stats reorder
#' @return A named list containing:
#' \describe{
#'   \item{dtm_summary}{A dataframe of terms and their frequencies.}
#'   \item{frequency_plot}{A bar plot of all term frequencies with example terms.}
#'   \item{frequency_plot_30_least}{A bar plot of the 30 least frequent terms (if numer of terms > 30).}
#'   \item{frequency_plot_30_most}{A bar plot of the 30 most frequent terms (if numer of terms > 30).}
#'   \item{historgam_of_frequencies}{A histogram of term frequencies (this is the same information as
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
    
    
    plot_all <- ggplot2::ggplot(data = dtm_summary, aes(x = reorder(term, freq), y = freq, fill = nr))+
      ggplot2::geom_bar(stat = "identity", show.legend = F)+
      ggplot2::labs(title = paste0("Frequencies of all terms in the DTM (N = ", nrow(dtm_summary), ")"),
           x = "Terms", 
           y = "Frequency",
           subtitle = paste0("Min = ", min(dtm_summary$freq), ", Max = ", max(dtm_summary$freq), ", Med = ",
                             med = median(dtm_summary$freq), ", SD = ", round(sd(dtm_summary$freq),2)))+
      ggplot2::scale_y_continuous(expand = c(0,0))+
      ggplot2::scale_fill_gradient(low = "#9BD7E9", high ="#15637F")+
      ggplot2::theme_minimal()+
      ggplot2::theme(
        axis.text.x = element_text(angle = 35, hjust = 1, size = 11),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())
    
    # histogram of term frequencies
    plot_hist <- ggplot2::ggplot(data = dtm_summary, aes(x = freq)) +
      ggplot2::geom_histogram(fill = "#15637F", color = "white") +
      ggplot2::labs(x = "Term frequency",
           y = "Number of terms with this frequency",
           title = paste0("Histogram of term frequencies (N = ", nrow(dtm_summary), ")"),
           subtitle = paste0("Min = ", min(dtm_summary$freq), ", Max = ", max(dtm_summary$freq), ", Med = ",
                             med = median(dtm_summary$freq), ", SD = ", round(sd(dtm_summary$freq),2)))+
      ggplot2::theme_minimal()
    
    # return output with only 2 plots
    return(list(dtm_summary = dtm_summary[,2:3],
                frequency_plot = plot_all,
                historgam_of_frequencies = plot_hist))
  }
  
  # Otherwise, split into 3 plots (first 30, last 30, overview):
  
  # subset first 30 terms (lowest frequencies) and last 30 terms (highest frequencies)
  dtm_summary_30_min <- dtm_summary[1:30,]
  dtm_summary_30_max <- dtm_summary[(nrow(dtm_summary)-30):nrow(dtm_summary),]
  
  
  # plot 30 least frequent
  plot_30_least <- ggplot2::ggplot(data = dtm_summary_30_min, aes(x = reorder(term, freq), y = freq))+
    ggplot2::geom_bar(stat = "identity", color = "white", fill = "#9BD7E9")+
    ggplot2::labs(title = "30 least frequent terms in the DTM",
         x = "Term", y = "Frequency")+
    ggplot2::scale_y_continuous(limits = c(0,max(dtm_summary$freq)))+
    ggplot2::theme_minimal()+
    ggplot2::theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 12),
          axis.title.y = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 15),
          axis.title.x = element_text(margin = margin(t = 5), size = 12, face = "bold"))
  
  # plot 30 msot frequent
  plot_30_most <- ggplot2::ggplot(data = dtm_summary_30_max, aes(x = reorder(term, freq), y = freq))+
    ggplot2::geom_bar(stat = "identity", color = "white", fill = "#15637F")+
    ggplot2::labs(title = "30 most frequent terms in the DTM",
         x = "Term", y = "Frequency")+
    ggplot2::theme_minimal()+
    ggplot2::theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 12),
          axis.title.y = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 15),
          axis.title.x = element_text(margin = margin(t = 5), size = 12, face = "bold"))
  
  
  # plot all terms and only show some example terms
  keep <- seq(nrow(dtm_summary), 1, -(ceiling(nrow(dtm_summary)/20))) 
  
  dtm_summary <- dtm_summary %>%
    mutate(example_terms =  ifelse(1:nrow(dtm_summary) %in% keep, term, ""))
  
  plot_all <- ggplot2::ggplot(data = dtm_summary, aes(x = reorder(term, freq), y = freq, fill = nr))+
    ggplot2::geom_bar(stat = "identity", show.legend = F)+
    ggplot2::labs(title = paste0("Frequencies of all terms in the DTM (N = ", nrow(dtm_summary), ")"),
         x = "Terms (only a few terms shown for illustration)", 
         y = "Frequency",
         subtitle = paste0("Min = ", min(dtm_summary$freq), ", Max = ", max(dtm_summary$freq), ", Med = ",
                           med = median(dtm_summary$freq), ", SD = ", round(sd(dtm_summary$freq),2)))+
    ggplot2::scale_x_discrete(labels = dtm_summary$example_terms) +
    ggplot2::scale_y_continuous(expand = c(0,0))+
    ggplot2::scale_fill_gradient(low = "#9BD7E9", high ="#15637F")+
    ggplot2::theme_minimal()+
    ggplot2::theme(
      axis.text.x = element_text(angle = 35, hjust = 1, size = 11),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank())
  
  
  # histogram of term frequencies
  plot_hist <- ggplot(data = dtm_summary, aes(x = freq)) +
    ggplot2::geom_histogram(fill = "#15637F", color = "white") +
    ggplot2::labs(x = "Term frequency",
         y = "Number of terms with this frequency",
         title = paste0("Histogram of term frequencies (N = ", nrow(dtm_summary), ")"),
         subtitle = paste0("Min = ", min(dtm_summary$freq), ", Max = ", max(dtm_summary$freq), ", Med = ",
                           med = median(dtm_summary$freq), ", SD = ", round(sd(dtm_summary$freq),2)))+
    ggplot2::theme_minimal()
  
  df_final <- dtm_summary %>% dplyr::select(term, freq)
  
  # output
  return(list(dtm_summary = df_final,
              frequency_plot = plot_all,
              frequency_plot_30_least = plot_30_least,
              frequency_plot_30_most = plot_30_most,
              historgam_of_frequencies = plot_hist))
}


#' Topic modelling
#' 
#' The function to create and train and an LDA model.
#' @param dtm (R_obj) The document term matrix
#' @param num_topics (integer) The number of topics to be created
#' @param num_top_words (integer) The number of top words to be displayed
#' @param num_iterations (integer) The number of iterations to run the model
#' @param seed (integer) The seed to set for reproducibility
#' @param save_dir (string) The directory to save the model, if NULL, the model will not be saved
#' @param load_dir (string) The directory to load the model from, if NULL, the model will not be loaded
#' @return A list of the model, the top terms, the labels, the coherence, and the prevalence
#' @examples
#' \donttest{
#' # Create LDA Topic Model 
#' save_dir_temp <- tempfile()
#' dtm <- topicsDtm(
#' data = dep_wor_data$Depphrase, 
#' save_dir = save_dir_temp)
#' 
#' model <- topicsModel(
#' dtm = dtm, # output of topicsDtm()
#' num_topics = 20,
#' num_top_words = 10,
#' num_iterations = 1000,
#' seed = 42,
#' save_dir = save_dir_temp)
#'                    
#' # Load precomputed LDA Topic Model
#' model <- topicsModel(
#' load_dir = save_dir_temp,
#' seed = 42,
#' save_dir = save_dir_temp)
#' }
#' @export
topicsModel <- function(
    dtm,
    num_topics = 20,
    num_top_words = 10,
    num_iterations = 1000,
    seed = 42,
    save_dir,
    load_dir = NULL){

  
  if (!is.null(load_dir)){
    model <- readRDS(paste0(load_dir, 
                            "/seed_",
                            seed, 
                            "/model.rds"))
  } else {
    
    dtm <- dtm$train_dtm
    
    if (length(Matrix::colSums(dtm)) == 0) {
      msg <- "The document term matrix is empty. Please provide a valid document term matrix."
      message(colourise(msg, "brown"))
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

      msg <- "Directory created successfully.\n"
      message(
        colourise(msg, "green"))
      
    } 
    if(!dir.exists(paste0(save_dir, "/seed_", seed))){
      dir.create(paste0(save_dir, "/seed_", seed))
    }
    msg <- paste0("The Model is saved in", save_dir,"/seed_", seed,"/model.rds")
    message(colourise(msg, "green"))
    saveRDS(model, paste0(save_dir, "/seed_", seed, "/model.rds"))
  }
  
  return(model)
}

#' N-grams
#' 
#' The function computes ngrams from a text
#' @param data (tibble) The data
#' @param n (integer) The length of ngram
#' @param sep (string) The separator
#' @param top_n (integer) The number of top ngrams to be displayed
#' @param pmi_threshold (integer) The pmi threshold, if it shall not be used set to 0
#' @importFrom ngram ngram get.ngrams get.phrasetable
#' @importFrom tibble as_tibble tibble
#' @importFrom stringr str_count
#' @importFrom dplyr mutate row_number filter 
#' @return A list containing tibble of the ngrams with the frequency and probability and 
#' a tibble containing the relative frequency of the ngrams for each user
#' @export
topicsGrams <- function(
    data, 
    n=2, 
    sep = " ", 
    top_n = NULL, 
    pmi_threshold=0){
  
  data <- tolower(data)
  data <- gsub("[()].$", "", data)
  
  ngrams <- list()
  counts <- c()
  for (i in 1:n){
    ngrams[[i]] <- ngram::ngram(data, n = i, sep = " ")
    ngrams[[i]] <- ngram::get.phrasetable(ngrams[[i]])
    counts <- c(counts,nrow(ngrams[[i]]))
  }
  
  total <- 0
  for (i in 1:n){
    total <- total + sum(ngrams[[i]]$freq)
  }
  for (i in 1:n){
    ngrams[[i]]$prop <- ngrams[[i]]$freq/total
  }

  # calculate the pmi score pmi $($ phrase $)=\log \frac{p(\text { phrase })}{\Pi_{w \in p h r a s e} p(w)}$
 
  for (k in 2:n){
    pmi <- list()
    for (i in 1:nrow(ngrams[[k]])) {
      words <- strsplit(ngrams[[k]]$ngrams[i], " ")[[1]]
      denum <- 1
      for (j in 1:length(words)) {
        denum <- denum * ngrams[[1]][ngrams[[1]]$ngrams == paste0(words[j], " "), "prop"]
      }
      pmi[[i]] <- log(ngrams[[k]]$prop[i]/denum)
    }
    ngrams[[k]]$pmi <- unlist(pmi)
  }
  ngrams[[1]]$pmi <- pmi_threshold
  
  
  # filter ngrams based on pmi
  if (!is.null(pmi_threshold)){
    removed <- c()
    for (i in 2:n){
      ngrams[[i]] <- ngrams[[i]] %>% dplyr::filter(pmi > pmi_threshold)
      removed <- c(removed, counts[[i]] - nrow(ngrams[[i]]))
    }
    removed <- c(0, removed)
  } else {
    removed <- rep(0, n)
  }
  
  stats <- tibble(
    ngram_type = paste0(1:n, "-grams"),           # Create n-gram labels dynamically
    initial_count = counts,  #  initial counts for illustration
    removed_count = removed,    #  removed counts for illustration
    final_count = counts - removed
  )
  
  # take top n ngrams
  if (!is.null(top_n)){
    for (i in 1:n){
      ngrams[[i]] <- ngrams[[i]][1:top_n,]
    }
  }
  ngrams <- do.call(rbind, ngrams) # concatenate
  freq_per_user <- list()
  # create unique integer for each row
  data <- as_tibble(data)
  data <- data %>% dplyr::mutate(row_id = row_number())
  freq_per_user$usertexts <- data$row_id
  
  # calculate the relative frequency per user
  for (i in 1:nrow(ngrams)) {
    temp <- c()
    
    for (j in 1:length(data$value)) {
      gram <- as.character(ngrams$ngrams[i])
      sentence <- as.character(tolower(data$value[j]))
      ngram_count <- str_count(sentence,gram)
      frequency <- ngrams$freq[i]
      relative_frequency <- ngram_count / frequency
      temp <- c(temp, relative_frequency)
    }
    freq_per_user[[paste(unlist(strsplit(as.character(ngrams$ngrams[i]), " ")), collapse = "_")]] <- temp
  }
  
  # change the ngrams to a single string with "_" as connector
  ngrams$ngrams <- sapply(ngrams$ngrams, function(x) paste(unlist(strsplit(x, " ")), collapse = "_"))
  
  return(list(ngrams=as_tibble(ngrams),
              freq_per_user = as_tibble(freq_per_user),
              stats=stats))
}


#' Predict topic distributions
#' 
#' The function to predict the topics of a new document with the trained model.
#' @param model (list) The trained model
#' @param data (tibble) The new data
#' @param num_iterations (integer) The number of iterations to run the model
#' @param seed (integer) The seed to set for reproducibility
#' @param save_dir (string) The directory to save the model, if NULL, the predictions will not be saved
#' @param load_dir (string) The directory to load the model from, if NULL, the predictions will not be loaded
#' @return A tibble of the predictions
#' @examples
#' \donttest{
#' # Predict topics for new data with the trained model
#' save_dir_temp <- tempfile()
#' 
#' dtm <- topicsDtm(
#' data = dep_wor_data$Depphrase, 
#' save_dir = save_dir_temp)
#' 
#' model <- topicsModel(dtm = dtm, # output of topicsDtm()
#'                      num_topics = 20,
#'                      num_top_words = 10,
#'                      num_iterations = 1000,
#'                      seed = 42,
#'                      save_dir = save_dir_temp)
#'                      
#' preds <- topicsPreds(
#' model = model, # output of topicsModel()
#' data = dep_wor_data$Depphrase, 
#' save_dir = save_dir_temp)
#' }
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr %>%
#' @export
topicsPreds <- function(
    model, # only needed if load_dir==NULL 
    data, # data vector to infer distribution for
    num_iterations=100, # only needed if load_dir==NULL,
    seed=42,
    save_dir,
    load_dir=NULL){

  set.seed(seed)

  if (!is.null(load_dir)){
    preds <- readRDS(paste0(load_dir, "/seed_", seed, "/preds.rds"))
  } else {
    
    if (length(data) == 0){
      msg <- "The data provided is empty. Please provide a list of text data."
      message(colourise(msg, "brown"))
      
      return(NULL)
    }
    # create an id column for the data
    
    if (is.data.frame(data)){
      pred_text <- data[, 1]
    } else {
      pred_text <- data
    }
    
    pred_ids <- as.character(1:length(data))
    
    new_instances <- compatible_instances(ids=pred_ids,
                                          texts=pred_text,
                                          instances=model$instances)
    
    inf_model <- model$inferencer
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
    preds <- preds %>% tibble::tibble()
    
  }
  
  if (!is.null(save_dir)){
    if (!dir.exists(save_dir)) {
      dir.create(save_dir)
      
      msg <- "Directory created successfully.\n"
      message(
        colourise(msg, "green"))
      
    } 
    if(!dir.exists(paste0(save_dir, "/seed_", seed))){
      dir.create(paste0(save_dir, "/seed_", seed))
    }
    msg <- paste0("Predictions are saved in", save_dir,"/seed_", seed,"/preds.rds")
    message(colourise(msg, "green"))
    saveRDS(preds, paste0(save_dir, "/seed_", seed, "/preds.rds"))
  }
  
  return(preds)
}

#' The function to test the LDA model
#' @param model (list) The trained model
#' @param data (tibble) The data to test on
#' @param preds (tibble) The predictions
#' @param pred_var (string) The variable to be predicted (only needed for regression or correlation)
#' @param group_var (string) The variable to group by (only needed for t-test)
#' @param control_vars (vector) The control variables
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
    pred_var=NULL, # for all test types except t-test
    group_var=NULL, # only one in the case of t-test
    control_vars=c(),
    test_method="linear_regression",
    p_adjust_method = "fdr",
    seed=42,
    load_dir=NULL,
    save_dir){
  
  if (!is.null(load_dir)){
    test_path <- paste0(load_dir, "/seed_", seed, "/test.rds")
    if (!file.exists(test_path)) {
      msg <- paste0("Test file not found at: ", test_path, ". Exiting function.")
      message(colourise(msg, "brown"))
      return(NULL)
    }
    test <- readRDS(test_path)
  } else {
    
    if (is.null(pred_var) && test_method != "t-test") {
      msg <- "Prediction variable is missing. Please input a prediction variable."
      message(colourise(msg, "brown"))
      return(NULL)
    }
    
    if (test_method == "t-test" && is.null(group_var)){
      msg <- "Group variable is missing. Please input a group variable"
      message(colourise(msg, "brown"))
      return(NULL)
    }
    
    if (!is.list(model)){
      msg <- "Input a model created with topicsModel"
      
      message(colourise(msg, "brown"))
      
      return(NULL)
    }
    
    if (length(data) == 0){
      msg <- "The data provided is empty. Please provide a list of text data."
      message(colourise(msg, "brown"))
      
      return(NULL)
    }
    
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
      df <- list(variable = group_var,
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


#' Statistically test topics
#' 
#' The function to test the lda model for multiple dimensions, e.g., 2.
#' @param model (list) The trained model
#' @param data (tibble) The data to test on
#' @param preds (tibble) The predictions
#' @param ngrams (list) output of the ngram function
#' @param pred_var_x (string) The x variable name to be predicted, and to be plotted (only needed for regression or correlation)
#' @param pred_var_y (string) The y variable name to be predicted, and to be plotted (only needed for regression or correlation)
#' @param group_var (string) The variable to group by (only needed for t-test)
#' @param control_vars (vector) The control variables (not supported yet)
#' @param test_method (string) The test method to use, either "correlation","t-test", "linear_regression","logistic_regression", or "ridge_regression"
#' @param p_alpha (numeric) Threshold of p value set by the user for visualising significant topics 
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
#'   pred_var_x = "Age",
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
    pred_var_x = NULL, # for all test types except t-test
    pred_var_y = NULL,
    group_var = NULL, # only one in the case of t-test
    control_vars = c(),
    test_method = "linear_regression",
    p_alpha = 0.05,
    p_adjust_method = "fdr",
    seed = 42,
    load_dir = NULL,
    save_dir){
  
  
  #### Warnings and instructions ####
  if(!is.null(pred_var_x) | !is.null(pred_var_x)){
    if(grepl("_", pred_var_x) | grepl("_", pred_var_x)){
      stop("Please note that at the moment pred_var_x or pred_var_y 
    cannot have an an underscore '_' in the name. Please rename the variable in the dataset.")
    }
  }
  
  if (length(control_vars) > 0){
    for (control_var in control_vars){
      if (!is.numeric(data[[control_var]])){
        
        msg <- paste0("The control variable '", 
                   control_var, 
                   "' should be numeric!\n")
        
        message(
          colourise(msg, "brown"))
        
        return (NULL)
    }}
  }
  
  if (is.null(pred_var_x) & is.null(group_var)){
    msg <- 'Please input the pred_var_x or group_var!'
    message(colourise(msg, "brown"))
    return (NULL)
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
                          top_terms = ngrams$ngrams)
    model$summary <- data.frame(model$summary)
  }
  
  
  #### Testing the elements (i.e., ngrams or topics) ####
  pred_vars_all <- c(pred_var_x, pred_var_y)
  
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
  for (i in 1:length(pred_vars_all)){
    
    topic_loading <- topicsTest1(
      model = model,
      preds = preds, 
      data = data,
      pred_var = pred_vars_all[i], # for all test types except t-test
      group_var= group_var, # only one in the case of t-test
      control_vars= control_vars,
      test_method= test_method,
      p_adjust_method = p_adjust_method,
      seed=seed,
      load_dir = load_dir,
      save_dir = save_dir
    )
    
    # Sorting output when not using ridge regression
    if (test_method != "ridge_regression") {
      
    
      colnames(topic_loading$test) <- c("topic", "top_terms", 
                                        paste(pre[i], 
                                              colnames(topic_loading$test)[3:6], 
                                              sep = "."))
      
      topic_loadings_all[[i]] <- topic_loading
    }
  }
  
  if (!is.null(pred_var_y)){
    # create the x.y.word.category
    topic_loadings_all[[3]] <- list()
    topic_loadings_all[[3]]$test <- dplyr::left_join(topic_loadings_all[[1]][[1]][,1:6], 
                                                     topic_loadings_all[[2]][[1]][,1:6],
                                                     by = c("topic", "top_terms"))
    topic_loadings_all[[3]]$test_method <- topic_loadings_all[[1]]$test_method
    topic_loadings_all[[3]]$pred_var <- paste0(topic_loadings_all[[1]]$pred_var, '_',
                                               topic_loadings_all[[2]]$pred_var) 
    
    bak1 <- colnames(topic_loadings_all[[3]]$test)[c(3,6,7,10)]
    colnames(topic_loadings_all[[3]]$test)[c(3,6,7,10)] <- c('x_plotted', 'adjusted_p_values.x',
                                                             'y_plotted', 'adjusted_p_values.y')
    
    #### This should be moved to topicsPlot <-  we do not want to set colour categories in the test
    topic_loadings_all[[3]]$test <- topicsNumAssign_dim2(topic_loadings_all[[3]]$test, p_alpha, 2)
    colnames(topic_loadings_all[[3]]$test)[c(3,6,7,10)] <- bak1
    
  } else {
    
    if (test_method == "linear_regression" | test_method == "logistic_regression"){
    
      msg <- "The parameter pred_var_y is not set! Output 1 dimensional results."
      message(colourise(msg, "brown"))
      
      topic_loadings_all[[2]] <- list()
      topic_loadings_all[[3]] <- list()
      topic_loadings_all[[3]]$test <- topic_loadings_all[[1]][[1]][,1:6]
      topic_loadings_all[[3]]$test_method <- topic_loadings_all[[1]]$test_method
      topic_loadings_all[[3]]$pred_var <- topic_loadings_all[[1]]$pred_var
      
      bak1 <- colnames(topic_loadings_all[[3]]$test)[c(3,6)]
      colnames(topic_loadings_all[[3]]$test)[c(3,6)] <- c('x_plotted', 'adjusted_p_values.x')
      
      #### This should be moved to topicsPlot <-  we do not want to set colour categories in the test
      topic_loadings_all[[3]]$test <- topicsNumAssign_dim2(topic_loadings_all[[3]]$test, p_alpha, 1)
      colnames(topic_loadings_all[[3]]$test)[c(3,6)] <- bak1
      
    } else if (test_method == "ridge_regression"){
      topic_loadings_all[[1]] <- topic_loading
    }
  }
  
  return(topic_loadings_all)
}


#### topicsScatterLegendOriginal ####

#' @param bivariate_color_codes A vector of color codes specifying colors for 
#' different categories in the scatter plot. 
#' Default: c("#398CF9", "#60A1F7", "#5dc688", "#e07f6a", "#EAEAEA", "#40DD52", "#FF0000", "#EA7467", "#85DB8E").
#' @param filtered_test A data frame containing the input data for the scatter plot. 
#' Must include columns like `color_categories` and other variables used in the function.
#' @param num_popout The number of topics to "pop out" in each category. Default: 1.
#'  Can be a single integer (applies to all categories) or a vector for specific categories.
#' @param way_popout_topics The method for selecting pop-out topics. Options: "mean", "max_y", or "max_x". Default: "mean".
#' @param user_spec_topics A vector of user-specified topics to highlight in the scatter plot. Default: NULL.
#' @param allow_topic_num_legend Logical; if TRUE, displays topic numbers in the legend. Default: FALSE.
#' @param y_axes_1 Specifies axis alignment for the scatter legend. Options: 1 (x-axis) or 2 (y-axis). Default: 2.
#' @param cor_var A string used for naming the correlation variable in labels or file names. Default: "".
#' @param label_x_name Label for the x-axis in the scatter plot. Default: "x".
#' @param label_y_name Label for the y-axis in the scatter plot. Default: "y".
#' @param save_dir Directory where the scatter legend plot will be saved. Default: "./results".
#' @param figure_format File format for the saved scatter plot. Examples: "svg", "png", "pdf". Default: "svg".
#' @param scatter_popout_dot_size Size of the dots for pop-out topics in the scatter legend. Default: 15.
#' @param scatter_bg_dot_size Size of the dots for background topics in the scatter legend. Default: 9.
#' @param width Width of the saved scatter plot in inches. Default: 10.
#' @param height Height of the saved scatter plot in inches. Default: 8.
#' @param seed Seed for reproducibility, ensuring consistent plot generation. Default: 42.
#' @importFrom ggplot2 ggplot geom_point scale_color_manual labs theme_minimal theme element_blank
#' @importFrom rlang sym !!
#' @importFrom dplyr select filter mutate anti_join summarise pull group_by group_modify ungroup
#' @noRd
topicsScatterLegendOriginal <- function(
    bivariate_color_codes = c(
      "#398CF9", "#60A1F7", "#5dc688",
      "#e07f6a", "#EAEAEA", "#40DD52",
      "#FF0000", "#EA7467", "#85DB8E"),
    filtered_test,
    num_popout = 1,
    way_popout_topics = "mean",
    user_spec_topics = NULL,
    allow_topic_num_legend = FALSE,
    y_axes_1 = 2,
    cor_var = "",
    label_x_name = "x",
    label_y_name = "y",
    save_dir,
    figure_format = "svg",
    scatter_popout_dot_size = 15,
    scatter_bg_dot_size = 9,
    width = 10, 
    height = 8,
    seed = 42
){
  
  only_two <- filtered_test %>%
    dplyr::summarise(contains_only_five = all(color_categories %in% 2)) %>%
    dplyr::pull(contains_only_five)
  
  if (only_two && y_axes_1 == 1){
    msg <- "There are only non-significant topics; so only a scatter legend is produced for these topics.\n"
    
    message(
      colourise(msg, "blue"))
    
    
    x_column <- names(filtered_test)[3]
    color_column <- names(filtered_test)[ncol(filtered_test)]
    plot_only3 <- dplyr::filter(tibble::as_tibble(filtered_test,.name_repair="minimal"),
                                color_categories == 1 | color_categories == 2 | color_categories == 3)
    plot <- ggplot2::ggplot() +
      ggplot2::geom_point(data = plot_only3, 
                          aes(x = !!rlang::sym(x_column), y = 1,
                              color = as.factor(.data[[color_column]])),
                          size = scatter_popout_dot_size, alpha = 0.8) +
      ggplot2::scale_color_manual(values = bivariate_color_codes) +
      ggplot2::labs(x = label_x_name, y = "", color = '') +
      ggplot2::theme_minimal() + 
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(), # Remove x-axis text
        axis.text.y = ggplot2::element_blank(), # Remove y-axis text
        axis.ticks.x = ggplot2::element_blank(), # Remove x-axis ticks
        axis.ticks.y = ggplot2::element_blank(),  # Remove y-axis ticks
        legend.position = "none"
      )
  }

  
  only_five <- filtered_test %>%
    dplyr::summarise(contains_only_five = all(color_categories %in% 5)) %>%
    dplyr::pull(contains_only_five)
  
  # Making scatter plot with no significant topics. 
  if (only_five && y_axes_1 == 2){
    
    x_column <- names(filtered_test)[3]
    y_column <- names(filtered_test)[7]
    color_column <- names(filtered_test)[ncol(filtered_test)]
    plot <- ggplot2::ggplot() +
      ggplot2::geom_point(data = filtered_test,
                          aes(x = !!sym(x_column),
                              y = !!sym(y_column),
                              color = as.factor(.data[[color_column]])), 
                          size = scatter_popout_dot_size, alpha = 0.8) +
      ggplot2::scale_color_manual(values = bivariate_color_codes) +
      ggplot2::labs(x = label_x_name, y = label_y_name, color = '') +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(), # Remove x-axis text
        axis.text.y = ggplot2::element_blank(), # Remove y-axis text
        axis.ticks.x = ggplot2::element_blank(), # Remove x-axis ticks
        axis.ticks.y = ggplot2::element_blank(),  # Remove y-axis ticks
        legend.position = "none"
      )
  }
  
  estimate_col_x <- colnames(filtered_test)[3]
  # User specifies the topics to popout
  if (!only_five && !is.null(user_spec_topics)){
    
    popout <- filtered_test %>%
      dplyr::filter(topic %in% user_spec_topics)
  }
  # No user specification
  if (!only_five && is.null(user_spec_topics) && length(num_popout) != 1 && length(num_popout) == 9 && y_axes_1 == 2){
    legend_map_num_pop <- c(
      "1" = num_popout[1], "2" = num_popout[2], "3" = num_popout[3],
      "4" = num_popout[4], "5" = num_popout[5], # 0 if skip non-sig center topics
      "6" = num_popout[6], "7" = num_popout[7], 
      "8" = num_popout[8], "9" = num_popout[9]
    )
    # check if there are too many specificied dots in each grid.
    table1 <- table(filtered_test$color_categories)
    for (i in 1:9){
      if (is.na(names(table1)[i])){next} # longer than the list table1 for lacking categories
      if (as.numeric(names(table1)[i]) != i){next} # skip the categories that not existing in the table1
      if (legend_map_num_pop[[i]] > table1[[i]]){
        msg1 <- paste0('Grid ', as.character(i), ' has only ',
                   table1[[i]], ' popped out topics. Cannot specify ',
                   as.character(legend_map_num_pop[[i]]), 
                   ' topics in it!\n')
        msg1 <- "Cannot save the scatter legend!\n"
        
        message(
          colourise(msg1, "brown"))
        message(
          colourise(msg2, "brown"))
        
        return (NULL)
      }
    }
  }
  if (!only_five && is.null(user_spec_topics) && length(num_popout) != 1 && length(num_popout) == 3 && y_axes_1 == 1){
    legend_map_num_pop <- c(
      "1" = num_popout[1], "2" = num_popout[2], # 0 if skip non-sig center topics
      "3" = num_popout[3]
    )
    # check if there are too many specificied dots in each grid.
    table1 <- table(filtered_test$color_categories)
    for (i in 1:3){
      if (is.na(names(table1)[i])){next} # longer than the list table1 for lacking categories
      if (as.numeric(names(table1)[i]) != i){next} # skip the categories that not existing in the table1
      if (legend_map_num_pop[[i]] > table1[[i]]){
        msg1 <- paste0('Grid ', as.character(i), ' has only ',
                       table1[[i]], ' popped out topics. Cannot specify ',
                       as.character(legend_map_num_pop[[i]]), 
                       ' topics in it!\n')
        msg1 <- "Cannot save the scatter legend!\n"
        
        message(
          colourise(msg1, "brown"))
        message(
          colourise(msg2, "brown"))
        return (NULL)
      }
    }
  }
  if (!only_five && is.null(user_spec_topics) && way_popout_topics == "max_y" && y_axes_1 == 2){
    msg <- "Generating emphasised topics based on 'max_y' in the scatter legend.\n"
    message(
      colourise(msg, "blue"))
    
    estimate_col_y <- colnames(filtered_test)[7]
    if (length(num_popout) > 1){
      popout <- filtered_test %>%
        dplyr::filter(color_categories != 5) %>%
        dplyr::mutate(map_num = dplyr::recode(as.character(color_categories), !!!legend_map_num_pop)) %>%
        dplyr::group_by(color_categories) %>%
        dplyr::group_modify(~ dplyr::slice_max(.x, order_by = abs(!!sym(estimate_col_y)), n = as.integer(.x$map_num[1]), with_ties = FALSE)) %>%
        dplyr::ungroup() # Will change the order of columns
      # Re-arrange the columns to keep the same.
      colname_bak <- names(filtered_test)
      popout <- popout %>%
        dplyr::select(dplyr::all_of(colname_bak), map_num)
    }
    if (length(num_popout) == 1){ # only 1 for num_popout
      popout <- filtered_test %>%
        dplyr::filter(color_categories != 5) %>%
        dplyr::group_by(color_categories) %>%
        dplyr::slice_max(order_by = abs(!!rlang::sym(estimate_col_y)), n = num_popout, with_ties = FALSE) %>%
        dplyr::ungroup()
    }
  }
  if (!only_five && is.null(user_spec_topics) && way_popout_topics == "mean" && y_axes_1 == 2){
    msg <- "Generating the  scatter legend with topics emphasised using max absolute 'mean'. \n"
    
    message(
      colourise(msg, "blue"))
    
    estimate_col_y <- colnames(filtered_test)[7]
    if (length(num_popout) > 1){
      popout <- filtered_test %>%
        dplyr::filter(color_categories != 5) %>%
        dplyr::mutate(map_num = dplyr::recode(as.character(color_categories), !!!legend_map_num_pop)) %>%
        dplyr::group_by(color_categories) %>%
        dplyr::group_modify(~ dplyr::slice_max(.x, order_by = rowMeans(cbind(
          abs(!!rlang::sym(estimate_col_x)), 
          abs(!!rlang::sym(estimate_col_y)))),
          n = as.integer(.x$map_num[1]), with_ties = FALSE)) %>%
        dplyr::ungroup() # Will change the order of columns
      # Re-arrange the columns to keep the same.
      colname_bak <- names(filtered_test)
      popout <- popout %>%
        dplyr::select(dplyr::all_of(colname_bak), map_num)
    }
    if (length(num_popout) == 1){
      popout <- filtered_test %>%
        dplyr::filter(color_categories != 5) %>%
        dplyr::mutate(mean_value = rowMeans(cbind(
          abs(!!rlang::sym(estimate_col_x)), 
          abs(!!rlang::sym(estimate_col_y))))) %>%
        dplyr::group_by(color_categories) %>%
        dplyr::slice_max(order_by = mean_value, n = num_popout, with_ties = FALSE) %>%
        dplyr::ungroup()
    }
  }
  if (!only_five && is.null(user_spec_topics) && way_popout_topics == "max_x" && y_axes_1 == 2){
    # if (y_axes_1 == 2){ # backup # 2dims but plot 1 dim
    #   popout <- filtered_test %>%
    #     dplyr::filter(color_categories != 5) %>%
    #     dplyr::group_by(color_categories) %>%
    #     dplyr::slice_max(order_by = abs(!!rlang::sym(estimate_col_x)), n = num_popout, with_ties = FALSE) %>%
    #     dplyr::ungroup()
    # }else{ # 1dim plot 1 dim
    msg <- "Generating the  scatter legend with topics emphasised using 'max_x'. \n"
    
    message(
      colourise(msg, "blue"))
    
    if (length(num_popout) > 1){
      popout <- filtered_test %>%
        dplyr::filter(color_categories != 5) %>%
        dplyr::mutate(map_num = dplyr::recode(as.character(color_categories), !!!legend_map_num_pop)) %>%
        dplyr::group_by(color_categories) %>%
        dplyr::group_modify(~ dplyr::slice_max(.x, order_by = abs(!!sym(estimate_col_x)), n = as.integer(.x$map_num[1]), with_ties = FALSE)) %>%
        dplyr::ungroup() # Will change the order of columns
      # Re-arrange the columns to keep the same.
      colname_bak <- names(filtered_test)
      popout <- popout %>%
        dplyr::select(dplyr::all_of(colname_bak), map_num)
    }
    if (length(num_popout) == 1){
      popout <- filtered_test %>%
        dplyr::filter(color_categories != 5) %>%
        dplyr::group_by(color_categories) %>%
        dplyr::slice_max(order_by = abs(!!rlang::sym(estimate_col_x)), n = num_popout, with_ties = FALSE) %>%
        dplyr::ungroup()
    }
  }
  if (!only_five && is.null(user_spec_topics) && way_popout_topics == "max_x" && y_axes_1 == 1){
    
    msg <- "Generating the scatter legend with topics emphasised using 'max_x' in 1 dimension. \n"
    
    message(
      colourise(msg, "blue"))
    
    if (length(num_popout) > 1){
      popout <- filtered_test %>%
        dplyr::filter(color_categories != 2) %>%
        dplyr::mutate(map_num = dplyr::recode(as.character(color_categories), !!!legend_map_num_pop)) %>%
        dplyr::group_by(color_categories) %>%
        dplyr::group_modify(~ dplyr::slice_max(.x, order_by = abs(!!sym(estimate_col_x)), n = as.integer(.x$map_num[1]), with_ties = FALSE)) %>%
        dplyr::ungroup() # Will change the order of columns
      # Re-arrange the columns to keep the same.
      colname_bak <- names(filtered_test)
      popout <- popout %>%
        dplyr::select(dplyr::all_of(colname_bak), map_num)
    }
    if (length(num_popout) == 1){
      popout <- filtered_test %>%
        dplyr::filter(color_categories != 2) %>%
        dplyr::group_by(color_categories) %>%
        dplyr::slice_max(order_by = abs(!!rlang::sym(estimate_col_x)), n = num_popout, with_ties = FALSE) %>%
        dplyr::ungroup()
    }
  }
  
  
  # Background dots in the scatter legend; added condition !only_two for version 0.10.1
  if (!only_five & !only_two){
    backgr_dots <- filtered_test %>%
      dplyr::anti_join(popout, by = colnames(filtered_test))
  }

  if(!only_five && y_axes_1 == 2){
    bivariate_color_codes <- bivariate_color_codes
    x_column <- names(filtered_test)[3]
    y_column <- names(filtered_test)[7]
    color_column <- names(filtered_test)[ncol(filtered_test)]
    if (FALSE){
      # Add labels
      # filtered_test <- filtered_test %>%
      #   dplyr::mutate(first_word = stringr::str_extract(top_terms, "^[^,]*"))
      # plot <- ggplot(filtered_test, ggplot2::aes(x = !!rlang::sym(x_column), y = !!rlang::sym(y_column), label = first_word)) +
      #   geom_point(ggplot2::aes(color = as.factor(.data[[color_column]]))) +
      #   geom_text(hjust = 1.5, vjust = 1.5) + # Adjust text position as needed
      # For future:check if 2dim plot only 1dim
      # if (length(strsplit(cor_var, "_")[[1]]) > 1){
      #   bivariate_color_codes <- bivariate_color_codes[4:6]}
    }
    popout <- popout %>%
      dplyr::mutate(topic_number = as.numeric(sub("t_", "", topic)))
    plot <- ggplot2::ggplot() +
      # !!! NOTE: If only 1 topic in a certain category, the ggplot will add a new factor to the popout layer, causing an error of color assignments 
      # Try K = 65 for the sample data. Like 7 is under 6,8,9 in the legend but having the color of 9 in the color_scheme.
      ggplot2::geom_point(data = backgr_dots,
                          ggplot2::aes(x = !!rlang::sym(x_column),
                                       y = !!rlang::sym(y_column),
                                       color = as.factor(.data[[color_column]])),
                          size = scatter_bg_dot_size, alpha = 0.3) +
      ggplot2::geom_point(data = popout,
                          ggplot2::aes(x = !!rlang::sym(x_column),
                                       y = !!rlang::sym(y_column),
                                       color = as.factor(.data[[color_column]])),
                          size = scatter_popout_dot_size, alpha = 0.8) +
      ggplot2::scale_color_manual(values = bivariate_color_codes) +
      ggplot2::labs(x = label_x_name, y = label_y_name, color = '') +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(), # Remove x-axis text
        axis.text.y = ggplot2::element_blank(), # Remove y-axis text
        axis.ticks.x = ggplot2::element_blank(), # Remove x-axis ticks
        axis.ticks.y = ggplot2::element_blank(),  # Remove y-axis ticks
        legend.position = "none"
      )
    # add topic number
    if (allow_topic_num_legend){
      plot <- plot + 
        ggplot2::geom_text(data = popout,
                           ggplot2::aes(x = !!sym(x_column),
                                        y = !!sym(y_column),
                                        label = topic_number),
                           size = scatter_popout_dot_size - 3, hjust = 0.5,vjust = 0.5, color = "black") 
    }
  }  
  # Added !only_two for version 0.10.1
  if(!only_two && y_axes_1 == 1){
    #bivariate_color_codes <- bivariate_color_codes[4:6]
    x_column <- names(filtered_test)[3]
    color_column <- names(filtered_test)[ncol(filtered_test)]
    plot_only3 <- dplyr::filter(tibble::as_tibble(popout,.name_repair="minimal"),
                                color_categories == 1 | color_categories == 2 | color_categories == 3 | color_categories == 4 | color_categories == 5 | color_categories == 6)
    plot_only3_bg <- dplyr::filter(tibble::as_tibble(backgr_dots,.name_repair="minimal"),
                                   color_categories == 1 | color_categories == 2 | color_categories == 3 | color_categories == 4 | color_categories == 5 | color_categories == 6)
    
    plot_only3 <- plot_only3 %>%
      dplyr::mutate(topic_number = as.numeric(sub("t_", "", topic)))
    plot <- ggplot2::ggplot() +
      ggplot2::geom_point(data = plot_only3_bg, 
                          ggplot2::aes(x = !!rlang::sym(x_column), y = 1,
                                       color = as.factor(.data[[color_column]])),
                          size = scatter_bg_dot_size, alpha = 0.3) + 
      ggplot2::geom_point(data = plot_only3, 
                          ggplot2::aes(x = !!rlang::sym(x_column), y = 1,
                                       color = as.factor(.data[[color_column]])), 
                          size = scatter_popout_dot_size, alpha = 0.8) +
      ggplot2::scale_color_manual(values = bivariate_color_codes) +
      ggplot2::labs(x = label_x_name, y = "", color = '') +
      ggplot2::theme_minimal() + 
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(), # Remove x-axis text
        axis.text.y = ggplot2::element_blank(), # Remove y-axis text
        axis.ticks.x = ggplot2::element_blank(), # Remove x-axis ticks
        axis.ticks.y = ggplot2::element_blank(),  # Remove y-axis ticks
        legend.position = "none"
      )
    if (allow_topic_num_legend){
      # add topic number
      plot <- plot + ggplot2::geom_text(data = plot_only3,
                                        ggplot2::aes(x = !!sym(x_column),
                                                     y = 1,
                                                     label = topic_number),
                                        size = scatter_popout_dot_size - 3, hjust = 0.5,vjust = 0.5, color = "black")
    }
  }  

  ggplot2::ggsave(paste0(save_dir,"/seed_", seed, 
                         "/wordclouds/",
                         "dot_legend_",
                         "corvar_", cor_var,
                         ".",
                         figure_format),
                  plot = plot, 
                  width = width, 
                  height = height, 
                  units = "in", 
                  create.dir = TRUE)   
}


#### topicsScatterLegendNew ####
#' @param bivariate_color_codes A vector of color codes specifying colors for 
#' different categories in the scatter plot. 
#' Default: c("#398CF9", "#60A1F7", "#5dc688", "#e07f6a", "#EAEAEA", "#40DD52", "#FF0000", "#EA7467", "#85DB8E").
#' @param filtered_test A data frame containing the input data for the scatter plot. 
#' Must include columns like `color_categories` and other variables used in the function.
#' @param num_popout The number of topics to "pop out" in each category. Default: 1.
#'  Can be a single integer (applies to all categories) or a vector for specific categories.
#' @param way_popout_topics The method for selecting pop-out topics. Options: "mean", "max_y", or "max_x". Default: "mean".
#' @param user_spec_topics A vector of user-specified topics to highlight in the scatter plot. Default: NULL.
#' @param allow_topic_num_legend Logical; if TRUE, displays topic numbers in the legend. Default: FALSE.
#' @param y_axes_1 Specifies axis alignment for the scatter legend. Options: 1 (x-axis) or 2 (y-axis). Default: 2.
#' @param cor_var A string used for naming the correlation variable in labels or file names. Default: "".
#' @param label_x_name Label for the x-axis in the scatter plot. Default: "x".
#' @param label_y_name Label for the y-axis in the scatter plot. Default: "y".
#' @param save_dir Directory where the scatter legend plot will be saved. Default: "./results".
#' @param figure_format File format for the saved scatter plot. Examples: "svg", "png", "pdf". Default: "svg".
#' @param scatter_popout_dot_size Size of the dots for pop-out topics in the scatter legend. Default: 15.
#' @param scatter_bg_dot_size Size of the dots for background topics in the scatter legend. Default: 9.
#' @param width Width of the saved scatter plot in inches. Default: 10.
#' @param height Height of the saved scatter plot in inches. Default: 8.
#' @param seed Seed for reproducibility, ensuring consistent plot generation. Default: 42.
#' @importFrom ggplot2 ggplot geom_point scale_color_manual labs theme_minimal theme element_blank
#' @importFrom rlang sym !!
#' @importFrom dplyr select filter mutate anti_join summarise pull group_by group_modify ungroup
#' @noRd
topicsScatterLegendNew <- function(
    bivariate_color_codes,
    filtered_test, 
    num_popout = 1, 
    way_popout_topics = "mean", 
    user_spec_topics = NULL, 
    allow_topic_num_legend = FALSE,
    y_axes_1 = 2, 
    cor_var = "", 
    label_x_name = "x", 
    label_y_name = "y", 
    save_dir, 
    figure_format = "svg",
    scatter_popout_dot_size = 15, 
    scatter_bg_dot_size = 9, 
    width = 10, 
    height = 8, 
    seed = 42
) {
  
  # Determine x, y, and color columns
  x_column <- names(filtered_test)[3]
  y_column <- if (y_axes_1 == 2) names(filtered_test)[7] else NULL
  color_column <- names(filtered_test)[ncol(filtered_test)]
  
  # Check for only significant or non-significant topics
  contains_category <- function(cat) {
    filtered_test %>%
      summarise(contains_only = all(color_categories %in% cat)) %>%
      pull(contains_only)
  }
  
  only_two <- contains_category(2)  # Non-significant topics
  only_five <- contains_category(5) # Significant topics
  
  # Logic for handling topics to be emphasised in scatter plot
  # User-specified topics for popout.
  if (!is.null(user_spec_topics)) {
    popout <- filtered_test %>% filter(topic %in% user_spec_topics)
    backgr_dots <- filtered_test %>% anti_join(popout, by = colnames(filtered_test))
    
    # Only non-significant topics. Generating scatter legend.
  } else if (only_two && y_axes_1 == 1) {
    popout <- filtered_test %>% filter(color_categories %in% 1:3)
    backgr_dots <- tibble() # No background dots
    
    # Only significant topics. Generating scatter plot.\n
  } else if (only_five && y_axes_1 == 2) {
    popout <- filtered_test
    backgr_dots <- tibble() # No background dots
    
    # Generating scatter plot based on specified popout criteria.\n
  } else {
    popout <- determine_popout_topics(filtered_test, num_popout, way_popout_topics, y_column, x_column)
    
    # Convert `color_categories` in `popout` back to integer
    popout <- popout %>%
      mutate(color_categories = as.integer(color_categories))
    
    # Perform anti_join
    backgr_dots <- filtered_test %>% dplyr::anti_join(popout, by = colnames(filtered_test))
  }
  
  # Generate scatter plot
  plot <- generate_scatter_plot(
    popout = popout,
    background = backgr_dots,
    bivariate_color_codes = bivariate_color_codes,
    x_col = x_column, 
    y_col = y_column, 
    label_x_name = label_x_name, 
    label_y_name = label_y_name, 
    color_col = color_column, 
    popout_size = scatter_popout_dot_size, 
    bg_size = scatter_bg_dot_size, 
    allow_topic_num_legend = allow_topic_num_legend
  )
  
  # Save the plot
  ggsave(paste0(save_dir, "/seed_", seed, 
                "/wordclouds/",
                "Experimental_2_dot_legend_",
                "corvar_", cor_var, ".", 
                figure_format),
         plot = plot, 
         width = width, height = height, units = "in", device = figure_format)
}


#' @param filtered_test A data frame containing the input data, which must include a `color_categories` column. 
#' This column specifies the categories used for determining pop-out topics.
#' @param num_popout A vector of exactly 9 integers, specifying the number of topics to "pop out" for each 
#' category in a 3x3 grid. Each value corresponds to a category in `color_categories`.
#' @param way_popout_topics A string specifying the criterion for selecting pop-out topics. Options:
#'.  - "max_y": Selects topics with the maximum absolute values in the `y_col` column.
#'.  - "max_x": Selects topics with the maximum absolute values in the `x_col` column.
#'.  - "mean": Selects topics based on the highest mean of the absolute values of `x_col` and `y_col`.
#' @param y_col A string specifying the name of the column to be used for `y` values in the selection process.
#' This column must exist in `filtered_test`.
#' @param x_col A string specifying the name of the column to be used for `x` values in the selection process.
#' This column must exist in `filtered_test`.
#' @noRd
determine_popout_topics <- function(
    filtered_test, 
    num_popout, 
    way_popout_topics, 
    y_col, 
    x_col) {
  # Ensure `color_categories` exists
  if (!"color_categories" %in% colnames(filtered_test)) {
    stop("The `filtered_test` dataset must include a `color_categories` column.")
  }
  
  # Convert `color_categories` to character for consistent comparison
  filtered_test <- filtered_test %>%
    mutate(color_categories = as.character(color_categories))
  
  # Check for NA or unexpected values
  if (any(is.na(filtered_test$color_categories))) {
    stop("The `color_categories` column contains missing (NA) values.")
  }
  
  # Ensure `num_popout` has 9 values
  if (length(num_popout) != 9) {
    stop("`num_popout` must have exactly 9 values, one for each quadrant of a 3x3 grid.")
  }
  
  # Map `num_popout` to corresponding categories
  legend_map_num_pop <- setNames(as.integer(num_popout), as.character(1:9))
  
  # Filter for categories present in `filtered_test`
  existing_categories <- unique(filtered_test$color_categories)
  valid_map <- legend_map_num_pop[names(legend_map_num_pop) %in% existing_categories]
  
  if (length(valid_map) == 0) {
    stop("No valid `color_categories` in `filtered_test` match `num_popout` mapping.")
  }
  
  # Process each category based on the popout criteria
  filtered_test %>%
    filter(color_categories %in% names(valid_map)) %>%
    group_by(color_categories) %>%
    group_modify(~ {
      # Access the category directly from `.y`
      category <- .y$color_categories
      
      if (is.na(category) || category == "") {
        warning("Unexpected category format or missing value.")
      }
      
      # Get the number of items to pop out for this category
      n_pop <- valid_map[[category]]
      
      # Handle each `way_popout_topics` criterion
      if (n_pop > 0) {
        if (way_popout_topics == "max_y") {
          dplyr::slice_max(.x, order_by = abs(!!sym(y_col)), n = n_pop, with_ties = FALSE)
        } else if (way_popout_topics == "max_x") {
          dplyr::slice_max(.x, order_by = abs(!!sym(x_col)), n = n_pop, with_ties = FALSE)
        } else if (way_popout_topics == "mean") {
          .x %>%
            dplyr::mutate(mean_value = rowMeans(cbind(
              abs(!!sym(x_col)), 
              abs(!!sym(y_col))
            ))) %>%
            dplyr::slice_max(order_by = mean_value, n = n_pop, with_ties = FALSE)
        } else {
          stop("Invalid `way_popout_topics`. Supported values are 'max_y', 'max_x', or 'mean'.")
        }
      } else {
        .x[0, ]  # Return empty tibble for categories with 0 `n_pop`
      }
    }) %>%
    dplyr::ungroup()
}


#' @param popout A data frame containing the data points to be highlighted ("pop-out") in the scatter plot.
#' @param background A data frame containing the background data points for the scatter plot.
#'                   Can be empty if no background points are needed.
#' @param bivariate_color_codes A vector of color codes used to map `color_col` categories to colors in the scatter plot.
#' @param x_col A string specifying the name of the column to be used for the x-axis in the scatter plot.
#'              Must exist in both `popout` and `background` data frames.
#' @param y_col A string specifying the name of the column to be used for the y-axis in the scatter plot. Default: NULL.
#'              If NULL, a constant value of 1 is used for all points.
#' @param label_x_name Label for the x-axis in the scatter plot.
#' @param label_y_name Label for the y-axis in the scatter plot.
#' @param color_col A string specifying the name of the column in `popout` and `background` used to map categories to colors.
#' @param popout_size The size of the dots for pop-out points in the scatter plot.
#' @param bg_size The size of the dots for background points in the scatter plot.
#' @param allow_topic_num_legend Logical; if TRUE, adds topic numbers as text labels to the pop-out points. Default: FALSE.
#' @noRd
generate_scatter_plot <- function(
    popout,
    background,
    bivariate_color_codes,
    x_col, 
    y_col = NULL,
    label_x_name, 
    label_y_name,
    color_col, 
    popout_size, 
    bg_size, 
    allow_topic_num_legend
) {

  # Define aesthetics for popout and background points
  # Ensure y_col is valid and resolve y_aesthetic
  y_aesthetic <- if (!is.null(y_col) && y_col != "") sym(y_col) else 1
  
  # Create aes with defined y aesthetic
  popout_aes <- aes(
    x = !!sym(x_col),
    y = y_aesthetic,
    color = as.factor(.data[[color_col]])
  )
  
  # Resolve y aesthetic value
  y_value <- if (is.null(y_col)) 1 else sym(y_col)
  
  # Aesthetics for background points
  bg_aes <- if (is.null(y_col)) {
    aes(
      x = !!sym(x_col),
      y = 1,
      color = as.factor(.data[[color_col]])
    )
  } else {
    aes(
      x = !!sym(x_col),
      y = !!sym(y_col),
      color = as.factor(.data[[color_col]])
    )
  }
  
  popout_aes <- if (is.null(y_col)) {
    aes(
      x = !!sym(x_col),
      y = 1,
      color = as.factor(.data[[color_col]])
    )
  } else {
    aes(
      x = !!sym(x_col),
      y = !!sym(y_col),
      color = as.factor(.data[[color_col]])
    )
  }
  
  # Build the plot
  plot <- ggplot()
  
  # Add background points only if background is not empty
  if (nrow(background) > 0) {
    plot <- plot +
      geom_point(data = background, bg_aes, size = bg_size, alpha = 0.3)
  }
  
  # Add popout points
  plot <- plot +
    geom_point(data = popout, popout_aes, size = popout_size, alpha = 0.8) +
    scale_color_manual(values = bivariate_color_codes) +
    labs(x = label_x_name, y = label_y_name, color = '') +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    )
  # Add topic numbers if enabled
  if (allow_topic_num_legend) {
    plot <- plot + geom_text(
      data = popout, 
      aes(x = !!sym(x_col), 
          y = if (is.null(y_col)) 1 else !!sym(y_col), 
          label = topic_number),
      size = popout_size - 3, 
      color = "black", 
      hjust = 0.5, 
      vjust = 0.5
    )
  }
  
  return(plot)
}

#### topicsGridLegend ####
#' @param bivariate_color_codes A vector of color codes specifying the colors for the 3x3 grid legend.
#'                              Default: c("#398CF9", "#60A1F7", "#5dc688", "#e07f6a", "#EAEAEA", "#40DD52", "#FF0000", "#EA7467", "#85DB8E").
#' @param filtered_test A data frame containing the filtered topic data. Must include a `color_categories` column.
#' @param cor_var A string used to name the correlation variable, included in the file name of the saved plot. Default: "".
#' @param save_dir Directory where the grid legend plot will be saved. Default: "./results".
#' @param figure_format File format for the saved grid legend plot. Examples: "svg", "png", "pdf". Default: "svg".
#' @param seed Seed for reproducibility, ensuring consistent plot generation. Default: 42.
#' @param width Width of the saved grid legend in inches. Default: 10.
#' @param height Height of the saved grid legend in inches. Default: 8.
#' @param y_axes_1 Specifies axis alignment for the grid legend. Options: 2 (2D grid with x and y axes) or 1 (1D legend for x-axis only). Default: 2.
#' @param legend_title Title text for the grid legend. Must be provided by the user.
#' @param legend_title_size Font size of the legend title text. Must be provided by the user.
#' @param titles_color Color of the title and axis labels in the legend. Must be provided by the user.
#' @param legend_x_axes_label Label for the x-axis of the grid legend. Must be provided by the user.
#' @param legend_y_axes_label Label for the y-axis of the grid legend. Must be provided by the user.
#' @param topic_data_all A data frame containing all topic data, including the `color_categories` column used for counting topics.
#' @param legend_number_color Color of the numeric annotations in the grid legend. Must be provided by the user.
#' @param legend_number_size Font size of the numeric annotations in the grid legend. Must be provided by the user.' @return A legend plot saved that can be combined with the plot object.
#' @importFrom tidyr gather separate
#' @importFrom dplyr mutate
#' @importFrom ggplot2 geom_tile ggtitle scale_fill_identity labs theme_void annotate theme element_text coord_fixed ggsave
#' @noRd
topicsGridLegend <- function(
    bivariate_color_codes = c(
      "#398CF9", "#60A1F7", "#5dc688",
      "#e07f6a", "#EAEAEA", "#40DD52",
      "#FF0000", "#EA7467", "#85DB8E"),
    filtered_test,
    cor_var = "",
    save_dir,
    figure_format = 'svg',
    seed = 42,
    width = 10, 
    height = 8, 
    y_axes_1 = 2,
    legend_title,
    legend_title_size,
    titles_color,
    legend_x_axes_label,
    legend_y_axes_label,
    topic_data_all,
    legend_number_color,
    legend_number_size
) {
  if (y_axes_1 == 2){y_axes_1 <- ""}else{y_axes_1 <- "only_x_dimension"}
  legCor <- bivariate_color_codes
  if(y_axes_1 == "only_x_dimension"){
    bivariate_color_data <- tibble::tibble(
      "1 - 3" = '', "2 - 3" = '', "3 - 3" = '',
      "1 - 2" = legCor[1], "2 - 2" = legCor[2], "3 - 2" = legCor[3],
      "1 - 1" = '', "2 - 1" = '', "3 - 1" = '')
  }else{bivariate_color_data <- tibble::tibble(
    "1 - 3" = legCor[1], "2 - 3" = legCor[2], "3 - 3" = legCor[3],
    "1 - 2" = legCor[4], "2 - 2" = legCor[5], "3 - 2" = legCor[6],
    "1 - 1" = legCor[7], "2 - 1" = legCor[8], "3 - 1" = legCor[9]
  )}
  bivariate_color_data <- rbind(bivariate_color_data, bivariate_color_codes)
  bivariate_color_data <- bivariate_color_data[-1, ]
  
  if (y_axes_1 == "only_x_dimension") {
    # Only select 3 colors
    bivariate_color_data <- bivariate_color_data[, c(4, 5, 6)]
    colnames(bivariate_color_data) <- c("1 - 2", "2 - 2", "3 - 2")
    #bivariate_color_data
    # Remove the y axes title on the legend
    legend_y_axes_label <- " "}
  # To output the number of categories for dim 1 and dim 2 (plot 1dim or 2dim)
  if (y_axes_1 == "only_x_dimension") {
    # for future only x dim grid in topicsTest
    categoryTotal_x_axes = c(
      sum(topic_data_all$color_categories == 1,
          na.rm = TRUE),
      sum(topic_data_all$color_categories == 2,
          na.rm = TRUE),
      sum(topic_data_all$color_categories == 3,
          na.rm = TRUE))
  }else{ categoryTotal_x_axes = c(
    sum(topic_data_all$color_categories == 4,
        na.rm = TRUE),
    sum(topic_data_all$color_categories == 5,
        na.rm = TRUE),
    sum(topic_data_all$color_categories == 6,
        na.rm = TRUE))}
  
  legend <- bivariate_color_data %>%
    tidyr::gather("group", "fill") %>%
    tidyr::separate(group, into = c("x", "y"), sep = " - ") %>%
    dplyr::mutate(
      x = as.integer(x),
      y = as.integer(y)
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x, y)) +
    ggplot2::geom_tile(ggplot2::aes(fill = fill)) +
    ggplot2::ggtitle(paste0(legend_title)) +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(
      x = legend_x_axes_label,
      y = legend_y_axes_label
    ) +
    ggplot2::theme_void() +
    #    ggplot2::annotate(geom="text", x=2, y=2, label="ns",
    #               color = titles_color, size=legend_number_size)+
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 1, y = 3, label = sum(topic_data_all$color_categories == 1,
                                                   na.rm = TRUE
          ),
          color = legend_number_color, size = legend_number_size#bivariate_color_codes[1]
        )
      }
    } +
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 2, y = 3, label = sum(topic_data_all$color_categories == 2,
                                                   na.rm = TRUE
          ),
          color = legend_number_color, size = legend_number_size
        )
      }
    } +
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 3, y = 3, label = sum(topic_data_all$color_categories == 3,
                                                   na.rm = TRUE
          ),
          color = legend_number_color, size = legend_number_size
        )
      }
    } +
    ggplot2::annotate(
      geom = "text", x = 1, y = 2, label = categoryTotal_x_axes[1],
      color = legend_number_color, size = legend_number_size
    ) +
    ggplot2::annotate(
      geom = "text", x = 2, y = 2, label = categoryTotal_x_axes[2],
      color = legend_number_color, size = legend_number_size
    ) +
    ggplot2::annotate(
      geom = "text", x = 3, y = 2, label = categoryTotal_x_axes[3],
      color = legend_number_color, size = legend_number_size
    ) +
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 1, y = 1, label = sum(topic_data_all$color_categories == 7,
                                                   na.rm = TRUE
          ),
          color = legend_number_color, size = legend_number_size
        )
      }
    } +
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 2, y = 1, label = sum(topic_data_all$color_categories == 8,
                                                   na.rm = TRUE
          ),
          color = legend_number_color, size = legend_number_size
        )
      }
    } +
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 3, y = 1, label = sum(topic_data_all$color_categories == 9,
                                                   na.rm = TRUE
          ),
          color = legend_number_color, size = legend_number_size
        )
      }
    } +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = legend_title_size + 1),
      title = ggplot2::element_text(color = titles_color),
      axis.title.x = ggplot2::element_text(color = titles_color),
      axis.title = ggplot2::element_text(size = legend_title_size),
      axis.title.y = ggplot2::element_text(angle = 90, color = titles_color)
    ) +
    ggplot2::coord_fixed()
  ggplot2::ggsave(paste0(save_dir,"/seed_", seed, 
                         "/wordclouds/",
                         "grid_legend_",
                         "corvar_", cor_var,
                         ".",
                         figure_format),
                  plot = legend, 
                  width = width, 
                  height = height, 
                  units = "in", 
                  create.dir = TRUE)
}

#' The function to create lda wordclouds
#' @param model (list) The trained model
#' @param test (list) The test results
#' @param color_negative_cor (R_obj) The color gradient for negative correlations
#' @param color_positive_cor (R_obj) The color gradient for positive correlations
#' @param grid_pos (numeric) The position for grid topics
#' @param scale_size (logical) Whether to scale the size of the words
#' @param plot_topics_idx (vector) The topics to plot determined by index
#' @param p_threshold (integer) The p-value threshold to use for significance
#' @param save_dir (string) The directory to save the wordclouds
#' @param figure_format (string) Set the figure format, e.g., svg, or png.
#' @param width (integer) The width of the topic (units = "in"). 
#' @param height (integer) The width of the topic (units = "in").
#' @param max_size (integer) The max size of the words.
#' @param seed (integer) The seed to set for reproducibility
#' @return nothing is returned, the wordclouds are saved in the save_dir
#' @noRd
topicsPlot1 <- function(
    model = NULL,
    ngrams = NULL,
    test = NULL,
    color_negative_cor = NULL,
    color_positive_cor = NULL,
    grid_pos = "",
    scale_size = FALSE,
    plot_topics_idx = NULL,
    p_threshold = 0.05,
    save_dir,
    figure_format = "svg",
    width = 10, 
    height = 8,
    max_size = 10, 
    seed = 42){
  
   df_list = NULL

  if (!is.null(model)){
    
    # if model$model_type == "bert_topic" (i.e., a maller model return null on model$model_type)
    if (!is.null(model$model_type)) {
      
      if(!is.null(test)){
        num_topics <- nrow(test$test)
      } else {
        num_topics <- length(model$model$summary$topic)
        model$summary <- model$model$summary
      }
      
      if(num_topics == 0){
        stop("There are no significant topics to plot.")
      }
      
      df_list <- create_df_list_bert_topics(
        save_dir, 
        seed, 
        num_topics)
      
    } else {
    # if from mallet: 
      model <- name_cols_with_vocab(model, "phi", model$vocabulary)
      df_list <- create_topic_words_dfs(model$summary)
      df_list <- assign_phi_to_words(df_list, model$phi, "mallet")
    }
  }
  
  if (!is.null(test) && !is.null(model)){
    summary = model$summary
    cor_var = test$pred_var
    test_type = test$test_method
    test = test$test
    
  }
  
  if (!is.null(model) && is.null(test)){
    summary = model$summary
  }
  
  if (is.null(model) && !is.null(ngrams) && !is.null(test)){
    test = test$test
  }
  
  create_plots(
    df_list = df_list, 
    summary = summary,
    ngrams = ngrams$ngrams,
    test = test, 
    test_type = test_type,
    cor_var = cor_var,
    color_negative_cor = color_negative_cor,
    color_positive_cor = color_positive_cor,
    grid_pos = grid_pos,
    scale_size = scale_size,
    plot_topics_idx = plot_topics_idx,
    p_threshold = p_threshold,
    save_dir = save_dir,
    figure_format = figure_format,
    width = width, 
    height = height,
    max_size = max_size,
    seed = seed)
  
}


#' The function sets default colors or arranges user specified colors
#' @param color_scheme (string or vector of strings) 
#' @return default colors or specified user colours in the right order and structure.
#' @noRd
colour_settings <- function(
    color_scheme, 
    test, 
    ngrams, 
    model, 
    dim){

  bivariate_color_codes <- NULL
  bivariate_color_codes_b <- NULL
  bivariate_color_codes_f <- NULL  
  #### Checking and arranging colors ####
  if(!color_scheme[[1]] == "default"){
    
    # Dim 0 (i.e., no test)
    if(is.null(test)){
      
      if(length(color_scheme) == 2) {
        bivariate_color_codes <- rep(color_scheme, 2)
      } else {
        stop("Please provide 2 colours in the color_scheme parameter or set it to 'default'.")
      }
    }
    
    # Dim 1: N-gram
    if (dim == 1 && !is.null(ngrams)){
      
      if(length(color_scheme) == 4) {
        bivariate_color_codes <- color_scheme
      } else {
        stop("Please provide 4 colours  in the color_scheme parameter or set it to 'default'.")
      }
    }
    
    # Dim 1: topics
    if (dim == 1 && !is.null(model)){
      
      if(length(color_scheme) == 6) {
        # Select every second color for "back" colour in the gradient 
        bivariate_color_codes_b <- color_scheme[seq(1, length(color_scheme), by = 2)]
        # Select every second color for "front" colour in the gradient 
        bivariate_color_codes_f <- color_scheme[seq(2, length(color_scheme), by = 2)]
        
      } else {
        stop("Please provide 6 colours for the gradient.")
      }
    }
    
    # Dim 2: topics
    if (dim == 2 && !is.null(model)){
      
      if(length(color_scheme) == 18) {
        # Select every second color for "back" colour in the gradient 
        bivariate_color_codes_b <- color_scheme[seq(1, length(color_scheme), by = 2)]
        # Select every second color for "front" colour in the gradient 
        bivariate_color_codes_f <- color_scheme[seq(2, length(color_scheme), by = 2)]
        
      } else {
        stop("Please provide 18 colours or use color_scheme = 'default'.")
      }
    }
  }
  
  #### Setting the (default) colours ####
  if (color_scheme[[1]] == "default"){
    
    if (is.null(test)){ # && is.null(ngrams)
      bivariate_color_codes <- c(
        # gradient colours 1 and 2
        "#EAEAEA", "darkblue", 
        "#EAEAEA", "darkblue")
    }
    
    if (dim == 1 && !is.null(ngrams)){
      # gradient pairs
      bivariate_color_codes <- c(
        "#EAEAEA", "darkred", # negative ngrams colours
        "#EAEAEA", "darkgreen" # positve ngrams colours
      )
    }
    
    # Below colours are used in for loop iterations; so easiest to have back and front colors separate
    
    if (dim == 1 && is.null(ngrams)){
      # gradient pairs
      
      # Colors for the "background" words
      bivariate_color_codes_b <- c(
        "#e07f6a",  "lightgray","#5dc688"
      )
      # Colors for the "front" words
      bivariate_color_codes_f <- c(
        "darkred", "darkgray", "darkgreen"
      ) 
    }
    
    if (dim == 2){
      # Colors for the "background" words
      bivariate_color_codes_b <- rep("lightgray", 9)
      
      # Colors for the "front" words 
      bivariate_color_codes_f <-  c(
        "#398CF9",  # quadrant 1 (upper left corner)
        "#60A1F7",  # quadrant 2 
        "#5dc688",  # quadrant 3 (upper right corner)
        "#e07f6a",  # quadrant 4
        "darkgray", # quadrant 5 (middle square)
        "#40DD52",  # quadrant 6 
        "#FF0000",  # quadrant 7 (bottom left corner)
        "#EA7467",  # quadrant 8 
        "#85DB8E")  # quadrant 9 (bottom right corner)
    }
  }
  
  codes <- list(
    bivariate_color_codes,
    bivariate_color_codes_b,
    bivariate_color_codes_f)
  
  return(codes)
}


#' Plot word clouds
#' 
#' This function create word clouds and topic fugures
#' @param model (list) A trained topics model. For examples from topicsModel(). Should be NULL if plotting ngrams.
#' @param ngrams (list) The output from the the topicsGram() function . Should be NULL if plotting topics.
#' @param test (list) The test results; if plotting according to dimension(s) include the object from topicsTest() function. 
#' @param p_threshold (integer) The p-value threshold to use for significance testing.
#' @param color_scheme (string 'default' or vector) The color scheme.
#'  
#' For plots not including a test, the color_scheme should in clude 2 colours (1 gradient pair), such as:
#'
#' c("lightgray", "darkblue)
#' 
#' For 1 dimensional plots of n-grams it should contain 4 colours (2 gradient pairs), such as: 
#'
#' c(
#' "#EAEAEA", "darkred", # negative ngrams colors
#' 
#' "#EAEAEA", "darkgreen" # positve ngrams colors)
#' 
#' 
#' 
#' For 1-dimension plots of topics, it should contain 6 colours (3 gradient pairs), such as 
#'
#'  c(
#' "#EAEAEA", "darkred",     # negative topics colors
#' 
#' "#EAEAEA", "darkgray",     # colours of topics not significantly associated
#' 
#' "#EAEAEA", "darkgreen"     # positve topics colors)
#' 
#' 
#'
#'  For 2-dimensional plots of topics, the color scheme should contain 18 colours (9 gradient pairs), such as:
#'  
#'  c(
#'   "lightgray", "#398CF9",     # quadrant 1 (upper left corner)
#'   
#'   "lightgray", "#60A1F7",     # quadrant 2 
#'   
#'   "lightgray", "#5dc688",     # quadrant 3 (upper right corner)
#'   
#'   "lightgray", "#e07f6a",     # quadrant 4
#'   
#'   "lightgray", "darkgray",     # quadrant 5 (middle square)
#'   
#'   "lightgray", "#40DD52",     # quadrant 6 
#'   
#'   "lightgray", "#FF0000",     # quadrant 7 (bottom left corner)
#'   
#'   "lightgray", "#EA7467",     # quadrant 8 
#'   
#'   "lightgray", "#85DB8E")     # quadrant 9 (bottom right corner)
#'
#' 
#' @param scale_size (logical) Whether to scale the size of the words.
#' @param plot_topics_idx (vector)  The index or indeces of the topics to plot 
#' (e.g., look in the model-object for the indices; can for example, be c(1, 3:5) to plot topic t_1, t_3, t_4 and t_5) (optional). 
#' @param save_dir (string) The directory to save the plots.
#' @param figure_format (string) Set the figure format, e.g., ".svg", or ".png".
#' @param width (integer) The width of the topic (units = "in"). 
#' @param height (integer) The width of the topic (units = "in").
#' @param max_size (integer) The max size of the words.
#' @param seed (integer) The seed to set for reproducibility
#' @param scatter_legend_dot_size (integer) The size of dots in the scatter legend.
#' @param scatter_legend_bg_dot_size (integer) The size of background dots in the scatter legend.
#' @param scatter_legend_n (numeric or vector) A vector determining the number of dots to emphasis in each quadrant of the scatter legend.
#' For example: c(1,1,1,1,0,1,1,1,1) result in one dot in each quadrant except for the middle quadrant. 
#' @param scatter_legend_method (string) The method to filter topics to be emphasised in the scatter legend. 
#' Can be either "mean", "max_x", or "max_y"
#' @param scatter_legend_specified_topics (vector) Specify which topic(s) to be emphasised in the scatter legend. 
#' For example c("t_1", "t_2"). If set, scatter_legend_method will have no effect.
#' @param scatter_legend_topic_n (boolean) Allow showing the topic number or not in the scatter legend
#' @param grid_legend_title The title of grid topic plot.
#' @param grid_legend_title_size The size of the title of the plot.
#' @param grid_legend_title_color The color of the legend title.
#' @param grid_legend_x_axes_label The label of the x axes.
#' @param grid_legend_y_axes_label The label of the y axes.
#' @param grid_legend_number_color The color in the text in the legend.
#' @param grid_legend_number_size The color in the text in the legend.
#' @return The function saves figures in the save_dir.
#' @importFrom dplyr filter
#' @importFrom ggplot2 scale_color_gradient
#' @importFrom tibble as_tibble
#' @export
topicsPlot <- function(
    model = NULL,
    ngrams = NULL,
    test = NULL,
    p_threshold = 0.05, # Why is this set here since the test[[3]]$test$color_categories is determnied in in testTopics test?
    color_scheme = "default",
    scale_size = FALSE,
    plot_topics_idx = NULL,
    save_dir,
    figure_format = "svg",
    width = 10, 
    height = 8,
    max_size = 10, 
    seed = 42,
    scatter_legend_dot_size = 15,
    scatter_legend_bg_dot_size = 9,
    scatter_legend_n = c(1,1,1,1,0,1,1,1,1), 
    scatter_legend_method = c("mean"),
    scatter_legend_specified_topics = NULL,
    scatter_legend_topic_n = FALSE,
    grid_legend_title = "legend_title",
    grid_legend_title_size = 5,
    grid_legend_title_color = 'black',
    grid_legend_x_axes_label = "legend_x_axes_label",
    grid_legend_y_axes_label = "legend_y_axes_label",
    grid_legend_number_color = 'black',
    grid_legend_number_size = 5){
  
  #### Setting the number of dimensions to plot ####

  # If no test is provide set dim to 0 
  if(is.null(test)){
    dim = 0
  }
 
  # If a test is given
  if(!is.null(test)){
    
    # set default to 1 since that works for both n-grams and topics
    dim = 1
  
    # Only set dim to 2 if the test include enough tests
    if(ncol(test[[3]]$test) == 11) {
      dim = 2
    }
  }
  
  #### Setting colors ####
  codes <- colour_settings(
    color_scheme = color_scheme, 
    model = model,
    test = test, 
    ngrams = ngrams, 
    dim = dim)
 
  bivariate_color_codes   <- codes[[1]]
  bivariate_color_codes_b <- codes[[2]]
  bivariate_color_codes_f <- codes[[3]]
  
  #### Controlling parameter settings and giving instructions #####
  if (!is.vector(scatter_legend_n) || !is.numeric(scatter_legend_n)){
      msg <- "The parameter 'scatter_legend_n' should be either a numeric vector or a number.\n"
      message(colourise(msg, "brown"))
      return (NULL)
  }
 
  
  ### Setting colour categories: Selecting elements to plot according to the p_threshold
  
  #### Making the plots ####
  #### Plotting topics from model without at test | ####
  #### Plotting n-grams WIHT test | ### 
  #### Plotting n-grams WIHTOUT test | ####
  if(!is.null(model) & is.null(test) | 
     !is.null(ngrams) && !is.null(test)|
     !is.null(ngrams) && is.null(test)){
    
    topicsPlot1(
      model = model,
      ngrams = ngrams,
      test = test[[1]],
      p_threshold = p_threshold,
      scale_size = scale_size,
      plot_topics_idx = plot_topics_idx,
      color_negative_cor = ggplot2::scale_color_gradient(low = bivariate_color_codes[1], high = bivariate_color_codes[2]), # grey in hex code
      color_positive_cor = ggplot2::scale_color_gradient(low = bivariate_color_codes[3], high = bivariate_color_codes[4]),
      save_dir = save_dir,
      figure_format = figure_format,
      width = width, 
      height = height,
      max_size = max_size, 
      seed = seed
    )
    
  }
  

  #### 1- or 2 dimensional topci-plots ####
  if (!is.null(model) & !is.null(test)){
    
    if (dim == 1){
      #i=1
      for (i in 1:3){
        if (! (i %in% test[[3]]$test$color_categories)){next}
        
        filtered_test <- test[[3]]
        filtered_test$test <- dplyr::filter(
          tibble::as_tibble(filtered_test$test,.name_repair="minimal"),
                                            color_categories == i)
        color_b <- bivariate_color_codes_b[i]
        color_f <- bivariate_color_codes_f[i]
        
        plot_topics_idx <- as.numeric(sub(".*_", "", filtered_test[["test"]]$topic))
        
        topicsPlot1(
          model = model,
          test = filtered_test,
          color_negative_cor = ggplot2::scale_color_gradient(low = color_b, high = color_f),
          color_positive_cor = ggplot2::scale_color_gradient(low = color_b, high = color_f),
          grid_pos = i,
          scale_size = scale_size,
          plot_topics_idx = plot_topics_idx,
          p_threshold = p_threshold,
          save_dir = save_dir,
          figure_format = figure_format,
          width = width, 
          height = height,
          max_size = max_size, 
          seed = seed
        )
      }
    }
    
    if (dim == 2){
      
      for (k in 1:9){
        if (! (k %in% test[[3]]$test$color_categories)){next}
        filtered_test <- test[[3]]
            filtered_test$test <- dplyr::filter(
              tibble::as_tibble(filtered_test$test,.name_repair="minimal"),
                                                color_categories == k)
            color_b <- bivariate_color_codes_b[k]
            color_f <- bivariate_color_codes_f[k]
            
            plot_topics_idx <- as.numeric(sub(".*_", "", filtered_test[["test"]]$topic))
            
            topicsPlot1(
              model = model,
              test = filtered_test,
              color_negative_cor = ggplot2::scale_color_gradient(low = color_b, high = color_f),
              color_positive_cor = ggplot2::scale_color_gradient(low = color_b, high = color_f),
              grid_pos = k,
              scale_size = scale_size,
              plot_topics_idx = plot_topics_idx,
              p_threshold = p_threshold,
              save_dir = save_dir,
              figure_format = figure_format,
              width = width, 
              height = height,
              max_size = max_size, 
              seed = seed
            )
      }
    }
  
      topicsScatterLegendOriginal(
        bivariate_color_codes = bivariate_color_codes_f,
        filtered_test = test[[3]]$test,
        num_popout = scatter_legend_n,
        y_axes_1 = dim,
        cor_var = test[[3]]$pred_var,
        label_x_name = grid_legend_x_axes_label,
        label_y_name = grid_legend_y_axes_label,
        way_popout_topics = scatter_legend_method,
        user_spec_topics = scatter_legend_specified_topics,
        allow_topic_num_legend = scatter_legend_topic_n,
        scatter_popout_dot_size = scatter_legend_dot_size,
        scatter_bg_dot_size = scatter_legend_bg_dot_size,
        save_dir = save_dir,
        figure_format = figure_format,
        # width = 10, 
        # height = 8,
        seed = seed
        )
      
      topicsScatterLegendNew(
        bivariate_color_codes = bivariate_color_codes_f,
        filtered_test = test[[3]]$test,
        num_popout = scatter_legend_n,
        y_axes_1 = dim,
        cor_var = test[[3]]$pred_var,
        label_x_name = grid_legend_x_axes_label,
        label_y_name = grid_legend_y_axes_label,
        way_popout_topics = scatter_legend_method,
        user_spec_topics = scatter_legend_specified_topics,
        allow_topic_num_legend = scatter_legend_topic_n,
        scatter_popout_dot_size = scatter_legend_dot_size,
        scatter_bg_dot_size = scatter_legend_bg_dot_size,
        save_dir = save_dir,
        figure_format = figure_format,
        # width = 10, 
        # height = 8,
        seed = seed
      )
      
      topicsGridLegend(
            bivariate_color_codes = bivariate_color_codes_f,
            filtered_test = test[[3]]$test,
            cor_var = test[[3]]$pred_var,
            save_dir = save_dir,
            figure_format = figure_format,
            seed = seed,
            # width = 10, 
            # height = 8,
            y_axes_1 = dim,
            legend_title = grid_legend_title,
            legend_title_size = grid_legend_title_size,
            titles_color = grid_legend_title_color,
            legend_x_axes_label = grid_legend_x_axes_label,
            legend_y_axes_label = grid_legend_y_axes_label,
            topic_data_all = test[[3]][["test"]],
            legend_number_color = grid_legend_number_color,
            legend_number_size = grid_legend_number_size
          )
          msg <- "The grid plot legends are saved in the save_dir."
          message(colourise(msg, "green"))
  }
  
}



