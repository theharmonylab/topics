
#' N-grams
#' 
#' The function computes ngrams from a text
#' @param data (tibble) The data
#' @param ngram_window (list) the minimum and maximum n-gram length, e.g. c(1,3)
#' @param stopwords (stopwords) the stopwords to remove, e.g. stopwords::stopwords("en", source = "snowball")
#' @param top_n (integer) The number of most occuring ngrams to included in the output for every ngram type
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
    ngram_window = c(1, 3),
    stopwords = stopwords::stopwords("en", source = "snowball"), 
    top_n = NULL, 
    pmi_threshold = 0){
  
  data <- tolower(data)
  data <- gsub("[()].$?", "", data)
  
  # this currently breaks the code, as no ngrams with length 1 can be computed when stopwords are removed
  # data_stopped <- vector("character", length(data))
  # stop_words <- stopwords("en")
  # 
  # for (i in seq_along(data)) {
  #   words <- strsplit(data[i], " ")[[1]]
  #   words_stopped <- words[!tolower(words) %in% stop_words]
  #   data_stopped[i] <- paste(words_stopped, collapse = " ")
  # }
  # data <- data_stopped
  
  ngrams <- list()
  counts <- c()
  
  single_grams <- ngram::ngram(data, n = 1, sep = " ")
  single_grams <- ngram::get.phrasetable(single_grams)
  single_count <- nrow(single_grams)
  colnames(single_grams) <- c("ngrams", "freq", "prevalence")
  
  for (i in 1:length(ngram_window)){
    ngrams[[i]] <- ngram::ngram(data, n = ngram_window[i], sep = " ")
    ngrams[[i]] <- ngram::get.phrasetable(ngrams[[i]])
    counts <- c(counts,nrow(ngrams[[i]]))
  }
  total <- 0
  for (i in 1:length(ngram_window)){
    total <- total + sum(ngrams[[i]]$freq)
  }
  for (i in 1:length(ngram_window)){
    ngrams[[i]]$prevalence <- ngrams[[i]]$freq/total
    # this is to create the same columns in both topicsGrams and topcisModels()
    ngrams[[i]]$coherence <- NA
  }
  
  # calculate the pmi score pmi $($ phrase $)=\log \frac{p(\text { phrase })}{\Pi_{w \in p h r a s e} p(w)}$
  start <- 1
  pmi_threshold <- 0
  if (ngram_window[1] == 1){
    start <- 2
    ngrams[[1]]$pmi <- pmi_threshold
  }
  for (k in start:length(ngram_window)){
    pmi <- list()
    for (i in 1:nrow(ngrams[[k]])) {
      words <- strsplit(ngrams[[k]]$ngrams[i], " ")[[1]]
      denum <- 1
      for (j in 1:length(words)) {
        denum <- denum * single_grams[single_grams$ngrams == paste0(words[j], " "), "prevalence"]
      }
      pmi[[i]] <- log(ngrams[[k]]$prevalence[i]/denum)
    }
    ngrams[[k]]$pmi <- unlist(pmi)
  }
  
  # filter ngrams based on pmi
  if (!is.null(pmi_threshold)){
    removed <- c()
    start <- 1
    if (ngram_window[1] == 1){
      start <- 2
    }
    for (i in start:length(ngram_window)){
      ngrams[[i]] <- ngrams[[i]] %>% dplyr::filter(pmi > pmi_threshold)
      removed <- c(removed, counts[[i]] - nrow(ngrams[[i]]))
    }
    if (ngram_window[1] == 1)
      removed <- c(0, removed)
  } else {
    removed <- rep(0, length(n))
  }
  
  stats <- tibble(
    ngram_type = paste0(ngram_window, "-grams"),           # Create n-gram labels dynamically
    initial_count = counts,  #  initial counts for illustration
    removed_count = removed,    #  removed counts for illustration
    final_count = counts - removed
  )
  # take top n ngrams
  if (!is.null(top_n)){
    for (i in 1:length(ngram_window)){
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
      ngram_count <- stringr::str_count(sentence,gram)
      frequency <- ngrams$freq[i]
      relative_frequency <- ngram_count / frequency
      temp <- c(temp, relative_frequency)
    }
    freq_per_user[[paste(unlist(strsplit(as.character(ngrams$ngrams[i]), " ")), collapse = "_")]] <- temp
  }
  
  # change the ngrams to a single string with "_" as connector
  ngrams$ngrams <- sapply(ngrams$ngrams, function(x) paste(unlist(strsplit(x, " ")), collapse = "_"))
  return(list(
    ngrams = tibble::as_tibble(ngrams),
    freq_per_user = tibble::as_tibble(freq_per_user),
    stats=stats)
  )
  
}

