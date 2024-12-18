
# Function to remove stopwords from a single row of text
#' @param text The data to be be cleaned from stopwords
#' @param stopwords (character) Words to be removed
#' @importFrom dplyr pull select filter mutate anti_join summarise pull group_by group_modify ungroup
#' @noRd
remove_stopwords <- function(
    text, 
    stopwords) {
  
  words <- unlist(strsplit(text, "\\s+"))                 # Split the text into words
  cleaned_words <- words[!tolower(words) %in% stopwords] # Remove stopwords (case insensitive)
  cleaned_text <- paste(cleaned_words, collapse = " ")   # Reconstruct the cleaned text
  
  return(cleaned_text)
}


#' @param ngram_tibble: A tibble with n_grams and freq columns representing n-grams and their frequencies.
#' @param unigram_tibble: A tibble with n_grams and freq columns representing unigrams and their frequencies.
#' @param pmi_threshold: The PMI threshold for filtering n-grams (default is 0).
#' @importFrom dplyr mutate if_else filter 
#' @importFrom tibble as_tibble
#' @importFrom stringr str_detect
#' @importFrom purrr map_dbl
#' @noRd
filter_ngrams_by_pmi <- function(
    ngram_tibble, 
    unigram_tibble, 
    pmi_threshold) {
  
  # Compute total number of terms in the n-gram and unigram tibbles
  total_ngram_terms <- sum(ngram_tibble$freq)
  total_unigram_terms <- sum(unigram_tibble$freq)
  
  # Function to compute the probability of a single word
  compute_word_prob <- function(
    word, 
    unigram_tibble, 
    total_unigram_terms) {
    
    if (word %in% unigram_tibble$ngrams) {
      
      word_freq <- unigram_tibble$freq[unigram_tibble$ngrams == word]
      prob <- word_freq / total_unigram_terms
      return(prob)
      
    } else {
      
      return(1e-10)  # Assign a small probability for unmatched words
    }
  }
  
  # Compute joint probabilities for the n-grams
  ngram_tibble <- ngram_tibble %>%
    dplyr::mutate(joint_prob = freq / total_ngram_terms)
  
  # Compute PMI values, assigning pmi_threshold to unigrams
  total_unigram_terms <- sum(unigram_tibble$freq)
  
  ngram_tibble <- ngram_tibble %>%
    dplyr::mutate(component_prob = purrr::map_dbl(ngrams, function(ngram) {
      if (!stringr::str_detect(ngram, "\\s")) {
        return(NA_real_)  # Return NA for unigrams
      }
      components <- strsplit(ngram, "\\s+")[[1]]
      probs <- purrr::map_dbl(components, ~ compute_word_prob(.x, unigram_tibble, total_unigram_terms))
      component_prob <- prod(probs)
      return(component_prob)
    }))
  ngram_tibble <- tibble::as_tibble(ngram_tibble)

  # Compute PMI values, assigning pmi_threshold directly to unigrams
  ngram_tibble <- ngram_tibble %>%
    dplyr::mutate(
      pmi = 
        log2(joint_prob / pmax(component_prob, 1e-10))  # Compute PMI for n-grams
    )
  # Set the pmi of unigrams to the threshold value since pmi value make no sence for unigrams
  
  if(!is.null(pmi_threshold)){
    ngram_tibble <- ngram_tibble %>%
      dplyr::mutate(pmi = dplyr::if_else(n_gram_type == 1, 
                                         as.numeric(pmi_threshold), pmi))
    # Filter n-grams with PMI values above or equal to the threshold
    filtered_ngrams <- ngram_tibble %>%
      dplyr::filter(pmi >= pmi_threshold) %>%
      dplyr::mutate(ngrams = stringr::str_squish(ngrams)) %>%
      tibble::as_tibble()
  }
  
  if(is.null(pmi_threshold)){
    ngram_tibble <- ngram_tibble %>%
      dplyr::mutate(pmi = dplyr::if_else(n_gram_type == 1, 
                                         NA, pmi))
    # Filter n-grams with PMI values above or equal to the threshold
    filtered_ngrams <- ngram_tibble %>%
      dplyr::mutate(ngrams = stringr::str_squish(ngrams)) %>%
      tibble::as_tibble()
  }
  
  # Compute the stats
  initial_count = table(ngram_tibble$n_gram_type)
  final_count = table(filtered_ngrams$n_gram_type)
  
  stats <-  tibble::tibble(
    ngram_type = unique(ngram_tibble$n_gram_type),
    initial_count = initial_count,
    pmi_removed = initial_count - final_count,
    final_count = final_count
  )
  
  filtered_ngrams <- filtered_ngrams %>% 
    dplyr::select(-c(component_prob, n_gram_type, joint_prob)) 
  
  # Return the result as a list
  return(list(
    filtered_ngrams = filtered_ngrams,
    stats = stats)
    )
}


#' N-grams
#' 
#' The function computes ngrams from a text
#' @param data (tibble) The data
#' @param ngram_window (list) the minimum and maximum n-gram length, e.g. c(1,3)
#' @param stopwords (stopwords) the stopwords to remove, e.g. stopwords::stopwords("en", source = "snowball")
# @param top_n (integer) The number of most occuring ngrams to included in the output for every ngram type
#' @param pmi_threshold (integer) The pmi threshold, if it shall not be used set to 0
#' @return A list containing tibble of the ngrams with the frequency and probability and 
#' a tibble containing the relative frequency of the ngrams for each user
#' @importFrom ngram ngram get.ngrams get.phrasetable
#' @importFrom tibble as_tibble tibble
#' @importFrom stringr str_count str_replace_all str_trim
#' @importFrom dplyr mutate filter 
#' @export
topicsGrams <- function(
    data, 
    ngram_window = c(1, 3),
    stopwords = stopwords::stopwords("en", source = "snowball"), 
    pmi_threshold = 0) {
  
  # Preprocess the data
  data <- tolower(data)
  data <- gsub("[()].$?", "", data)
  data <- sapply(data, remove_stopwords, stopwords)
  data <- data[data != ""]
  data <- na.omit(data)
  
  
  # Initialize an empty list for storing n-grams
  ngrams <- list()
  counts <- c()
  
  # Loop through n-gram sizes and generate n-grams
  for (i in seq_along(ngram_window)) {
    
    filtered_data <- data[sapply(strsplit(data, "\\s+"), length) >= ngram_window[i]]
    
    if (length(filtered_data) > 0) {
      
      ngrams[[i]] <- ngram::get.phrasetable(ngram::ngram(
        filtered_data, 
        n = ngram_window[i], 
        sep = " "))
      
      ngrams[[i]]$n_gram_type <- ngram_window[i]
      counts <- c(counts, nrow(ngrams[[i]]))
      
    } else {
      
      ngrams[[i]] <- NULL
      counts <- c(counts, 0)
      
    }
  }
  
  # Combine all n-grams into a single tibble
  ngrams <- do.call(rbind, ngrams)

  # Compute prevalence
  total <- sum(ngrams$freq)
  ngrams$prevalence <- ngrams$freq / total
  ngrams$coherence <- NA  # Placeholder for coherence; to make the data frame look the same as the dtm output
  
  # Get the stats for unigrams
  single_grams <- ngram::ngram(data, n = 1, sep = " ")
  single_grams <- ngram::get.phrasetable(single_grams)
  colnames(single_grams) <- c("ngrams", "freq", "prevalence")
  
  # Clean the n-grams in bigram_tibble
  single_grams <- single_grams %>%
    dplyr::mutate(ngrams = stringr::str_trim(ngrams))
  
  # Filter n-grams based on PMI
  ngrams <- filter_ngrams_by_pmi(
    ngram_tibble = ngrams, 
    unigram_tibble = single_grams, 
    pmi_threshold = pmi_threshold)
  
  #### Calculate the relative frequency per user ####
  
  # Pre-escape the special characters in all n-grams
  ngrams$filtered_ngrams <- ngrams$filtered_ngrams %>%
    dplyr::mutate(ngrams_escaped = stringr::str_replace_all(
      ngrams, "([.\\^$*+?()\\[\\]{}|])", "\\\\\\1"))
  
  # Initialize an empty list to store results
  freq_per_user <- list()
  
  # Calculation of relative frequencies
  length_i <- nrow(ngrams$filtered_ngrams)
  for (i in 1:length_i) {
    
    gram <- as.character(ngrams$filtered_ngrams$ngrams[i])
    gram_escaped <- ngrams$filtered_ngrams$ngrams_escaped[i]
    frequency <- ngrams$filtered_ngrams$freq[i]
    
    # Counting of n-gram occurrences in all sentences
    ngram_counts <- stringr::str_count(data, gram_escaped)
    
    # Calculate relative frequencies
    relative_frequencies <- ngram_counts / frequency
    
    # Store the result with a clean name for the n-gram
    col_name <- paste(unlist(strsplit(gram, " ")), collapse = "_")
    freq_per_user[[col_name]] <- relative_frequencies
    #print(paste0(i, "/", length_i))
    
  }
  freq_per_user_tbl <- tibble::as_tibble(freq_per_user, .name_repair = "minimal")

  #### change the ngrams to a single string with "_" as connector ####
  ngrams$filtered_ngrams$ngrams <- sapply(ngrams$filtered_ngrams$ngrams, function(x) paste(unlist(strsplit(x, " ")), collapse = "_"))
  
  #### Structure output ####
  res <- list(
    ngrams = tibble::as_tibble(ngrams$filtered_ngrams),
    freq_per_user = freq_per_user_tbl,
    stats = ngrams$stats
  )
  
  return(res)
}

