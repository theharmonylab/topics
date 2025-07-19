# Function to add missing rows to a stats tibble in case one ngram is removed completely
#' @param input_tibble (tibble) A stats tibble with n_grams type column and the number of removed ngrams
#' @param value_list (character) A list of all ngram types
#' @importFrom dplyr bind_rows
#' @importFrom data.table :=
#' @noRd
add_missing_rows <- function(input_tibble, value_list) {
  # Identify the missing values (those not present in the first column of the tibble)

  input_tibble <- as_tibble(as.data.frame.table(input_tibble))
  input_tibble$Var1 <- as.integer(as.character(input_tibble$Var1))
  
  missing_values <- setdiff(value_list, input_tibble[[1]])
  
  # Create a tibble for the missing values with 0 in the second column
  missing_tibble <- tibble::tibble(
    !!colnames(input_tibble)[1] := missing_values,
    !!colnames(input_tibble)[2] := 0
  )
  
  # Combine the original tibble with the missing rows
  output_tibble <- dplyr::bind_rows(input_tibble, missing_tibble)

  # Convert the tibble to a table
  #output_table <- table(output_tibble$Var1, dnn = "Var1")
  #output_table <- table(rep(output_tibble$Var1, output_tibble$Freq))
  output_table <- setNames(output_tibble$Freq, output_tibble$Var1)

  # Set the frequency counts manually using names
  #output_table[as.integer(output_tibble$Var1)] <- output_tibble$Freq

  # Return the combined tibble
  return(output_table)
}

# Function to filter ngrams based on different modes
#' @param ngrams (tibble) A tibble with n_grams and freq columns representing n-grams and their frequencies.
#' @param removal_mode (character) The mode of filtering. Choose from 'term', 'frequency', or 'prevalence'.
#' @param removal_rate_most (numeric) The number of terms to remove from the top or the maximum frequency.
#' @param removal_rate_least (numeric) The number of terms to remove from the bottom or the minimum frequency.
#' @importFrom dplyr arrange filter
#' @noRd
filter_ngrams <- function(
    ngrams, 
    removal_mode, 
    removal_rate_most = NULL, 
    removal_rate_least = NULL) {
  if (removal_mode == "term") {
    
    ngrams_filtered <- ngrams %>%
      dplyr::arrange(desc(freq)) 
    # 1. Remove top n and bottom m terms based on frequency
    if (!is.null(removal_rate_most)){
      ngrams_filtered <- ngrams_filtered[-(1:removal_rate_most), ]
    }
    if (!is.null(removal_rate_least)){
      ngrams_filtered <- ngrams_filtered[-((nrow(ngrams_filtered) - removal_rate_least + 1):nrow(ngrams_filtered)), ]
    }
  } else if (removal_mode == "frequency") {
    # 2. Remove words with frequency over n and under m
    if (is.null(removal_rate_most)){
      removal_rate_most <- max(ngrams$freq)
    }
    if (is.null(removal_rate_least)){
      removal_rate_least <- 0
    }
    ngrams_filtered <- ngrams %>%
        dplyr::filter(freq <= removal_rate_most & freq >= removal_rate_least)
    
    
  } else if (removal_mode == "percentage") {
    # 3. Remove terms that occur in more than n% and less than m% of the time
    if (is.null(removal_rate_most)){
      removal_rate_most <- 0
    } 
    if (is.null(removal_rate_least)){
      removal_rate_least <- 1
    } 
    removal_rate_most <- nrow(ngrams) * removal_rate_most
    removal_rate_least <- nrow(ngrams) - nrow(ngrams) * removal_rate_least
    # take all rows from removal_rate_least to removal_rate_most
    ngrams_filtered <- ngrams[removal_rate_least:removal_rate_most, ]

    #ngrams_filtered <- ngrams %>%
    #    dplyr::filter(prevalence <= removal_rate_most & prevalence >= removal_rate_least)

    
  } else {
    stop("Invalid mode. Choose 'term', 'frequency', or 'prevalence'.")
  }
  # Compute the stats
  initial_count = table(ngrams$n_gram_type)
  final_count = table(ngrams_filtered$n_gram_type)
  
  stats <-  tibble::tibble(
    ngram_type = unique(ngrams$n_gram_type),
    removal_mode_removed = initial_count - final_count
  )
  
  return(list(
    ngrams_filtered = ngrams_filtered, 
    stats = stats))
}



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
  cleaned_text <- stringr::str_squish(cleaned_text) # Removes leading and trailing whitespace; ensures one space between words
  
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
    #initial_count = initial_count,
    pmi_removed = initial_count - final_count
    #final_count = final_count
  )
  
  filtered_ngrams <- filtered_ngrams %>% 
    dplyr::select(-c(component_prob, joint_prob)) 
  
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
#' @param occurance_rate (numerical) The occurance rate (0-1) removes words that occur less then in (occurance_rate)*(number of documents). Example: If the training dataset has 1000 documents and the occurrence rate is set to 0.05, the code will remove terms that appear in less than 50 documents. 
#' @param removal_mode (character) The mode of removal, either "term", frequency" or "percentage"
#' @param removal_rate_most (numeric) The rate of most frequent ngrams to remove
#' @param removal_rate_least (numeric) The rate of least frequent ngrams to remove
#' @param top_frequent (integer) The number of most frequently occuring ngrams to included in the output.
#' @param pmi_threshold (integer) The pmi threshold, if it shall not be used set to 0
#' @return A list containing tibble of the ngrams with the frequency and probability and 
#' a tibble containing the relative frequency of the ngrams for each user
#' @importFrom ngram ngram get.ngrams get.phrasetable
#' @importFrom tibble as_tibble tibble
#' @importFrom stringr str_count str_replace_all str_trim
#' @importFrom dplyr mutate filter  slice_head all_of
#' @importFrom stats na.omit
#' @export
topicsGrams <- function(
    data, 
    ngram_window = c(1, 3),
    stopwords = stopwords::stopwords("en", source = "snowball"), 
    occurance_rate = 0,
    removal_mode = "frequency",
    removal_rate_most = NULL,
    removal_rate_least = NULL,
    pmi_threshold = 0, 
    top_frequent = 200) {
  
  # Preprocess the data
  data <- tolower(data)
  data <- gsub("[()].$?", "", data)
  data <- sapply(data, remove_stopwords, stopwords)
  
  data_cleaned <- data[data != ""]
  data_cleaned <- stats::na.omit(data_cleaned)
  
  
  # Initialize an empty list for storing n-grams
  ngrams <- list()
  counts <- c()
  
  # Loop through n-gram sizes and generate n-grams 
  for (i in seq_along(ngram_window)) {
    
    filtered_data <- data_cleaned[sapply(strsplit(data_cleaned, "\\s+"), length) >= ngram_window[i]]
    
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
  ngram_types <- ngrams$n_gram_type
  

  # Compute prevalence
  total <- sum(ngrams$freq)
  ngrams$prevalence <- ngrams$freq / total
  ngrams$coherence <- NA  # Placeholder for coherence; to make the data frame look the same as the dtm output
  
  # Removing prop to not confue with prevalence (prop is f)
  ngrams$prop <- NULL
  
  # Get number of documents for each ngram
  # doc_count <- docs_per_ngram(ngrams, data_cleaned)
  # Create a data frame with ngrams and their sums
  # docs_per_ngram <- data.frame(
  #   ngrams = names(doc_count),
  #   docs = as.integer(doc_count)
  # )
  # ngrams <- merge(ngrams, doc_count, by = "ngrams")
  # if (occurance_rate > 0){
  #   removal_frequency <- round(nrow(ngrams) * occurance_rate) - 1
  #   ngrams <- ngrams %>%
  #     dplyr::filter(docs >= removal_frequency)
  # }
  initial_count <- table(ngrams$n_gram_type)

  ngrams <- filter_ngrams(ngrams = ngrams,
                          removal_mode = removal_mode,
                          removal_rate_most = removal_rate_most,
                          removal_rate_least = removal_rate_least)
  filter_stats <- ngrams$stats
  ngrams <- ngrams$ngrams_filtered
  
  
  # Get the stats for unigrams
  single_grams <- ngram::ngram(data_cleaned, n = 1, sep = " ")
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
  
  pmi_stats <- ngrams$stats
  
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
    # ngram_counts <- stringr::str_count(data, gram_escaped) # version .53
    ngram_counts <- stringr::str_count(data, paste0("\\b", gram_escaped, "\\b"))

    
    # Calculate relative frequencies
    relative_frequencies <- ngram_counts / frequency
    
    # Store the result with a clean name for the n-gram
    col_name <- paste(unlist(strsplit(gram, " ")), collapse = "_")
    freq_per_user[[col_name]] <- relative_frequencies
    
  }
  freq_per_user_tbl <- tibble::as_tibble(freq_per_user, .name_repair = "minimal")
  
  # Calculate the number of documents where each n-gram occurs
  num_docs <- colSums(freq_per_user_tbl != 0)

  # Create a new data frame with the non-zero counts
  result_df <- tibble::tibble(
    ngrams_escaped = ngrams$filtered_ngrams$ngrams_escaped,
    num_docs = num_docs
  )

  ngrams$filtered_ngrams <- merge(ngrams$filtered_ngrams, result_df, by = "ngrams_escaped")
  
  # Filter n-grams based on their occurance in the documents
  occurance_before <- table(ngrams$filtered_ngrams$n_gram_type)
  occurance_before <- add_missing_rows(occurance_before, ngram_window)
  if (occurance_rate > 0){
    removal_frequency <- round(length(data) * occurance_rate)-1
    ngrams$filtered_ngrams <- ngrams$filtered_ngrams %>%
      dplyr::filter(num_docs >= removal_frequency)
  }
  occurance_after <- table(ngrams$filtered_ngrams$n_gram_type)
  occurance_after <- add_missing_rows(occurance_after, ngram_window)
  

  # Calculate the stats for the n-grams
  occurance_stats <-  tibble::tibble(
    ngram_type = unique(ngram_types),
    occurance_removed = occurance_before - occurance_after,
  )

  final_count <- initial_count - occurance_stats$occurance_removed - filter_stats$removal_mode_removed - pmi_stats$pmi_removed
  

  stats <- merge(pmi_stats, filter_stats, by = "ngram_type")
  ngrams$stats <- merge(stats, occurance_stats, by = "ngram_type")
  ngrams$stats$initial_count <- initial_count
  ngrams$stats$final_count <- final_count
  ngrams$stats <- ngrams$stats[, c("ngram_type", "initial_count", "pmi_removed", "removal_mode_removed", "occurance_removed", "final_count")]
  
  
  # sort the ngrams by frequency
  ngrams$filtered_ngrams <- ngrams$filtered_ngrams %>% 
    dplyr::arrange(desc(freq))
  
  # Select the top most frequent n-grams (to limit testing too many in topicsTest)
  if (!is.null(top_frequent)){
   
    ngrams$filtered_ngrams <- ngrams$filtered_ngrams %>% 
      dplyr::slice_head(n=top_frequent)
  }
  
  ngrams$filtered_ngrams <- ngrams$filtered_ngrams %>% 
    dplyr::select(-c(n_gram_type))
  
  #### change the ngrams to a single string with "_" as connector ####
  ngrams$filtered_ngrams$ngrams <- sapply(ngrams$filtered_ngrams$ngrams, function(x) paste(unlist(strsplit(x, " ")), collapse = "_"))
  
  # Recompute prevalence after filtering
  total <- sum(ngrams$filtered_ngrams$freq)
  ngrams$filtered_ngrams$prevalence <- ngrams$filtered_ngrams$freq / total
  
  # Filter the columns of freq_per_user_tbl to only include the filtered n-grams
  freq_per_user_tbl <- freq_per_user_tbl %>%
    select(dplyr::all_of(ngrams$filtered_ngrams$ngrams))
  
  #### Structure output ####
  res <- list(
    ngrams = tibble::as_tibble(ngrams$filtered_ngrams),
    freq_per_user = freq_per_user_tbl,
    stats = ngrams$stats
  )
  
  return(res)
}

