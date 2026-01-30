# Function to add missing rows to a stats tibble in case one ngram is removed completely
#' @param input_tibble (tibble) A stats tibble with n_grams type column and the number of removed ngrams
#' @param value_list (character) A list of all ngram types
#' @importFrom dplyr bind_rows
#' @importFrom data.table :=
#' @noRd
add_missing_rows <- function(vec, full_range) {
  # 1. Handle NULLs immediately
  if (is.null(vec) || length(vec) == 0) {
    return(rep(0, length(full_range)))
  }
  
  # 2. Ensure we are working with a plain numeric vector
  # unname() strips the "1", "2", "3" labels that cause the dimnames error
  clean_vec <- as.numeric(unname(vec))
  
  # 3. If the vector is already the right length, just return it
  if (length(clean_vec) == length(full_range)) {
    return(clean_vec)
  }
  
  # 4. If it's shorter (e.g., missing 3-grams), pad it with 0s
  # (This assumes your input vec is always in order of ngram_type)
  length_diff <- length(full_range) - length(clean_vec)
  return(c(clean_vec, rep(0, length_diff)))
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
    removal_mode = c("frequency", "percentage", "term"),
    removal_rate_most = NULL,
    removal_rate_least = NULL
) {
  removal_mode <- match.arg(removal_mode)
  
  # Work per n-gram type (so 1-grams and 3-grams are filtered comparably)
  split_list <- split(ngrams, ngrams$n_gram_type)
  before <- vapply(split_list, nrow, integer(1))
  
  filtered_list <- lapply(split_list, function(df) {
    if (nrow(df) == 0) return(df)
    
    # sort by frequency (high -> low)
    df <- dplyr::arrange(df, dplyr::desc(freq))
    n <- nrow(df)
    
    if (removal_mode == "percentage") {
      k_most  <- if (!is.null(removal_rate_most)  && removal_rate_most  > 0) ceiling(n * removal_rate_most)  else 0L
      k_least <- if (!is.null(removal_rate_least) && removal_rate_least > 0) ceiling(n * removal_rate_least) else 0L
      
      keep <- rep(TRUE, n)
      
      # drop most frequent (top rows)
      if (k_most > 0) {
        k_most <- min(k_most, n)
        keep[seq_len(k_most)] <- FALSE
      }
      
      # drop least frequent (bottom rows)
      if (k_least > 0) {
        k_least <- min(k_least, sum(keep))
        idx <- which(keep)
        drop_idx <- tail(idx, k_least)
        keep[drop_idx] <- FALSE
      }
      
      df <- df[keep, , drop = FALSE]
      
    } else if (removal_mode == "term") {
      k_most  <- if (!is.null(removal_rate_most)  && removal_rate_most  > 0) as.integer(removal_rate_most)  else 0L
      k_least <- if (!is.null(removal_rate_least) && removal_rate_least > 0) as.integer(removal_rate_least) else 0L
      
      keep <- rep(TRUE, n)
      
      if (k_most > 0) {
        k_most <- min(k_most, n)
        keep[seq_len(k_most)] <- FALSE
      }
      
      if (k_least > 0) {
        k_least <- min(k_least, sum(keep))
        idx <- which(keep)
        drop_idx <- tail(idx, k_least)
        keep[drop_idx] <- FALSE
      }
      
      df <- df[keep, , drop = FALSE]
      
    } else if (removal_mode == "frequency") {
      # Here rates are thresholds, not percentages
      if (!is.null(removal_rate_least)) {
        df <- df[df$freq > removal_rate_least, , drop = FALSE]
      }
      if (!is.null(removal_rate_most)) {
        df <- df[df$freq < removal_rate_most, , drop = FALSE]
      }
    }
    
    df
  })
  
  ngrams_filtered <- dplyr::bind_rows(filtered_list)
  after <- table(ngrams_filtered$n_gram_type)
  
  # build stats
  ngram_types <- sort(unique(ngrams$n_gram_type))
  after_vec <- as.integer(after[as.character(ngram_types)])
  after_vec[is.na(after_vec)] <- 0L
  before_vec <- as.integer(before[as.character(ngram_types)])
  before_vec[is.na(before_vec)] <- 0L
  
  stats <- tibble::tibble(
    ngram_type = as.integer(ngram_types),
    removal_mode_removed = as.integer(before_vec - after_vec)
  )
  
  list(
    ngrams_filtered = ngrams_filtered,
    stats = stats
  )
}
# #filter_ngrams <- function(
# #    ngrams, 
# #    removal_mode, 
# #    removal_rate_most = NULL, 
# #    removal_rate_least = NULL) {
# #  if (removal_mode == "term") {
# #    
# #    ngrams_filtered <- ngrams %>%
# #      dplyr::arrange(desc(freq)) 
# #    # 1. Remove top n and bottom m terms based on frequency
# #    if (!is.null(removal_rate_most)){
# #      ngrams_filtered <- ngrams_filtered[-(1:removal_rate_most), ]
# #    }
# #    if (!is.null(removal_rate_least)){
# #      ngrams_filtered <- ngrams_filtered[-((nrow(ngrams_filtered) - removal_rate_least + 1):nrow(ngrams_filtered)), ]
# #    }
# #  } else if (removal_mode == "frequency") {
# #    # 2. Remove words with frequency over n and under m
# #    if (is.null(removal_rate_most)){
# #      removal_rate_most <- max(ngrams$freq)
# #    }
# #    if (is.null(removal_rate_least)){
# #      removal_rate_least <- 0
# #    }
# #    ngrams_filtered <- ngrams %>%
# #        dplyr::filter(freq <= removal_rate_most & freq >= removal_rate_least)
# #    
# #    
# #  } else if (removal_mode == "percentage") {
# #    # 3. Remove terms that occur in more than n% and less than m% of the time
# #    if (is.null(removal_rate_most)){
# #      removal_rate_most <- 0
# #    } 
# #    if (is.null(removal_rate_least)){
# #      removal_rate_least <- 1
# #    } 
# #    removal_rate_most <- nrow(ngrams) * removal_rate_most
# #    removal_rate_least <- nrow(ngrams) - nrow(ngrams) * removal_rate_least
# #    # take all rows from removal_rate_least to removal_rate_most
# #    ngrams_filtered <- ngrams[removal_rate_least:removal_rate_most, ]
# #
# #    #ngrams_filtered <- ngrams %>%
# #    #    dplyr::filter(prevalence <= removal_rate_most & prevalence >= removal_rate_least)
# #
# #    
# #  } else {
# #    stop("Invalid mode. Choose 'term', 'frequency', or 'prevalence'.")
# #  }
# #  # Compute the stats
# #  initial_count = table(ngrams$n_gram_type)
# #  final_count = table(ngrams_filtered$n_gram_type)
# #  
# #  stats <-  tibble::tibble(
# #    ngram_type = unique(ngrams$n_gram_type),
# #    removal_mode_removed = initial_count - final_count
# #  )
# #  
# #  return(list(
# #    ngrams_filtered = ngrams_filtered, 
# #    stats = stats))
# #}



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
filter_ngrams_by_pmi <- function(ngram_tibble, unigram_tibble, pmi_threshold) {
  if(nrow(ngram_tibble) == 0) return(list(filtered_ngrams=ngram_tibble, stats=NULL))
  
  # 1. Use the same "Total N" for both to keep probabilities on the same scale
  # Usually, this is the total number of tokens (unigrams) in the corpus
  total_tokens <- sum(unigram_tibble$freq)
  
  # 2. Probability lookup for individual words
  uni_lookup <- setNames(unigram_tibble$freq / total_tokens, unigram_tibble$ngrams)
  
  # 3. Probability of the n-gram occurring
  # NOTE: Use total_tokens here as well to normalize
  ngram_tibble$joint_prob <- ngram_tibble$freq / total_tokens
  
  # 4. Calculate the product of individual probabilities
  ngram_tibble$comp_prob <- sapply(strsplit(as.character(ngram_tibble$ngrams), "\\s+"), function(w) {
    p <- uni_lookup[w]
    p[is.na(p)] <- 1e-10 # Floor for missing words
    prod(p)
  })
  
  # 5. The Formula: log2( P(xy) / (P(x)P(y)) )
  ngram_tibble$pmi <- log2(ngram_tibble$joint_prob / pmax(ngram_tibble$comp_prob, 1e-10))
  
  # 6. FIX: Handle Unigrams
  # We should probably set them to NA or a value that indicates "Not Applicable"
  # but ensures they survive the filter.
  is_uni <- ngram_tibble$n_gram_type == 1
  ngram_tibble$pmi[is_uni] <- Inf # Unigrams always pass
  
  # 7. Apply the filter
  filtered <- if(!is.null(pmi_threshold)) {
    ngram_tibble[ngram_tibble$pmi >= pmi_threshold, ]
  } else {
    ngram_tibble
  }
  
  # Clean up Inf for the final display
  filtered$pmi[is.infinite(filtered$pmi)] <- NA
  
  # 8. Stats
  types <- sort(unique(ngram_tibble$n_gram_type))
  stats <- tibble::tibble(
    ngram_type = types,
    pmi_removed = as.numeric(unname(table(factor(ngram_tibble$n_gram_type, levels=types)) - 
                                      table(factor(filtered$n_gram_type, levels=types))))
  )
  
  return(list(filtered_ngrams = filtered[, !names(filtered) %in% c("joint_prob", "comp_prob")], stats = stats))
}


#' N-grams
#'
#' Compute n-grams and per-document relative frequencies from text.
#'
#' This function extracts n-grams using  and returns:
#' (1) a tibble of n-grams with frequency/prevalence and document frequency,
#' (2) per-document relative frequencies for the retained n-grams (wide or long format),
#' (3) filtering statistics,
#' (4) a named list of settings (for reproducibility).
#'
#' @param data A character vector of texts or a data.frame/tibble (first column used as text).
#' @param ngram_window Integer vector specifying n-gram sizes.
#'   If length 1, only that n is used (e.g., \code{2}).
#'   If length 2 and increasing, it is interpreted as an inclusive range (e.g., \code{c(1, 3)} -> 1,2,3).
#'   Otherwise it is treated as an explicit set (e.g., \code{c(1, 3)} means only 1 and 3).
#' @param stopwords Character vector of stopwords to remove (e.g.,
#'   \code{stopwords::stopwords("en", source = "snowball")}).
#' @param removalword Character vector of words to remove from the raw text prior to tokenization.
#' @param pmi_threshold Numeric PMI threshold used to filter multi-word n-grams.
#'   If \code{NULL}, PMI filtering is skipped.
#' @param occurance_rate Numeric in [0,1]. Terms are removed if they occur in fewer than
#'   \code{round(N_docs * occurance_rate) - 1} documents, where \code{N_docs} is the number of texts.
#'   Example: with 1000 documents and \code{occurance_rate = 0.05}, terms occurring in fewer than ~50 documents are removed.
#' @param removal_mode Character. Removal mode passed to \code{filter_ngrams()}.
#'   Common options include \code{"frequency"} and \code{"percentage"} (depending on your helper implementation).
#' @param removal_rate_most Numeric. Rate/threshold for removing the most frequent n-grams
#'   (interpreted by \code{filter_ngrams()} according to \code{removal_mode}).
#' @param removal_rate_least Numeric. Rate/threshold for removing the least frequent n-grams
#'   (interpreted by \code{filter_ngrams()} according to \code{removal_mode}).
#' @param shuffle Logical. If \code{TRUE}, texts are shuffled (seed-controlled) before processing.
#'   The original ids are preserved in output.
#' @param lower Logical. If \code{TRUE}, text is lowercased prior to tokenization.
#' @param remove_punctuation Logical. Passed to \code{quanteda::tokens(remove_punct = ...)}.
#' @param remove_numbers Logical. Passed to \code{quanteda::tokens(remove_numbers = ...)}.
#' @param stem_lemma_function Optional function applied to tokens (via \code{quanteda::tokens_apply()}).
#'   Should accept and return a character vector of tokens.
#' @param verbose Logical. If \code{TRUE}, prints simple progress messages (if implemented).
#' @param seed Integer. Random seed used when \code{shuffle = TRUE}.
#' @param threads Integer. Number of threads used by \pkg{quanteda} via \code{quanteda_options(threads = ...)}.
#' @param top_frequent Integer or \code{NULL}. If set, keeps only the \code{top_frequent} most frequent n-grams
#'   after filtering (recommended for large corpora). If \code{NULL}, keeps all retained n-grams.
#' @param freq_per_user_format Output format for \code{freq_per_user}:
#'   \describe{
#'     \item{\code{"wide"}}{Wide document-by-ngram table (may be large).}
#'     \item{\code{"long"}}{Long sparse table with columns \code{id}, \code{ngram}, \code{rel_freq}.}
#'     \item{\code{"auto"}}{Uses wide format only if \code{N_docs * N_ngrams <= max_wide_cells}; otherwise returns long.}
#'   }
#' @param max_wide_cells Numeric. Safety limit for \code{"auto"}: if \code{N_docs * N_ngrams} exceeds this,
#'   \code{freq_per_user} is returned in long format to avoid dense allocation and excessive memory use.
#'
#' @return A named list with:
#' \describe{
#'   \item{settings}{A named list of settings used to generate the results (for reproducibility).}
#'   \item{n_grams_pmi}{PMI information if available from your PMI helper, otherwise a message.}
#'   \item{ngrams}{A tibble of retained n-grams and statistics (e.g., \code{freq}, \code{prevalence}, \code{num_docs}).}
#'   \item{freq_per_user}{Per-document relative frequencies, either wide or long depending on \code{freq_per_user_format}.}
#'   \item{stats}{A tibble summarizing how many n-grams were removed by each filtering step, by n-gram size.}
#' }
#' @importFrom quanteda tokens tokens_remove tokens_ngrams dfm featnames dfm_select quanteda_options
#' @importFrom Matrix colSums
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr arrange bind_cols filter select slice_head
#' @importFrom stats complete.cases
#' @export
topicsGrams <- function(
    data,
    ngram_window = c(1, 3),
    stopwords = stopwords::stopwords("en", source = "snowball"),
    removalword = "",
    pmi_threshold = NULL,
    occurance_rate = 0,
    removal_mode = "frequency",
    removal_rate_most = NULL,
    removal_rate_least = NULL,
    shuffle = TRUE,
    lower = TRUE,
    remove_punctuation = TRUE,
    remove_numbers = TRUE,
    stem_lemma_function = NULL,
    verbose = FALSE,
    seed = 42L,
    threads = 1,
    top_frequent = 200,
    freq_per_user_format = c("auto", "wide", "long"),
    max_wide_cells = 5e7
    ) {
      
      freq_per_user_format <- match.arg(freq_per_user_format)
      
      # ---- harmonize ngram_window behavior (like topicsDtm) ----
      # - if length 1 -> same min/max
      # - if length 2 and increasing -> inclusive range
      # - else treat as explicit set
      if (length(ngram_window) == 1) {
        ngram_sizes <- ngram_window
      } else if (length(ngram_window) == 2 && isTRUE(ngram_window[2] >= ngram_window[1])) {
        ngram_sizes <- seq(ngram_window[1], ngram_window[2])
      } else {
        ngram_sizes <- sort(unique(ngram_window))
      }
      ngram_sizes <- as.integer(ngram_sizes)
      if (any(ngram_sizes < 1)) stop("ngram_window must contain values >= 1")
      
      # ---- settings ----
      settings <- list(
        ngram_window = ngram_window,
        ngram_sizes = ngram_sizes,
        stopwords = stopwords,
        removalword = removalword,
        pmi_threshold = pmi_threshold,
        occurance_rate = occurance_rate,
        removal_mode = removal_mode,
        removal_rate_most = removal_rate_most,
        removal_rate_least = removal_rate_least,
        shuffle = shuffle,
        lower = lower,
        remove_punctuation = remove_punctuation,
        remove_numbers = remove_numbers,
        stem_lemma_function = stem_lemma_function,
        verbose = verbose,
        seed = seed,
        threads = threads,
        top_frequent = top_frequent,
        freq_per_user_format = freq_per_user_format,
        max_wide_cells = max_wide_cells
      )
      
      # ---- input handling ----
      if (length(data) == 0) {
        if (verbose) message("The data provided is empty.")
        return(NULL)
      }
      if (is.data.frame(data)) data <- data[, 1]
      
      set.seed(seed)
      
      text_cols <- tibble::tibble(
        id = seq_along(data),
        text = as.character(data)
      )
      text_cols <- text_cols[stats::complete.cases(text_cols), , drop = FALSE]
      text_cols <- text_cols[text_cols$text != "", , drop = FALSE]
      
      if (nrow(text_cols) == 0) {
        return(list(
          settings = settings,
          ngrams = tibble::tibble(),
          freq_per_user = tibble::tibble(),
          stats = tibble::tibble(),
          n_grams_pmi = "No texts after cleaning."
        ))
      }
      
      # shuffle like topicsDtm (but keep id so you can map back)
      if (shuffle) {
        text_cols <- text_cols[sample.int(nrow(text_cols)), , drop = FALSE]
      }
      
      # removalword (same idea as topicsDtm)
      if (length(removalword) > 0 && any(removalword != "")) {
        rw <- removalword
        if (lower) rw <- stringi::stri_trans_tolower(rw)
        pattern <- paste0("\\b(", paste(rw, collapse = "|"), ")\\b")
        text_cols$text <- gsub(pattern, "", text_cols$text, perl = TRUE)
      }
      
      #  lowercase (optional)
      if (lower) {
        text_cols$text <- stringi::stri_trans_tolower(text_cols$text)
      }
      
      # quanteda threads (restore on exit) 
      old_threads <- tryCatch(quanteda::quanteda_options("threads"), error = function(e) NULL)
      quanteda::quanteda_options(threads = threads)
      on.exit({
        if (!is.null(old_threads)) {
          quanteda::quanteda_options(threads = old_threads)
        }
      }, add = TRUE)
      
      # tokenize once 
      toks <- quanteda::tokens(
        text_cols$text,
        what = "word",
        remove_punct = remove_punctuation,
        remove_numbers = remove_numbers,
        remove_symbols = TRUE,
        remove_separators = TRUE
      )
      
      if (length(stopwords) > 0) {
        toks <- quanteda::tokens_remove(toks, pattern = stopwords)
      }
      
      # optional stemming/lemmatization hook (user-provided function)
#      # expected: function(character_vector) -> character_vector
#      if (!is.null(stem_lemma_function)) {
#        toks <- quanteda::tokens_apply(toks, FUN = stem_lemma_function)
#      }
#      
      # build document–feature matrix (dfm) for each requested n and bind columns (base::cbind works) ----
      dfm_list <- vector("list", length(ngram_sizes))
      ngrams_list <- vector("list", length(ngram_sizes))
      
      for (i in seq_along(ngram_sizes)) {
        n <- ngram_sizes[i]
        toks_n <- quanteda::tokens_ngrams(toks, n = n, concatenator = "_")
        dfm_n <- quanteda::dfm(toks_n)
        
        # feature frequencies (sparse-safe)
        freq <- Matrix::colSums(dfm_n)
        
        ngrams_list[[i]] <- tibble::tibble(
          ngrams = names(freq),                # underscore form (matches your end format)
          freq = as.numeric(freq),
          n_gram_type = n
        )
        dfm_list[[i]] <- dfm_n
      }
      
      ngrams <- dplyr::bind_rows(ngrams_list)
      if (nrow(ngrams) == 0) {
        return(list(
          settings = settings,
          ngrams = tibble::tibble(),
          freq_per_user = tibble::tibble(),
          stats = tibble::tibble(),
          n_grams_pmi = if (!is.null(pmi_threshold)) tibble::tibble() else "No pmi_threshold was used"
        ))
      }
      
      # combined dfm across n
      dfm_all <- do.call(base::cbind, dfm_list)
      
      # prevalence + placeholder coherence
      total <- sum(ngrams$freq)
      ngrams$prevalence <- ngrams$freq / total
      ngrams$coherence <- NA_real_
      
      ngram_types_seen <- ngrams$n_gram_type
      initial_count <- table(ngrams$n_gram_type)
      
      # filter_ngrams 
      tmp <- filter_ngrams(
        ngrams = ngrams,
        removal_mode = removal_mode,
        removal_rate_most = removal_rate_most,
        removal_rate_least = removal_rate_least
      )
      filter_stats <- tmp$stats
      ngrams_filtered <- tmp$ngrams_filtered
      
      # PMI filtering (your helper uses spaces; convert just for PMI step) ----
      pmi_tibble <- NULL
      
      if (!is.null(pmi_threshold)) {
        
        # unigrams for PMI
        toks_1 <- quanteda::tokens_ngrams(toks, n = 1, concatenator = "_")
        dfm_1 <- quanteda::dfm(toks_1)
        freq_1 <- Matrix::colSums(dfm_1)
        
        unigram_tibble <- tibble::tibble(
          ngrams = gsub("_", " ", names(freq_1), fixed = TRUE),
          freq = as.numeric(freq_1),
          prevalence = as.numeric(freq_1) / sum(freq_1)
        )
        
        # convert candidate ngrams to spaces
        ngrams_for_pmi <- ngrams_filtered
        ngrams_for_pmi$ngrams <- gsub("_", " ", ngrams_for_pmi$ngrams, fixed = TRUE)
        
        tmp2 <- filter_ngrams_by_pmi(
          ngram_tibble = ngrams_for_pmi,
          unigram_tibble = unigram_tibble,
          pmi_threshold = pmi_threshold
        )
        
        pmi_stats <- tmp2$stats
        ngrams_filtered <- tmp2$filtered_ngrams
        
        # back to underscore form
        if (nrow(ngrams_filtered) > 0) {
          ngrams_filtered$ngrams <- gsub(" ", "_", ngrams_filtered$ngrams, fixed = TRUE)
        }
        
        # if your helper returns extra PMI info, keep it if present
        if (!is.null(tmp2$pmi_tibble)) pmi_tibble <- tmp2$pmi_tibble
        
      } else {
        # create a compatible stats skeleton if PMI not used
        pmi_stats <- tibble::tibble(
          ngram_type = sort(unique(ngram_types_seen)),
          pmi_removed = 0L
        )
      }
      
      # nothing left after PMI
      if (nrow(ngrams_filtered) == 0) {
        stats <- merge(pmi_stats, filter_stats, by = "ngram_type", all = TRUE)
        stats$initial_count <- as.integer(initial_count[match(stats$ngram_type, names(initial_count))])
        stats$removal_mode_removed[is.na(stats$removal_mode_removed)] <- 0L
        stats$pmi_removed[is.na(stats$pmi_removed)] <- 0L
        stats$occurance_removed <- 0L
        stats$final_count <- 0L
        stats <- stats[, c("ngram_type","initial_count","pmi_removed","removal_mode_removed","occurance_removed","final_count")]
        
        return(list(
          settings = settings,
          ngrams = tibble::tibble(),
          freq_per_user = tibble::tibble(),
          stats = stats,
          n_grams_pmi = if (!is.null(pmi_threshold)) pmi_tibble else "No pmi_threshold was used"
        ))
      }
      
      # Fast doc frequency + occurrence filter
      present <- ngrams_filtered$ngrams %in% quanteda::featnames(dfm_all)
      ngrams_filtered <- ngrams_filtered[present, , drop = FALSE]
      
      dfm_filtered <- quanteda::dfm_select(dfm_all, pattern = ngrams_filtered$ngrams)
      M <- methods::as(dfm_filtered, "dgCMatrix")
      
      # document frequency
      num_docs <- Matrix::colSums(M > 0)
      ngrams_filtered$num_docs <- as.integer(num_docs[match(ngrams_filtered$ngrams, names(num_docs))])
      
      occurance_before <- table(ngrams_filtered$n_gram_type)
      occurance_before <- add_missing_rows(occurance_before, ngram_sizes)
      
      if (occurance_rate > 0) {
        removal_frequency <- round(nrow(text_cols) * occurance_rate) - 1
        ngrams_filtered <- ngrams_filtered %>% dplyr::filter(num_docs >= removal_frequency)
      }
      
      occurance_after <- table(ngrams_filtered$n_gram_type)
      occurance_after <- add_missing_rows(occurance_after, ngram_sizes)
      
      occurance_stats <- tibble::tibble(
        ngram_type = sort(unique(ngram_types_seen)),
        occurance_removed = as.integer(occurance_before - occurance_after)
      )
      
      # final stats table
      final_count <- initial_count -
        occurance_stats$occurance_removed -
        filter_stats$removal_mode_removed -
        pmi_stats$pmi_removed
      
      stats <- merge(pmi_stats, filter_stats, by = "ngram_type", all = TRUE)
      stats <- merge(stats, occurance_stats, by = "ngram_type", all = TRUE)
      
      stats$initial_count <- as.integer(initial_count[match(stats$ngram_type, names(initial_count))])
      stats$pmi_removed[is.na(stats$pmi_removed)] <- 0L
      stats$removal_mode_removed[is.na(stats$removal_mode_removed)] <- 0L
      stats$occurance_removed[is.na(stats$occurance_removed)] <- 0L
      stats$final_count <- as.integer(final_count)
      
      stats <- stats[, c("ngram_type","initial_count","pmi_removed","removal_mode_removed","occurance_removed","final_count")]
      
      # sort, optional top_frequent BEFORE freq_per_user to control size
      ngrams_filtered <- ngrams_filtered %>% dplyr::arrange(dplyr::desc(freq))
      if (!is.null(top_frequent)) {
        ngrams_filtered <- ngrams_filtered %>% dplyr::slice_head(n = top_frequent)
      }
      
      # recompute prevalence after filtering
      total2 <- sum(ngrams_filtered$freq)
      ngrams_filtered$prevalence <- ngrams_filtered$freq / total2
      
      # freq_per_user (sparse-safe) 
      keep_terms <- ngrams_filtered$ngrams
      keep_idx <- match(keep_terms, colnames(M))
      keep_idx <- keep_idx[!is.na(keep_idx)]
      M2 <- M[, keep_idx, drop = FALSE]
      
      col_tot <- Matrix::colSums(M2)
      col_tot[col_tot == 0] <- 1
      relM <- Matrix::t(Matrix::t(M2) / col_tot)
      
      n_docs <- nrow(relM)
      n_terms <- ncol(relM)
      would_be_cells <- as.double(n_docs) * as.double(n_terms)
      
      # long sparse (always safe) with original ids
      trip <- Matrix::summary(relM)
      freq_long <- tibble::tibble(
        id = text_cols$id[trip$i],
        ngram = colnames(relM)[trip$j],
        rel_freq = trip$x
      )
      
      if (freq_per_user_format == "long") {
        freq_out <- freq_long
      } else {
        use_wide <- (freq_per_user_format == "wide") ||
          (freq_per_user_format == "auto" && would_be_cells <= max_wide_cells)
        
        if (!use_wide) {
          freq_out <- freq_long
          attr(freq_out, "note") <- paste0(
            "Returned long format to avoid dense allocation: docs=", n_docs,
            ", ngrams=", n_terms, ", docs*ngrams=", format(would_be_cells, scientific = FALSE),
            " > max_wide_cells=", format(max_wide_cells, scientific = FALSE),
            ". Set top_frequent or freq_per_user_format='wide' if you really want wide."
          )
        } else {
          wide_mat <- as.matrix(relM)
          
          ord <- order(text_cols$id)
          wide_mat <- wide_mat[ord, , drop = FALSE]
          
          freq_out <- tibble::as_tibble(wide_mat, .name_repair = "minimal")
          colnames(freq_out) <- colnames(relM)
          
        }
      }
      
      # match your original end behavior: drop n_gram_type in the ngrams output
      ngrams_filtered <- ngrams_filtered %>% dplyr::select(-n_gram_type)
      
      list(
        settings = settings,
        ngrams = tibble::as_tibble(ngrams_filtered),
        n_grams_pmi = if (!is.null(pmi_threshold)) pmi_tibble else "No pmi_threshold was used",
        freq_per_user = freq_out,
        stats = stats
      )
}
