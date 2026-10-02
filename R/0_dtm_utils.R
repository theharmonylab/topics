#' Create a document-term matrix (internal)
#'
#' Builds a sparse document-term matrix with quanteda. Pre-processing mirrors
#' what topics previously got from textmineR::CreateDtm(): optional lower-casing,
#' replacing non-alphanumeric characters and digits with spaces, word-boundary
#' tokenisation, stopword removal, an optional stemming/lemmatisation hook, and
#' n-grams joined with "_" (e.g., "word1_word2").
#' @param doc_vec (character) The documents.
#' @param doc_names (vector) Document names, used as rownames.
#' @param ngram_window (integer) Min and max n-gram length, e.g., c(1, 3).
#' @param stopword_vec (character) Stopwords to remove before forming n-grams.
#' @param lower (boolean) Lower-case documents and stopwords.
#' @param remove_punctuation (boolean) Replace non-alphanumeric characters with spaces.
#' @param remove_numbers (boolean) Replace digits with spaces.
#' @param stem_lemma_function (function) Optional function applied to the
#'   character vector of tokens of each document.
#' @param verbose (boolean) Unused; kept for call compatibility.
#' @return A dgCMatrix with documents as rows and terms as columns.
#' @importFrom stringr str_replace_all str_split
#' @importFrom stringi stri_split_boundaries
#' @importFrom quanteda as.tokens tokens_ngrams dfm featnames
#' @importFrom Matrix sparseMatrix colSums
#' @noRd
create_dtm_internal <- function(
    doc_vec,
    doc_names = names(doc_vec),
    ngram_window = c(1, 1),
    stopword_vec = character(0),
    lower = TRUE,
    remove_punctuation = TRUE,
    remove_numbers = TRUE,
    stem_lemma_function = NULL,
    verbose = FALSE){

  doc_vec <- as.character(doc_vec)
  stopword_vec <- as.character(stopword_vec)
  if (is.null(doc_names)){
    doc_names <- seq_along(doc_vec)
  }

  split_words <- function(x){
    unique(unlist(stringr::str_split(string = x, pattern = "\\s+")))
  }

  if (lower){
    doc_vec <- tolower(doc_vec)
    stopword_vec <- tolower(stopword_vec)
  }
  if (remove_punctuation){
    doc_vec <- stringr::str_replace_all(doc_vec, "[^[:alnum:]]", " ")
    stopword_vec <- split_words(
      stringr::str_replace_all(stopword_vec, "[^[:alnum:]]", " "))
  }
  if (remove_numbers){
    doc_vec <- stringr::str_replace_all(doc_vec, "[0-9]", " ")
    stopword_vec <- split_words(
      stringr::str_replace_all(stopword_vec, "[0-9]", " "))
  }

  toks <- stringi::stri_split_boundaries(
    doc_vec, type = "word", skip_word_none = TRUE)
  # Missing documents give no tokens
  toks <- lapply(toks, function(x) x[!is.na(x)])

  if (length(stopword_vec) > 0){
    toks <- lapply(toks, function(x) x[!x %in% stopword_vec])
  }
  if (!is.null(stem_lemma_function)){
    toks <- lapply(toks, stem_lemma_function)
  }

  # quanteda gets ASCII ids ("w1", "w2", ...) instead of the words, since it
  # can mangle non-ASCII text in non-UTF-8 locales; words are mapped back below.
  types <- unique(unlist(toks, use.names = FALSE))
  toks <- lapply(toks, function(x) sprintf("w%d", match(x, types)))
  names(toks) <- paste0("doc", seq_along(toks))
  # In non-UTF-8 locales quanteda warns about translating strings to UTF-8;
  # irrelevant here since the ids are ASCII.
  dfm_mat <- withCallingHandlers({
    toks <- quanteda::as.tokens(toks)
    toks <- quanteda::tokens_ngrams(
      toks,
      n = seq(ngram_window[1], ngram_window[2]),
      concatenator = "_")
    quanteda::dfm(toks, tolower = FALSE)
  }, warning = function(w){
    if (grepl("not representable in native encoding", conditionMessage(w))){
      invokeRestart("muffleWarning")
    }
  })

  term_ids <- strsplit(quanteda::featnames(dfm_mat), "_", fixed = TRUE)
  terms <- vapply(
    term_ids,
    function(x) paste(types[as.integer(substring(x, 2))], collapse = "_"),
    character(1))

  dtm <- Matrix::sparseMatrix(
    i = dfm_mat@i,
    p = dfm_mat@p,
    x = as.numeric(dfm_mat@x),
    dims = dim(dfm_mat),
    dimnames = list(as.character(doc_names), terms),
    index1 = FALSE,
    repr = "C")

  # Order terms by total count, then alphabetically (C locale), as textmineR
  # did. Frequency-based term removal in topicsDtm() breaks ties by this order.
  term_order <- order(Matrix::colSums(dtm), colnames(dtm), method = "radix")
  dtm <- dtm[, term_order, drop = FALSE]

  return(dtm)
}

#' Convert a document-term matrix back to pseudo-documents (internal)
#'
#' Each term is repeated as many times as it occurs in a document, giving one
#' whitespace-separated string per row. Replaces textmineR::Dtm2Docs().
#' @param dtm A document-term matrix with terms as colnames.
#' @return A named character vector with one pseudo-document per row of dtm.
#' @importFrom methods as
#' @noRd
dtm_to_docs <- function(dtm){

  dtm <- methods::as(methods::as(dtm, "CsparseMatrix"), "generalMatrix")
  # Row-oriented triplets, so each document's terms stay in column order
  trip <- Matrix::summary(Matrix::t(dtm))
  vocab <- colnames(dtm)

  tokens <- rep(vocab[trip$i], times = trip$x)
  doc_index <- rep(trip$j, times = trip$x)

  docs <- rep("", nrow(dtm))
  if (length(tokens) > 0){
    pasted <- vapply(
      split(tokens, doc_index),
      function(x) paste0(" ", x, " ", collapse = ""),
      character(1))
    docs[as.integer(names(pasted))] <- pasted
  }
  names(docs) <- rownames(dtm)

  return(docs)
}

#' Calculate P(topic | word) from phi and theta (internal)
#'
#' Applies Bayes' rule: gamma[k, w] = P(w | k) * P(k) / P(w), with
#' P(k) = mean topic proportion over documents and P(w) = sum_k P(w | k) P(k).
#' Adapted from CalcGamma() in the 'textmineR' package
#' (Copyright (c) 2019 Thomas W. Jones, MIT license).
#' @param phi Topics x words matrix of P(word | topic).
#' @param theta Documents x topics matrix of P(topic | document).
#' @return Topics x words matrix of P(topic | word). Missing values
#'   (words with zero probability) are set to 0.
#' @noRd
calc_gamma <- function(phi, theta){

  p_t <- colMeans(theta)
  p_w <- as.vector(p_t %*% phi)

  gamma <- phi * p_t
  gamma <- sweep(gamma, 2, p_w, "/")
  gamma[is.na(gamma)] <- 0

  rownames(gamma) <- rownames(phi)
  colnames(gamma) <- colnames(phi)

  return(gamma)
}
