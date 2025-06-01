

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
#' @importFrom utils combn
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
  
  #### Compute a Coherence Score ####
  
  # Step 1: Calculate Term Co-occurrence Matrix from the DTM
  # Convert the document-term matrix (DTM) to a binary form indicating term presence/absence
  # Multiplying by 1 converts the TRUE values to 1 and FALSE values to 0.
  binary_dtm <- (dtm > 0) * 1
  
  # Compute the co-occurrence matrix by multiplying the transposed binary DTM with itself
  # This results in a matrix where each entry [i, j] represents the number of documents where both terms i and j co-occur
  co_occurrence <- Matrix::t(binary_dtm) %*% binary_dtm
  
  # Step 2: Compute Total Number of Documents
  N <- nrow(dtm)  # The total number of documents in the corpus
  
  # Step 3: Calculate the Probability of Each Individual Term
  # p_wi is a vector where each entry represents the proportion of documents containing a specific term
  p_wi <- Matrix::diag(co_occurrence) / N
  
  # Step 4: Define a Function to Compute Coherence for a Topic
  compute_topic_coherence <- function(terms, co_occurrence, p_wi, N) {
    
    # Get the indices of the terms in the DTM's vocabulary
    term_indices <- match(terms, colnames(dtm))
    
    # Create all possible pairs of the term indices
    term_pairs <- utils::combn(term_indices, 2)
    
    # Calculate PMI (Pointwise Mutual Information) for each pair of terms
    pmi_values <- apply(term_pairs, 2, function(pair) {
      w1 <- pair[1]
      w2 <- pair[2]
      
      # Ensure both term indices are valid and co-occur in at least one document
      if (!is.na(w1) && !is.na(w2) && co_occurrence[w1, w2] > 0) {
        
        # Calculate the joint probability of terms w1 and w2
        p_wi_wj <- co_occurrence[w1, w2] / N
        
        # Calculate the PMI score for the pair
        pmi <- log(p_wi_wj / (p_wi[w1] * p_wi[w2]))
        
        return(pmi)
      } else {
        return(0)  # Return 0 if terms do not co-occur or indices are invalid
      }
    })
    
    # Return the average PMI for all term pairs in the topic
    mean(pmi_values)
  }
  
  # Step 5: Compute Coherence Scores for Each Topic
  coherence_scores <- sapply(1:num_topics, function(i) {
    
    # Get the top terms for the current topic from the `df_top_terms` data frame
    top_terms <- df_top_terms[, i]
    
    # Compute the coherence score for the current topic using the defined function
    compute_topic_coherence(top_terms, co_occurrence, p_wi, N)
  })
  
  # Step 6: Add the Computed Coherence Scores to the `return_model` List
  return_model$coherence <- coherence_scores
  
  ##############
  
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



#' Topic modelling
#' 
#' The function to create and train and an LDA model.
#' @param dtm (R_obj) The document term matrix -> output of topicsDtm function
#' @param num_topics (integer) The number of topics to be created
#' @param num_top_words (integer) The number of top words to be displayed
#' @param num_iterations (integer) The number of iterations to run the model
#' @param seed (integer) A seed to set for reproducibility
# @param save_dir (string) The directory to save the model, if NULL, the model will not be saved
# @param load_dir (string) The directory to load the model from, if NULL, the model will not be loaded
#' @return A named list containing the following elements:
#' \describe{
#'   \item{name}{Description}
#'   \item{instances}{Java object reference: A list of all documents used for topic modeling, 
#'   in which each document is preprocessed (e.g., tokenized and vectorized). This object is part of 
#'   the Mallet package's internal structure.}
#'   \item{inferencer}{Java object reference: This is the topic inferencer, which allows the inference 
#'   of topic distributions for new, unseen documents based on the trained model.}
#'   \item{top_terms_mallet}{A data frame containing the top terms of each topic, showing which concepts
#'    each topic likely represents. The number of top terms shown here can be adjusted with the argument
#'     num_top_words.}
#'   \item{top_terms}{A data frame containing the top terms of each topic, showing which concepts each 
#'   topic likely represents. The number of top terms shown here can be adjusted with the argument 
#'   num_top_words.}
#'   \item{phi}{A matrix of the topic-word distribution: Each row represents a topic, and each column 
#'   represents a word from the document term matrix. The values show the probability of a word given a 
#'   topic P(word|topic).}
#'   \item{topic_docs}{A matrix of document-topic distribution: Each row represents a document, and each 
#'   column represents a topic. The values show the probability of a topic given a document, P(topic|document).}
#'   \item{frequencies}{A data frame of term frequencies. word = every word in the document term matrix, 
#'   word.freq = the frequency of each word across all documents, doc.freq = the number of documents in which each word appears.}
#'   \item{vocabulary}{A character vector of all unique terms in the document term matrix.}
#'   \item{labels}{A list of topic labels. These short labels are the most representative term for each topic, 
#'   making it easy to identify and understand them.}
#'   \item{theta}{A data frame of document-topic probabilities: each row represents a document, and each column
#'    represents a topic. Similar to topic_docs, this shows the contribution of each topic to each document. 
#'    Each row sums to 1, representing the document’s composition of topics.}
#'   \item{prevalence}{A numeric vector showing the overall prevalence (prominence) of each topic in the corpus.
#'    The prevalences are expressed as percentages relative to the other topics  and add up to 100%. 
#'    Higher values indicate topics that are present in more documents.}
#'   \item{coherence}{A numeric vector showing the coherence of each topic. Coherence scores indicate how
#'    semantically consistent and interpretable the topics are. Higher coherence generally indicates 
#'    better-quality topics.}
#'   \item{pred_model}{A list containing components of the predictive model, including phi
#'    (word-topic probability matrix), theta (document-topic probabilities matrix), alpha (Dirichlet prior of topics), 
#'    gamma (hyperparameters of word-topic assignments), and data (sparse matrix representing the document term matrix.)}
#'   \item{dtm_settings}{A list of settings used for preprocessing and building the document term matrix (dtm), 
#'   including n-gram ranges, stopword removal, frequency thresholds, and random seed settings.}
#'   \item{summary}{A summary data frame comprising of the topic numbers, labels, coherence scores, prevalence scores, and top terms.}
#'  }
#' @examples
#' \donttest{
#' # Create LDA Topic Model 
#' save_dir_temp <- tempfile()
#' dtm <- topicsDtm(data = dep_wor_data$Depphrase)
#' 
#' model <- topicsModel(
#' dtm = dtm, # output of topicsDtm()
#' num_topics = 20,
#' num_top_words = 10,
#' num_iterations = 1000,
#' seed = 42)
#' }
#' @export
topicsModel <- function(
    dtm,
    num_topics = 20,
    num_top_words = 10,
    num_iterations = 1000,
    seed = 42){
  
  set.seed(seed)
  
  dtm_settings <- dtm$settings
  dtm <- dtm$train_dtm
  
  if (length(Matrix::colSums(dtm)) == 0) {
    msg <- "The document term matrix is empty. Please provide a valid document term matrix."
    message(colourise(msg, "brown"))
    return(NULL)
  }
  
  model <- get_mallet_model(
    dtm = dtm,
    num_topics = num_topics,
    num_top_words = num_top_words,
    num_iterations = num_iterations, 
    seed = seed)
  
  model$dtm_settings <- dtm_settings
  
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
  
  model$model_type <- "mallet"
  
  return(model)
}

#' Predict topic distributions
#' 
#' The function to predict the topics of a new document with the trained model.
#' @param model (list) The trained model.
#' @param data (tibble) The text variable for which you want to infer the topic distribution. This can be the same data as used to create the dtm or new data.
#' @param num_iterations (integer) The number of iterations to run the model.
#' @param sampling_interval The number of iterations between consecutive samples collected.
#' during the Gibbs Sampling process. This technique, known as thinning, helps reduce the 
#' correlation between consecutive samples and improves the quality of the final estimates 
#' by ensuring they are more independent.
#' Purpose: By specifying a sampling_interval, you avoid collecting highly correlated samples, which can 
#' lead to more robust and accurate topic distributions.
#' Example: If sampling_interval = 10, the algorithm collects a 
#' sample every 10 iterations (e.g., at iteration 10, 20, 30, etc.).
#' Typical Values: Default: 10; Range: 5 to 50 (depending on the complexity and size of the data).
#' @param burn_in The number of initial iterations discarded during the Gibbs Sampling process. 
#' These early iterations may not be representative of the final sampling distribution because the model is still stabilizing.
#' Purpose: The burn_in period allows the model to converge to a more stable state before collecting samples, 
#' improving the quality of the inferred topic distributions.
#' Example: If burn_in = 50, the first 50 iterations of the Gibbs Sampling process are discarded,
#' and sampling begins afterward. Typical Values: Default: 50 to 100 
#' Range: 10 to 1000 (larger datasets or more complex models may require a longer burn-in period).
#' @param seed (integer) A seed to set for reproducibility.
#' @param create_new_dtm (boolean) If applying the model on new data (not used in training), it can help to make a new dtm.
#' Currently this is experimental, and using the textmineR::CreateDtm() function rather than the topicsDtm() function, which has more functions.
#' @return A tibble of the predictions: The rows represent the documents, and the columns represent the topics. The values in the cells indicate the proportion of each topic within the corresponding document.
#' @examples
#' \donttest{
#' # Predict topics for new data with the trained model
#' 
#' dtm <- topicsDtm(
#' data = dep_wor_data$Depphrase)
#' 
#' model <- topicsModel(dtm = dtm, # output of topicsDtm()
#'                      num_topics = 20,
#'                      num_top_words = 10,
#'                      num_iterations = 1000,
#'                      seed = 42)
#'                      
#' preds <- topicsPreds(model = model, # output of topicsModel()
#'                      data = dep_wor_data$Depphrase)
#' }
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr %>%
#' @export
topicsPreds <- function(
    model, 
    data, 
    num_iterations = 200,
    sampling_interval = 10, # aka thinning
    burn_in = 10, 
    seed = 42,
    create_new_dtm = FALSE
    ){
  
  set.seed(seed)
    
    if (length(data) == 0){
      msg <- "The data provided is empty. Please provide a list of text data."
      message(colourise(msg, "brown"))
      
      stop()
    }
    # create an id column for the data
    
    if (is.data.frame(data)){
      pred_text <- data[, 1]
    } else {
      pred_text <- data
    }
    
    pred_ids <- as.character(1:length(data))
    
    ###### 
    if (create_new_dtm){
      # Preprocess new data to create DTM
      new_dtm <- list()
      new_dtm$train_dtm <- textmineR::CreateDtm(
        doc_vec = data,
        doc_names = as.character(1:length(data)),
        ngram_window = model$dtm_settings$ngram_window,
        stopword_vec = model$dtm_settingsstopwords,  # Use the same stopwords as training
        lower = TRUE,
        remove_punctuation = TRUE,
        remove_numbers = TRUE,
        verbose = FALSE
      )
      
      # Align new DTM with model vocabulary
      model_vocab <- model$vocabulary
      new_vocab <- colnames(new_dtm$train_dtm)
      
      # Identify missing terms
      missing_terms <- setdiff(model_vocab, colnames(new_dtm$train_dtm))
      
      # Add missing terms with zero counts
      if (length(missing_terms) > 0) {
        zero_matrix <- Matrix::Matrix(0, nrow = nrow(new_dtm$train_dtm), ncol = length(missing_terms),
                                      dimnames = list(rownames(new_dtm$train_dtm), missing_terms))
        new_dtm$train_dtm <- cbind(new_dtm$train_dtm, zero_matrix)
      }
      
      new_dtm$train_dtm <- new_dtm$train_dtm[, model_vocab, drop = FALSE]
      # Convert the new DTM to the Mallet instances format
      new_docs <- textmineR::Dtm2Docs(new_dtm$train_dtm)
      
      new_instances <- mallet::mallet.import(
        as.character(seq_along(new_docs)),
        new_docs,
        preserve.case = FALSE,
        token.regexp = "[\\p{L}\\p{N}_]+|[\\p{P}]+\ "
      )
    }
    
    
    ######
    if (create_new_dtm == FALSE){
      
      new_instances <- compatible_instances(
        ids = pred_ids,
        texts = pred_text,
        instances = model$instances)
      
    }
    
    
    # Use the inferencer to predict topics for new instances
    inf_model <- model$inferencer
    
    
    preds <- infer_topics(
      inferencer = inf_model,
      instances = new_instances,
      n_iterations = num_iterations,
      sampling_interval = sampling_interval, # aka "thinning"
      burn_in = burn_in,
      random_seed = seed
    )
    
    preds <- tibble::as_tibble(preds, .name_repair = "minimal")
    colnames(preds) <- paste("t_", 1:ncol(preds), sep="")
    preds <- preds %>% tibble::tibble()
  
  return(preds)
}

