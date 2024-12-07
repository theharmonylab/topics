
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
    term_pairs <- combn(term_indices, 2)
    
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
#' @param dtm (R_obj) The document term matrix
#' @param num_topics (integer) The number of topics to be created
#' @param num_top_words (integer) The number of top words to be displayed
#' @param num_iterations (integer) The number of iterations to run the model
#' @param seed (integer) The seed to set for reproducibility
#' @param save_dir (string) The directory to save the model, if NULL, the model will not be saved
#' @param load_dir (string) The directory to load the model from, if NULL, the model will not be loaded
#' @return A list of the model, the top terms, the labels, the coherence (experimental), and the prevalence.
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
    
    new_instances <- compatible_instances(
      ids = pred_ids,
      texts = pred_text,
      instances = model$instances)
    
    inf_model <- model$inferencer
    preds <- infer_topics(
      inferencer = inf_model,
      #instances = model$instances,
      instances = new_instances,
      n_iterations = 200,
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

