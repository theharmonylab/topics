
#' inferencer
#' @param model (tibble) A model
#' @return inferencer
#' @noRd
inferencer <- function (
    model) {
  model$model$getInferencer()
}


#' write_inferencer
#' @param inf  a reference to a topic inferencer, from inferencer()
#' @param out_file (string)  the name of a file to save to (will overwrite an existing file)
#' @return save an inferencer object to a file
#' @importFrom rJava .jnew .jcast
#' @noRd
write_inferencer <- function (
    inf,
    out_file) {
  
  fos <- rJava::.jnew("java/io/FileOutputStream", 
                      out_file)
  
  oos <- rJava::.jnew("java/io/ObjectOutputStream",
                      rJava::.jcast(fos, 
                                    "java/io/OutputStream"))
  
  oos$writeObject(inf)
  oos$close()
}


#' retrieve an inferencer object from a file
#' @param in_file (tibble) The initial tibble
#' @return returns a reference to a topic inferencer object
#' @importFrom rJava J
#' @importFrom methods new
#' @noRd
read_inferencer <- function (
    in_file) {
  
  rJava::J("cc.mallet.topics.TopicInferencer")$read(
    methods::new(rJava::J("java.io.File"), 
                 in_file)
  )
}

#'  Infer document topics. This is like the Gibbs sampling process for making a  
#'  topic model, but the topic-word proportions are not updated.
#' @param inferencer (tibble) a topic inferencer object
#' @param instances an instances list object from compatible_instances()---or 
#' any instances that are compatible with the inferencer, i.e. their
#' vocabulary has to correspond to that of the instances used to create 
#' the model that yielded the inference
#' @param n_iterations number of Gibbs sampling iterations
#' @param sampling_interval thinning interval
#' @param burn_in number of burn-in iterations
#' @param random_seed integer random seed; set for reproducibility
#' @return returns a matrix of estimated document-topic proportions m, where m[i, j]  
#' gives the proportion (between 0 and 1) of topic j in document i. The 
#' inferencer sampling state is not accessible.
#' @importFrom rJava J .jcall
#' @noRd
infer_topics <- function (
    inferencer,
    instances,
    n_iterations = 100,
    sampling_interval = 10, # aka "thinning"
    burn_in = 10,
    random_seed = seed) {
  
  iter <- instances$iterator()
  n_iterations <- as.integer(n_iterations)
  sampling_interval <- as.integer(sampling_interval)
  burn_in <- as.integer(burn_in)
  
  if (!is.null(random_seed)) {
    inferencer$setRandomSeed(as.integer(random_seed))
  }
  
  doc_topics <- vector("list", 
                       instances$size())
  
  for (j in 1:instances$size()) {
    
    inst <- rJava::.jcall(iter, 
                          "Ljava/lang/Object;", 
                          "next")
    
    doc_topics[[j]] <- inferencer$getSampledDistribution(
      inst,
      n_iterations, 
      sampling_interval, 
      burn_in)
  }
  
  do.call(rbind, 
          doc_topics)
}


#' given an existing instances list object and some new texts, 
#' generate a compatible instances list object which can be input 
#' into the inferencer
#' @param ids character vector of item ids
#' @param texts character vector of texts (same length as ids)
#' @param instances instances to enforce compatibility with
#' @return  returns a reference to the new instances list object. Save this to disk
#'  with the litdata package function write_instances()
#' @importFrom rJava .jnew .jcast J
#' @noRd
compatible_instances <- function (
    ids, 
    texts, 
    instances) {
  
  mallet_pipe <- instances$getPipe()
  
  new_insts <- rJava::.jnew("cc/mallet/types/InstanceList",
                            rJava::.jcast(mallet_pipe, "cc/mallet/pipe/Pipe"))
  
  java_ids <- rJava::.jarray(ids, 
                             "java/lang/String")
  
  java_texts <- rJava::.jarray(texts, 
                               "java/lang/String")
  
  
  rJava::J("cc/mallet/topics/RTopicModel")$addInstances(new_insts, 
                                                        ids, 
                                                        texts)
  
  new_insts
}


#' number of tokens in each document in an instance list
#' @param instances reference to an instances list
#' @return  returns a vector of integers with token counts
#' @importFrom rJava .jcall
#' @noRd
instances_lengths <- function (instances) {
  
  iter <- instances$iterator()
  
  replicate(instances$size(),
            rJava::.jcall(iter, 
                          "Ljava/lang/Object;", 
                          "next")$getData()$size()
  )
}

