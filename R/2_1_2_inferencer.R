#' check_java_available
#'
#' Checks whether the specified package (typically 'rJava') is installed and, 
#' if applicable, whether Java is correctly configured. 
#' Used to safely guard functions that rely on Java dependencies.
#'
#' @param pkg (string) The name of the package to check, default is "rJava".
#' @param func_name (string) Optional. The name of the calling function, 
#'        used to improve error messaging.
#' @return (invisible) TRUE if the package and Java (if applicable) are available.
#' @noRd
check_java_available <- function(pkg = "rJava", func_name = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- paste0("This function requires the '", pkg, "' package, which is not installed.",
                  "\nPlease install it using: install.packages('", pkg, "')")
    if (!is.null(func_name)) {
      msg <- paste0(msg, "\n\nWhile running: ", func_name)
    }
    message(colourise(msg, "brown"))
    return(FALSE)
  }
  
  # Dynamically call .jinit() only if rJava is available
  if (pkg == "rJava") {
    ok <- tryCatch(
      {
        getNamespace("rJava")[[".jinit"]]()  # avoids hard dependency
        TRUE
      },
      error = function(e) {
        msg <- paste0("Java is not properly configured. Please ensure Java is installed and working.",
                      "\nError from rJava:::.jinit(): ", conditionMessage(e))
        message(colourise(msg, "brown"))
        FALSE
      }
    )
    if (!ok) return(FALSE)
  }
  
  TRUE
}


#' Internal helper to safely access rJava functions dynamically
#' @param name The name of the rJava function (e.g. ".jnew", "J")
#' @return The corresponding function from rJava, or NULL if unavailable
#' @noRd
.j <- function(name) {
  getNamespace("rJava")[[name]]
}

#' inferencer
#' @param model (tibble) A model
#' @return inferencer
#' @noRd
inferencer <- function(model) {
  model$model$getInferencer()
}


#' write_inferencer
#' @param inf  a reference to a topic inferencer, from inferencer()
#' @param out_file (string)  the name of a file to save to (will overwrite an existing file)
#' @return save an inferencer object to a file
#' @noRd
write_inferencer <- function(inf, out_file) {
  if (!check_java_available(pkg = "rJava", func_name = "write_inferencer")) return(NULL)
  
  .jnew <- .j(".jnew")
  .jcast <- .j(".jcast")
  
  fos <- .jnew("java/io/FileOutputStream", out_file)
  oos <- .jnew("java/io/ObjectOutputStream", .jcast(fos, "java/io/OutputStream"))
  
  oos$writeObject(inf)
  oos$close()
}


#' retrieve an inferencer object from a file
#' @param in_file (string) path to inferencer file
#' @return returns a reference to a topic inferencer object
#' @noRd
read_inferencer <- function(in_file) {
  if (!check_java_available(pkg = "rJava", func_name = "read_inferencer")) return(NULL)
  
  J <- .j("J")
  new_obj <- methods::new(J("java.io.File"), in_file)
  J("cc.mallet.topics.TopicInferencer")$read(new_obj)
}


#' Infer document topics using an inferencer and compatible instances
#' @param inferencer a topic inferencer object
#' @param instances an instances list object from compatible_instances()
#' @param n_iterations number of Gibbs sampling iterations
#' @param sampling_interval thinning interval
#' @param burn_in number of burn-in iterations
#' @param random_seed integer random seed; set for reproducibility
#' @return matrix of estimated document-topic proportions
#' @noRd
infer_topics <- function(
    inferencer,
    instances,
    n_iterations = 100,
    sampling_interval = 10,
    burn_in = 10,
    random_seed = NULL) {
  
  if (!check_java_available(pkg = "rJava", func_name = "infer_topics")) return(NULL)
  
  .jcall <- .j(".jcall")
  
  iter <- instances$iterator()
  n_iterations <- as.integer(n_iterations)
  sampling_interval <- as.integer(sampling_interval)
  burn_in <- as.integer(burn_in)
  
  if (!is.null(random_seed)) {
    inferencer$setRandomSeed(as.integer(random_seed))
  }
  
  doc_topics <- vector("list", instances$size())
  
  for (j in 1:instances$size()) {
    inst <- .jcall(iter, "Ljava/lang/Object;", "next")
    doc_topics[[j]] <- inferencer$getSampledDistribution(
      inst,
      n_iterations,
      sampling_interval,
      burn_in
    )
  }
  
  do.call(rbind, doc_topics)
}


#' Create a compatible instance list for inference
#' @param ids character vector of item ids
#' @param texts character vector of texts
#' @param instances reference to a training instance list
#' @return a new compatible instance list
#' @noRd
compatible_instances <- function(ids, texts, instances) {
  if (!check_java_available(pkg = "rJava", func_name = "compatible_instances")) return(NULL)
  
  .jnew <- .j(".jnew")
  .jcast <- .j(".jcast")
  .jarray <- .j(".jarray")
  J <- .j("J")
  
  mallet_pipe <- instances$getPipe()
  
  new_insts <- .jnew("cc/mallet/types/InstanceList",
                     .jcast(mallet_pipe, "cc/mallet/pipe/Pipe"))
  
  java_ids <- .jarray(ids, "java/lang/String")
  java_texts <- .jarray(texts, "java/lang/String")
  
  J("cc.mallet.topics.RTopicModel")$addInstances(new_insts, ids, texts)
  
  new_insts
}


#' Number of tokens in each document
#' @param instances reference to an instance list
#' @return vector of token counts
#' @noRd
instances_lengths <- function(instances) {
  if (!check_java_available(pkg = "rJava", func_name = "instances_lengths")) return(NULL)
  
  .jcall <- .j(".jcall")
  iter <- instances$iterator()
  
  replicate(instances$size(),
            .jcall(iter, "Ljava/lang/Object;", "next")$getData()$size())
}