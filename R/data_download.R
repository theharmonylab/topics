#' Download and Prepare Tutorial Data
#' 
#' Downloads the Big5 Essays dataset from Hugging Face, filters by word count,
#' and returns a sample of a specified size.
#' @param sample_size (integer) The number of rows to return. Default is 1000.
#' @param min_word_count (integer) Minimum words per essay. Default is 250.
#' @param max_word_count (integer) Maximum words per essay. Default is 750.
#' @param seed (integer) Seed for reproducibility. Default is 42.
#' @param verbose (boolean) Whether to print status messages. Default is TRUE.
#' @return A processed tibble ready for topic modeling.
# @importFrom arrow read_parquet
# @importFrom httr GET write_disk status_code
#' @importFrom dplyr mutate filter
#' @importFrom stringr str_count
#' @export
topicsTutorialData <- function(
    sample_size = 1000,
    min_word_count = 250,
    max_word_count = 750,
    seed = 42,
    verbose = TRUE) {
  
  if (verbose) message(colourise("Downloading tutorial data from Hugging Face...", "blue"))
  
  url <- "https://huggingface.co/datasets/jingjietan/essays-big5/resolve/main/data/train-00000-of-00001.parquet"
  temp_file <- tempfile(fileext = ".parquet")
  
  # Download
  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE))
  
  if (httr::status_code(response) != 200) {
    stop("Failed to download data. Check connection or URL.")
  }
  
  # Read
  df <- arrow::read_parquet(temp_file)
  if (file.exists(temp_file)) unlink(temp_file)
  
  if (verbose) message(colourise("Processing and filtering data...", "blue"))
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Process
  df_processed <- df %>%
    dplyr::mutate(
      WordCount = stringr::str_count(text, "\\S+") 
    ) %>%
    dplyr::filter(WordCount >= min_word_count, 
                  WordCount <= max_word_count)
  
  # Safety check for sample size
  actual_sample_size <- min(nrow(df_processed), sample_size)
  
  if (actual_sample_size < sample_size && verbose) {
    warning(paste("Only", actual_sample_size, "rows met the criteria. Returning all."))
  }
  
  df_processed <- df_processed[sample(nrow(df_processed), actual_sample_size), ]
  
  msg <- paste("Successfully loaded", nrow(df_processed), "rows.")
  if (verbose) message(colourise(msg, "blue"))
  
  return(df_processed)
}
