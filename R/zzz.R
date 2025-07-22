check_java_installed <- function(verbose = TRUE) {
  
  java_path <- Sys.which("java")[[1]]
  has_java <- java_path != ""
  
  summary_lines <- character()
  
  if (!has_java) {
    summary_lines <- c(
      "Java is NOT installed or not found in your system PATH.",
      "To install Java:",
      "  - On macOS: brew install openjdk",
      "  - On Ubuntu/Debian: sudo apt-get install default-jdk",
      "  - On Windows: Install Java from https://www.java.com/"
    )
  } else {
    # Try running `java -version` to confirm it's accessible
    java_version_output <- tryCatch(
      system2("java", args = "-version", stderr = TRUE, stdout = TRUE),
      error = function(e) NA
    )
    
    if (is.na(java_version_output)[[1]]) {
      summary_lines <- c(
        "Java is found, but 'java -version' failed to run.",
        "There may be permission or PATH issues."
      )
      has_java <- FALSE
    } else {
      summary_lines <- c("Java is installed.")
      if (length(java_version_output) > 0) {
        summary_lines <- c(summary_lines, paste("Version info:", java_version_output[1]))
      }
    }
  }
  
  if (verbose) message(paste(summary_lines, collapse = "\n"))
  
  list(
    java_found = has_java,
    java_path = if (has_java) java_path else NULL,
    version_info = if (has_java) java_version_output else NULL,
    summary_lines = summary_lines
  )
}


#' @importFrom utils packageVersion
#' @noRd
.onAttach <- function(libname, pkgname) {
  
  if (!grepl(x = R.Version()$arch, pattern = "64")) {
    warning("\n\nThe topic package requires running R on a 64-bit systems.")
  }

  log <- check_java_installed(verbose = FALSE)
  
  if(log$java_found){
    java_msg <- "" 
  } else {
    java_msg <- "\nPlease note that the topics package requires you to download and install java from www.java.com. \n" 
  }
  
  topics_version_nr <- tryCatch(
    {
      topics_version_nr1 <- paste(" (version ", 
                                packageVersion("topics"), ")", 
                                sep = "")
    },
    error = function(e) {
      topics_version_nr1 <- ""
    }
  )
  
  nowarranty <- c("The topics package is provided 'as is' without any warranty of any kind. \n")
  
  
  packageStartupMessage(
    colourise(
      paste("\nThis is topics: your text's new best friend",
            topics_version_nr,
        ".\n",
        sep = ""
      ),
      fg = "blue", bg = NULL
    ),
    colourise(java_msg,
      fg = "brown", bg = NULL
    ),
    colourise(nowarranty,
              fg = "purple", bg = NULL
    ),
    colourise("\nFor more information about the topics package see www.r-topics.org and www.r-text.org.",
      fg = "green", bg = NULL
    )
  )
}


# Below function is from testthat:
# https://github.com/r-lib/testthat/blob/717b02164def5c1f027d3a20b889dae35428b6d7/R/colour-text.r
#' Colourise text for display in the terminal.
#'
#' If R is not currently running in a system that supports terminal colours
#' the text will be returned unchanged.
#'
#' Allowed colours are: black, blue, brown, cyan, dark gray, green, light
#' blue, light cyan, light gray, light green, light purple, light red,
#' purple, red, white, yellow
#'
#' @param text character vector
#' @param fg foreground colour, defaults to white
#' @param bg background colour, defaults to transparent
# @examples
#' @noRd
colourise <- function(text, fg = "black", bg = NULL) {
  term <- Sys.getenv()["TERM"]
  colour_terms <- c("xterm-color", "xterm-256color", "screen", "screen-256color")

  if (rcmd_running() || !any(term %in% colour_terms, na.rm = TRUE)) {
    return(text)
  }

  col_escape <- function(col) {
    paste0("\033[", col, "m")
  }

  col <- .fg_colours[tolower(fg)]
  if (!is.null(bg)) {
    col <- paste0(col, .bg_colours[tolower(bg)], sep = ";")
  }

  init <- col_escape(col)
  reset <- col_escape("0")
  paste0(init, text, reset)
}

.fg_colours <- c(
  "black" = "0;30",
  "blue" = "0;34",
  "green" = "0;32",
  "cyan" = "0;36",
  "red" = "0;31",
  "purple" = "0;35",
  "brown" = "0;33"
  # "light gray" = "0;37",
  # "dark gray" = "1;30",
  # "light blue" = "1;34",
  # "light green" = "1;32",
  # "light cyan" = "1;36",
  # "light red" = "1;31",
  # "light purple" = "1;35",
  # "yellow" = "1;33",
  # "white" = "1;37"
)

.bg_colours <- c(
  "black" = "40",
  "red" = "41",
  "green" = "42",
  "brown" = "43",
  "blue" = "44",
  "purple" = "45",
  "cyan" = "46",
  "light gray" = "47"
)

rcmd_running <- function() {
  nchar(Sys.getenv("R_TESTS")) != 0
}
