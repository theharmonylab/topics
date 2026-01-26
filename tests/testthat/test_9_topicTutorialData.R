library(testthat)

library(topics)


test_that("topicsTutorialData ", {
  

  testthat::skip_on_os(c("linux", "windows"))  # Skip on Ubuntu (Linux) and Windows
  testthat::skip_on_cran()
  
  df <- topics::topicsTutorialData()
  testthat::expect_true(tibble::is_tibble(df))
  
  testthat::expect_equal(
    colnames(df), 
    c("O", "C", "E", "A", "N", "ptype", "text", "__index_level_0__", "WordCount")
    )
  
   
})