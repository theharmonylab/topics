library(testthat)
library(topics)

#test_check("topics")
#
#
## The submission to CRAN has been rejected several times because the
## time to check the package is too long (>10 min). Thus,  we'll avoid running all tests on
## CRAN.
#
#if (identical(Sys.getenv("NOT_CRAN"), "true")) { # emulates `testthat:::on_cran()`
#  if (requireNamespace("xml2")) {
#    test_check("topics",
#               reporter = MultiReporter$new(reporters = list(
#                 JunitReporter$new(file = "test-results.xml"),
#                 CheckReporter$new()
#               ))
#    )
#  } else {
#    test_check("topics")
#  }
#}
