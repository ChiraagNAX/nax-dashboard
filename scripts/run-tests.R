suppressPackageStartupMessages(library(testthat))
test_results <- test_dir("tests", reporter = "summary")
if (any(sapply(test_results, function(r) r$failed > 0))) quit(status = 1)
