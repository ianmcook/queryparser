devtools::install()
library(testthat)
library(covr)
library(queryparser)

pkg_result <- package_coverage()

env_result_base <- environment_coverage(
  env = with(
    queryparser:::translation_environment_direct_base,
    queryparser:::translation_environment_indirect_base
  ),
  test_files <- paste0("tests/testthat/",list.files("tests/testthat/", pattern = "\\.R$"))
)

env_result_tidy <- environment_coverage(
  env = with(
    queryparser:::translation_environment_direct_tidyverse,
    queryparser:::translation_environment_indirect_tidyverse
  ),
  test_files <- paste0("tests/testthat/",list.files("tests/testthat/", pattern = "\\.R$"))
)

result <- structure(c(
  pkg_result,
  env_result_base,
  env_result_tidy
), class = "coverage")

codecov(coverage = result)
