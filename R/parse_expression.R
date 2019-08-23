# Copyright 2019 Cloudera Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' SQL expression parser
#'
#' @description Parses a SQL expression into an R expression
#'
#' @param expr a character string containing a SQL expression
#' @param tidyverse set to \code{TRUE} to use functions from tidyverse packages
#'   including dplyr, stringr, and lubridate in the returned R expression
#'   expression (for example, \code{n} and \code{n_distinct})
#' @return an unevaluated R expression (a \code{\link{call}})
#' @examples
#' expr <- "round(AVG(arr_delay))"
#' parse_expression(expr)
#' @details The expression must not end with a column alias assignment. Use
#'   \code{\link{extract_alias}} to extract column alias assignments.
#' @export
parse_expression <- function(expr, tidyverse = FALSE) {
  expr <- trimws(expr, whitespace = ws_regex)

  # extract the column alias if there is one
  expr <- extract_alias(expr)
  column_alias <- names(expr)

  # mask text enclosed in quotations
  rc_in <- rawConnection(raw(0L), "r+")
  writeChar(expr, rc_in)
  len <- seek(rc_in, 0L) - 1L

  rc_out <- rawConnection(raw(0L), "r+")

  in_quotes <- FALSE
  was_in_quotes <- FALSE

  while((pos <- seek(rc_in, NA)) < len) {
    char <- readChar(rc_in, 1L)

    if (char %in% quote_chars) {
      if (!in_quotes) {
        in_quotes <- TRUE
        quo_char <- char
      } else if (char == quo_char) {
        seek(rc_in, -2L, "current")
        esc_quo <- c(quo_char, "\\")
        if (!readChar(rc_in, 1L) %in% esc_quo) {
          in_quotes <- FALSE
          rm(quo_char)
        }
        seek(rc_in, 1L, "current")
      }
    }

    if (in_quotes && was_in_quotes) {
      writeChar("\U001", rc_out, eos = NULL)
    } else {
      writeChar(char, rc_out, eos = NULL)
    }
    was_in_quotes <- in_quotes
  }

  seek(rc_out, 0L)
  expr_quotes_masked <- readChar(rc_out, len)

  close(rc_in)
  close(rc_out)

  # make the SQL query into a valid R expression
  expr_quotes_masked <- make_function_names_lowercase(expr_quotes_masked)

  expr_quotes_masked <- replace_operators_unary_postfix(expr_quotes_masked) # this must be second
  expr_quotes_masked <- replace_operators_binary_symbolic(expr_quotes_masked)
  expr_quotes_masked <- replace_operators_binary_word(expr_quotes_masked)
  expr_quotes_masked <- replace_operators_unary_prefix(expr_quotes_masked)


  # RESUME HERE

  # DEAL WITH
  #   * (the star)
  #   COUNT
  #   DISTINCT or ALL inside functions (just get rid of all)
  #     replace
  #       count followed by whitespace then ( then DISTINCT
  #     with
  #       count_distinct
  #

  # handle functions that have their arguments in a different order
  # maybe using a method like what's used in unpipe()

  # keep in mind that functions can have a space between the function name and the open paren


  # unmask text enclosed in quotations
  expr_out_split <- strsplit(expr_quotes_masked, "")[[1]]
  expr_out_split[expr_out_split == "\U001"] <- strsplit(expr, "")[[1]][expr_out_split == "\U001"]
  expr_out <- paste(expr_out_split, collapse = "")

  # convert from string to R expression
  call_out <- str2lang(expr_out) # most errors will happen on this line! try-catch here?

  # replace SQL functions with R functions
  if (tidyverse) {
    translation_environment_direct <- translation_environment_direct_tidyverse
    translation_environment_indirect <- translation_environment_indirect_tidyverse
  } else {
    translation_environment_direct <- translation_environment_direct_base
    translation_environment_indirect <- translation_environment_indirect_base
  }
  call_out <- do.call(substitute, list(call_out, translation_environment_direct))
  call_out <- partial_eval(call_out, translation_environment_indirect)
  call_out <- unpipe(call_out)

  call_out
}

make_function_names_lowercase <- function(expr_quotes_masked) {
  all_names <- c(
    names(translations_operators_binary_word),
    names(translations_operators_unary_prefix),
    names(translations_operators_unary_postfix),
    names(translations_direct_generic),
    names(translations_direct_base),
    names(translations_direct_tidyverse)
  )
  for (x in all_names) {
    expr_quotes_masked <- gsub(paste0("\\b",x,"\\b"), tolower(x), expr_quotes_masked, ignore.case = TRUE)
  }
  expr_quotes_masked
}

replace_operators_binary_symbolic <- function(expr_quotes_masked) {
  for (i in seq_along(translations_operators_binary_symbolic)) {
    expr_quotes_masked <- gsub(
      paste0("(",non_operator_regex, "+)(",names(translations_operators_binary_symbolic)[i],")(", non_operator_regex, "+)"),
      paste0("\\1",translations_operators_binary_symbolic[i],"\\3"),
      expr_quotes_masked
    )
  }
  expr_quotes_masked
}

replace_operators_binary_word <- function(expr_quotes_masked) {
  for (i in seq_along(translations_operators_binary_word)) {
    expr_quotes_masked <- gsub(
      paste0("\\b",names(translations_operators_binary_word)[i],"\\b"),
      translations_operators_binary_word[i],
      expr_quotes_masked
    )
  }
  expr_quotes_masked
}

replace_operators_unary_prefix <- function(expr_quotes_masked) {
  for (i in seq_along(translations_operators_unary_prefix)) {
    expr_quotes_masked <- gsub(
      paste0("\\b",names(translations_operators_unary_prefix)[i],"\\b"),
      translations_operators_unary_prefix[i],
      expr_quotes_masked
    )
  }
  expr_quotes_masked
}

replace_operators_unary_postfix <- function(expr_quotes_masked) {
  for(i in seq_along(translations_operators_unary_postfix)) {
    expr_quotes_masked <- gsub(
      paste0("\\b",names(translations_operators_unary_postfix)[i],"\\b"),
      translations_operators_unary_postfix[i],
      expr_quotes_masked
    )
  }
  expr_quotes_masked
}


