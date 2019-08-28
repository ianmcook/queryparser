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

#' Parse a SQL expression
#'
#' @description Parses a SQL expression into an R expression
#'
#' @param expr a character string containing a SQL expression
#' @param tidyverse set to \code{TRUE} to use functions from tidyverse packages
#'   including dplyr, stringr, and lubridate in the returned R expression
#'   expression (for example, \code{n} and \code{n_distinct})
#' @param secure set to \code{FALSE} to allow potentially dangerous functions in
#'   the returned R expression
#' @return an unevaluated R expression (a \code{\link{call}})
#' @examples
#' expr <- "round(AVG(arr_delay))"
#' parse_expression(expr)
#' @details The expression must not end with a column alias assignment. Use
#'   \code{\link{extract_alias}} to extract column alias assignments.
#'
#'   The expression must not contain any unquoted whitespace characters except
#'   spaces, and there must be no unquoted runs or two or more spaces. Use
#'   \code{\link{squish_sql}} to satisfy this whitespace requirement.
#' @export
parse_expression <- function(expr, tidyverse = FALSE, secure = TRUE) {
  if (!identical(typeof(expr), "character") ||
      !identical(length(expr), 1L) ||
      !identical(typeof(tidyverse), "logical") ||
      !identical(length(tidyverse), 1L) ||
      !identical(typeof(secure), "logical") ||
      !identical(length(secure), 1L)) {
    stop("Unexpected input to parse_expression()", call. = FALSE)
  }

  expr <- trimws(expr, whitespace = ws_regex)

  # mask text enclosed in quotations
  rc_in <- rawConnection(raw(0L), "r+")
  writeChar(expr, rc_in)
  len <- seek(rc_in, 0L) - 1L

  rc_out <- rawConnection(raw(0L), "r+")
  rc_quo <- rawConnection(raw(0L), "r+")

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
      writeChar(MASKING_CHARACTER, rc_out, eos = NULL)
      writeChar(char, rc_quo, eos = NULL)
    } else {
      writeChar(char, rc_out, eos = NULL)
    }
    was_in_quotes <- in_quotes
  }

  seek(rc_out, 0L)
  expr_quotes_masked <- readChar(rc_out, len)

  seek(rc_quo, 0L)
  masked_chars <- readChar(rc_quo, len)

  close(rc_in)
  close(rc_out)
  close(rc_quo)

  # make the SQL query into a valid R expression
  expr_quotes_masked <- make_function_names_and_keywords_lowercase(expr_quotes_masked) # this must be first
  expr_quotes_masked <- replace_all_distinct_keyword(expr_quotes_masked) # this must be second
  expr_quotes_masked <- replace_operators_unary_postfix(expr_quotes_masked) # this must be third
  expr_quotes_masked <- replace_star(expr_quotes_masked, tidyverse)
  expr_quotes_masked <- replace_operators_binary_symbolic(expr_quotes_masked)
  expr_quotes_masked <- replace_special_functions(expr_quotes_masked)
  expr_quotes_masked <- replace_special_keywords(expr_quotes_masked, tidyverse)
  expr_quotes_masked <- replace_null_with_na(expr_quotes_masked)
  expr_quotes_masked <- replace_in_operator(expr_quotes_masked)
  expr_quotes_masked <- replace_operators_binary_word(expr_quotes_masked)
  expr_quotes_masked <- replace_operators_unary_prefix(expr_quotes_masked)
  expr_quotes_masked <- quote_data_types(expr_quotes_masked) # this must be last

  # unmask text enclosed in quotations
  if (length(masked_chars) < 1 || nchar(masked_chars) < 1) {
    expr_out <- expr_quotes_masked
  } else {
    expr_quotes_masked_split <- strsplit(expr_quotes_masked, "")[[1]]
    masked_chars_split <- strsplit(masked_chars, "")[[1]]
    expr_out <- paste(
      replace(
        expr_quotes_masked_split,
        expr_quotes_masked_split == MASKING_CHARACTER,
        masked_chars_split
      ),
      collapse = ""
    )
  }

  # parse the string and return an unevaluated R expression
  call_out <- str2lang(expr_out) # most errors will happen on this line! try-catch here?

  # stop if contains illegal functions or operators
  if (secure) {
    secure_expression(call_out)
  }

  # stop if any column names are R reserved words
  all_words <- all_cols(call_out)
  res_words <- c(
    intersect(all_words, r_reserved_words),
    all_words[substr(all_words, 1, 2) == ".."]
  )
  if (length(res_words) > 0) {
    stop(
      "Query contains R reserved words: ",
      paste(res_words, collapse = ", "),
      call. = FALSE
    )
  }

  # translate SQL functions to R functions
  call_out <- translate_distinct_functions(call_out, tidyverse) # this must be second
  call_out <- translate_nin(call_out)
  call_out <- translate_direct(call_out, tidyverse)
  call_out <- translate_indirect(call_out, tidyverse)
  call_out <- unpipe(call_out) # this must be second to last
  call_out <- wrap_bangs(call_out) # this must be last

  call_out
}
