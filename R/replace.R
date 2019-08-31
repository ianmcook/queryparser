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

replace_null_with_na <- function(expr_quotes_masked) {
  gsub(
    paste0("\\bnull\\b"),
    "NA",
    expr_quotes_masked,
    ignore.case = TRUE
  )
}

replace_in_operator <- function(expr_quotes_masked) {
  # need to change this when adding support for subqueries
  expr_quotes_masked <- gsub(
    "\\bnot in ?\\(",
    "%nin% c(", # this `%nin%` is translated later by translate_nin()
    expr_quotes_masked,
    ignore.case = TRUE
  )
  expr_quotes_masked <- gsub(
    "\\bin ?\\(",
    "%in% c(",
    expr_quotes_masked,
    ignore.case = TRUE
  )
  expr_quotes_masked
}

replace_special_functions <- function(expr_quotes_masked) {
  gsub(
    "\\bif ?\\(",
    "ifelse(",
    expr_quotes_masked,
    ignore.case = TRUE
  )
}

replace_special_keywords <- function(expr_quotes_masked, tidyverse) {

  special_keywords <- c("CAST", "BETWEEN")

  if (!grepl(paste0("\\b(", paste(special_keywords, collapse = "|"), ")\\b"), expr_quotes_masked, ignore.case = TRUE)) {
    return(expr_quotes_masked)
  }

  original_encoding <- Encoding(expr_quotes_masked)
  Encoding(expr_quotes_masked) <- "bytes"
  nchar_bytes <- nchar(expr_quotes_masked, type = "bytes")

  # replace CAST
  if (grepl(paste0("\\bCAST\\b"), expr_quotes_masked, ignore.case = TRUE)) {

    # identify positions of "cast(x as y)"
    cast_as_pos <- find_keyword_pairs(expr_quotes_masked, "cast", "as", parens_diff = +1)

    # replace "cast(x as y)" with "cast(x,y)"
    for (pos in cast_as_pos) {
      first_pos <- c(0L, pos + 1L)
      first_pos[3L] <- first_pos[3L]
      first_pos <- append(first_pos, first_pos[3L] + 2L)
      last_pos <- c(first_pos[2L:4L] - 1L, nchar_bytes)
      #last_pos <- c(last_pos[1L], last_pos + 2L, nchar_bytes)
      repl_strings <- substring(expr_quotes_masked, first_pos, last_pos)
      repl_strings <- c(repl_strings[1], repl_strings[2], ",", repl_strings[4])
      expr_quotes_masked <- paste(repl_strings, collapse = "")
    }
  }

  # replace NOT BETWEEN
  if (grepl(paste0("\\bNOT BETWEEN\\b"), expr_quotes_masked, ignore.case = TRUE)) {

    # identify positions of "x not between y and z"
    # (need to do this before identifying positions of "x between y and z")
    expr_quotes_masked <- gsub(
      "\\bnot between\\b",
      "notbetween",
      expr_quotes_masked,
      ignore.case = TRUE
    )
    not_between_and_pos <- find_keyword_pairs(expr_quotes_masked, "notbetween", "and", operands = TRUE)

    # replace "x not between y and z" with "(x < y | x > z)" or "!between(x,y,z)"
    for (pos in not_between_and_pos) {
      last_pos <- c(pos[1L], pos[2L] - 1L, pos[2L] + 10L, pos[3L] - 1L, pos[4L], nchar_bytes)
      first_pos <- c(0L, pos[1L] + 1, pos[2L], pos[2L] + 11L, pos[3L] + 4L, pos[4L] + 1L)
      repl_strings <- substring(expr_quotes_masked, first_pos, last_pos)
      if (tidyverse) {
        repl_strings <- c(repl_strings[1], " !dplyr::between(", repl_strings[2], ",", repl_strings[4], ",", repl_strings[5], ")", repl_strings[6])
      } else {
        repl_strings <- c(repl_strings[1], " (", repl_strings[2], "<", repl_strings[4], " | ", repl_strings[2], ">", repl_strings[5], ")", repl_strings[6])
      }
      expr_quotes_masked <- paste(repl_strings, collapse = "")
    }
  }

  # replace BETWEEN
  if (grepl(paste0("\\bBETWEEN\\b"), expr_quotes_masked, ignore.case = TRUE)) {

    # identify positions of "x between y and z"
    between_and_pos <- find_keyword_pairs(expr_quotes_masked, "between", "and", operands = TRUE)

    # replace "x between y and z" with "(x >= y & x <= z)" or "between(x,y,z)"
    for (pos in between_and_pos) {
      last_pos <- c(pos[1L], pos[2L] - 1L, pos[2L] + 7L, pos[3L] - 1L, pos[4L], nchar_bytes)
      first_pos <- c(0L, pos[1L] + 1, pos[2L], pos[2L] + 8L, pos[3L] + 4L, pos[4L] + 1L)
      repl_strings <- substring(expr_quotes_masked, first_pos, last_pos)
      if (tidyverse) {
        repl_strings <- c(repl_strings[1], " dplyr::between(", repl_strings[2], ",", repl_strings[4], ",", repl_strings[5], ")", repl_strings[6])
      } else {
        repl_strings <- c(repl_strings[1], " (", repl_strings[2], ">=", repl_strings[4], " & ", repl_strings[2], "<=", repl_strings[5], ")", repl_strings[6])
      }
      expr_quotes_masked <- paste(repl_strings, collapse = "")
    }
  }

  Encoding(expr_quotes_masked) <- original_encoding

  expr_quotes_masked
}

replace_operators_binary_symbolic <- function(expr_quotes_masked) {
  for (i in seq_along(translations_operators_binary_symbolic)) {
    expr_quotes_masked <- gsub(
      paste0("(",non_operator_regex, ")(",names(translations_operators_binary_symbolic)[i],")(", non_operator_regex, ")"),
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
      expr_quotes_masked,
      ignore.case = TRUE
    )
  }
  expr_quotes_masked
}

replace_operators_unary_prefix <- function(expr_quotes_masked) {
  for (i in seq_along(translations_operators_unary_prefix)) {
    expr_quotes_masked <- gsub(
      paste0("\\b",names(translations_operators_unary_prefix)[i],"\\b"),
      translations_operators_unary_prefix[i],
      expr_quotes_masked,
      ignore.case = TRUE
    )
  }
  expr_quotes_masked
}

replace_operators_unary_postfix <- function(expr_quotes_masked) {
  for(i in seq_along(translations_operators_unary_postfix)) {
    expr_quotes_masked <- gsub(
      paste0("\\b",names(translations_operators_unary_postfix)[i],"\\b"),
      translations_operators_unary_postfix[i],
      expr_quotes_masked,
      ignore.case = TRUE
    )
  }
  expr_quotes_masked
}

replace_all_distinct_keyword <- function(expr_quotes_masked) {
  agg_funs <- paste(sql_aggregate_functions, collapse = "|")
  expr_quotes_masked <- gsub(
    paste0("\\b(",agg_funs,") ?\\( ?all\\b"),
    "\\1",
    expr_quotes_masked,
    ignore.case = TRUE
  )
  expr_quotes_masked <- gsub(
    paste0("\\b(",agg_funs,") ?\\( ?distinct\\b"),
    "\\1_distinct(",
    expr_quotes_masked,
    ignore.case = TRUE
  )
  expr_quotes_masked
}

replace_star <- function(expr_quotes_masked, tidyverse) {
  if (expr_quotes_masked == "*") {
    if (tidyverse) {
      return("dplyr::everything()")
    } else {
      return(".")
    }
  }
  gsub(
    paste0("\\bcount ?\\( ?\\*"),
    "count_star(",
    expr_quotes_masked,
    ignore.case = TRUE
  )
}
