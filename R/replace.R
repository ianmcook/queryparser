# Copyright 2020 Cloudera Inc.
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

replace_special_keywords <- function(expr_quotes_masked) {

  special_keywords <- c("CAST", "TRY_CAST", "BETWEEN", "CASE")

  if (!grepl(paste0("\\b(", paste(special_keywords, collapse = "|"), ")\\b"), expr_quotes_masked, ignore.case = TRUE)) {
    return(expr_quotes_masked)
  }

  original_encoding <- Encoding(expr_quotes_masked)
  if (original_encoding == "unknown") {
    original_encoding <- "UTF-8"
  }
  Encoding(expr_quotes_masked) <- "bytes"
  nchar_bytes <- nchar(expr_quotes_masked, type = "bytes")

  # replace CAST
  expr_quotes_masked <- replace_cast_as(expr_quotes_masked, "CAST")

  # replace TRY_CAST
  expr_quotes_masked <- replace_cast_as(expr_quotes_masked, "TRY_CAST")

  # replace NOT BETWEEN and BETWEEN
  if (grepl(paste0("\\bBETWEEN\\b"), expr_quotes_masked, ignore.case = TRUE)) {

    expr_quotes_masked <- gsub(
      "\\bbetween\\b",
      "yesbetween",
      expr_quotes_masked,
      ignore.case = TRUE
    )
    expr_quotes_masked <- gsub(
      "\\bnot yesbetween\\b",
      "notbetween",
      expr_quotes_masked,
      ignore.case = TRUE
    )
    Encoding(expr_quotes_masked) <- "bytes"

    # identify positions of "x not between y and z"
    err <- "Found operator NOT BETWEEN without the keyword AND following it"
    not_between_and_pos <-
      find_keyword_pairs(expr_quotes_masked, "notbetween", "and", operands = TRUE, error_message = err)
    nchar_bytes <- nchar(expr_quotes_masked, type = "bytes")

    # replace "x not between y and z" with "(x < y | x > z)" or "!between(x,y,z)"
    char_offset <- 0
    for (pos in not_between_and_pos) {
      last_pos <- c(pos[1L], pos[2L] - 1L, pos[2L] + 10L, pos[3L] - 1L, pos[4L], nchar_bytes)
      first_pos <- c(0L, pos[1L] + 1, pos[2L], pos[2L] + 11L, pos[3L] + 4L, pos[4L] + 1L)
      last_pos[1L:6L] <- last_pos[1L:6L] - char_offset
      first_pos[2L:6L] <- first_pos[2L:6L] - char_offset
      repl_strings <- substring(expr_quotes_masked, first_pos, last_pos)
      length_before_replacement <- nchar(expr_quotes_masked, type = "bytes")
      repl_strings <- c(repl_strings[1], " notbetween(", repl_strings[2], ",", repl_strings[4], ",", repl_strings[5], ")", repl_strings[6])
      expr_quotes_masked <- paste(repl_strings, collapse = "")
      length_after_replacement <- nchar(expr_quotes_masked, type = "bytes")
      char_offset <-  char_offset + length_before_replacement - length_after_replacement
    }

    # identify positions of "x between y and z"
    err <- "Found operator BETWEEN without the keyword AND following it"
    between_and_pos <-
      find_keyword_pairs(expr_quotes_masked, "yesbetween", "and", operands = TRUE, error_message = err)
    nchar_bytes <- nchar(expr_quotes_masked, type = "bytes")

    # replace "x between y and z" with "(x >= y & x <= z)" or "between(x,y,z)"
    char_offset <- 0
    for (pos in between_and_pos) {
      last_pos <- c(pos[1L], pos[2L] - 1L, pos[2L] + 10L, pos[3L] - 1L, pos[4L], nchar_bytes)
      first_pos <- c(0L, pos[1L] + 1, pos[2L], pos[2L] + 11L, pos[3L] + 4L, pos[4L] + 1L)
      last_pos[1L:6L] <- last_pos[1L:6L] - char_offset
      first_pos[2L:6L] <- first_pos[2L:6L] - char_offset
      repl_strings <- substring(expr_quotes_masked, first_pos, last_pos)
      length_before_replacement <- nchar(expr_quotes_masked, type = "bytes")
      repl_strings <- c(repl_strings[1], " yesbetween(", repl_strings[2], ",", repl_strings[4], ",", repl_strings[5], ")", repl_strings[6])
      expr_quotes_masked <- paste(repl_strings, collapse = "")
      length_after_replacement <- nchar(expr_quotes_masked, type = "bytes")
      char_offset <-  char_offset + length_before_replacement - length_after_replacement
    }
  }

  # replace CASE
  if (grepl(paste0("\\bCASE\\b"), expr_quotes_masked, ignore.case = TRUE)) {

    # identify positions of "CASE ... END"
    err <- "Found CASE without END following it"
    case_end_pos <-
      find_keyword_pairs(expr_quotes_masked, "case", "end", operands = FALSE, error_message = err)
    nchar_bytes <- nchar(expr_quotes_masked, type = "bytes")

    char_offset <- 0
    for (pos in case_end_pos) {
      first_pos <- pos[1L] + 1L - char_offset
      last_pos <- pos[2L] + 3L - char_offset
      case_expression_in <- substring(expr_quotes_masked, first_pos, last_pos)

      err <- "In CASE expression, found WHEN without THEN following it"
      when_then_pos <-
        find_keyword_pairs(case_expression_in, "when", "then", operands = FALSE, error_message = err)

      num_when_then <- length(when_then_pos)
      if (num_when_then < 1) {
        stop("Found CASE expression without any WHEN ... THEN inside it", call. = FALSE)
      }

      before_first_when <- trimws(substring(
        case_expression_in,
        5L,
        when_then_pos[[1L]][1L]
      ))
      if (nchar(before_first_when) > 0) {
        case_expression_out <- paste0("casevalue(value = ", before_first_when, ", ")
      } else {
        case_expression_out <- "casewhen("
      }

      after_last_then <- substring(
        case_expression_in,
        when_then_pos[[num_when_then]][2L] + 5L
      )
      if (grepl("\\belse\\b", after_last_then, ignore.case = TRUE)) {
        else_end_pos <-
          find_keyword_pairs(after_last_then, "else", "end", operands = FALSE)[[1]]
      } else {
        else_end_pos <- NULL
      }
      if (!is.null(else_end_pos)) {
        final_then_operand_boundary <-
          when_then_pos[[num_when_then]][2L] + 5L + else_end_pos[1L] - 1L
      } else {
        final_then_operand_boundary <- nchar(case_expression_in) - 3L
      }

      for (i in seq_along(when_then_pos)) {
        if (i < length(when_then_pos)) {
          end_then_pos <- when_then_pos[[i+1L]][1L] # - 1L?
        } else {
          end_then_pos <- final_then_operand_boundary
        }
        case_expression_out <- paste0(
          case_expression_out,
          ifelse(i == 1, "", ", "),
          trimws(substr(case_expression_in, when_then_pos[[i]][1L] + 5L, when_then_pos[[i]][2L])),
          ", ",
          trimws(substr(case_expression_in, when_then_pos[[i]][2L] + 5L, end_then_pos))
        )
      }

      if (!is.null(else_end_pos)) {
        case_expression_out <- paste0(
          case_expression_out,
          ", otherwise = ",
          trimws(substring(
            after_last_then,
            else_end_pos[1L] + 5L,
            else_end_pos[2L]
          ))
        )
      }

      case_expression_out <- paste0(case_expression_out, ")")

      length_before_replacement <- nchar(case_expression_in)
      length_after_replacement <- nchar(case_expression_out)
      expr_quotes_masked <- paste0(
        substring(expr_quotes_masked, 1, first_pos - 1L),
        case_expression_out,
        substring(expr_quotes_masked, last_pos + 1L, nchar_bytes - char_offset)
      )
      char_offset <-  char_offset + length_before_replacement - length_after_replacement
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
  for (i in seq_along(translations_operators_unary_postfix)) {
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

replace_qualified_names <- function(expr_quotes_masked) {
  if (!grepl(paste0("\\.", quote_char_regex, MASKING_CHARACTER), expr_quotes_masked) &&
      !grepl(paste0(MASKING_CHARACTER, quote_char_regex, "\\."), expr_quotes_masked)) {
    return(expr_quotes_masked)
  }
  # a.`b` --> `a.b`
  expr_quotes_masked <- gsub(
    paste0("\\b(",word_char_regex,"+?)\\.(",quote_char_regex,")(",MASKING_CHARACTER,"+?)(",quote_char_regex,")"),
    "\\2\\1.\\3\\4",
    expr_quotes_masked
  )
  # `a`.b --> `a.b`
  expr_quotes_masked <- gsub(
    paste0("(",quote_char_regex,")(",MASKING_CHARACTER,"+?)(",quote_char_regex,")\\.(",word_char_regex,"+?)\\b"),
    "\\1\\2.\\4\\3",
    expr_quotes_masked
  )
  # `a`.`b` --> `a.b`
  expr_quotes_masked <- gsub(
    paste0("(",quote_char_regex,")(",MASKING_CHARACTER,"+?)(",quote_char_regex,")\\.(",quote_char_regex,")(",MASKING_CHARACTER,"+?)(",quote_char_regex,")"),
    "\\1\\2.\\5\\1",
    expr_quotes_masked
  )
  expr_quotes_masked
}

replace_cast_as <- function(expr_quotes_masked, func) {
  if (!grepl(paste0("\\b", func, "\\b"), expr_quotes_masked, ignore.case = TRUE)) {
    return(expr_quotes_masked)
  }

  # identify positions of "func(x as y)"
  err <- paste0("Found function ", func, "() without the keyword AS inside the parentheses after it")
  cast_as_pos <-
    find_keyword_pairs(expr_quotes_masked, func, "as", parens_diff = +1, error_message = err)
  nchar_bytes <- nchar(expr_quotes_masked, type = "bytes")

  # replace "func(x as y)" with "func(x,y)"
  iter <- 0L
  for (pos in cast_as_pos) {
    first_pos <- c(0L, pos[1L] + 1L, pos[2L] + 1L, pos[2L] + 3L)
    first_pos[3L:4L] <- first_pos[3L:4L] - iter
    last_pos <- c(first_pos[2L:4L] - 1L, nchar_bytes - iter)
    repl_strings <- substring(expr_quotes_masked, first_pos, last_pos)
    repl_strings <- c(repl_strings[1], repl_strings[2], ",", repl_strings[4])
    expr_quotes_masked <- paste(repl_strings, collapse = "")
    iter <- iter + 1L # each iteration changes "as" to "," which shortens the string on the left by 1
  }

  expr_quotes_masked
}
