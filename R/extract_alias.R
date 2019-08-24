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

#' Extract column alias from SQL expression
#'
#' @description Extracts the column alias from an expression used in the SELECT
#'   list of a SQL query
#'
#' @param expr a character string containing a SQL expression which might have a
#'   column alias assignment at the end
#' @return a character string containing the inputed SQL expression with the
#'   column alias assignment removed (if it was present) and with the assigned
#'   alias as its name
#' @examples
#' expr <- "round(AVG(arr_delay)) AS avg_delay"
#' extract_alias(expr)
#' @export
extract_alias <- function(expr) {
  expr <- trimws(expr, whitespace = ws_regex)

  bytes_in_chars <- nchar(strsplit(expr, "")[[1]], "bytes")

  rc <- rawConnection(raw(0L), "r+")
  on.exit(close(rc))
  writeChar(expr, rc)
  len <- seek(rc, 0L) - 1L

  column_alias <- NULL
  expr_without_alias <- NULL

  found_as_before_alias <- FALSE
  look_for_char_before_alias <- FALSE
  look_for_as_keyword <- FALSE
  quoted_string_at_end <- FALSE
  quote_at_end <- FALSE
  possible_word_at_end <- FALSE
  was_in_quotes <- FALSE
  in_quotes <- FALSE

  # go backwards, from end toward beginning
  step_number <- 0
  step_positions <- c(rev(cumsum(bytes_in_chars)),0)[-1]
  advance_positions <- 1
  char_is_quote_escape <- FALSE
  while(TRUE) {
    if (advance_positions > 0) {
      if (step_number + advance_positions > length(step_positions)) {
        break;
      }
      step_number <- step_number + advance_positions
      pos <- step_positions[step_number]
    }
    advance_positions <- 1
    seek(rc, pos)
    char <- readChar(rc, 1L)

    if (char %in% quote_chars) {
      if (pos == step_positions[1]) {
        quote_at_end <- TRUE
      }
      if (!in_quotes) {
        in_quotes <- TRUE
        quo_char <- char
      } else if (char == quo_char && !char_is_quote_escape) {
        if (pos == 0) {
          in_quotes <- FALSE
        } else {
          seek(rc, -2L, "current")
          esc_quo <- c(quo_char, "\\")
          if (readChar(rc, 1L, useBytes = TRUE) %in% esc_quo) {
            char_is_quote_escape <- TRUE
          } else {
            char_is_quote_escape <- FALSE
            in_quotes <- FALSE
            rm(quo_char)
          }
          seek(rc, 1L, "current")
        }
      }
    }

    if (look_for_char_before_alias) {
      if (quoted_string_at_end && !found_as_before_alias) {
        seek(rc, 0)
        expr_without_alias <- trimws(readChar(rc, pos + 1, useBytes = TRUE), whitespace = ws_regex)
      } else if (is_non_word_character(char)) {
        seek(rc, 0)
        expr_without_alias <- trimws(readChar(rc, pos + 1, useBytes = TRUE), whitespace = ws_regex)
      } else if (found_as_before_alias) {
        seek(rc, 0)
        expr_without_alias <- trimws(readChar(rc, pos + 3, useBytes = TRUE), whitespace = ws_regex)
      }
      break;
    }

    if (
        !in_quotes && !char %in% quote_chars &&
        !possible_word_at_end &&
        !is_whitespace_character(char) &&
        !is_word_character(char)) {
      break;
    }

    if (look_for_as_keyword) {
      advance_positions <- 0
      prev_char <- char
      if (identical(prev_char, " ")) {
        seek(rc, -2L, "current")
        prev_char <- readChar(rc, 1L, useBytes = TRUE)
        advance_positions <- 1
      }
      if (prev_char %in% c("s","S")) {
        seek(rc, -2L, "current")
        prev_char <- readChar(rc, 1L, useBytes = TRUE)
        if (prev_char %in% c("a","A")) {
          found_as_before_alias <- TRUE
          advance_positions <- advance_positions + 2
        } else {
          advance_positions <- 0
        }
      } else {
        advance_positions <- 0
      }
      look_for_as_keyword <- FALSE
      look_for_char_before_alias <- TRUE
    }

    if (quote_at_end && was_in_quotes && !in_quotes) {
      quoted_string_at_end <- TRUE
      look_for_as_keyword <- TRUE
      column_alias <- readChar(rc, len - pos - 2, useBytes = TRUE)
    }

    if (possible_word_at_end && is_non_word_character(char)) {
      next_char <- readChar(rc, 1L)
      if (is_word_start_character(next_char)) {
        seek(rc, -1 * nchar(next_char, type = "bytes"), "current")
        column_alias <- readChar(rc, len - pos - 1L, useBytes = TRUE)
        if (char == " ") {
          look_for_as_keyword <- TRUE
        } else {
          look_for_char_before_alias <- TRUE
        }
      }
      advance_positions <- 0
      possible_word_at_end <- FALSE
    }

    if (pos == step_positions[1] && is_word_character(char)) {
      possible_word_at_end <- TRUE
    }

    was_in_quotes <- in_quotes
  }

  if (is.null(column_alias) || is.null(expr_without_alias)) {
    expr
  } else if (tolower(column_alias) %in% sql_reserved_words && !quoted_string_at_end) {
    expr
  } else {
    names(expr_without_alias) <- column_alias
    expr_without_alias
  }
}
