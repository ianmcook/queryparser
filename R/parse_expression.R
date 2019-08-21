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

extract_alias <- function(expr) {
  expr <- trimws(expr, whitespace = ws_regex)

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
  char_is_quote_escape <- FALSE
  seek(rc, len + 1)
  while((pos <- seek(rc, NA)) > 1) {
    seek(rc, pos - 2)
    char <- readChar(rc, 1L)

    if (char %in% quote_chars) {
      if (pos == len + 1) {
        quote_at_end <- TRUE
      }
      if (!in_quotes) {
        in_quotes <- TRUE
        quo_char <- char
      } else if (char == quo_char && !char_is_quote_escape) {
        seek(rc, -2L, "current")
        esc_quo <- c(quo_char, "\\")
        if (readChar(rc, 1L) %in% esc_quo) {
          char_is_quote_escape <- TRUE
        } else {
          char_is_quote_escape <- FALSE
          in_quotes <- FALSE
          rm(quo_char)
        }
        seek(rc, 1L, "current")
      }
    }

    if (look_for_char_before_alias) {
      if (quoted_string_at_end && !found_as_before_alias) {
        seek(rc, 0)
        expr_without_alias <- trimws(readChar(rc, pos - 1), whitespace = ws_regex)
      } else if (grepl(non_word_char_regex, char)) {
        seek(rc, 0)
        expr_without_alias <- trimws(readChar(rc, pos - 1), whitespace = ws_regex)
      } else if (found_as_before_alias) {
        seek(rc, 0)
        expr_without_alias <- trimws(readChar(rc, pos + 1), whitespace = ws_regex)
      }
      break;
    }

    if (!in_quotes && !char %in% quote_chars && !grepl(ws_regex, char) && !grepl(word_char_regex, char)) {
      break;
    }

    if (look_for_as_keyword) {
      if (identical(tolower(char), " ")) {
        seek(rc, -2L, "current")
        char <- readChar(rc, 1L)
      }
      if (identical(tolower(char), "s")) {
        seek(rc, -2L, "current")
        char <- readChar(rc, 1L)
        if (identical(tolower(char), "a")) {
          found_as_before_alias <- TRUE
          pos <- pos + 1
        } else {
          seek(rc, pos)
        }
      } else {
        seek(rc, pos)
      }
      look_for_as_keyword <- FALSE
      look_for_char_before_alias <- TRUE
    }

    if (quote_at_end && was_in_quotes && !in_quotes) {
      quoted_string_at_end <- TRUE
      look_for_as_keyword <- TRUE
      column_alias <- readChar(rc, len - pos)
      seek(rc, pos - 1L)
    }

    if (possible_word_at_end && grepl(non_word_char_regex, char)) {
      if (grepl(word_start_regex, readChar(rc, 1L))) {
        seek(rc, -1L, "current")
        column_alias <- readChar(rc, len - pos + 1L)
        if (char == " ") {
          look_for_as_keyword <- TRUE
        } else {
          look_for_char_before_alias <- TRUE
        }
      }
      seek(rc, pos)
      possible_word_at_end <- FALSE
    }

    if (pos == len + 1 && grepl(word_char_regex, char)) {
      possible_word_at_end <- TRUE
    }

    was_in_quotes <- in_quotes
  }

  if (is.null(column_alias) || is.null(expr_without_alias)) {
    expr
  } else {
    names(expr_without_alias) <- column_alias
    expr_without_alias
  }
}

