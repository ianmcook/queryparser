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

#' Squish a SQL query or SQL expression
#'
#' @description Replaces every unquoted run of whitespace characters with a
#'   single space and removes all line comments (\code{--}) and block comments
#'   (\code{/* */}). Whitespace and comment marks within quotes are not
#'   modified.
#'
#' @param x a character string containing a SQL query or expression
#' @return a character string containing the squished query or expression with
#'   comments removed
#' @export
squish_sql <- function(x) {
  if (!identical(typeof(x), "character") || !identical(length(x), 1L)) {
    stop("Unexpected input to squish_sql()", call. = FALSE)
  }

  rc_in <- rawConnection(raw(0L), "r+")
  writeChar(x, rc_in)
  len <- seek(rc_in, 0L) - 1L

  rc_out <- rawConnection(raw(0L), "r+")

  in_quotes <- FALSE
  in_ws <- FALSE
  in_line_comment <- FALSE
  in_block_comment <- FALSE
  escaped <- FALSE

  while((pos <- seek(rc_in, NA)) < len) {
    char <- readChar(rc_in, 1L)

    if (in_line_comment) {
      if (identical(char, "\n")) {
        in_line_comment <- FALSE
      }
      next;
    } else if (in_block_comment) {
      if(identical(char, "*")) {
        if (identical(readChar(rc_in, 1L), "/")) {
          in_block_comment <- FALSE
        } else {
          seek(rc_in, -1L, "current")
        }
      }
      next;
    } else if (char %in% quote_chars) {
      if (!in_quotes) {
        in_quotes <- TRUE
        escaped <- FALSE
        quo_char <- char
      } else if (char == quo_char) {
        if (escaped) {
          escaped <- FALSE
        } else {
          esc_quo <- c(quo_char, "\\")
          if (!readChar(rc_in, 1L) %in% esc_quo) {
            in_quotes <- FALSE
            escaped <- FALSE
            rm(quo_char)
          } else {
            escaped <- TRUE
          }
          seek(rc_in, -1L, "current")
        }
      }
      writeChar(char, rc_out, eos = NULL)
      in_ws <- FALSE
      next;
    } else {
      escaped <- FALSE
    }

    if (!in_quotes && !in_line_comment && !in_block_comment) {
      if (identical(char, "-")) {
        if (identical(readChar(rc_in, 1L), "-")) {
          in_line_comment <- TRUE
          next;
        }
        seek(rc_in, -1L, "current")
      } else if (identical(char, "/")) {
        if (identical(readChar(rc_in, 1L), "*")) {
          in_block_comment <- TRUE
          next;
        }
        seek(rc_in, -1L, "current")
      } else if (isTRUE(is_whitespace_character(char))) {
        # this is a whitespace character
        if (in_ws) {
          # was already in whitespace
          # so write nothing
          next;
        } else {
          # is first whitespace character
          # so write a space
          writeChar(" ", rc_out, eos = NULL)
          in_ws <- TRUE
          next;
        }
      } else {
        writeChar(char, rc_out, eos = NULL)
        in_ws <- FALSE
        next;
      }
    }

    writeChar(char, rc_out, eos = NULL)
  }

  if (in_block_comment) {
    stop("Query or expression contains unclosed block comment", call. = FALSE)
  }

  seek(rc_out, 0L)
  out <- readChar(rc_out, len) # len might be too long but that's ok

  close(rc_in)
  close(rc_out)

  out
}
