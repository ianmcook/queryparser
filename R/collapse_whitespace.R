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

#' Collapse whitespace
#'
#' @description Replaces each unquoted run of whitespace characters with a
#'   single space
#'
#' @param x a character string
#' @return a character string
collapse_whitespace <- function(x) {

  rc_in <- rawConnection(raw(0L), "r+")
  writeChar(x, rc_in)
  len <- seek(rc_in, 0L) - 1L

  rc_out <- rawConnection(raw(0L), "r+")

  in_quotes <- FALSE
  in_ws <- FALSE

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
      writeChar(char, rc_out, eos = NULL)
      in_ws <- FALSE
      next;
    }

    if (!in_quotes) {
      if (isTRUE(grepl(ws_regex, char))) {
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

  seek(rc_out, 0L)
  out <- readChar(rc_out, len) # len might be too long but that's ok

  close(rc_in)
  close(rc_out)

  out
}
