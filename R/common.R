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

quote_chars <- c("\"", "'", "`")

ws_regex <- "[ \t\r\n]"

word_char_regex <- "[A-Za-z0-9_]"

non_word_char_regex <- "[^A-Za-z0-9_]"

word_start_regex <- "[A-Za-z_]"

is_word_start_character <- function(char) {
  grepl(word_start_regex, char)
}

is_word_character <- function(char) {
  grepl(word_char_regex, char)
}

keyword_starts_here <- function(rc, keyword) {
  pos <- seek(rc, NA)
  on.exit(seek(rc, pos))
  at_start <- tryCatch({
    seek(rc, -1L, "current")
    FALSE
  }, error = function(e) {
    TRUE
  })
  if (at_start) {
    begin_regex <- ""
    nchars <- nchar(keyword) + 1L
  } else {
    begin_regex <- non_word_char_regex
    nchars <- nchar(keyword) + 2L
  }
  chars <- readChar(rc, nchars)
  keyword_regex <- paste0(
    "^",
    begin_regex,
    keyword,
    non_word_char_regex,
    "$"
  )
  grepl(keyword_regex,  chars, ignore.case = TRUE)
}

# replaces each unquoted run of whitespace
# characters with a single space
collapse_whitespace <- function(expr) {

  rc_in <- rawConnection(raw(0L), "r+")
  writeChar(expr, rc_in)
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
