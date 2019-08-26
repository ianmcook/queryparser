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

word_char_regex <- "\\w"

non_word_char_regex <- "\\W"

digit_regex <- "[0-9]"

non_operator_regex <- "[^<>=%!]"

sql_characters_expecting_right_operands <- c(
  ">", "<", "=", "&", "|",
  "+", "-", "*", "/", "%",
  "^", "!", "~"
)

sql_words_expecting_right_operands <- c(
  "and", "or", "as", "from", "not",
  "in", "div", "between",
  "all", "any", "some",
  "like", "ilike", "regexp", "iregexp", "rlike"
)

sql_reserved_words <- c(
  "add",
  "aggregate",
  "all",
  "alter",
  "analytic",
  "and",
  "anti",
  "api_version",
  "array",
  "as",
  "asc",
  "authorization",
  "between",
  "bigint",
  "binary",
  "boolean",
  "both",
  "buckets",
  "by",
  "cached",
  "cascade",
  "case",
  "cast",
  "change",
  "char",
  "class",
  "close_fn",
  "column",
  "columns",
  "comment",
  "compute",
  "conf",
  "create",
  "cross",
  "cube",
  "current",
  "current_date",
  "current_timestamp",
  "cursor",
  "data",
  "database",
  "databases",
  "date",
  "datetime",
  "decimal",
  "delete",
  "delimited",
  "desc",
  "describe",
  "distinct",
  "distribute",
  "div",
  "double",
  "drop",
  "else",
  "end",
  "escaped",
  "exchange",
  "exists",
  "explain",
  "extended",
  "external",
  "false",
  "fetch",
  "fields",
  "fileformat",
  "finalize_fn",
  "first",
  "float",
  "following",
  "for",
  "format",
  "formatted",
  "from",
  "full",
  "function",
  "functions",
  "grant",
  "group",
  "grouping",
  "hash",
  "having",
  "if",
  "ignore",
  "ilike",
  "import",
  "in",
  "incremental",
  "init_fn",
  "inner",
  "inpath",
  "insert",
  "int",
  "integer",
  "intermediate",
  "intersect",
  "interval",
  "into",
  "invalidate",
  "iregexp",
  "is",
  "join",
  "last",
  "lateral",
  "left",
  "less",
  "like",
  "limit",
  "lines",
  "load",
  "local",
  "location",
  "macro",
  "map",
  "merge_fn",
  "metadata",
  "more",
  "none",
  "not",
  "null",
  "nulls",
  "of",
  "offset",
  "on",
  "or",
  "order",
  "out",
  "outer",
  "over",
  "overwrite",
  "partialscan",
  "partition",
  "partitioned",
  "partitions",
  "percent",
  "preceding",
  "prepare_fn",
  "preserve",
  "procedure",
  "produced",
  "purge",
  "range",
  "rcfile",
  "reads",
  "real",
  "reduce",
  "refresh",
  "regexp",
  "rename",
  "replace",
  "restrict",
  "returns",
  "revoke",
  "right",
  "rlike",
  "role",
  "roles",
  "rollup",
  "row",
  "rows",
  "schema",
  "schemas",
  "select",
  "semi",
  "sequencefile",
  "serdeproperties",
  "serialize_fn",
  "set",
  "show",
  "smallint",
  "split",
  "stats",
  "stored",
  "straight_join",
  "string",
  "symbol",
  "table",
  "tables",
  "tablesample",
  "tblproperties",
  "terminated",
  "textfile",
  "then",
  "timestamp",
  "tinyint",
  "to",
  "transform",
  "trigger",
  "true",
  "truncate",
  "unbounded",
  "uncached",
  "union",
  "uniquejoin",
  "update",
  "update_fn",
  "use",
  "user",
  "using",
  "utc_tmestamp",
  "values",
  "varchar",
  "view",
  "when",
  "where",
  "window",
  "with"
)

is_whitespace_character <- function(char, useBytes = FALSE) {
  grepl(ws_regex, char, useBytes = useBytes)
}

is_word_start_character <- function(char, useBytes = FALSE) {
  grepl(word_char_regex, char, useBytes = useBytes) && !grepl(digit_regex, char, useBytes = useBytes)
}

is_word_character <- function(char, useBytes = FALSE) {
  grepl(word_char_regex, char, useBytes = useBytes)
}

is_non_word_character <- function(char, useBytes = FALSE) {
  grepl(non_word_char_regex, char, useBytes = useBytes)
}

keyword_starts_here <- function(rc, keyword) {
  pos <- seek(rc, NA)
  on.exit(seek(rc, pos))
  at_start <- tryCatch({
    seek(rc, -1L, "current")
    seek(rc, -1L, "current")
    FALSE
  }, error = function(e) {
    TRUE
  })
  if (at_start) {
    begin_regex <- ""
    nchars <- nchar(keyword, type = "bytes") + 1L
  } else {
    begin_regex <- non_word_char_regex
    nchars <- nchar(keyword, type = "bytes") + 2L
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

find_keyword_pairs <- function(expr_quotes_masked, keyword_1, keyword_2, right_operand = FALSE, parens_diff = 0) {
  # returns the positions of the start of each keyword
  # in every instance of the specified matching pair of keywords
  # both at the same parentheses nesting level

  keyword_1_length <- nchar(keyword_1, type = "bytes")
  keyword_2_length <- nchar(keyword_2, type = "bytes")

  # only for use on expressions with quoted text masked

  keyword_pair_pos <- list()

  rc <- rawConnection(raw(0L), "r+")

  writeChar(paste0(expr_quotes_masked, " "), rc)
  len <- seek(rc, 0L) - 1L

  in_parens <- 0

  pos <- -1
  while(pos < len) {
    pos <- pos + 1

    seek(rc, pos)
    char <- readChar(rc, 1L)

    if (char == "(") {
      in_parens <- in_parens + 1
    } else if (char == ")") {
      in_parens <- in_parens - 1
    }

    if (keyword_starts_here(rc, keyword_1)) {
      keyword_1_pos <- pos
      seek(rc, keyword_1_pos + keyword_1_length)
      keyword_2_pos <- find_this_keyword_after(rc, len, keyword_2, in_parens, parens_diff)
      if (!is.null(keyword_2_pos)) {
        if (right_operand) {
          seek(rc, keyword_2_pos + keyword_2_length + 1L)
          keyword_pair_pos <- append(
            keyword_pair_pos,
            list(c(
              keyword_1_pos,
              keyword_2_pos,
              find_end_of_operand_after(rc, len, in_parens)
            ))
          )
        } else {
          keyword_pair_pos <- append(
            keyword_pair_pos,
            list(c(keyword_1_pos, keyword_2_pos))
          )
        }
      }
    }

  }
  close(rc)
  keyword_pair_pos
}

find_this_keyword_after <- function(rc, len, keyword, in_parens, parens_diff = 0) {
  # returns the position of the start of the specified keyword
  # at the specified parentheses nesting level

  # only for use on expressions with quoted text masked

  orig_pos <- seek(rc, NA)
  on.exit(seek(rc, orig_pos))
  orig_parens <- in_parens
  while((pos <- seek(rc, NA)) <= len) {
    char <- readChar(rc, 1L)

    if (char == "(") {
      in_parens <- in_parens + 1
    } else if (char == ")") {
      in_parens <- in_parens - 1
    }

    if (in_parens == orig_parens + parens_diff && keyword_starts_here(rc, keyword)) {
      return(pos);
    }

    seek(rc, pos + 1)
  }
  return(NULL)
}

find_end_of_operand_after <- function(rc, len, in_parens) {
  # returns the next position after right operand that follows
  # the current position

  # only for use on expressions with quoted text masked

  orig_pos <- seek(rc, NA)
  on.exit(seek(rc, orig_pos))
  orig_parens <- in_parens

  while((pos <- seek(rc, NA)) <= len) {
    char <- readChar(rc, 1L)

    if (char == "(") {
      in_parens <- in_parens + 1
    } else if (char == ")") {
      in_parens <- in_parens - 1
    } else if (in_parens == orig_parens && is_non_word_character(char)) {
      return(pos)
    } else if (in_parens < orig_parens) {
      return(pos)
    }

    seek(rc, pos + 1L)
  }
  return(pos - 2L)
}


preceded_by_keyword <- function(rc, keyword) {
  pos <- seek(rc, NA)
  on.exit(seek(rc, pos))
  nchars <- nchar(keyword, type = "bytes")
  at_start <- tryCatch({
    seek(rc, -nchars - 2L, "current")
    FALSE
  }, error = function(e) {
    TRUE
  })
  if (at_start) {
    return(FALSE)
  }
  chars <- readChar(rc, nchars + 1L)
  keyword_regex <- paste0(
    "^",
    non_word_char_regex,
    keyword,
    "$"
  )
  grepl(keyword_regex,  chars, ignore.case = TRUE)
}

ends_with_operator_expecting_right_operand <- function(expr) {
  expr <- trimws(expr, whitespace = ws_regex)
  expr_length <- nchar(expr, type = "bytes")
  last_char <- substr(expr, expr_length, expr_length)
  if (last_char %in% sql_characters_expecting_right_operands) return(TRUE)

  words_regex <- paste0("(", paste(sql_words_expecting_right_operands, collapse = "|"), ")")
  if (grepl(paste0("\\b",words_regex, "$"), expr, ignore.case = TRUE)) return(TRUE)
  FALSE
}
