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

#' @include compat.R
NULL

MASKING_CHARACTER <- "\U001"

quote_chars <- c("\"", "'", "`")

ws_regex <- "[ \t\r\n]"

word_char_regex <- "\\w"

non_word_char_regex <- "\\W"

digit_regex <- "[0-9]"

non_operator_regex <- "[^<>=%!]"

r_symbolic_constants <- c(
  "pi", "TRUE", "FALSE", "NULL", "Inf", "NA", "NaN",
  "NA_integer_", "NA_real_", "NA_complex_", "NA_character_"
)

r_reserved_words <- c(
  "if", "else", "repeat", "while", "function",
  "for", "in", "next", "break"
  # also the dots (..., ..1, ..2, and so on)
)

sql_characters_expecting_right_operands <- c(
  ">", "<", "=", "&", "|",
  "+", "-", "*", "/", "%",
  "^", "!", "~"
)

sql_words_expecting_right_operands <- c(
  "and", "or", "xor", "as", "from", "not",
  "in", "div", "between",
  "all", "any", "some",
  "like", "ilike", "regexp", "iregexp", "rlike"
)

sql_logical_operators_with_left_operands <- c(
  "and", "or", "xor"
)

sql_logical_operators_with_right_operands <- c(
  "and", "or", "xor", "not"
)

sql_data_types_with_args <- c(
  "decimal", "char", "varchar"
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

keyword_starts_here <- function(rc, keyword, useBytes = FALSE, look_back = TRUE) {
  pos <- seek(rc, NA)
  on.exit(seek(rc, pos))
  at_start <- !look_back || tryCatch({
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
  isTRUE(grepl(keyword_regex,  chars, ignore.case = TRUE, useBytes = useBytes))
}

find_keyword_pairs <- function(expr_quotes_masked, keyword_1, keyword_2, operands = FALSE, parens_diff = 0) {
  # returns the positions of the start of each keyword
  # in every instance of the specified matching pair of keywords
  # both at the same parentheses nesting level

  # optionally also returns the positions of the left and right operands
  # for when an operator pair and their three operands create a boolean expression
  # (as is the case with the BETWEEN ... AND operator pair)

  # optionally also searches for the second keyword in a different
  # parentheses nesting level than the first keyword
  # (as is the case with CAST( AS ))

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
    } else if (char == MASKING_CHARACTER) {
      next;
    }

    if (keyword_starts_here(rc, keyword_1)) {
      if (operands) {
        left_operand_start <- find_beginning_of_boolean_operand_before(rc, in_parens)
      }
      keyword_1_pos <- pos
      seek(rc, keyword_1_pos + keyword_1_length)
      keyword_2_pos <- find_this_keyword_after(rc, len, keyword_2, in_parens, parens_diff)
      if (!is.null(keyword_2_pos)) {
        if (operands) {
          seek(rc, keyword_2_pos + keyword_2_length + 1L)
          right_operand_end <- find_end_of_boolean_operand_after(rc, len, in_parens)
          keyword_pair_pos <- append(
            keyword_pair_pos,
            list(c(
              left_operand_start,
              keyword_1_pos,
              keyword_2_pos,
              right_operand_end
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

find_end_of_boolean_operand_after <- function(rc, len, in_parens) {
  orig_pos <- seek(rc, NA)
  on.exit(seek(rc, orig_pos))
  orig_parens <- in_parens

  while((pos <- seek(rc, NA)) <= len) {
    char <- readChar(rc, 1L)

    next_thing <- get_next_character_word_or_number(rc, len)

    if (char == "(") {
      in_parens <- in_parens + 1
    } else if (char == ")") {
      in_parens <- in_parens - 1
      if (in_parens < orig_parens) {
        return(pos + 1L)
      }
    } else if (in_parens == orig_parens &&
               isTRUE(tolower(next_thing) %in% c(sql_logical_operators_with_left_operands, ","))) {
      return(pos + 1L)
    }
  }
  return(len)
}

find_beginning_of_boolean_operand_before <- function(rc, in_parens) {
  orig_pos <- seek(rc, NA)
  on.exit(seek(rc, orig_pos))
  orig_parens <- in_parens

  pos <- orig_pos - 1L
  while(pos > 0L) {
    pos <- pos - 1L
    seek(rc, pos)
    char <- readChar(rc, 1L)

    previous_thing <- get_previous_character_word_or_number(rc)

    if (char == ")") {
      in_parens <- in_parens + 1
    } else if (char == "(") {
      in_parens <- in_parens - 1
      if (in_parens < orig_parens) {
        return(pos + 1L)
      }
    } else if (in_parens == orig_parens &&
               isTRUE(tolower(previous_thing) %in% c(sql_logical_operators_with_right_operands, ","))) {
      return(pos)
    }
  }
  return(0L)
}

get_next_character_word_or_number <- function(rc, len) {
  # get the next non-space character
  # or if it's a word character or digit, then get the whole word or number
  # or if there is nothing after, then return character(0)

  out_str <- character(0)

  orig_pos <- seek(rc, NA)
  on.exit(seek(rc, orig_pos))

  while((pos <- seek(rc, NA)) <= len) {
    char <- readChar(rc, 1L)

    # if it's a space, get the next one
    if (is_whitespace_character(char)) {
      next;
    }

    # if it's the masking character, get the next one
    if (isTRUE(char == MASKING_CHARACTER)) {
      next;
    }

    # if it's a word character or digit, keep going
    # until there's a non-word character or non-digit,
    # and return the whole word or number
    if (is_word_character(char)) { # this matches digits
      while(is_word_character(char)) { # this matches digits
        out_str <- paste0(out_str, char)
        char <- readChar(rc, 1L)
      }
      break;
    }

    # if it's some other character, then return that character
    out_str <- char
    break;
  }
  out_str
}

get_previous_character_word_or_number <- function(rc) {
  # get the previous non-space character
  # or if it's a word character or digit, then get the whole word or number
  # or if there is nothing before, then return character(0)

  out_str <- character(0)

  orig_pos <- seek(rc, NA)
  on.exit(seek(rc, orig_pos))

  pos <- orig_pos
  while(pos > 1) {
    seek(rc, -2L, "current")
    char <- readChar(rc, 1L)

    # if it's a space, get the previous one
    if (is_whitespace_character(char)) {
      next;
    }

    # if it's the masking character, get the previous one
    if (isTRUE(char == MASKING_CHARACTER)) {
      next;
    }

    # if it's a word character or digit, keep going
    # until there's a non-word character or non-digit,
    # and return the whole word or number
    if (is_word_character(char)) { # this matches digits
      while(is_word_character(char)) { # this matches digits
        out_str <- paste0(char, out_str)
        at_start <- tryCatch({
          seek(rc, -2L, "current")
          FALSE
        }, error = function(e) {
          TRUE
        })
        if (at_start) {
          break;
        }
        char <- readChar(rc, 1L)
      }
      break;
    }

    # if it's some other character, then return that character
    out_str <- char
    break;
  }
  out_str
}

preceded_by_keyword <- function(rc, keyword, useBytes = FALSE) {
  pos <- seek(rc, NA)
  on.exit(seek(rc, pos))
  nchars <- nchar(keyword, type = "bytes")
  at_start <- tryCatch({
    seek(rc, -nchars - 3L, "current")
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
    " ?",
    "$"
  )
  grepl(keyword_regex,  chars, ignore.case = TRUE, useBytes = useBytes)
}

ends_with_operator_expecting_right_operand <- function(expr, except = c()) {
  expr <- trimws(expr)
  expr_length <- nchar(expr, type = "bytes")
  original_encoding <- Encoding(expr)
  Encoding(expr) <- "bytes"
  last_char <- substr(expr, expr_length, expr_length)
  Encoding(expr) <- original_encoding
  if (last_char %in% setdiff(sql_characters_expecting_right_operands, except)) return(TRUE)

  words_regex <- paste0("(", paste(setdiff(sql_words_expecting_right_operands, except), collapse = "|"), ")")
  if (grepl(paste0("\\b",words_regex, "$"), expr, ignore.case = TRUE)) return(TRUE)
  FALSE
}

is_one_valid_r_name <- function(x) {
  names <- make.names(x)
  length(names) == 1 && names == x
}

is_function <- function(expr, name) {
  if (!is.call(expr)) {
    return(FALSE)
  } else {
    if (deparse(expr[[1]]) == name) {
      return(TRUE)
    }
    out <- lapply(expr, is_function, name)
  }
  any(sapply(out, isTRUE))
}

all_funs <- function(expr) {
  names <- all_names(expr)
  names[vapply(names, function(name) {is_function(expr, name)}, TRUE)]
}

all_cols <- function(expr) {
  setdiff(all.vars(expr), r_symbolic_constants)
}

all_names <- function(expr) {
  setdiff(all.names(expr), r_symbolic_constants)
}

is_constant <- function(expr) {
  length(all_cols(expr)) == 0
}
