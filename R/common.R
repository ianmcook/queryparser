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
