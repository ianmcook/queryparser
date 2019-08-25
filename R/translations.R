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

#' @include common.R
NULL

translations_data_types_generic <- list(
  `string` = "character",
  `char` = "character",
  `varchar` = "character",
  `boolean` = "logical",
  `int` = "integer",
  `integer` = "integer",
  `bigint` = "integer",
  `smallint` = "integer",
  `tinyint` = "integer",
  `double` = "double",
  `real` = "double",
  `float` = "single",
  `decimal` = "numeric"
)

translations_data_types_base <- list(
  `timestamp` = "POSIXct",
  `interval` = "difftime"
)

translations_data_types_tidyverse <- list(
  `timestamp` = "datetime",
  `interval` = "duration"
)
# TBD: translate cast as timestamp to as_datetime for tidyverse

translations_operators_binary_symbolic <- list(
  `%` = "%%",
  `<>` = "!=",
  `=` = "=="
)

translations_operators_binary_word <- list(
  `and` = "&&",
  `or` = "||",
  `div` = "%/%",

  `as` = "," # used in cast function



  # `in` and `not in` are handled elsewhere
)

translations_operators_unary_prefix <- list(
  not = "!"
)

translations_operators_unary_postfix <- list(
  `is null` = "%>% is.na()",
  `is not null` = "%>% is.na() %>% `!`"
)

translations_direct_generic <- list(

  # constants
  true = quote(TRUE),
  false = quote(FALSE),

  # mathematical functions
  ceil = quote(ceiling),
  is_nan = quote(is.nan),
  is_inf = quote(is.infinite),
  mod = quote(`%%`),
  negative = quote(`-`),
  pmax = quote(greatest),
  pmin = quote(least),
  positive = quote(`+`),
  pow = quote(`^`),
  power = quote(`^`),
  quotient = quote(`%/%`),

  # string functions
  concat = quote(paste0)

)

translations_direct_base <- list(

  # string functions
  length = quote(nchar),
  lower = quote(tolower),
  upper = quote(toupper),
  to_date = quote(as.Date),
  trim = quote(trimws)

)

translations_direct_tidyverse <- list(

  # string functions
  length = quote(str_length),
  lower = quote(str_to_lower),
  upper = quote(str_to_upper),
  to_date = quote(as_date),
  trim = quote(str_trim),

  # conditional functions
  nullif = quote(na_if)

  # add other lubridate, stringr, and dplyr functions
)

translations_indirect_generic <- list(
  ln = function(x) {
    eval(substitute(quote(log(x, base = exp(1)))))
  },
  cast = function(x, y = NULL) {
    if (is.null(y)) stop("Unspecified data type in CAST", call. = FALSE)
    data_type <- sql_data_types[sql_data_types == gsub(" ?\\(.+", "", y)][[1]]
    if (is.null(data_type)) stop("Unrecognized data type in CAST", call. = FALSE)
    func <- str2lang(paste0("as.", data_type))
    eval(substitute(quote(func(x))))
  },
  regexp_replace = function(x, pattern, replacement) {
    eval(substitute(quote(gsub(pattern, replacement, x))))
  },
  concat_ws = function(sep, ...) {
    eval(substitute(quote(paste(..., sep = sep))))
  } # I was worried that the sep before the equals sign would get replaced too, but it did not
)

translations_indirect_base <- list(
  nullif = function(x, y) {
    eval(substitute(quote(ifelse(is.na(x), x, y))))
  }
)

translations_indirect_tidyverse <- list(

)

translations_indirect_generic_agg <- list(
  avg = function(x) {
    eval(substitute(quote(mean(x, na.rm = TRUE))))
  },
  count = function(x) {
    eval(substitute(quote(sum(!is.na(x)))))
  },
  group_concat = function(x, sep = ", ") {
    eval(substitute(quote(paste0(x, collapse = sep))))
  },
  max = function(x) {
    eval(substitute(quote(max(x, na.rm = TRUE))))
  },
  min = function(x) {
    eval(substitute(quote(min(x, na.rm = TRUE))))
  },
  std = function(x) {
    eval(substitute(quote(sd(x, na.rm = TRUE))))
  },
  stddev = function(x) {
    eval(substitute(quote(sd(x, na.rm = TRUE))))
  },
  sum = function(x) {
    eval(substitute(quote(sum(x, na.rm = TRUE))))
  },
  percentile = function(x, p) {
    eval(substitute(quote(quantile(x, p, na.rm = TRUE))))
  },
  variance = function(x) {
    eval(substitute(quote(var(x, na.rm = TRUE))))
  }
)

translations_indirect_base_agg <- list(

  # for count all, use into length() for count(*), otherwise length(!is.na(x))
  # for count distinct, use length(unique(x)) if only one column,
  #   but it's unclear how best to handle the multiple columns case
  #   e.g. length(unique(mtcars[c("gear", "carb"),])) is 11
  #     ( that's the right answer, and what mtcars %>% summarise(n_distinct(gear, carb)) returns )
  #   but these give the wrong ansers:
  #      mtcars %>% summarise(length(unique(gear, carb))) is 28
  #      mtcars %>% summarise(length(unique(c(gear, carb)))) is 7
  #   the safest thing would probably be to limit it to the one-variable case
)

translations_indirect_tidyverse_agg <- list(
  count_star = function() {
    eval(substitute(quote(n())))
  }

  # for count all, use n() for count(*), otherwise length(!is.na(x))
  # for count distinct, use n_distinct(...)
)
