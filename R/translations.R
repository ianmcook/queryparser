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

translations_operators_binary_symbolic <- list(
  `%` = "%%",
  `<>` = "!=",
  `=` = "=="
)

translations_operators_binary_word <- list(
  `and` = "&&",
  `or` = "||",
  `in` = "%in%"

  # maybe do something like this:
  #`in(` = "%in% c(",
  #`in (` = "%in% c("

)

translations_operators_unary_prefix <- list(
  not = "!"
)

translations_operators_unary_postfix <- list(
  `is null` = "%>% is.na()",
  `is not null` = "%>% is.na() %>% `!`"
)

translations_direct_generic <- list(
  true = quote(TRUE),
  false = quote(FALSE),
  ceil = quote(ceiling),
  mod = quote(`%%`),
  negative = quote(`-`),
  positive = quote(`+`),
  pow = quote(`^`),
  power = quote(`^`),
  quotient = quote(`%/%`)
)

translations_direct_base <- list(
  length = quote(nchar),
  lower = quote(tolower),
  upper = quote(toupper),
  trim = quote(trimws)
)

translations_direct_tidyverse <- list(
  length = quote(str_length),
  lower = quote(str_to_lower),
  upper = quote(str_to_upper),
  trim = quote(str_trim)
)

translations_indirect_generic <- list(
  ln = function(x) {
    eval(substitute(quote(log(x, base = exp(1)))))
  }
)

translations_indirect_base <- list(
)

translations_indirect_tidyverse <- list(
)

translations_indirect_generic_agg <- list(
  avg = function(x) {
    eval(substitute(quote(mean(x, na.rm = TRUE))))
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
  # for count all, use n() for count(*), otherwise length(!is.na(x))
  # for count distinct, use n_distinct(...)
)
