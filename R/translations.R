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
  avg = quote(mean),
  ceil = quote(ceiling),
  mod = quote(`%%`),
  negative = quote(`-`),
  positive = quote(`+`),
  pow = quote(`^`),
  power = quote(`^`),
  quotient = quote(`%/%`)
)

translations_direct_base <- list(
  #count_distinct = quote(length %>% unique), # handle elsewhere?
  length = quote(nchar),
  lower = quote(tolower),
  upper = quote(toupper),
  trim = quote(trimws)
)

translations_direct_tidyverse <- list(
  #count_distinct = quote(n_distinct), # handle elsewhere?
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
