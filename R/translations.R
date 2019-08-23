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

translation_environment_base <- new.env()
translation_environment_tidyverse <- new.env()
translation_environment_unary_postfix <- new.env()

translations_for_base <- c(
  translations_direct_generic,
  translations_direct_base
)
for (i in seq_along(translations_for_base)) {
  do.call(
    delayedAssign,
    list(
      x = names(translations_for_base)[i],
      value = translations_for_base[[i]],
      assign.env = translation_environment_base
    )
  )
}

translations_for_tidyverse <- c(
  translations_direct_generic,
  translations_direct_tidyverse
)
for (i in seq_along(translations_for_tidyverse)) {
  do.call(
    delayedAssign,
    list(
      x = names(translations_for_tidyverse)[i],
      value = translations_for_tidyverse[[i]],
      assign.env = translation_environment_tidyverse
    )
  )
}
