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

#' @include common.R translations.R
NULL

sql_data_types <- unique(c(
  names(translations_data_types_generic),
  names(translations_data_types_base),
  names(translations_data_types_tidyverse)
))

data_type_translations_for_base <- c(
  translations_data_types_generic,
  translations_data_types_base
)

data_type_translations_for_tidyverse <- c(
  translations_data_types_generic,
  translations_data_types_tidyverse
)

sql_aggregate_functions <- unique(c(
  names(translations_indirect_generic_agg),
  names(translations_indirect_base_agg),
  names(translations_indirect_tidyverse_agg)
))

translation_environment_direct_base <- new.env()
translation_environment_direct_tidyverse <- new.env()
translation_environment_indirect_base <- new.env()
translation_environment_indirect_tidyverse <- new.env()

translations_direct_for_base <- c(
  translations_direct_generic,
  translations_direct_base
)
for (i in seq_along(translations_direct_for_base)) {
  do.call(
    delayedAssign,
    list(
      x = names(translations_direct_for_base)[i],
      value = translations_direct_for_base[[i]],
      assign.env = translation_environment_direct_base
    )
  )
}

translations_direct_for_tidyverse <- c(
  translations_direct_generic,
  translations_direct_tidyverse
)
for (i in seq_along(translations_direct_for_tidyverse)) {
  do.call(
    delayedAssign,
    list(
      x = names(translations_direct_for_tidyverse)[i],
      value = translations_direct_for_tidyverse[[i]],
      assign.env = translation_environment_direct_tidyverse
    )
  )
}

translations_indirect_for_base <- c(
  translations_indirect_generic,
  translations_indirect_base,
  translations_indirect_generic_agg,
  translations_indirect_base_agg
)
for (i in seq_along(translations_indirect_for_base)) {
  do.call(
    delayedAssign,
    list(
      x = names(translations_indirect_for_base)[i],
      value = translations_indirect_for_base[[i]],
      assign.env = translation_environment_indirect_base
    )
  )
}

translations_indirect_for_tidyverse <- c(
  translations_indirect_generic,
  translations_indirect_tidyverse,
  translations_indirect_generic_agg,
  translations_indirect_tidyverse_agg
)
for (i in seq_along(translations_indirect_for_tidyverse)) {
  do.call(
    delayedAssign,
    list(
      x = names(translations_indirect_for_tidyverse)[i],
      value = translations_indirect_for_tidyverse[[i]],
      assign.env = translation_environment_indirect_tidyverse
    )
  )
}
