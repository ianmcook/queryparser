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

replace_null_with_na <- function(expr_quotes_masked) {
  gsub(
    paste0("\\bnull\\b"),
    "NA",
    expr_quotes_masked,
    ignore.case = TRUE
  )
}

replace_in_operator <- function(expr_quotes_masked) {
  # need to change this when adding support for subqueries
  expr_quotes_masked <- gsub(
    "\\bnot in ?\\(",
    "%nin% c(", # this `%nin%` is replaced later by replace_nin()
    expr_quotes_masked,
    ignore.case = TRUE
  )
  expr_quotes_masked <- gsub(
    "\\bin ?\\(",
    "%in% c(",
    expr_quotes_masked,
    ignore.case = TRUE
  )
  expr_quotes_masked
}

replace_operators_binary_symbolic <- function(expr_quotes_masked) {
  for (i in seq_along(translations_operators_binary_symbolic)) {
    expr_quotes_masked <- gsub(
      paste0("(",non_operator_regex, "+)(",names(translations_operators_binary_symbolic)[i],")(", non_operator_regex, "+)"),
      paste0("\\1",translations_operators_binary_symbolic[i],"\\3"),
      expr_quotes_masked
    )
  }
  expr_quotes_masked
}

replace_operators_binary_word <- function(expr_quotes_masked) {
  for (i in seq_along(translations_operators_binary_word)) {
    expr_quotes_masked <- gsub(
      paste0("\\b",names(translations_operators_binary_word)[i],"\\b"),
      translations_operators_binary_word[i],
      expr_quotes_masked,
      ignore.case = TRUE
    )
  }
  expr_quotes_masked
}

replace_operators_unary_prefix <- function(expr_quotes_masked) {
  for (i in seq_along(translations_operators_unary_prefix)) {
    expr_quotes_masked <- gsub(
      paste0("\\b",names(translations_operators_unary_prefix)[i],"\\b"),
      translations_operators_unary_prefix[i],
      expr_quotes_masked,
      ignore.case = TRUE
    )
  }
  expr_quotes_masked
}

replace_operators_unary_postfix <- function(expr_quotes_masked) {
  for(i in seq_along(translations_operators_unary_postfix)) {
    expr_quotes_masked <- gsub(
      paste0("\\b",names(translations_operators_unary_postfix)[i],"\\b"),
      translations_operators_unary_postfix[i],
      expr_quotes_masked,
      ignore.case = TRUE
    )
  }
  expr_quotes_masked
}

replace_all_distinct_keyword <- function(expr_quotes_masked) {
  agg_funs <- paste(sql_aggregate_functions, collapse = "|")
  expr_quotes_masked <- gsub(
    paste0("\\b(",agg_funs,") ?\\( ?all\\b"),
    "\\1",
    expr_quotes_masked,
    ignore.case = TRUE
  )
  expr_quotes_masked <- gsub(
    paste0("\\b(",agg_funs,") ?\\( ?distinct\\b"),
    "\\1_distinct(",
    expr_quotes_masked,
    ignore.case = TRUE
  )
  expr_quotes_masked
}

replace_count_star <- function(expr_quotes_masked) {
  expr_quotes_masked <- gsub(
    paste0("\\bcount ?\\( ?\\*"),
    "count_star(",
    expr_quotes_masked,
    ignore.case = TRUE
  )
}

replace_nin <- function(expr) {
  if (length(expr) == 1) {
    return(expr)
  } else {
    if (expr[[1]] == quote(`%nin%`)) {
      expr[[1]] <- quote(`%in%`)
      return(as.call(lapply(
        str2lang(paste0("!(", deparse(expr),")")),
        replace_nin
      )))
    } else {
      return(as.call(lapply(expr, replace_nin)))
    }
  }
}

replace_distinct_functions <- function(expr, tidyverse = FALSE) {
  if (tidyverse) {
    sql_aggregate_functions <- setdiff(sql_aggregate_functions, "count")
  }
  for (func in sql_aggregate_functions) {
    expr <- replace_distinct_function(expr, func)
  }
  expr
}

replace_distinct_function <- function(expr, func) {
  if (length(expr) == 1) {
    return(expr)
  } else {
    if (expr[[1]] == str2lang(paste0(func, "_distinct"))) {
      return(as.call(lapply(
        str2lang(paste0(gsub(
          paste0("^", func, "_distinct\\("),
          paste0(func, "(unique("),
          deparse(expr),
          ignore.case = TRUE
        ),
        ")")), replace_distinct_function, func
      )))
    } else {
      return(as.call(lapply(expr, replace_distinct_function, func)))
    }
  }
}

make_function_names_and_keywords_lowercase <- function(expr_quotes_masked) {
  all_names <- paste(unique(c(
    sql_data_types,
    names(translations_operators_binary_word),
    names(translations_operators_unary_prefix),
    names(translations_operators_unary_postfix),
    names(translations_direct_generic),
    names(translations_direct_base),
    names(translations_direct_tidyverse),
    names(translations_indirect_generic),
    names(translations_indirect_base),
    names(translations_indirect_tidyverse),
    sql_aggregate_functions
  )), collapse = "|")
  gsub(paste0("\\b(", all_names, ")\\b"), "\\L\\1", expr_quotes_masked, ignore.case = TRUE, perl = TRUE)
}

quote_data_types <- function(expr_quotes_masked) {
  data_type_names <- paste(sql_data_types, collapse = "|")
  gsub(
    paste0("\\b((",data_type_names,")\\b( ?\\([0-9, ]*\\))?)"),
    "'\\1'",
    expr_quotes_masked,
    ignore.case = TRUE
  )
}
