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

unqualify_column_names_in_clause <- function(exprs, prefix) {
  attrs <- attributes(exprs)
  if (is.null(exprs)) return(NULL)
  exprs <- lapply(exprs, function(expr) {
    column_names <- all_cols(expr)
    unqualify_column_names_in_expression(expr, prefix, column_names)
  })
  attributes(exprs) <- attrs
  exprs
}

unqualify_column_names_in_expression <- function(expr, prefix, column_names) {
  if (length(expr) == 1) {
    if (identical(typeof(expr), "symbol")) {
      qualified_column_name <- deparse(expr)
      if (qualified_column_name %in% column_names) {
        unqualified_column_name <- sub(paste0("^", prefix, "\\."), "", qualified_column_name)
        return(str2lang(unqualified_column_name))
      } else {
        return(expr)
      }
    } else {
      return(expr)
    }
  } else {
    return(as.call(lapply(expr, unqualify_column_names_in_expression, prefix, column_names)))
  }
}
