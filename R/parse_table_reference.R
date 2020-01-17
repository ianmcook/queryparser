# Copyright 2020 Cloudera Inc.
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

parse_table_reference <- function(expr, tidyverse, secure) {
  expr <- extract_alias(expr)
  expr <- remove_enclosing_parentheses(expr)
  table_alias <- names(expr)
  expr <- parse_expression(expr, tidyverse = tidyverse, secure = secure)

  expr_parts <- strsplit(deparse(expr), "::")[[1]]

  if (length(expr_parts) == 2) {
    if (!all(vapply(
      expr_parts,
      is_one_valid_r_name,
      TRUE
    ))) {
      stop("Invalid name in FROM clause", call. = FALSE)
    }
  } else if (length(expr_parts) == 1) {
    if (!is_one_valid_r_name(expr_parts)) {
      stop("Invalid name in FROM clause", call. = FALSE)
    }
  } else {
    stop("Invalid name in FROM clause", call. = FALSE)
  }

  output <- list(expr)
  names(output) <- table_alias
  output
}
