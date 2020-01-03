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

parse_select <- function(exprs, tidyverse, secure = TRUE) {
  exprs <- unlist(lapply(exprs, extract_alias))
  exprs <- sapply(exprs, parse_expression, tidyverse = tidyverse, secure = secure, USE.NAMES = FALSE)
  exprs
}

parse_from <- function(exprs, tidyverse, secure = TRUE) {
  if (is.null(exprs)) return(NULL)
  expr <- exprs[[1]]
  if (grepl(" join |\\,", expr, ignore.case = TRUE)) {
    # this might be a join query
    from <- parse_join(expr, tidyverse, secure)
  } else {
    # this is not a join query
    from <- parse_table_reference(expr, tidyverse, secure)
  }
  from
}

parse_where <- function(exprs, tidyverse, secure = TRUE) {
  if (is.null(exprs)) return(NULL)
  expr <- exprs[[1]]
  expr <- parse_expression(expr, tidyverse = tidyverse, secure = secure)

  if (is_aggregate_expression(expr)) {
    stop("Aggregate functions are not allowed in the WHERE clause. ",
         "Use the HAVING clause to filter by aggregates.", call. = FALSE)
  }

  list(expr)
}

parse_group_by <- function(exprs, tidyverse, secure = TRUE) {
  if (is.null(exprs)) return(NULL)
  exprs <- sapply(exprs, parse_expression, tidyverse = tidyverse, secure = secure, USE.NAMES = FALSE)

  if (any(are_aggregate_expressions(exprs))) {
    stop("Aggregate functions are not allowed in the GROUP BY clause. ", call. = FALSE)
  }

  exprs
}

parse_having <- function(exprs, tidyverse, secure = TRUE) {
  if (is.null(exprs)) return(NULL)

  expr <- exprs[[1]]

  expr <- parse_expression(expr, tidyverse = tidyverse, secure = secure)

  list(expr)
}

parse_order_by <- function(exprs, tidyverse, secure = TRUE) {
  if (is.null(exprs)) return(NULL)

  if (any(tolower(exprs) %in% c("asc", "desc"))) {
    stop("Invalid use of ASC or DESC in the ORDER BY clause", call. = FALSE)
  }

  descending_cols <- order_is_desc(exprs)

  exprs <- remove_asc_desc(exprs)

  suppressWarnings(int_positions <- as.integer(exprs))
  if (any(!is.na(int_positions))) {
    stop("Positional column references in the ORDER BY clause are not supported")
  }

  if (tidyverse) {
    for (i in which(descending_cols)) {
      exprs[[i]] <- paste0("dplyr::desc(", exprs[[i]], ")")
    }
  }

  exprs <- sapply(exprs, parse_expression, tidyverse = tidyverse, secure = secure, USE.NAMES = FALSE)

  if (!tidyverse) {
    attr(exprs, "decreasing") <- descending_cols
  }

  exprs
}

parse_limit <- function(exprs) {
  if (is.null(exprs)) return(NULL)
  expr <- exprs[[1]]

  suppressWarnings(expr <- as.integer(expr))

  if (any(is.na(expr)) || length(expr) != 1 || isTRUE(expr < 0)) {
    stop("The LIMIT clause may contain only a single ",
         "constant non-negative integer", call. = FALSE)
  }

  list(expr)
}

order_is_desc <- function(exprs) {
  grepl("\\bDESC$", exprs, ignore.case = TRUE)
}

remove_asc_desc <- function(exprs) {
  gsub("\\b ?(A|DE)SC$", "", exprs, ignore.case = TRUE)
}
