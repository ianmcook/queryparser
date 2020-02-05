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

parse_select <- function(exprs, tidyverse, secure = TRUE) {
  exprs <- unlist(lapply(exprs, extract_alias))
  exprs <- sapply(exprs, parse_expression, tidyverse = tidyverse, secure = secure, USE.NAMES = FALSE)
  exprs
}

parse_from <- function(exprs, tidyverse, secure = TRUE) {
  if (is.null(exprs)) return(NULL)
  expr <- exprs[[1]]
  expr <- remove_enclosing_parentheses(expr)
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
  nulls_first_cols <- order_is_nulls_first(exprs)
  nulls_last_cols <- order_is_nulls_last(exprs)

  exprs <- remove_asc_desc(exprs)
  exprs <- remove_nulls_first_last(exprs)

  suppressWarnings(int_positions <- as.integer(exprs))
  if (any(!is.na(int_positions))) {
    stop("Positional column references in the ORDER BY clause are not supported", call. = FALSE)
  }

  for (i in which(descending_cols)) {
    if (tidyverse) {
      exprs[[i]] <- paste0("dplyr::desc(", exprs[[i]], ")")
    } else {
      exprs[[i]] <- paste0("-xtfrm(", exprs[[i]], ")")
    }
  }

  exprs <- as.list(exprs)
  for (i in which(nulls_first_cols)) {
    exprs[[i]] <- c(paste0("!is.na(", exprs[[i]], ")"), exprs[[i]])
  }

  for (i in which(nulls_last_cols)) {
    exprs[[i]] <- c(paste0("is.na(", exprs[[i]], ")"), exprs[[i]])
  }
  exprs <- unlist(exprs, recursive = FALSE, use.names = FALSE)

  exprs <- sapply(exprs, parse_expression, tidyverse = tidyverse, secure = secure, USE.NAMES = FALSE)

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
  grepl("\\bDESC\\b", exprs, ignore.case = TRUE)
}

order_is_nulls_first <- function(exprs) {
  grepl("\\bNULLS FIRST\\b", exprs, ignore.case = TRUE)
}

order_is_nulls_last <- function(exprs) {
  grepl("\\bNULLS LAST\\b", exprs, ignore.case = TRUE)
}

remove_asc_desc <- function(exprs) {
  gsub("\\b ?(A|DE)SC\\b", "", exprs, ignore.case = TRUE)
}

remove_nulls_first_last <- function(exprs) {
  gsub("\\b NULLS (FIRST|LAST)\\b", "", exprs, ignore.case = TRUE)
}
