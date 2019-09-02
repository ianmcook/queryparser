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
  exprs <- sapply(exprs, parse_expression, tidyverse = tidyverse, secure = secure, USE.NAMES = FALSE)

  if (length(exprs) < 1) {
    stop("No name found in the FROM clause", call. = FALSE)
  }

  if (length(exprs) > 1) {
    stop("Multi-table queries are not supported", call. = FALSE)
  }

  expr <- exprs[[1]]

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

  exprs
}

parse_where <- function(exprs, tidyverse, secure = TRUE) {
  if (is.null(exprs)) return(NULL)
  exprs <- sapply(exprs, parse_expression, tidyverse = tidyverse, secure = secure, USE.NAMES = FALSE)

  if (length(exprs) > 1) {
    stop("The WHERE clause must contain a single Boolean expression", call. = FALSE)
  }

  expr <- exprs[[1]]

  if (is_aggregate_expression(expr)) {
    stop("Aggregate functions are not allowed in the WHERE clause. ",
         "Use the HAVING clause to filter by aggregates.", call. = FALSE)
  }

  exprs
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
  exprs <- sapply(exprs, parse_expression, tidyverse = tidyverse, secure = secure, USE.NAMES = FALSE)

  if (length(exprs) > 1) {
    stop("The HAVING clause must contain a single Boolean expression", call. = FALSE)
  }

  exprs
}

parse_order_by <- function(exprs, tidyverse, secure = TRUE) {
  if (is.null(exprs)) return(NULL)

  if (any(tolower(exprs) %in% c("asc", "desc"))) {
    stop("Invalid use of ASC or DESC in the ORDER BY clause", call. = FALSE)
  }

  descending_cols <- order_is_desc(exprs)

  exprs <- remove_asc_desc(exprs)

  if (tidyverse) {
    for (i in which(descending_cols)) {
      exprs[[i]] <- paste0("dplyr::desc(", exprs[[i]], ")")
    }
  }

  exprs <- sapply(exprs, parse_expression, tidyverse = tidyverse, secure = secure, USE.NAMES = FALSE)

  if (!tidyverse) {
    attr(exprs, "descreasing") <- descending_cols
  }

  exprs
}

parse_limit <- function(exprs) {
  if (is.null(exprs)) return(NULL)

  suppressWarnings(exprs <- as.integer(exprs))

  if (any(is.na(exprs)) || length(exprs) != 1 || isTRUE(exprs < 0)) {
    stop("The LIMIT clause may contain only a single ",
         "constant non-negative integer", call. = FALSE)
  }

  list(exprs)
}

order_is_desc <- function(exprs) {
  grepl("\\bDESC$", exprs, ignore.case = TRUE)
}

remove_asc_desc <- function(exprs) {
  gsub("\\b ?(A|DE)SC$", "", exprs, ignore.case = TRUE)
}
