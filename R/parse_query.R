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

#' @include translations.R process_translations.R
NULL

#' Parse a SQL query
#'
#' @description Parses a SQL \code{SELECT} statement into a list with R
#'   expressions
#'
#' @param query a character string containing a SQL \code{SELECT} statement
#' @param tidyverse set to \code{TRUE} to use functions from \pkg{tidyverse}
#'   packages including \pkg{dplyr}, \pkg{stringr}, and \pkg{lubridate} in the R
#'   expressions
#' @param secure set to \code{FALSE} to allow potentially dangerous functions in
#'   the returned R expressions
#' @return A list object with named elements representing the clauses of the
#'   query, containing sublists of unevaluated R expressions translated from the
#'   SQL expressions in the query.
#'
#'   Depending on the arguments, the returned list and its sublists will have
#'   attributes named \code{distinct}, \code{aggregate}, and \code{decreasing}
#'   with logical values that can aid in the evaluation of the R expressions.
#' @details The query must not contain line comments (\code{--}) or block
#'   comments (\code{/* */}).
#' @examples
#' query <- "SELECT origin, dest,
#'     COUNT(flight) AS num_flts,
#'     round(AVG(distance)) AS dist,
#'     round(AVG(arr_delay)) AS avg_delay
#'   FROM flights
#'   WHERE distance BETWEEN 200 AND 300
#'     AND air_time IS NOT NULL
#'   GROUP BY origin, dest
#'   HAVING num_flts > 5000
#'   ORDER BY num_flts DESC, avg_delay DESC
#'   LIMIT 100;"
#'
#' parse_query(query)
#'
#' parse_query(query, tidyverse = TRUE)
#' @export
parse_query <- function(query, tidyverse = FALSE, secure = TRUE) {
  if (!identical(typeof(query), "character") ||
      !identical(length(query), 1L) ||
      !identical(typeof(tidyverse), "logical") ||
      !identical(length(tidyverse), 1L) ||
      !identical(typeof(secure), "logical") ||
      !identical(length(secure), 1L)) {
    stop("Unexpected input to parse_query()", call. = FALSE)
  }

  tree <- split_query(query)

  is_select_distinct <- isTRUE(attr(tree$select, "distinct"))
  is_select_star <- any(tree$select == "*")

  has_from <- !is.null(tree$from)
  has_where <- !is.null(tree$where)
  has_group_by <- !is.null(tree$group_by)
  has_having <- !is.null(tree$having)
  has_order_by <- !is.null(tree$order_by)
  has_limit <- !is.null(tree$limit)

  tree$select <- parse_select(tree$select, tidyverse, secure)
  tree$from <- parse_from(tree$from, tidyverse, secure)
  tree$where <- parse_where(tree$where, tidyverse, secure)
  tree$group_by <- parse_group_by(tree$group_by, tidyverse, secure)
  tree$having <- parse_having(tree$having, tidyverse, secure)
  tree$order_by <- parse_order_by(tree$order_by, tidyverse, secure)
  tree$limit <- parse_limit(tree$limit)

  is_aggregate_expression_in_select_list <- are_aggregate_expressions(tree$select)
  is_aggregate_expression_in_order_by_list <- are_aggregate_expressions(tree$order_by)
  has_aggregates_in_select <- any(is_aggregate_expression_in_select_list)
  has_aggregates_in_order_by <- any(is_aggregate_expression_in_order_by_list)

  is_aggregating_query <- has_group_by || has_having || has_aggregates_in_select || has_aggregates_in_order_by

  if (is_aggregating_query) {

    if (is_select_distinct) {
      stop("SELECT DISTINCT cannot be used together with ",
           "aggregate expressions or a GROUP BY clause", call. = FALSE)
    }

    if (is_select_star) {
      stop("SELECT * cannot be used together with ",
           "aggregate expressions or a GROUP BY clause", call. = FALSE)
    }

    group_by_cols <- vapply(tree$group_by, deparse, "")
    agg_aliases <- names(tree$select)
    if (is.null(agg_aliases)) {
      agg_aliases <- rep("", length(tree$select))
    }
    group_by_refs <- as.character(tree$select[agg_aliases %in% as.character(tree$group_by)])
    valid_agg_cols <- setdiff(c(group_by_cols, agg_aliases, group_by_refs), "")

    if (has_having && !is_valid_expression_in_aggregation(tree$having[[1]], valid_agg_cols)) {
      stop("The expression in the HAVING clause is invalid in an aggregation context ",
           "or incompatible with the GROUP BY clause", call. = FALSE)
    }

    select_cols_to_check <- tree$select[!agg_aliases %in% group_by_cols & !tree$select %in% group_by_cols]
    group_by_col_alias_refs <- vapply(tree$select[agg_aliases %in% group_by_cols], deparse, "")
    group_by_cols_and_refs <- c(group_by_cols, group_by_col_alias_refs)
    if (!all(are_valid_expressions_in_aggregation(select_cols_to_check, group_by_cols_and_refs))) {
      stop("The SELECT list includes expressions that are invalid in an aggregation context ",
           "or incompatible with the GROUP BY clause", call. = FALSE)
    }

    agg_expr_aliases <- agg_aliases[is_aggregate_expression_in_select_list]
    if (any(agg_expr_aliases %in% group_by_cols)) {
      stop("Aliases of aggregate expressions are not allowed in the GROUP BY clause", call. = FALSE)
    }

    if (tidyverse && length(valid_agg_cols) > 0) {
      valid_agg_cols_for_order_by <- c(valid_agg_cols, paste0("dplyr::desc(",valid_agg_cols,")"))
    } else {
      valid_agg_cols_for_order_by <- valid_agg_cols
    }
    if (has_order_by && !all(are_valid_expressions_in_aggregation(tree$order_by, valid_agg_cols_for_order_by))) {
      stop("The ORDER BY list includes expressions that are invalid in an aggregation context ",
           "or incompatible with the GROUP BY clause", call. = FALSE)
    }

    attr(tree$select, "aggregate") <- is_aggregate_expression_in_select_list

    if (has_order_by) {
      attr(tree$order_by, "aggregate") <- is_aggregate_expression_in_order_by_list
    }

    attr(tree, "aggregate") <- TRUE

  } else if (is_select_distinct) {

    distinct_cols <- vapply(tree$select, deparse, "")
    distinct_aliases <- names(tree$select)
    if (is.null(distinct_aliases)) {
      distinct_aliases <- rep("", length(tree$select))
    }
    valid_distinct_cols <- setdiff(c(distinct_cols, distinct_aliases), "")

    if (tidyverse && length(valid_distinct_cols) > 0) {
      valid_distinct_cols_for_order_by <- c(valid_distinct_cols, paste0("dplyr::desc(",valid_distinct_cols,")"))
    } else {
      valid_distinct_cols_for_order_by <- valid_distinct_cols
    }
    if (!all(are_valid_expressions_in_distinct(tree$order_by, valid_distinct_cols_for_order_by))) {
      stop("The ORDER BY list includes expressions that are invalid ",
           "in a SELECT DISTINCT query", call. = FALSE)
    }

    attr(tree$select, "distinct") <- TRUE

  }

  tree
}
