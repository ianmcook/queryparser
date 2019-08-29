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
#' @description Parses a SQL \code{SELECT} statement into a list of R
#'   expressions
#'
#' @param query a character string containing a SQL \code{SELECT} statement
#' @param tidyverse set to \code{TRUE} to use functions from tidyverse packages
#'   including dplyr, stringr, and lubridate in the R expressions
#' @param secure set to \code{FALSE} to allow potentially dangerous functions in
#'   the returned R expressions
#' @return A list object with named elements representing the clauses of the
#'   query and containing lists of unevaluated R expressions translated from the
#'   SQL expressions in the query
#' @details The query must not contain line comments (\code{--}) or block
#'   comments (\code{/* */}).
#' @examples
#' query <- "SELECT origin, dest,
#'     COUNT(flight) AS num_flts,
#'     round(AVG(distance)) AS dist,
#'     round(AVG(arr_delay)) AS avg_delay
#'   FROM flights
#'   WHERE distance BETWEEN 200 AND 300
#'   GROUP BY origin, dest
#'   HAVING num_flts > 5000
#'   ORDER BY num_flts DESC, avg_delay DESC
#'   LIMIT 100;"
#'
#' parse_query(query)
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

  has_from <- !is.null(tree$from)
  has_where <- !is.null(tree$where)
  has_group_by <- !is.null(tree$group_by)
  has_having <- !is.null(tree$having)
  has_order_by <- !is.null(tree$order_by)
  has_limit <- !is.null(tree$limit)

  tree$select <- parse_select(tree$select, tidyverse)
  tree$from <- parse_from(tree$from, tidyverse)
  tree$where <- parse_where(tree$where, tidyverse)
  tree$group_by <- parse_group_by(tree$group_by, tidyverse)
  tree$having <- parse_having(tree$having, tidyverse)
  tree$order_by <- parse_order_by(tree$order_by, tidyverse)
  tree$limit <- parse_limit(tree$limit)

  valid_agg_cols <- sapply(tree$group_by, deparse)

  is_aggregate_expression <- are_aggregate_expressions(tree$select)
  has_aggregates_in_select_list <- any(is_aggregate_expression)
  if (has_aggregates_in_select_list) {
    valid_agg_cols <- setdiff(c(valid_agg_cols, names(tree$select)[is_aggregate_expression]), "")
  }

  has_aggregates_in_order_by_clause <- any(are_aggregate_expressions(tree$order_by))

  if (is_select_distinct && (has_group_by || has_aggregates_in_select_list || has_having)) {
    stop("SELECT DISTINCT cannot be used together with ",
         "aggregate expressions or a GROUP BY clause", call. = FALSE)
  }

  if (!is_valid_expression_in_aggregation(tree$having[[1]], valid_agg_cols)) {
    stop("The expression in the HAVING clause is invalid in an aggregation context ",
         "or incompatible with the GROUP BY clause", call. = FALSE)
  }

  # use tree$group_by (not valid_agg_cols) in this test
  #because we can't refer to aliases in the select list itself
  if (has_aggregates_in_select_list &&
      !all(are_valid_expressions_in_aggregation(tree$select, tree$group_by))) {
    stop("The SELECT list includes expressions that are invalid in an aggregation context ",
         "or incompatible with the GROUP BY clause", call. = FALSE)
  }

  if (has_aggregates_in_order_by_clause &&
      !all(are_valid_expressions_in_aggregation(tree$order_by, valid_agg_cols))) {
    stop("The ORDER BY list includes expressions that are invalid in an aggregation context ",
         "or incompatible with the GROUP BY clause", call. = FALSE)
  }

  if (is_select_distinct) {
    attr(tree$select, "distinct") <- TRUE
  }

  tree
}
