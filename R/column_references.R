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

#' Return the column references in a parsed SQL query
#'
#' @description Returns a character vector containing all the column references
#'   in the clauses of a parsed SQL \code{SELECT} statement
#'
#' @param tree a list returned by \code{\link{parse_query}} containing named
#'   elements representing the clauses of a SQL \code{SELECT} statement
#' @return A character vector containing all the unique column references found
#'   in the \code{SELECT}, \code{FROM}, \code{WHERE}, \code{GROUP BY},
#'   \code{HAVING}, and \code{ORDER BY} clauses of the \code{SELECT} statement
#' @details The returned character vector includes only \emph{column}
#'   references, not table references. Column aliases assigned in the
#'   \code{SELECT} list are not included unless they are used in other clauses.
#' @examples
#' my_query <- "SELECT f.flight,
#'     manufacturer, p.model
#'   FROM flights f
#'     JOIN planes p USING (tailnum);"
#'
#' column_references(parse_query(my_query))
#' @seealso \code{\link{parse_query}}
#' @export
column_references <- function(tree) {
  if (!is.list(tree) || !("select" %in% names(tree))) {
    stop("Unexpected input to column_references()", call. = FALSE)
  }
  unique(c(
    column_references_in_clause(tree$select),
    column_references_in_clause(attr(tree$from, "join_conditions")),
    column_references_in_clause(tree$where),
    column_references_in_clause(tree$group_by),
    column_references_in_clause(tree$having),
    column_references_in_clause(tree$order_by)
  ))
}

column_references_in_clause <- function(exprs) {
  if (is.null(exprs)) return(character(0))
  unlist(lapply(exprs, all_cols))
}
