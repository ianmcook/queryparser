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

#' Remove prefixes from column references in a SQL query
#'
#' @description Unqualifies column references in the clauses of a SQL
#'   \code{SELECT} statement that begin with any of the specified prefixes
#'   followed by a dot
#'
#' @param tree a list returned by \code{\link{squish_sql}} containing named
#'   elements representing the clauses of a SQL \code{SELECT} statement
#' @param prefixes a character vector containing one or more table names or
#'   table aliases
#' @param except a character vector containing column references to leave as is
#'   (optional)
#' @return A list the same as \code{tree} but with all column references in the
#'   \code{SELECT}, \code{WHERE}, \code{GROUP BY}, \code{HAVING}, and
#'   \code{ORDER BY} clauses unqualified, except those in \code{except}. The
#'   \code{FROM} clause is unmodified
#' @export
unqualify_query <- function(tree, prefixes, except = NULL) {
  tree$select <- unqualify_clause(tree$select, prefixes, except)
  tree$where <- unqualify_clause(tree$where, prefixes, except)
  tree$group_by <- unqualify_clause(tree$group_by, prefixes, except)
  tree$having <- unqualify_clause(tree$having, prefixes, except)
  tree$order_by <- unqualify_clause(tree$order_by, prefixes, except)
  tree
}

unqualify_clause <- function(exprs, prefixes, except) {
  attrs <- attributes(exprs)
  if (is.null(exprs)) return(NULL)
  exprs <- lapply(exprs, function(expr) {
    column_names <- setdiff(all_cols(expr), except)
    unqualify_expression(expr, prefixes, column_names)
  })
  attributes(exprs) <- attrs
  exprs
}

unqualify_expression <- function(expr, prefixes, column_names) {
  if (length(expr) == 1) {
    if (identical(typeof(expr), "symbol")) {
      qualified_column_name <- deparse(expr)
      if (qualified_column_name %in% column_names) {
        unqualified_column_name <- sub(
          # From ?base::regex: "If you want to remove the special meaning from a
          # sequence of characters, you can do so by putting them between \Q and \E."
          paste0("^(\\Q", paste0(prefixes, collapse = "\\E|\\Q"), "\\E)\\."),
          "",
          qualified_column_name
        )
        return(str2lang(unqualified_column_name))
      } else {
        return(expr)
      }
    } else {
      return(expr)
    }
  } else {
    return(as.call(lapply(expr, unqualify_expression, prefixes, column_names)))
  }
}
