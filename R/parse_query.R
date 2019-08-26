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

#' Parse a SQL query
#'
#' @description Parses a SQL SELECT statement into a list of R expressions
#'
#' @param query a character string containing a SQL SELECT statement
#' @param tidyverse set to \code{TRUE} to use functions from tidyverse packages
#'   including dplyr, stringr, and lubridate in the R expressions
#' @return A list object with named elements representing the clauses of the
#'   query and containing unevaluated R expressions representing the SQL
#'   expressions in the query
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
parse_query <- function(query, tidyverse = FALSE) {

  tree <- split_query(query)
  if (!names(tree)[1] %in% c("select", "distinct")) {
    stop("Unexpected output from split_query()", call. = FALSE)
  }
  # ALSO HANDLE WHEN IT'S tree$distinct
  # maybe do it positionally and check the name
  tree[[1]] <- unlist(lapply(tree[[1]], extract_alias))
  tree[[1]] <- sapply(tree[[1]], parse_expression, tidyverse = tidyverse, USE.NAMES = !is.null(names(tree[[1]])))




  # ...
  tree
}
