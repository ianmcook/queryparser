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

are_aggregate_expressions <- function(exprs) {
  sapply(exprs, is_aggregate_expression)
}

is_aggregate_expression <- function(expr) {
  if (length(expr) == 1) {
    return(FALSE)
  } else {
    if (is_aggregate_function_call(expr)) {
      return(TRUE)
    }
    out <- lapply(expr, is_aggregate_expression)
  }
  any(sapply(out, isTRUE))
}
# can't just test
#any(r_aggregate_functions %in% all_funs(expr))
# because paste() also is also an aggregate function
# but only when !is.null(collapse)

are_valid_expressions_in_aggregation <- function(exprs, allowed_names) {
  sapply(exprs, is_valid_expression_in_aggregation, allowed_names = sapply(allowed_names, deparse))
}

is_valid_expression_in_aggregation <- function(expr, allowed_names, var_names = all_cols(expr), agg = FALSE) {
  if(deparse(expr) %in% allowed_names) {
    return(TRUE)
  } else if (length(expr) == 1) {
    if (!agg && deparse(expr) %in% var_names) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else {
    if (is_aggregate_function_call(expr)) {
      agg <- TRUE
    }
    out <- lapply(expr, is_valid_expression_in_aggregation, allowed_names, var_names, agg)
  }
  all(sapply(out, isTRUE))
}

is_aggregate_function_call <- function(expr) {
  identical(typeof(expr), "language") && length(expr) > 1 &&
    (deparse(expr[[1]]) %in% r_aggregate_functions ||
       (deparse(expr[[1]]) %in% c("paste","paste0") && "collapse" %in% names(expr) &&
          !is.null(expr[[which(names(expr) == "collapse")]])))
}

# example usage:
#
#allowed_names <- "Species"
#good <- quote(1 + mean(Sepal.Length + 3))
#bad <- quote((Sepal.Length + 3) - mean(Sepal.Length + 3))
#
#is_aggregate_expression(good)                      # returns TRUE
#is_valid_expression_in_aggregation(good, allowed_names) # returns TRUE
#
#is_aggregate_expression(good)                      # returns TRUE
#is_valid_expression_in_aggregation(bad, allowed_names)  # returns FALSE
#

# note:
#
# there are cases where is_aggregate_expression() returns FALSE
# but is_valid_expression_in_aggregation() returns TRUE
# for example:
#
#is_aggregate_expression(quote(Species))
#is_valid_expression_in_aggregation(quote(Species), "Species")
#
# this is deliberate
