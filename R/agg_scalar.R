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

# R and SQL interpret aggregate functions with only scalar arguments differently
# (for example: the expression sum(1))
# in practice this only affects SUM() and GROUP_CONCAT()
# this function finds instances of these functions (post-translation) that contain no column references
# and changes them to behave as they would in a SQL engine

#' @include compat.R
NULL

translate_agg_scalar <- function(expr, tidyverse) {
  if (length(expr) == 1) {
    return(expr)
  } else {
    if (deparse(expr[[1]]) == "sum" && is.call(expr) && length(all_cols(expr)) == 0) {
      if (tidyverse) {
        fun <- str2lang("dplyr::n()")
      } else {
        fun <- quote(nrow(.))
      }
      return(as.call(lapply(
        as.call(append(quote(`*`), list(fun, expr[[2]]))),
        translate_agg_scalar,
        tidyverse
      )))
    } else if (deparse(expr[[1]]) %in% c("paste","paste0","stringr::str_flatten") &&
               is.call(expr) && length(all_cols(expr)) == 0 &&
               "collapse" %in% names(expr) && !is.null(expr[[which(names(expr) == "collapse")]])) {
      if (tidyverse) {
        fun <- str2lang("dplyr::n()")
      } else {
        fun <- quote(nrow(.))
      }
      return(as.call(lapply(
        as.call(append(expr[[1]],
           list(as.call(append(quote(rep),
              list(expr[[2]], times = fun))), collapse = expr[[3]]))),
        translate_agg_scalar,
        tidyverse
      )))
    } else {
      return(as.call(lapply(expr, translate_agg_scalar, tidyverse)))
    }
  }
}
