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

# prevent unquotation with tidyverse packages
# by enclosing operand of `!` in parentheses
wrap_bangs <- function(expr) {
  if (length(expr) == 1) {
    return(expr)
  } else {
    if (expr[[1]] == quote(`!`) && is.call(expr[[2]]) &&
        length(expr[[2]]) && expr[[2]][[1]] == quote(`!`)) {
      return(as.call(lapply(
        str2lang(paste0("!(", deparse(expr[[2]]),")")),
        wrap_bangs
      )))
    } else {
      return(as.call(lapply(expr, wrap_bangs)))
    }
  }
}
