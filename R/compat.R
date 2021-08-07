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

# for compatibility with R versions earlier than 3.6.0
if (!exists("str2lang")) {
  str2lang <- function(s) {
    parse(text = s, keep.source = FALSE)[[1]]
  }
}

# for compatibility with R versions earlier than 3.3.0
if (!exists("validEnc")) {
  validEnc <- function(x) {
    !is.na(nchar(x, type = "width"))
  }
}

# to avoid problems with expressions longer than about 60 characters
deparse <- function(expr, width.cutoff = 500, ...) {
  paste0(trimws(base::deparse(expr, width.cutoff, ...)), collapse = " ")
}

# to avoid "truncating string with embedded nuls" warnings
# in R version 4.0.0 and higher
readChar <- function (con, nchars, useBytes = FALSE) {
  suppressWarnings(base::readChar(con, nchars, useBytes))
}

# to avoid incompatibilities between newer versions of queryparser and older
# versions of tidyquery, call this function at the top of every queryparser
# function that is imported by tidyquery
assert_tidyquery_version <- function(min_version = "0.2.0") {
  # is the function that called this function being called from tidyquery?
  if (identical(get0(".packageName", topenv(parent.frame(2)), inherits = FALSE), "tidyquery")) {
    current_version <-
      mget(
        ".tidyquery.version",
        envir = asNamespace("tidyquery"),
        ifnotfound = list(package_version("0.0.0")),
        inherits = FALSE
      )[[1]]
    # if yes, error if tidyquery version too old
    if (current_version < package_version(min_version)) {
      stop(
        "Incompatible tidyquery version. Install tidyquery ",
        min_version,
        " or higher",
        call. = FALSE
      )
    }
  }
}
