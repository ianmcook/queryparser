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

#' Split a SQL query
#'
#' @description Splits a SQL \code{SELECT} statement into clauses, and splits
#'   comma-separated column lists within the clauses.
#'
#' @param query a character string containing a SQL \code{SELECT} statement
#' @return A list object with named elements representing the clauses of the
#'   query
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
#' split_query(query)
#' @seealso \code{\link{parse_query}}
#' @export
split_query <- function(query) {
  if (!identical(typeof(query), "character") || !identical(length(query), 1L)) {
    stop("Unexpected input to split_query()", call. = FALSE)
  }

  original_encoding <- Encoding(query)

  query <- trimws(query, whitespace = ws_regex)
  query <- squish_sql(query)
  query <- sub(";$", "", query)

  rc <- rawConnection(raw(0L), "r+")
  on.exit(close(rc))
  writeChar(query, rc)
  len <- seek(rc, 0L) - 1L

  if (!keyword_starts_here(rc, "select", useBytes = TRUE)) {
    stop("Query must begin with the SELECT keyword", call. = FALSE)
  }

  seek(rc, 8L)
  select_distinct <- FALSE
  if (keyword_starts_here(rc, "all", useBytes = TRUE)) {
    seek(rc, 10L)
  } else if (keyword_starts_here(rc, "distinct", useBytes = TRUE)) {
    select_distinct <- TRUE
    seek(rc, 15L)
  } else {
    seek(rc, 6L)
  }

  pos_from <- NULL
  pos_where <- NULL
  pos_group_by <- NULL
  pos_having <- NULL
  pos_order_by <- NULL
  pos_limit <- NULL

  in_quotes <- FALSE
  in_parens <- 0
  escaped <- FALSE

  while((pos <- seek(rc, NA)) <= len) {

    # identify when inside strings and parentheses
    char <- readChar(rc, 1L, useBytes = TRUE)
    if (char %in% quote_chars) {
      if (!in_quotes) {
        in_quotes <- TRUE
        escaped <- FALSE
        quo_char <- char
      } else if (char == quo_char) {
        if (escaped) {
          escaped <- FALSE
        } else {
          esc_quo <- c(quo_char, "\\")
          if (!readChar(rc, 1L, useBytes = TRUE) %in% esc_quo) {
            in_quotes <- FALSE
            escaped <- FALSE
            rm(quo_char)
          } else {
            escaped <- TRUE
          }
          seek(rc, -1L, "current")
        }
      }
    } else if (!in_quotes && char == "(") {
      escaped <- FALSE
      in_parens <- in_parens + 1
    } else if (!in_quotes && char == ")") {
      escaped <- FALSE
      in_parens <- in_parens - 1
    }

    if (!in_quotes) {

      # identify unsupported syntax
      if (keyword_starts_here(rc, "case", useBytes = TRUE)) {
        stop("CASE expressions are not supported", call. = FALSE)
      }
      if (keyword_starts_here(rc, "over", useBytes = TRUE)) {
        stop("OVER clauses are not supported", call. = FALSE)
      }
      if (keyword_starts_here(rc, "select", useBytes = TRUE)) {
        if (in_parens > 0) {
          stop("Subqueries are not supported", call. = FALSE)
        } else {
          stop("The SELECT keyword is used two or more times", call. = FALSE)
        }
      }
    }

    if (!in_quotes && in_parens <= 0) {

      # identify unsupported syntax
      if (keyword_starts_here(rc, "join", useBytes = TRUE)) {
        stop("Joins are not supported", call. = FALSE)
      }
      if (keyword_starts_here(rc, "union", useBytes = TRUE)) {
        stop("The UNION operator is not supported", call. = FALSE)
      }
      if (keyword_starts_here(rc, "intersect", useBytes = TRUE)) {
        stop("The INTERSECT operator is not supported", call. = FALSE)
      }
      if (keyword_starts_here(rc, "except", useBytes = TRUE)) {
        stop("The EXCEPT operator is not supported", call. = FALSE)
      }

      # identify beginnings of clauses
      if (keyword_starts_here(rc, "from", useBytes = TRUE)) {
        # don't split on the "from" is "is [not] distinct from"
        if (!preceded_by_keyword(rc, "distinct", useBytes = TRUE)) {
          pos_from <- append(pos_from, pos)
        }
      } else if (keyword_starts_here(rc, "where", useBytes = TRUE)) {
        pos_where <- append(pos_where, pos)
      } else if (keyword_starts_here(rc, "group by", useBytes = TRUE)) {
        pos_group_by <- append(pos_group_by, pos)
      } else if (keyword_starts_here(rc, "having", useBytes = TRUE)) {
        pos_having <- append(pos_having, pos)
      } else if (keyword_starts_here(rc, "order by", useBytes = TRUE)) {
        pos_order_by <- append(pos_order_by, pos)
      } else if (keyword_starts_here(rc, "limit", useBytes = TRUE)) {
        pos_limit <- append(pos_limit, pos)
      }

    }

    seek(rc, pos + 1)
  }

  if (in_quotes) {
    stop("Query contains unmatched quotation marks", call. = FALSE)
  }
  if (in_parens > 0) {
    stop("Query contains unmatched parentheses", call. = FALSE)
  }

  start_pos <- list(
    "select" = 0,
    "from" = pos_from,
    "where" = pos_where,
    "group_by" = pos_group_by,
    "having" = pos_having,
    "order_by" = pos_order_by,
    "limit" = pos_limit
  )
  if (any(lapply(start_pos, length) > 1)) {
    stop("One or more clauses is used two or more times", call. = FALSE)
  }
  start_pos <- unlist(start_pos) + 1
  if (any(diff(start_pos) < 0)) {
    stop("Clauses are in an incorrect order", call. = FALSE)
  }
  stop_pos <- c(start_pos[-1] - 1, len)
  names(stop_pos) <- names(start_pos)
  clauses <- mapply(
    function(x, y) list(start = x, stop = y),
    start_pos,
    stop_pos,
    SIMPLIFY = FALSE
  )
  Encoding(query) <- "bytes"
  clauses <- lapply(
    clauses, function(x) {
      substr(query, x$start, x$stop)
    }
  )

  clauses$select <- split_select(clauses$select)
  clauses$from <- split_from(clauses$from)
  clauses$where <- split_where(clauses$where)
  clauses$group_by <- split_group_by(clauses$group_by)
  clauses$having <- split_having(clauses$having)
  clauses$order_by <- split_order_by(clauses$order_by)
  clauses$limit <- split_limit(clauses$limit)

  clauses <- lapply(clauses, function(clause) {Encoding(clause) <- original_encoding; clause})

  if (select_distinct) {
    attr(clauses$select, "distinct") <- TRUE
  }

  clauses
}

split_select <- function(clause) {
  split_comma_list(split_clause(clause, "select( all)?( distinct)?"))
}

split_from <- function(clause) {
  split_clause(clause, "from")
}

split_where <- function(clause) {
  split_clause(clause, "where")
}

split_group_by <- function(clause) {
  split_comma_list(split_clause(clause, "group by"))
}

split_having <- function(clause) {
  split_clause(clause, "having")
}

split_order_by <- function(clause) {
  split_comma_list(split_clause(clause, "order by"))
}

split_limit <- function(clause) {
  split_clause(clause, "limit")
}

split_clause <- function(clause, keyword) {
  if (is.null(clause)) return(NULL)
  clause <- trimws(clause, whitespace = ws_regex)
  keyword_regex <- paste0("^", keyword, ws_regex, "*")
  clause <- sub(keyword_regex, "", clause, ignore.case = TRUE, useBytes = TRUE)
  clause
}

split_comma_list <- function(comma_list) {
  if (is.null(comma_list)) return(NULL)

  rc <- rawConnection(raw(0L), "r+")
  on.exit(close(rc))
  writeChar(comma_list, rc)
  len <- seek(rc, 0L) - 1L

  pos_comma <- NULL

  in_quotes <- FALSE
  in_parens <- 0
  escaped <- FALSE

  while((pos <- seek(rc, NA)) <= len) {
    char <- readChar(rc, 1L, useBytes = TRUE)
    if (char %in% quote_chars) {
      if (!in_quotes) {
        in_quotes <- TRUE
        escaped <- FALSE
        quo_char <- char
      } else if (char == quo_char) {
        if (escaped) {
          escaped <- FALSE
        } else {
          esc_quo <- c(quo_char, "\\")
          if (!readChar(rc, 1L, useBytes = TRUE) %in% esc_quo) {
            in_quotes <- FALSE
            escaped <- FALSE
            rm(quo_char)
          } else {
            escaped <- TRUE
          }
          seek(rc, -1L, "current")
        }
      } else {
        escaped <- FALSE
      }
    } else if (!in_quotes && char == "(") {
      escaped <- FALSE
      in_parens <- in_parens + 1
    } else if (!in_quotes && char == ")") {
      escaped <- FALSE
      in_parens <- in_parens - 1
    } else if (!in_quotes && in_parens <= 0) {
      escaped <- FALSE
      if (char == ",") {
        pos_comma <- append(pos_comma, pos)
      }
    } else {
      escaped <- FALSE
    }
  }

  pos_comma <- pos_comma + 1

  if (is.null(pos_comma)) {
    trimws(comma_list, whitespace = ws_regex)
  } else {
    original_encoding <- Encoding(comma_list)
    Encoding(comma_list) <- "bytes"
    out <- trimws(
      substring(comma_list, c(1, pos_comma + 1), c(pos_comma - 1, len)),
      whitespace = ws_regex
    )
    Encoding(out) <- original_encoding
    out
  }
}
