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

#' SQL query lexer
#'
#' @description Splits a SQL SELECT statement into clauses and splits
#'   comma-separated column lists within the clauses. Finer-grained lexing is
#'   performed by \code{\link{parse_query}}.
#'
#' @param query a SQL SELECT statement
#' @return A list object with named elements representing the clauses of the
#'   query
#' @examples
#' query <- "SELECT origin, dest,
#'     COUNT(*) AS num_flts,
#'     round(AVG(distance)) AS dist,
#'     round(AVG(arr_delay)) AS avg_delay
#'   FROM flights
#'   WHERE distance BETWEEN 200 AND 300
#'   GROUP BY origin, dest
#'   HAVING num_flts > 5000
#'   ORDER BY num_flts DESC, avg_delay DESC
#'   LIMIT 100;"
#' lex_query(query)
#' @seealso \code{\link{parse_query}}
#' @export
lex_query <- function(query) {
  query <- trimws(query, whitespace = ws_regex)
  query <- sub(";$", "", query)

  rc <- rawConnection(raw(0L), "r+")
  on.exit(close(rc))
  writeChar(query, rc)
  len <- seek(rc, 0L) - 1L

  if (!keyword_starts_here(rc, "select")) {
    stop("Query must begin with the SELECT keyword", call. = FALSE)
  }
  seek(rc, 6L)

  pos_from <- NULL
  pos_where <- NULL
  pos_group_by <- NULL
  pos_having <- NULL
  pos_order_by <- NULL
  pos_limit <- NULL

  in_quotes <- FALSE
  in_parens <- 0

  while((pos <- seek(rc, NA)) <= len) {

    # identify when inside strings and parentheses
    char <- readChar(rc, 1L)
    if (char %in% quote_chars) {
      if (!in_quotes) {
        in_quotes <- TRUE
        quo_char <- char
      } else if (char == quo_char) {
        seek(rc, -2L, "current")
        esc_quo <- c(quo_char, "\\")
        if (!readChar(rc, 1L) %in% esc_quo) {
          in_quotes <- FALSE
          rm(quo_char)
        }
        seek(rc, 1L, "current")
      }
    } else if (!in_quotes && char == "(") {
      in_parens <- in_parens + 1
    } else if (!in_quotes && char == ")") {
      in_parens <- in_parens - 1
    }

    if (!in_quotes) {

      # identify unsupported syntax
      if (keyword_starts_here(rc, "case")) {
        stop("CASE expressions are not supported", call. = FALSE)
      }
      if (keyword_starts_here(rc, "select")) {
        if (in_parens > 0) {
          stop("Subqueries are not supported", call. = FALSE)
        } else {
          stop("The SELECT keyword is used two or more times", call. = FALSE)
        }
      }
    }

    if (!in_quotes && in_parens <= 0) {

      # identify unsupported syntax
      if (keyword_starts_here(rc, "union")) {
        stop("The UNION operator is not supported", call. = FALSE)
      }
      if (keyword_starts_here(rc, "intersect")) {
        stop("The INTERSECT operator is not supported", call. = FALSE)
      }
      if (keyword_starts_here(rc, "except")) {
        stop("The EXCEPT operator is not supported", call. = FALSE)
      }

      # identify beginnings of clauses
      if (keyword_starts_here(rc, "from")) {
        pos_from <- append(pos_from, pos)
      } else if (keyword_starts_here(rc, "where")) {
        pos_where <- append(pos_where, pos)
      } else if (keyphrase_starts_here(rc, "group by")) {
        pos_group_by <- append(pos_group_by, pos)
      } else if (keyword_starts_here(rc, "having")) {
        pos_having <- append(pos_having, pos)
      } else if (keyphrase_starts_here(rc, "order by")) {
        pos_order_by <- append(pos_order_by, pos)
      } else if (keyword_starts_here(rc, "limit")) {
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
  original_encoding <- Encoding(query)
  Encoding(query) <- "bytes"
  clauses <- lapply(
    clauses, function(x) {
      clause <- substr(query, x$start, x$stop)
      Encoding(clause) <- original_encoding
      clause
    }
  )

  clauses$select <- lex_select(clauses$select)
  clauses$from <- lex_from(clauses$from)
  clauses$where <- lex_where(clauses$where)
  clauses$group_by <- lex_group_by(clauses$group_by)
  clauses$having <- lex_having(clauses$having)
  clauses$order_by <- lex_order_by(clauses$order_by)
  clauses$limit <- lex_limit(clauses$limit)

  clauses
}

lex_select <- function(clause) {
  lex_comma_list(lex_clause(clause, "select"))
}

lex_from <- function(clause) {
  lex_clause(clause, "from")
}

lex_where <- function(clause) {
  lex_clause(clause, "where")
}

lex_group_by <- function(clause) {
  lex_comma_list(lex_clause(clause, paste0("group", ws_regex, "+by")))
}

lex_having <- function(clause) {
  lex_clause(clause, "having")
}

lex_order_by <- function(clause) {
  lex_comma_list(lex_clause(clause, paste0("order", ws_regex, "+by")))
}

lex_limit <- function(clause) {
  lex_clause(clause, "limit")
}

lex_clause <- function(clause, keyword_regex) {
  if (is.null(clause)) return(NULL)
  clause <- trimws(clause, whitespace = ws_regex)
  keyword_regex <- paste0("^", keyword_regex, ws_regex, "*")
  clause <- sub(keyword_regex, "", clause, ignore.case = TRUE)
  clause
}

lex_comma_list <- function(comma_list) {
  if (is.null(comma_list)) return(NULL)

  rc <- rawConnection(raw(0L), "r+")
  on.exit(close(rc))
  writeChar(comma_list, rc)
  len <- seek(rc, 0L) - 1L

  pos_comma <- NULL

  in_quotes <- FALSE
  in_parens <- 0
  while((pos <- seek(rc, NA)) <= len) {
    char <- readChar(rc, 1L)

    if (char %in% quote_chars) {
      if (!in_quotes) {
        in_quotes <- TRUE
        quo_char <- char
      } else if (char == quo_char) {
        seek(rc, -2L, "current")
        esc_quo <- c(quo_char, "\\")
        if (!readChar(rc, 1L) %in% esc_quo) {
          in_quotes <- FALSE
          rm(quo_char)
        }
        seek(rc, 1L, "current")
      }
    } else if (!in_quotes && char == "(") {
      in_parens <- in_parens + 1
    } else if (!in_quotes && char == ")") {
      in_parens <- in_parens - 1
    } else if (!in_quotes && in_parens <= 0) {
      if (char == ",") {
        pos_comma <- append(pos_comma, pos)
      }
    }
  }

  pos_comma <- pos_comma + 1

  if (is.null(pos_comma)) {
    trimws(comma_list, whitespace = ws_regex)
  } else {
    original_encoding <- Encoding(query)
    Encoding(comma_list) <- "bytes"
    out <- trimws(
      substring(comma_list, c(1, pos_comma + 1), c(pos_comma - 1, len)),
      whitespace = ws_regex
    )
    Encoding(out) <- original_encoding
    out
  }
}
