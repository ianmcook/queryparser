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

parse_join <- function(expr, tidyverse, secure) {
  rc <- rawConnection(raw(0L), "r+")
  on.exit(close(rc))
  writeChar(expr, rc)
  len <- seek(rc, 0L) - 1L

  join_tables <- NULL
  join_types <- NULL
  join_conditions <- NULL

  in_on_clause <- FALSE
  in_using_clause <- FALSE

  table_reference_expected <- TRUE
  table_reference_start_pos <- 0L
  table_alias_expected <- FALSE
  table_alias_required <- FALSE
  conditions_expected <- FALSE

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
      in_word <- FALSE
    } else if (!in_quotes && char == "(") {
      escaped <- FALSE
      in_parens <- in_parens + 1
      in_word <- FALSE
    } else if (!in_quotes && char == ")") {
      escaped <- FALSE
      in_parens <- in_parens - 1
      in_word <- FALSE
    } else if (is_word_character(char, allow_colons_in_word = TRUE)) {
      escaped <- FALSE
      in_word <- TRUE
    } else {
      escaped <- FALSE
      in_word <- FALSE
    }

    if (!in_quotes && in_parens <= 0 && char == ",") {
      stop("SQL-89-style (implicit) join syntax is not supported", call. = FALSE)
    }

    if (table_alias_expected && char %in% c("", " ") && !in_quotes && in_parens <= 0) {

      next_thing <- get_next_character_word_or_number(rc, len, allow_colons_in_word = TRUE)
      if (identical(tolower(next_thing), "as")) {
        if (table_alias_required) {
          stop("Repeated AS keyword in FROM clause", call. = FALSE)
        }
        table_alias_required <- TRUE
        table_alias_start_pos <- pos + 3L
      } else if (char == "" || is_join_keyword(next_thing)) {

        if (table_alias_required && pos <= table_alias_start_pos) {
          stop("Missing table alias in FROM clause", call. = FALSE)
        }
        rm(table_alias_start_pos)

        # found end of table reference (including alias, if present)

        table_reference_end_pos <- pos - 1L
        if (table_reference_end_pos < table_reference_start_pos) {
          stop("Missing table reference in join", call. = FALSE)
        }
        seek(rc, table_reference_start_pos)
        table_reference_expr <- readChar(
          rc,
          table_reference_end_pos - table_reference_start_pos + 1L,
          useBytes = TRUE
        )
        rm(table_reference_start_pos, table_reference_end_pos)
        pos <- pos - 1L
        seek(rc, pos)

        #message("Table reference is ", table_reference_expr)

        join_tables <- append(
          join_tables,
          parse_table_reference(table_reference_expr, tidyverse, secure)
        )

        table_alias_expected <- FALSE
        table_alias_required <- FALSE
      }

    } else if (table_reference_expected) {

      if (!exists("table_reference_start_pos")) {
        table_reference_start_pos <- pos + 1L
      }

      if (char == " " && !in_quotes && in_parens <= 0) {
        table_reference_expected <- FALSE
        table_alias_expected <- TRUE
        table_alias_start_pos <- pos
        pos <- pos - 1L
        seek(rc, pos)
      }

    } else if (in_on_clause && char %in% c(" ", "") && !in_quotes) {

      next_on_clause_symbol <- get_next_character_word_or_number(rc, len)

      if (exists("previous_on_clause_symbol")) {
        if ((length(next_on_clause_symbol) == 0 ) || (
          is_not_join_condition_operator(previous_on_clause_symbol) &&
          is_not_join_condition_operator(next_on_clause_symbol))) {

          rm(previous_on_clause_symbol)

          # found end of ON clause

          conditions_end_pos <- pos
          seek(rc, conditions_start_pos)
          on_expr <- readChar(rc, conditions_end_pos - conditions_start_pos + 1L, useBytes = TRUE)
          rm(conditions_start_pos, conditions_end_pos)
          pos <- pos - 1L
          seek(rc, pos)

          #message("ON expression is ", on_expr)

          on_expr <- sub("^ *\\( *", "", on_expr)
          on_expr <- sub(" *\\) *$", "", on_expr)
          parsed_on_expr <- parse_expression(on_expr, tidyverse, secure)
          check_on_expression(parsed_on_expr)
          join_conditions <- append(
            join_conditions,
            parsed_on_expr
          )

          in_on_clause <- FALSE

        }
      } else if (length(next_on_clause_symbol) == 0) {
        stop("Unexpected end of join conditions", call. = FALSE)
      }

      if (in_on_clause) {
        previous_on_clause_symbol <- next_on_clause_symbol
      }

    } else if (in_using_clause && !in_word && !in_quotes) {

      if (in_parens <= 0 && !char %in% c(" ", "(", ")")) {
        stop("In join conditions, USING must be followed by open parenthesis", call. = FALSE)
      }
      if (in_parens <= 0 && char == ")") {

        # found end of USING clause

        conditions_end_pos <- pos
        seek(rc, conditions_start_pos)
        using_expr <- readChar(rc, conditions_end_pos - conditions_start_pos + 1L, useBytes = TRUE)
        rm(conditions_start_pos, conditions_end_pos)
        seek(rc, pos)

        #message("USING expression is ", using_expr)

        using_expr <- sub("^ *\\( *", "", using_expr)
        using_expr <- sub(" *\\) *$", "", using_expr)
        using_exprs <- trimws(strsplit(using_expr, ",", fixed = TRUE)[[1]])
        if (length(using_exprs) == 0 || any(nchar(using_exprs) == 0)) {
          stop("Missing column references in USING clause", call. = FALSE)
        }

        parsed_using_exprs <- lapply(using_exprs, parse_expression, tidyverse, secure)
        check_using_expressions(parsed_using_exprs)
        translated_using_expressions <- translate_using_expressions(
          parsed_using_exprs,
          join_tables[length(join_tables)-1],
          join_tables[length(join_tables)]
        )
        join_conditions <- append(
          join_conditions,
          translated_using_expressions
        )

        in_using_clause <- FALSE

      }

    } else if (!in_quotes && in_parens <= 0 && !in_word) {

      if (clause_starts_here(rc, "join")) {
        if (conditions_expected) {
          stop("Use the ON or USING clause to specify required join conditions", call. = FALSE)
        }

        if (preceded_by_keyword(rc, "cross")) {

          seek(rc, -6L, "current")

          # specified: CROSS JOIN
          join_types <- append(join_types, "cross join")
          conditions_expected <- FALSE
          join_conditions <- append(join_conditions, NA)
          table_reference_expected <- TRUE

          seek(rc, 6L, "current")

        } else if (preceded_by_keyword(rc, "natural")) {

          seek(rc, -8L, "current")

          # specified: NATURAL JOIN
          join_types <- append(join_types, "natural inner join")
          conditions_expected <- FALSE
          join_conditions <- append(join_conditions, NA)
          table_reference_expected <- TRUE

          seek(rc, 8L, "current")

        } else if (preceded_by_keyword(rc, "inner")) {

          seek(rc, -6L, "current")

          if (preceded_by_keyword(rc, "natural")) {

            seek(rc, -8L, "current")

            # specified: NATURAL INNER JOIN
            join_types <- append(join_types, "natural inner join")
            conditions_expected <- FALSE
            join_conditions <- append(join_conditions, NA)
            table_reference_expected <- TRUE

            seek(rc, 8L, "current")

            } else if (preceded_by_keyword(rc, "left") ||
                       preceded_by_keyword(rc, "right") ||
                       preceded_by_keyword(rc, "full") ||
                       preceded_by_keyword(rc, "outer")) {

              stop("Invalid JOIN type", call. = FALSE)

            } else {

            # specified: INNER JOIN
            join_types <- append(join_types, "inner join")
            conditions_expected <- TRUE
            table_reference_expected <- TRUE

          }
          seek(rc, 6L, "current")

        } else if (preceded_by_keyword(rc, "outer")) {

          seek(rc, -6L, "current")

          if (preceded_by_keyword(rc, "natural")) {

            seek(rc, -8L, "current")

            # specified: NATURAL OUTER JOIN
            join_types <- append(join_types, "natural left outer join")
            conditions_expected <- FALSE
            join_conditions <- append(join_conditions, NA)
            table_reference_expected <- TRUE

            seek(rc, 8L, "current")

          } else if (preceded_by_keyword(rc, "left")) {

            seek(rc, -5L, "current")

            if (preceded_by_keyword(rc, "natural")) {

              seek(rc, -8L, "current")

              # specified: NATURAL LEFT OUTER JOIN
              join_types <- append(join_types, "natural left outer join")
              conditions_expected <- FALSE
              join_conditions <- append(join_conditions, NA)
              table_reference_expected <- TRUE

              seek(rc, 8L, "current")

            } else {

              # specified: LEFT OUTER JOIN
              join_types <- append(join_types, "left outer join")
              conditions_expected <- TRUE
              table_reference_expected <- TRUE

            }

            seek(rc, 5L, "current")

          } else if (preceded_by_keyword(rc, "right")) {

            seek(rc, -6L, "current")

            if (preceded_by_keyword(rc, "natural")) {

              seek(rc, -8L, "current")

              # specified: NATURAL RIGHT OUTER JOIN
              join_types <- append(join_types, "natural right outer join")
              conditions_expected <- FALSE
              join_conditions <- append(join_conditions, NA)
              table_reference_expected <- TRUE

              seek(rc, 8L, "current")

            } else {

              # specified: RIGHT OUTER JOIN
              join_types <- append(join_types, "right outer join")
              conditions_expected <- TRUE
              table_reference_expected <- TRUE

            }

            seek(rc, 6L, "current")

          } else if (preceded_by_keyword(rc, "full")) {

            seek(rc, -5L, "current")

            if (preceded_by_keyword(rc, "natural")) {

              seek(rc, -8L, "current")

              # specified: NATURAL FULL OUTER JOIN
              join_types <- append(join_types, "natural full outer join")
              conditions_expected <- FALSE
              join_conditions <- append(join_conditions, NA)
              table_reference_expected <- TRUE

              seek(rc, 8L, "current")

            } else {

              # specified: FULL OUTER JOIN
              join_types <- append(join_types, "full outer join")
              conditions_expected <- TRUE
              table_reference_expected <- TRUE

            }

            seek(rc, 5L, "current")

          } else if (preceded_by_keyword(rc, "inner")) {

            stop("Invalid JOIN type", call. = FALSE)

          } else {

            # specified: OUTER JOIN
            join_types <- append(join_types, "left outer join")
            conditions_expected <- TRUE
            table_reference_expected <- TRUE

          }

          seek(rc, 6L, "current")

        } else if (preceded_by_keyword(rc, "semi")) {

          seek(rc, -5L, "current")

          if (preceded_by_keyword(rc, "left")) {

            seek(rc, -5L, "current")

            if (preceded_by_keyword(rc, "natural")) {

              seek(rc, -8L, "current")

              # specified: NATURAL LEFT SEMI JOIN
              join_types <- append(join_types, "natural left semi join")
              conditions_expected <- FALSE
              join_conditions <- append(join_conditions, NA)
              table_reference_expected <- TRUE

              seek(rc, 8L, "current")

            } else {

              # specified: LEFT SEMI JOIN
              join_types <- append(join_types, "left semi join")
              conditions_expected <- TRUE
              table_reference_expected <- TRUE

            }

            seek(rc, 5L, "current")

          } else if (preceded_by_keyword(rc, "right")) {

            seek(rc, -6L, "current")

            if (preceded_by_keyword(rc, "natural")) {

              seek(rc, -8L, "current")

              # specified: NATURAL RIGHT SEMI JOIN
              join_types <- append(join_types, "natural right semi join")
              conditions_expected <- FALSE
              join_conditions <- append(join_conditions, NA)
              table_reference_expected <- TRUE

              seek(rc, 8L, "current")

            } else {

              # specified: RIGHT SEMI JOIN
              join_types <- append(join_types, "right semi join")
              conditions_expected <- TRUE
              table_reference_expected <- TRUE

            }

            seek(rc, 6L, "current")

          } else {
            stop("Specify LEFT or RIGHT before SEMI JOIN", call. = FALSE)
          }

          seek(rc, 5L, "current")

        } else if (preceded_by_keyword(rc, "anti")) {

          seek(rc, -5L, "current")

          if (preceded_by_keyword(rc, "left")) {

            seek(rc, -5L, "current")

            if (preceded_by_keyword(rc, "natural")) {

              seek(rc, -8L, "current")

              # specified: NATURAL LEFT ANTI JOIN
              join_types <- append(join_types, "natural left anti join")
              conditions_expected <- FALSE
              join_conditions <- append(join_conditions, NA)
              table_reference_expected <- TRUE

              seek(rc, 8L, "current")

            } else {

              # specified: LEFT ANTI JOIN
              join_types <- append(join_types, "left anti join")
              conditions_expected <- TRUE
              table_reference_expected <- TRUE

            }

            seek(rc, 5L, "current")

          } else if (preceded_by_keyword(rc, "right")) {

            seek(rc, -6L, "current")

            if (preceded_by_keyword(rc, "natural")) {

              seek(rc, -8L, "current")

              # specified: NATURAL RIGHT ANTI JOIN
              join_types <- append(join_types, "natural right anti join")
              conditions_expected <- FALSE
              join_conditions <- append(join_conditions, NA)
              table_reference_expected <- TRUE

              seek(rc, 8L, "current")

            } else {

              # specified: RIGHT ANTI JOIN
              join_types <- append(join_types, "right anti join")
              conditions_expected <- TRUE
              table_reference_expected <- TRUE

            }

            seek(rc, 6L, "current")

          } else {
            stop("Must specify LEFT or RIGHT before ANTI JOIN", call. = FALSE)
          }

          seek(rc, 5L, "current")

        } else if (preceded_by_keyword(rc, "left")) {

          seek(rc, -5L, "current")

          if (preceded_by_keyword(rc, "natural")) {

            seek(rc, -8L, "current")

            # specified: NATURAL LEFT JOIN
            join_types <- append(join_types, "natural left outer join")
            conditions_expected <- FALSE
            join_conditions <- append(join_conditions, NA)
            table_reference_expected <- TRUE

            seek(rc, 8L, "current")

          } else {

            # specified: LEFT JOIN
            join_types <- append(join_types, "left outer join")
            conditions_expected <- TRUE
            table_reference_expected <- TRUE

          }

          seek(rc, 5L, "current")

        } else if (preceded_by_keyword(rc, "right")) {

          seek(rc, -6L, "current")

          if (preceded_by_keyword(rc, "natural")) {

            seek(rc, -8L, "current")

            # specified: NATURAL RIGHT JOIN
            join_types <- append(join_types, "natural right outer join")
            conditions_expected <- FALSE
            join_conditions <- append(join_conditions, NA)
            table_reference_expected <- TRUE

            seek(rc, 8L, "current")

          } else {

            # specified: RIGHT JOIN
            join_types <- append(join_types, "right outer join")
            conditions_expected <- TRUE
            table_reference_expected <- TRUE

          }

          seek(rc, 6L, "current")

        } else if (preceded_by_keyword(rc, "full")) {

          seek(rc, -5L, "current")

          if (preceded_by_keyword(rc, "natural")) {

            seek(rc, -8L, "current")

            # specified: NATURAL FULL JOIN
            join_types <- append(join_types, "natural full outer join")
            conditions_expected <- FALSE
            join_conditions <- append(join_conditions, NA)
            table_reference_expected <- TRUE

            seek(rc, 8L, "current")

          } else {

            # specified: FULL JOIN
            join_types <- append(join_types, "full outer join")
            conditions_expected <- TRUE
            table_reference_expected <- TRUE

          }

          seek(rc, 5L, "current")

        } else {

          # specified: JOIN
          join_types <- append(join_types, "inner join")
          conditions_expected <- TRUE
          table_reference_expected <- TRUE

        }

        # skip to the end of the word "join"
        pos <- pos + 4L
        seek(rc, pos)

      } else if (clause_starts_here(rc, "on")) {

        if (!conditions_expected) {
          stop("Unexpected ON clause", call. = FALSE)
        }
        if (table_reference_expected) {
          stop("Missing table reference after JOIN", call. = FALSE)
        }

        seek(rc, 2L, "current")
        pos <- pos + 2L
        in_on_clause <- TRUE
        conditions_start_pos <- pos + 1L
        conditions_expected <- FALSE

      } else if (clause_starts_here(rc, "using")) {

        if (!conditions_expected) {
          stop("Unexpected USING clause", call. = FALSE)
        }
        if (table_reference_expected) {
          stop("Missing table reference after JOIN", call. = FALSE)
        }

        seek(rc, 5L, "current")
        pos <- pos + 5L
        in_using_clause <- TRUE
        conditions_start_pos <- pos + 1L
        conditions_expected <- FALSE

      } else if (!table_reference_expected && !table_alias_expected && !in_on_clause && !in_using_clause) {

        next_thing <- get_next_character_word_or_number(rc, len, allow_colons_in_word = TRUE)
        if (length(next_thing) > 0 && !identical(next_thing, " ") && !is_join_keyword(next_thing)) {
          stop("Unexpected word or symbol in FROM clause", call. = FALSE)
        }

      }
    }

    seek(rc, pos + 1L)
  }

  if (in_quotes) {
    stop("FROM clause contains unmatched quotation marks", call. = FALSE)
  }
  if (in_parens > 0) {
    stop("FROM clause contains unmatched parentheses", call. = FALSE)
  }
  if (table_reference_expected) {
    stop("Missing table reference in FROM clause", call. = FALSE)
  }
  if (table_alias_required) {
    stop("Missing table alias in FROM clause", call. = FALSE)
  }
  if (conditions_expected) {
    stop("Use the ON or USING clause to specify required join conditions", call. = FALSE)
  }
  if (in_on_clause || in_using_clause) {
    stop("Malformed join conditions", call. = FALSE)
  }
  if (length(join_types) + 1 != length(join_tables)) {
    stop("Unexpected result from parsing join types", call. = FALSE)
  }
  if (length(join_conditions) + 1 != length(join_tables)) {
    stop("Unexpected result from parsing join conditions", call. = FALSE)
  }
  if (is.null(names(join_tables))) {
    table_references <- as.character(join_tables)
  } else {
    table_references <-  unname(unlist(
      Map(
        function(x,y) {if (x != "") x else y},
        names(join_tables),
        as.character(join_tables)
      )
    ))
  }
  if (any(duplicated(table_references))) {
    stop("Tables in a join must have different names or aliases", call. = FALSE)
  }

  output <- join_tables
  attr(output, "join_types") <- join_types
  attr(output, "join_conditions") <- join_conditions
  output
}

check_using_expression <- function(expr) {
  if (!identical(typeof(expr), "symbol")) {
    stop("Only column names are allowed after USING in join conditions", call. = FALSE)
  }
}

check_using_expressions <- function(exprs) {
  lapply(exprs, check_using_expression)
}

check_on_expression <- function(expr) {
  if (typeof(expr) != "language") {
    stop("Malformed join conditions", call. = FALSE)
  }
  if (length(setdiff(all_funs(expr), c("&", "=="))) > 0) {
    stop("Only equality comparisons combined with AND are allowed after ON in join conditions", call. = FALSE)
  }
  if (!identical(expr[[1]], quote(`&`)) && !identical(expr[[1]], quote(`==`))) {
    stop("Malformed join conditions", call. = FALSE)
  }
  if (identical(expr[[1]], quote(`&`))) {
    lapply(as.list(expr)[-1], function(subexpr) {
      if (typeof(subexpr) != "language") {
        stop("Malformed join conditions", call. = FALSE)
      }
      if (identical(subexpr[[1]], quote(`&`))) {
        check_on_expression(subexpr)
      } else if (identical(subexpr[[1]], quote(`==`))) {
        if (!identical(unique(vapply(subexpr, typeof, character(1))), "symbol")) {
          stop("Malformed join conditions", call. = FALSE)
        }
      } else {
        stop("Malformed join conditions", call. = FALSE)
      }
    })
  }
}

get_prefix_for_join_conditions <- function(table_reference) {
  if (is.null(names(table_reference)) || is.na(names(table_reference)) || names(table_reference) == "") {
    deparse(table_reference[[1]])
  } else {
    names(table_reference)
  }
}

translate_using_expressions <- function(exprs, left_table, right_table) {
  left_table_prefix <- get_prefix_for_join_conditions(left_table)
  right_table_prefix <- get_prefix_for_join_conditions(right_table)
  str2lang(paste(
    lapply(
      exprs,
      function(expr) {
        paste0("`", left_table_prefix, ".", deparse(expr), "` == `", right_table_prefix, ".", deparse(expr), "`")
      }),
    collapse = " & "
  ))
}
