parse_query <- function(x) {
  x <- trimws(x, whitespace = whitespace_regex)
  rc <- rawConnection(raw(0L), "r+")
  on.exit(close(rc))
  writeChar(x, rc)
  len <- seek(rc, 0L) - 1L

  lex_query(rc, len)

  # ...

}

lex_query <- function(rc, len) {
  if (tolower(readChar(rc, 6L)) != "select") {
    stop("Query must begin with a SELECT statement", call. = FALSE)
  }

  pos_from <- NULL
  pos_where <- NULL
  pos_group <- NULL
  pos_having <- NULL
  pos_order <- NULL
  pos_limit <- NULL

  in_quo <- FALSE
  in_sub <- 0
  while((pos <- seek(rc, NA)) <= len) {
    char <- readChar(rc, 1L)

    if (char %in% quote_chars) {
      if (!in_quo) {
        in_quo <- TRUE
        quo_char <- char
      } else if (char == quo_char) {
        seek(rc, -2L, "current")
        esc_quo <- c(quo_char, "\\")
        if (!readChar(rc, 1L) %in% esc_quo) {
          in_quo <- FALSE
          rm(quo_char)
        }
        seek(rc, 1L, "current")
      }
    } else if (!in_quo && char == "(") {
      in_sub <- in_sub + 1
    } else if (!in_quo && char == ")") {
      in_sub <- in_sub - 1
    } else if (!in_quo && in_sub <= 0) {
      if (tolower(char) == "u") {
        if (tolower(readChar(rc, 4L)) == "nion") {
          stop("The UNION operator is not supported", call. = FALSE)
        }
      } else if (tolower(char) == "i") {
        if (tolower(readChar(rc, 8L)) == "ntersect") {
          stop("The INTERSECT operator is not supported", call. = FALSE)
        }
      } else if (tolower(char) == "e") {
        if (tolower(readChar(rc, 5L)) == "xcept") {
          stop("The EXCEPT operator is not supported", call. = FALSE)
        }
      } else if (tolower(char) == "f") {
        if (tolower(readChar(rc, 3L)) == "rom") {
          pos_from <- append(pos_from, pos)
        }
      } else if (tolower(char) == "w") {
        if (tolower(readChar(rc, 4L)) == "here") {
          pos_where <- append(pos_where, pos)
        }
      } else if (tolower(char) == "g") {
        if (tolower(readChar(rc, 4L)) == "roup") {
          while(isTRUE(grepl(whitespace_regex, readChar(rc, 1L)))) {}
          seek(rc, -1L, "current")
          if (tolower(readChar(rc, 2L)) == "by") {
            pos_group <- append(pos_group, pos)
          }
        }
      } else if (tolower(char) == "h") {
        if (tolower(readChar(rc, 5L)) == "aving") {
          pos_having <- append(pos_having, pos)
        }
      } else if (tolower(char) == "o") {
        if (tolower(readChar(rc, 4L)) == "rder") {
          while(isTRUE(grepl(whitespace_regex, readChar(rc, 1L)))) {}
          seek(rc, -1L, "current")
          if (tolower(readChar(rc, 2L)) == "by") {
            pos_order <- append(pos_order, pos)
          }
        }
      } else if (tolower(char) == "l") {
        if (tolower(readChar(rc, 4L)) == "imit") {
          pos_limit <- append(pos_limit, pos)
        }
      }
      seek(rc, pos + 1)
    }
  }
  if (in_quo) {
    stop("Query contains unmatched quotation marks", call. = FALSE)
  }
  if (in_sub > 0) {
    stop("Query contains unmatched parentheses", call. = FALSE)
  }
  list(
    "select" = 0,
    "from" = pos_from,
    "where" = pos_where,
    "group" = pos_group,
    "having" = pos_having,
    "order" = pos_order,
    "limit" = pos_limit
  )
}

quote_chars <- c("\"", "'", "`")

whitespace_regex <- "[ \t\r\n]"
