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

#' @include common.R
NULL

translations_data_types_generic <- list(
  `string` = "character",
  `char` = "character",
  `varchar` = "character",
  `boolean` = "logical",
  `int` = "integer",
  `integer` = "integer",
  `bigint` = "integer",
  `smallint` = "integer",
  `tinyint` = "integer",
  `double` = "double",
  `real` = "double",
  `float` = "single",
  `decimal` = "numeric"
)

translations_data_types_base <- list(
  `timestamp` = "POSIXct"
  # `interval` = "difftime" # does not work directly
)

translations_data_types_tidyverse <- list(
  `timestamp` = "datetime",
  `interval` = "duration"
)
attr(translations_data_types_tidyverse[["timestamp"]], "function") <- "as_datetime"
attr(translations_data_types_tidyverse[["timestamp"]], "package") <- "lubridate"
attr(translations_data_types_tidyverse[["interval"]], "package") <- "lubridate"

translations_operators_binary_symbolic <- list(
  `%` = "%%",
  `<>` = "!=",
  `=` = "==",
  `<=>` = "%<=>%"
)

translations_operators_binary_word <- list(
  `and` = "&&",
  `or` = "||",
  `div` = "%/%",

  # variants negated by prefixing "not " must come BEFORE their positive equivalents
  # these are translated further by the indirect translations specified below
  `not like` = "%nlike%",
  `like` = "%like%",
  `not ilike` = "%nilike%",
  `ilike` = "%ilike%",
  `is not distinct from` = "%<=>%",
  `is distinct from` = "%<!=>%",

  # other operators that are procesed further below
  `xor` = "%xor%",
  `regexp` = "%regexp%",
  `rlike` = "%regexp%",
  `iregexp` = "%iregexp%"

  # `in` and `not in` are handled elsewhere
)

translations_operators_unary_prefix <- list(
  not = "!"
)

translations_operators_unary_postfix <- list(
  `is null` = "%>% is.na()",
  `is not null` = "%>% is.na() %>% `!`",
  `is unknown` = "%>% is.na()",
  `is not unknown` = "%>% is.na() %>% `!`",
  `is true` = "%>% as.logical()",
  `is not true` = "%>% as.logical() %>% `!`",
  `is false` = "%>% as.logical() %>% `!`",
  `is not false` = "%>% as.logical()"
)

translations_direct_generic <- list(

  # operators
  `%xor%` = quote(xor),

  # constants
  true = quote(TRUE),
  false = quote(FALSE),

  # mathematical functions
  abs = quote(abs),
  ceil = quote(ceiling),
  ceiling = quote(ceiling),
  exp = quote(exp),
  factorial = quote(factorial),
  floor = quote(floor),
  greatest = quote(pmax),
  is_nan = quote(is.nan),
  is_inf = quote(is.infinite),
  least = quote(pmin),
  log10 = quote(log10),
  log2 = quote(log2),
  mod = quote(`%%`),
  negative = quote(`-`),
  positive = quote(`+`),
  pow = quote(`^`),
  power = quote(`^`),
  quotient = quote(`%/%`),
  round = quote(round),
  sign = quote(sign),
  sqrt = quote(sqrt),
  truncate = quote(trunc), # trunc is translated below

  # trigonometric functions
  acos = quote(acos),
  asin = quote(asin),
  atan = quote(atan),
  atan2 = quote(atan2),
  cos = quote(cos),
  cosh = quote(cosh),
  cot = quote(cot),
  coth = quote(coth),
  sin = quote(sin),
  sinh = quote(sinh),
  tan = quote(tan),
  tanh = quote(tanh),

  # string functions
  concat = quote(paste0),
  substring = quote(substr) # substr is translated below

)

translations_direct_base <- list(

  # string functions
  length = quote(nchar),
  lcase = quote(tolower),
  lower = quote(tolower),
  ucase = quote(toupper),
  upper = quote(toupper),
  to_date = quote(as.Date)


)

translations_direct_tidyverse <- list(

  # string functions
  length = quote(stringr::str_length),
  lower = quote(stringr::str_to_lower),
  upper = quote(stringr::str_to_upper),
  to_date = quote(lubridate::as_date),

  # conditional functions
  coalesce = quote(coalesce),
  nullif = quote(dplyr::na_if)

  # add other lubridate, stringr, and dplyr functions
)

# the return value of these indirect expressions must be in the form:
#   eval(substitute(quote(  expression  )))
# the body of each function can process scalar arguments, but all
# operations on the data in columns must happen in the returned expression

translations_indirect_generic <- list(
  `%like%` = function(x, wc) {
    rx <- translate_wildcard_to_regex(wc)
    eval(substitute(quote(grepl(rx, x))))
  },
  `%nlike%` = function(x, wc) {
    rx <- translate_wildcard_to_regex(wc)
    eval(substitute(quote(!grepl(rx, x))))
  },
  `%ilike%` = function(x, wc) {
    rx <- translate_wildcard_to_regex(wc)
    eval(substitute(quote(grepl(rx, x, ignore.case = TRUE))))
  },
  `%nilike%` = function(x, wc) {
    rx <- translate_wildcard_to_regex(wc)
    eval(substitute(quote(!grepl(rx, x, ignore.case = TRUE))))
  },
  `%regexp%` = function(x, rx) {
    eval(substitute(quote(grepl(rx, x))))
  },
  `%iregexp%` = function(x, rx) {
    eval(substitute(quote(grepl(rx, x, ignore.case = TRUE))))
  },
  `%<=>%` = function(x, y) {
    # x is not distinct from y
    # is equivalent to
    # (x IS NULL AND y IS NULL ) OR (x IS NOT NULL AND y IS NOT NULL) AND (x = y)
    # or
    # if(x IS NULL OR y IS NULL, (x IS NULL) = (y IS NULL), x = y)
    # this translation uses the latter version
    eval(substitute(quote(
      #( (is.na(x) && is.na(y)) || (!is.na(x) && !is.na(y)) && (x == y) )
      ifelse(is.na(x) || is.na(y), is.na(x) == is.na(y), x == y)
    )))
  },
  `%<!=>%` = function(x, y) {
    # x is distinct from y
    # is equivalent to
    # if(x IS NULL OR y IS NULL, x IS NULL != y IS NULL, x != y)
    eval(substitute(quote(
      ifelse(is.na(x) || is.na(y), is.na(x) != is.na(y), x != y)
    )))
  },
  degrees = function(rad) {
    eval(substitute(quote(rad*180/pi)))
  },
  e = function(x) {
    eval(substitute(quote(exp(1))))
  },
  ln = function(x) {
    eval(substitute(quote(log(x, base = exp(1)))))
  },
  log = function(x, y) {
    eval(substitute(quote(log(x, base = y))))
  },
  pi = function() {
    eval(substitute(quote(pi)))
  },
  radians = function(deg) {
    eval(substitute(quote(deg*pi/180)))
  },
  rand = function(seed = NULL) {
    if(!is.null(seed)) {
      warning("Function rand() currently ignores the seed argument", call. = FALSE)
    }
    eval(substitute(quote(runif(1))))
  },
  regexp_replace = function(x, pattern, replacement) {
    eval(substitute(quote(gsub(pattern, replacement, x))))
  },
  trunc = function(x, d = 0) {
    if(!is_constant(eval(substitute(quote(d))))) {
      stop("The second argument to trunc() or truncate() ",
           "must be a constant value", call. = FALSE)
    }
    if (d != 0) {
      mult <- 10^as.integer(-d)
      eval(substitute(quote(trunc(x / mult) * mult)))
    } else {
      eval(substitute(quote(trunc(x, d))))
    }
  },
  concat_ws = function(sep, ...) {
    eval(substitute(quote(paste(..., sep = sep))))
  } # I was worried that the sep before the equals sign would get replaced too, but it did not
)

translations_indirect_base <- list(
  cast = function(x, y = NULL) {
    if(!is_constant(eval(substitute(quote(y))))) {
      stop("Invalid data type in CAST", call. = FALSE)
    }
    if (is.null(y)) stop("Unspecified data type in CAST", call. = FALSE)
    data_type <- data_type_translations_for_base[[gsub(" ?\\(.+", "", y)]]
    if (is.null(data_type)) stop("Unrecognized data type in CAST", call. = FALSE)
    func_name <- attr(data_type, "function")
    if (is.null(func_name)) {
      func_name <- paste0("as.", data_type)
    }
    pkg_name <- attr(data_type, "package")
    if (!is.null(pkg_name)) {
      func_name <- paste(pkg_name, func_name, sep = "::")
    }
    func <- str2lang(func_name)
    eval(substitute(quote(func(x))))
  },
  coalesce = function(...) {
    dots <- eval(substitute(alist(...)))
    assign("dots", dots, envir = .GlobalEnv)
    if (length(dots) < 1) {
      stop("At least one argument must be passed to coalesce()", call. = FALSE)
    }
    x <- dots[[1]]
    expr <- paste0("if (!is.na(", x, ")) ", x, " ")
    for (x in dots[-1]) {
      expr <- paste0(expr, "else if (!is.na(", x, ")) ", x, " ")
    }
    expr <- paste0(expr, "else NA")
    eval(substitute(str2lang(expr)))
  },
  nullif = function(x, y) {
    eval(substitute(quote(ifelse(is.na(x), x, y))))
  },
  lpad = function(str, len, pad) {
    if(!is_constant(eval(substitute(quote(len)))) ||
       !is_constant(eval(substitute(quote(pad))))) {
      stop("The second and third arguments to lpad() ",
           "must be constant values", call. = FALSE)
    }
    if (is.null(pad) || !as.character(pad) %in% c(" ", "0")) {
      stop(
        "Translation for function lpad() only supports ",
        "' '  or '0' as the padding character when tidyverse = FALSE",
        call. = FALSE
      )
    }
    format_string <- paste0("%", pad, len, "s")
    eval(substitute(quote(sprintf(format_string, str))))
  },
  rpad = function(str, len, pad) {
    if(!is_constant(eval(substitute(quote(len)))) ||
       !is_constant(eval(substitute(quote(pad))))) {
      stop("The second and third arguments to rpad() ",
           "must be constant values", call. = FALSE)
    }
    if (is.null(pad) || !as.character(pad) %in% c(" ")) {
      stop(
        "Translation for function rpad() only supports ",
        "' ' as the padding character when tidyverse = FALSE",
        call. = FALSE
      )
    }
    format_string <- paste0("%-", len, "s")
    eval(substitute(quote(sprintf(format_string, str))))
  },
  trim = function(x) {
    eval(substitute(quote(trimws(x))))
  },
  ltrim = function(x) {
    eval(substitute(quote(trimws(x, which = "left"))))
  },
  rtrim = function(x) {
    eval(substitute(quote(trimws(x, which = "right"))))
  },
  substr = function(x, start, len) {
    if(!is_constant(eval(substitute(quote(start)))) ||
       !is_constant(eval(substitute(quote(len))))) {
      stop("The second and third arguments to substr() or ",
           "substring() must be constant values", call. = FALSE)
    }
    if (start <= 0) {
      # interpret non-positive start as an offset from the end
      start_offset <- -start - 1L
      stop_offset <- -pmax(as.integer(len) - start_offset - 1L, start - 1L)
      eval(substitute(quote(substr(x, nchar(x) - start_offset, nchar(x) - stop_offset))))
    } else {
      stop <- pmax(as.integer(len) + start - 1L, 0L)
      eval(substitute(quote(substr(x, start, stop))))
    }
  }
)

translations_indirect_tidyverse <- list(
  cast = function(x, y = NULL) {
    if(!is_constant(eval(substitute(quote(y))))) {
      stop("Invalid data type in CAST", call. = FALSE)
    }
    if (is.null(y)) stop("Unspecified data type in CAST", call. = FALSE)
    data_type <- data_type_translations_for_tidyverse[[gsub(" ?\\(.+", "", y)]]
    if (is.null(data_type)) stop("Unrecognized data type in CAST", call. = FALSE)
    func_name <- attr(data_type, "function")
    if (is.null(func_name)) {
      func_name <- paste0("as.", data_type)
    }
    pkg_name <- attr(data_type, "package")
    if (!is.null(pkg_name)) {
      func_name <- paste(pkg_name, func_name, sep = "::")
    }
    func <- str2lang(func_name)
    eval(substitute(quote(func(x))))
  },
  lpad = function(str, len, pad) {
    eval(substitute(quote(stringr::str_pad(str, len, side = "left", pad = pad))))
  },
  rpad = function(str, len, pad) {
    eval(substitute(quote(stringr::str_pad(str, len, side = "right", pad = pad))))
  },
  trim = function(x) {
    eval(substitute(quote(stringr::str_trim(x))))
  },
  ltrim = function(x) {
    eval(substitute(quote(stringr::str_trim(x, side = "left"))))
  },
  rtrim = function(x) {
    eval(substitute(quote(stringr::str_trim(x, side = "right"))))
  },
  substr = function(x, start, len) {
    if(!is_constant(eval(substitute(quote(start)))) ||
       !is_constant(eval(substitute(quote(len))))) {
      stop("The second and third arguments to substr() or ",
           "substring() must be constant values", call. = FALSE)
    }
    if (start <= 0) {
      # interpret non-positive start as an offset from the end
      start_offset <- -start - 1L
      stop_offset <- -pmax(as.integer(len) - start_offset - 1L, start - 1L)
      eval(substitute(quote(stringr::str_sub(x, nchar(x) - start_offset, nchar(x) - stop_offset))))
    } else {
      stop <- pmax(as.integer(len) + start - 1L, 0L)
      eval(substitute(quote(stringr::str_sub(x, start, stop))))
    }
  }
)

translations_indirect_generic_agg <- list(

  # when adding functions here, also add their tranlated
  # R function names to the list r_aggregate_functions below

  avg = function(x) {
    eval(substitute(quote(mean(x, na.rm = TRUE))))
  },
  count = function(x) {
    eval(substitute(quote(sum(!is.na(x)))))
  },
  group_concat = function(x, sep = ", ") {
    eval(substitute(quote(paste0(x, collapse = sep))))
  },
  max = function(x) {
    eval(substitute(quote(max(x, na.rm = TRUE))))
  },
  median = function(x) {
    eval(substitute(quote(median(x, na.rm = TRUE))))
  },
  min = function(x) {
    eval(substitute(quote(min(x, na.rm = TRUE))))
  },
  std = function(x) {
    eval(substitute(quote(sd(x, na.rm = TRUE))))
  },
  stddev = function(x) {
    eval(substitute(quote(sd(x, na.rm = TRUE))))
  },
  sum = function(x) {
    eval(substitute(quote(sum(x, na.rm = TRUE))))
  },
  percentile = function(x, p) {
    eval(substitute(quote(quantile(x, p, na.rm = TRUE))))
  },
  variance = function(x) {
    eval(substitute(quote(var(x, na.rm = TRUE))))
  }
)

translations_indirect_base_agg <- list(

  # when adding functions here, also add their tranlated
  # R function names to the list r_aggregate_functions below

  count_star = function() {
    eval(substitute(quote(nrow(.))))
  }
  # count_distinct for base R is translated elsewhere

  # for count all, we translate to nrow(.) for count(*), otherwise length(!is.na(x))
  # for count distinct, we use length(unique(x)) if only one column,
  #   but it's unclear how best to handle the multiple columns case
  #   e.g. length(unique(mtcars[c("gear", "carb"),])) is 11
  #     ( that's the right answer, and what mtcars %>% summarise(n_distinct(gear, carb)) returns )
  #   but these give the wrong ansers:
  #      mtcars %>% summarise(length(unique(gear, carb))) is 28
  #      mtcars %>% summarise(length(unique(c(gear, carb)))) is 7
  #   the safest thing would probably be to keep it limited to the one-variable case

)

translations_indirect_tidyverse_agg <- list(

  # when adding functions here, also add their tranlated
  # R function names to the list r_aggregate_functions below

  count_star = function() {
    eval(substitute(quote(dplyr::n())))
  },
  count_distinct = function(...) {
    eval(substitute(quote(dplyr::n_distinct(..., na.rm = TRUE))))
  }
)

r_aggregate_functions <- c(
  "mean",
  "count",
  "max",
  "median",
  "min",
  "std",
  "stddev",
  "sum",
  "percentile",
  "variance",
  "nrow",
  "dplyr::n",
  "dplyr::n_distinct"
)
# paste() with !is.null(collapse) is also an aggregate function
