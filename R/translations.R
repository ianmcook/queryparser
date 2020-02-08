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

#' @include common.R
NULL

translations_data_types_generic <- list(

  # when adding data types here, if their SQL forms take arguments,
  # then also add them to sql_data_types_with_args in common.R

  `string` = "character",
  `char` = "character", # takes arguments
  `varchar` = "character", # takes arguments
  `boolean` = "logical",
  `int` = "integer",
  `integer` = "integer",
  `bigint` = "integer",
  `smallint` = "integer",
  `tinyint` = "integer",
  `double` = "double",
  `real` = "double",
  `float` = "single",
  `decimal` = "numeric" # takes arguments
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
  `and` = "&",
  `or` = "|",
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
  null = quote(NA),

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
  std = quote(stddev), # stddev is translated below
  truncate = quote(trunc), # trunc is translated below

  # trigonometric functions
  acos = quote(acos),
  asin = quote(asin),
  atan = quote(atan),
  atan2 = quote(atan2),
  cos = quote(cos),
  cosh = quote(cosh),
  sin = quote(sin),
  sinh = quote(sinh),
  tan = quote(tan),
  tanh = quote(tanh),

  # string functions
  substring = quote(substr), # substr is translated below
  to_hex = quote(as.hexmode),

  # logic functions
  iif = quote(ifelse)
)

translations_direct_base <- list(

  # string functions
  char_length = quote(nchar),
  character_length = quote(nchar),
  concat = quote(paste0),
  len = quote(nchar),
  length = quote(nchar),
  # consider whether to translate length(x) to nchar(x, type = "bytes")
  # which would be consistent with MySQL but not with PostgreSQL
  lcase = quote(tolower),
  lower = quote(tolower),
  replicate = quote(strrep),
  ucase = quote(toupper),
  upper = quote(toupper),
  to_date = quote(as.Date),

  # bitwise functions
  bitnot = quote(bitwNot),
  bitand = quote(bitwAnd),
  bitor = quote(bitwOr),
  bitxor = quote(bitwXor),
  shiftleft = quote(bitwShiftL),
  shiftright = quote(bitwShiftR)
)

translations_direct_tidyverse <- list(

  # string functions
  initcap = str2lang("stringr::str_to_title"),
  char_length = str2lang("stringr::str_length"),
  character_length = str2lang("stringr::str_length"),
  concat = str2lang("stringr::str_c"),
  len = str2lang("stringr::str_length"),
  length = str2lang("stringr::str_length"),
  lower = str2lang("stringr::str_to_lower"),
  replicate = str2lang("stringr::str_dup"),
  reverse = str2lang("stringi::stri_reverse"),
  upper = str2lang("stringr::str_to_upper"),
  to_date = str2lang("lubridate::as_date"),
  trim = str2lang("stringr::str_trim"),

  # conditional functions
  coalesce = str2lang("dplyr::coalesce"),
  nullif = str2lang("dplyr::na_if"),

  # date and time
  year = str2lang("lubridate::year"),
  month = str2lang("lubridate::month"),
  weekofyear = str2lang("lubridate::isoweek"),
  dayofyear = str2lang("lubridate::yday"),
  day = str2lang("lubridate::mday"),
  dayofweek = str2lang("lubridate::wday"),
  hour = str2lang("lubridate::hour"),
  minute = str2lang("lubridate::minute"),
  now = str2lang("lubridate::now"),

  # logic
  choose = str2lang("dplyr::recode")
)

# the return value of these indirect expressions must be in the form:
#   eval(substitute(quote(  expression  )))
# the body of each function can process scalar arguments, but all
# operations on the data in columns must happen in the returned expression

translations_indirect_generic <- list(
  `%like%` = function(x, wc) {
    if (!is_constant(eval(substitute(quote(wc))))) {
      stop("The operand on the right side of LIKE must be a constant value", call. = FALSE)
    }
    rx <- translate_wildcard_to_regex(wc)
    eval(substitute(quote(grepl(rx, x))))
  },
  `%nlike%` = function(x, wc) {
    if (!is_constant(eval(substitute(quote(wc))))) {
      stop("The operand on the right side of NLIKE must be a constant value", call. = FALSE)
    }
    rx <- translate_wildcard_to_regex(wc)
    eval(substitute(quote(!grepl(rx, x))))
  },
  `%ilike%` = function(x, wc) {
    if (!is_constant(eval(substitute(quote(wc))))) {
      stop("The operand on the right side of ILIKE must be a constant value", call. = FALSE)
    }
    rx <- translate_wildcard_to_regex(wc)
    eval(substitute(quote(grepl(rx, x, ignore.case = TRUE))))
  },
  `%nilike%` = function(x, wc) {
    if (!is_constant(eval(substitute(quote(wc))))) {
      stop("The operand on the right side of NILIKE must be a constant value", call. = FALSE)
    }
    rx <- translate_wildcard_to_regex(wc)
    eval(substitute(quote(!grepl(rx, x, ignore.case = TRUE))))
  },
  `%regexp%` = function(x, rx) {
    if (!is_constant(eval(substitute(quote(wc))))) {
      stop("The operand on the right side of REGEXP must be a constant value", call. = FALSE)
    }
    eval(substitute(quote(grepl(rx, x))))
  },
  `%iregexp%` = function(x, rx) {
    if (!is_constant(eval(substitute(quote(wc))))) {
      stop("The operand on the right side of IREGEXP must be a constant value", call. = FALSE)
    }
    eval(substitute(quote(grepl(rx, x, ignore.case = TRUE))))
  },
  `%<=>%` = function(x, y) {
    # x is not distinct from y
    # is equivalent to
    # if (x IS NULL OR y IS NULL, x IS NULL AND y IS NULL, x = y)
    eval(substitute(quote(
      ifelse(is.na(x) | is.na(y), is.na(x) & is.na(y), x == y)
    )))
  },
  `%<!=>%` = function(x, y) {
    # x is distinct from y
    # is equivalent to
    # if (x IS NULL OR y IS NULL, x IS NULL XOR y IS NULL, x != y)
    eval(substitute(quote(
      ifelse(is.na(x) | is.na(y), xor(is.na(x), is.na(y)), x != y)
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
    if (!is.null(seed)) {
      warning("Function rand() currently ignores the seed argument", call. = FALSE)
    }
    eval(substitute(quote(runif(1))))
  },
  regexp_replace = function(x, pattern, replacement) {
    eval(substitute(quote(gsub(pattern, replacement, x))))
  },
  trunc = function(x, d = 0) {
    if (!is_constant(eval(substitute(quote(d))))) {
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
  ifnull = function(x, y) {
    eval(substitute(quote(ifelse(is.na(x), y, x))))
  },
  isnull = function(x, y) {
    eval(substitute(quote(ifelse(is.na(x), y, x))))
  },
  nvl = function(x, y) {
    eval(substitute(quote(ifelse(is.na(x), y, x))))
  }
)

translations_indirect_base <- list(
  yesbetween = function(x, left, right) {
    if (missing(x) || missing(left) || missing(right)) {
      stop("BETWEEN requires three operands", call. = FALSE)
    }
    eval(substitute(quote((x >= left & x <= right))))
  },
  notbetween = function(x, left, right) {
    if (missing(x) || missing(left) || missing(right)) {
      stop("NOT BETWEEN requires three operands", call. = FALSE)
    }
    eval(substitute(quote((x < left | x > right))))
  },
  cast = function(x, y = NULL) {
    y <- eval(substitute(quote(y)))
    if (is.call(y) && !is_constant(y)) {
      stop("Invalid data type in CAST", call. = FALSE)
    }
    if (is.null(y)) stop("Unspecified data type in CAST", call. = FALSE)
    if (is.call(y)) {
      data_type <- data_type_translations_for_base[[tolower(deparse(y[[1]]))]]
    } else {
      data_type <- data_type_translations_for_base[[tolower(deparse(y))]]
    }
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
  try_cast = function(x, y = NULL) {
    y <- eval(substitute(quote(y)))
    if (is.call(y) && !is_constant(y)) {
      stop("Invalid data type in TRY_CAST", call. = FALSE)
    }
    if (is.null(y)) stop("Unspecified data type in TRY_CAST", call. = FALSE)
    if (is.call(y)) {
      data_type <- data_type_translations_for_base[[tolower(deparse(y[[1]]))]]
    } else {
      data_type <- data_type_translations_for_base[[tolower(deparse(y))]]
    }
    if (is.null(data_type)) stop("Unrecognized data type in TRY_CAST", call. = FALSE)
    func_name <- attr(data_type, "function")
    if (is.null(func_name)) {
      func_name <- paste0("as.", data_type)
    }
    pkg_name <- attr(data_type, "package")
    if (!is.null(pkg_name)) {
      func_name <- paste(pkg_name, func_name, sep = "::")
    }
    func <- str2lang(func_name)
    eval(substitute(quote(suppressWarnings(func(x)))))
  },
  convert = function(y = NULL, x) {
    y <- eval(substitute(quote(y)))
    if (is.call(y) && !is_constant(y)) {
      stop("Invalid data type in CONVERT", call. = FALSE)
    }
    if (is.null(y)) stop("Unspecified data type in CONVERT", call. = FALSE)
    if (is.call(y)) {
      data_type <- data_type_translations_for_base[[tolower(deparse(y[[1]]))]]
    } else {
      data_type <- data_type_translations_for_base[[tolower(deparse(y))]]
    }
    if (is.null(data_type)) stop("Unrecognized data type in CONVERT", call. = FALSE)
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
  try_convert = function(y = NULL, x) {
    y <- eval(substitute(quote(y)))
    if (is.call(y) && !is_constant(y)) {
      stop("Invalid data type in TRY_CONVERT", call. = FALSE)
    }
    if (is.null(y)) stop("Unspecified data type in TRY_CONVERT", call. = FALSE)
    if (is.call(y)) {
      data_type <- data_type_translations_for_base[[tolower(deparse(y[[1]]))]]
    } else {
      data_type <- data_type_translations_for_base[[tolower(deparse(y))]]
    }
    if (is.null(data_type)) stop("Unrecognized data type in TRY_CONVERT", call. = FALSE)
    func_name <- attr(data_type, "function")
    if (is.null(func_name)) {
      func_name <- paste0("as.", data_type)
    }
    pkg_name <- attr(data_type, "package")
    if (!is.null(pkg_name)) {
      func_name <- paste(pkg_name, func_name, sep = "::")
    }
    func <- str2lang(func_name)
    eval(substitute(quote(suppressWarnings(func(x)))))
  },
  casewhen = function(... , otherwise) {
    dots <- eval(substitute(alist(...)))
    otherwise <- eval(substitute(quote(otherwise)))
    expr <- ""
    i <- 1L
    while(i + 1 <= length(dots)) {
      expr <- paste0(
        expr,
        "ifelse(",
        deparse(dots[[i]]),
        ", ",
        deparse(dots[[i + 1]]),
        ", "
      )
      i <- i + 2L
    }
    if (missing(otherwise)) {
      expr <- paste0(expr, "NA", paste0(rep(")", length(dots) %/% 2), collapse = ""))
    } else {
      expr <- paste0(expr, deparse(otherwise), paste0(rep(")", length(dots) %/% 2), collapse = ""))
    }
    eval(substitute(str2lang(expr)))
  },
  casevalue = function(value, ... , otherwise) {
    value <- eval(substitute(quote(value)))
    dots <- eval(substitute(alist(...)))
    otherwise <- eval(substitute(quote(otherwise)))
    expr <- ""
    i <- 1L
    while(i + 1 <= length(dots)) {
      expr <- paste0(
        expr,
        "ifelse(",
        deparse(value),
        " == ",
        deparse(dots[[i]]),
        ", ",
        deparse(dots[[i + 1]]),
        ", "
      )
      i <- i + 2L
    }
    if (missing(otherwise)) {
      expr <- paste0(expr, "NA", paste0(rep(")", length(dots) %/% 2), collapse = ""))
    } else {
      expr <- paste0(expr, deparse(otherwise), paste0(rep(")", length(dots) %/% 2), collapse = ""))
    }
    eval(substitute(str2lang(expr)))
  },
  coalesce = function(...) {
    dots <- eval(substitute(alist(...)))
    if (length(dots) < 1) {
      stop("At least one argument must be passed to coalesce()", call. = FALSE)
    }
    expr <- ""
    for (x in dots) {
      expr <- paste0(expr, "ifelse(!is.na(", deparse(x), "), ", deparse(x), ", ")
    }
    expr <- paste0(expr, "NA", paste0(rep(")", length(dots)), collapse = ""))
    eval(substitute(str2lang(expr)))
  },
  concat_ws = function(sep, ...) {
    eval(substitute(quote(paste(..., sep = sep))))
  },
  nullif = function(x, y) {
    eval(substitute(quote(ifelse(x==y, NA, x))))
  },
  lpad = function(str, len, pad) {
    if (!is_constant(eval(substitute(quote(len)))) ||
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
    if (!is_constant(eval(substitute(quote(len)))) ||
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
    if (!is_constant(eval(substitute(quote(start)))) ||
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
  },
  charindex = function(string, substring) {
    warning("Using CHARINDEX with non-ASCII characters may return incorrect results due to multiple ways to represent the same character", call. = FALSE)
    eval(substitute(quote(regexpr(substring, string, fixed = TRUE)[1])))
  },
  reverse = function(x) {
    eval(substitute(quote(sapply(lapply(strsplit(x, ""), rev), paste, collapse = ""))))
  },
  replace = function(string, substring, replacement) {
    warning("Using REPLACE with non-ASCII characters may return incorrect results due to multiple ways to represent the same character", call. = FALSE)
    eval(substitute(quote(gsub(substring, replacement, string, fixed = TRUE))))
  }
)

translations_indirect_tidyverse <- list(
  yesbetween = function(x, left, right) {
    if (missing(x) || missing(left) || missing(right)) {
      stop("BETWEEN requires three operands", call. = FALSE)
    }
    fun <- str2lang("dplyr::between")
    eval(substitute(quote(fun(x, left, right))))
  },
  notbetween = function(x, left, right) {
    if (missing(x) || missing(left) || missing(right)) {
      stop("NOT BETWEEN requires three operands", call. = FALSE)
    }
    fun <- str2lang("dplyr::between")
    eval(substitute(quote(!fun(x, left, right))))
  },
  cast = function(x, y = NULL) {
    y <- eval(substitute(quote(y)))
    if (is.call(y) && !is_constant(y)) {
      stop("Invalid data type in CAST", call. = FALSE)
    }
    if (is.null(y)) stop("Unspecified data type in CAST", call. = FALSE)
    if (is.call(y)) {
      data_type <- data_type_translations_for_tidyverse[[tolower(deparse(y[[1]]))]]
    } else {
      data_type <- data_type_translations_for_tidyverse[[tolower(deparse(y))]]
    }
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
  convert = function(y = NULL, x) {
    y <- eval(substitute(quote(y)))
    if (is.call(y) && !is_constant(y)) {
      stop("Invalid data type in CONVERT", call. = FALSE)
    }
    if (is.null(y)) stop("Unspecified data type in CONVERT", call. = FALSE)
    if (is.call(y)) {
      data_type <- data_type_translations_for_tidyverse[[tolower(deparse(y[[1]]))]]
    } else {
      data_type <- data_type_translations_for_tidyverse[[tolower(deparse(y))]]
    }
    if (is.null(data_type)) stop("Unrecognized data type in CONVERT", call. = FALSE)
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
  casewhen = function(... , otherwise) {
    dots <- eval(substitute(alist(...)))
    otherwise <- eval(substitute(quote(otherwise)))
    expr <- "dplyr::case_when("
    i <- 1L
    while(i + 1 <= length(dots)) {
      expr <- paste0(
        expr,
        deparse(dots[[i]]),
        " ~ ",
        deparse(dots[[i + 1]])
      )
      if (i + 1 < length(dots)) {
        expr <- paste0(expr, ", ")
      }
      i <- i + 2L
    }
    if (!missing(otherwise)) {
      expr <- paste0(expr, ", TRUE ~ ", deparse(otherwise))
    }
    expr <- paste0(expr, ")")
    eval(substitute(str2lang(expr)))
  },
  casevalue = function(value, ... , otherwise) {
    value <- eval(substitute(quote(value)))
    dots <- eval(substitute(alist(...)))
    otherwise <- eval(substitute(quote(otherwise)))
    expr <- "dplyr::case_when("
    i <- 1L
    while(i + 1 <= length(dots)) {
      expr <- paste0(
        expr,
        deparse(value),
        " == ",
        deparse(dots[[i]]),
        " ~ ",
        deparse(dots[[i + 1]])
      )
      if (i + 1 < length(dots)) {
        expr <- paste0(expr, ", ")
      }
      i <- i + 2L
    }
    if (!missing(otherwise)) {
      expr <- paste0(expr, ", TRUE ~ ", deparse(otherwise))
    }
    expr <- paste0(expr, ")")
    eval(substitute(str2lang(expr)))
  },
  concat_ws = function(sep, ...) {
    fun <- str2lang("stringr::str_c")
    eval(substitute(quote(fun(..., sep = sep))))
  },
  lpad = function(str, len, pad) {
    fun <- str2lang("stringr::str_pad")
    eval(substitute(quote(fun(str, len, side = "left", pad = pad))))
  },
  rpad = function(str, len, pad) {
    fun <- str2lang("stringr::str_pad")
    eval(substitute(quote(fun(str, len, side = "right", pad = pad))))
  },
  ltrim = function(x) {
    fun <- str2lang("stringr::str_trim")
    eval(substitute(quote(fun(x, side = "left"))))
  },
  rtrim = function(x) {
    fun <- str2lang("stringr::str_trim")
    eval(substitute(quote(fun(x, side = "right"))))
  },
  substr = function(x, start, len) {
    if (!is_constant(eval(substitute(quote(start)))) ||
       !is_constant(eval(substitute(quote(len))))) {
      stop("The second and third arguments to substr() or ",
           "substring() must be constant values", call. = FALSE)
    }
    if (start <= 0) {
      # interpret non-positive start as an offset from the end
      start_offset <- -start - 1L
      stop_offset <- -pmax(as.integer(len) - start_offset - 1L, start - 1L)
      fun <- str2lang("stringr::str_sub")
      eval(substitute(quote(fun(x, nchar(x) - start_offset, nchar(x) - stop_offset))))
    } else {
      stop <- pmax(as.integer(len) + start - 1L, 0L)
      fun <- str2lang("stringr::str_sub")
      eval(substitute(quote(fun(x, start, stop))))
    }
  },
  charindex = function(string, substring) {
    fun <- str2lang("stringr::str_locate")
    fun2 <- str2lang("stringr::coll")
    eval(substitute(quote(fun(string, fun2(substring))[1])))
  },
  replace = function(string, substring, replacement) {
    fun <- str2lang("stringr::str_replace")
    fun2 <- str2lang("stringr::coll")
    eval(substitute(quote(fun(string, fun2(substring), replacement))))
  },
  dayname = function(x) {
    fun <- str2lang("lubridate::wday")
    eval(substitute(quote(fun(x, label = TRUE))))
  },
  date_trunc = function(unit, x) {
    fun <- str2lang("lubridate::floor_date")
    eval(substitute(quote(fun(x, unit))))
  },
  second = function(x) {
    fun <- str2lang("lubridate::second")
    eval(substitute(quote(floor(fun(x)))))
  }
)

translations_indirect_generic_agg <- list(

  # when adding functions here, also add their tranlated
  # R function names to r_aggregate_functions below

  avg = function(x) {
    if (nargs() != 1) {
      stop("Function AVG() requires one parameter", call. = FALSE)
    }
    eval(substitute(quote(mean(x, na.rm = TRUE))))
  },
  count = function(x) {
    if (nargs() != 1) {
      stop("Function COUNT() requires one parameter", call. = FALSE)
    }
    eval(substitute(quote(sum(!is.na(x)))))
  },
  max = function(x) {
    if (nargs() != 1) {
      stop("Function MAX() requires one parameter", call. = FALSE)
    }
    eval(substitute(quote(max(x, na.rm = TRUE))))
  },
  median = function(x) {
    if (nargs() != 1) {
      stop("Function MEDIAN() requires one parameter", call. = FALSE)
    }
    eval(substitute(quote(median(x, na.rm = TRUE))))
  },
  min = function(x) {
    if (nargs() != 1) {
      stop("Function MIN() requires one parameter", call. = FALSE)
    }
    eval(substitute(quote(min(x, na.rm = TRUE))))
  },
  stddev = function(x) {
    if (nargs() != 1) {
      stop("Function SD() or STDDEV() requires one parameter", call. = FALSE)
    }
    eval(substitute(quote(sd(x, na.rm = TRUE))))
  },
  sum = function(x) {
    if (nargs() != 1) {
      stop("Function SUM() requires one parameter", call. = FALSE)
    }
    eval(substitute(quote(sum(x, na.rm = TRUE))))
  },
  percentile = function(x, p) {
    if (nargs() != 2) {
      stop("Function PERCENTILE() requires two parameters", call. = FALSE)
    }
    eval(substitute(quote(quantile(x, p, na.rm = TRUE))))
  },
  variance = function(x) {
    if (nargs() != 1) {
      stop("Function VARIANCE() requires one parameter", call. = FALSE)
    }
    eval(substitute(quote(var(x, na.rm = TRUE))))
  }
)

translations_indirect_base_agg <- list(

  # when adding functions here, also add their tranlated
  # R function names to r_aggregate_functions below

  count_star = function() {
    eval(substitute(quote(nrow(.))))
  },
  # count_distinct for base R is translated elsewhere
  group_concat = function(x, sep = ", ") {
    if (!nargs() %in% c(1,2)) {
      stop("Function GROUP_CONCAT() requires one or two parameters", call. = FALSE)
    }
    eval(substitute(quote(paste0(x, collapse = sep))))
  },
  string_agg = function(x, sep) {
    if (!nargs() == 2) {
      stop("Function STRING_AGG() requires two parameters", call. = FALSE)
    }
    eval(substitute(quote(paste0(x, collapse = sep))))
  }
)

translations_indirect_tidyverse_agg <- list(

  # when adding functions here, also add their tranlated
  # R function names to r_aggregate_functions below

  count_star = function() {
    fun <- str2lang("dplyr::n")
    eval(substitute(quote(fun())))
  },
  count_distinct = function(...) {
    if (nargs() < 1) {
      stop("Function COUNT(DISTINCT ) requires at least one parameter", call. = FALSE)
    }
    fun <- str2lang("dplyr::n_distinct")
    eval(substitute(quote(fun(..., na.rm = TRUE))))
  },
  group_concat = function(x, sep = ", ") {
    if (!nargs() %in% c(1,2)) {
      stop("Function GROUP_CONCAT() requires one or two parameters", call. = FALSE)
    }
    fun <- str2lang("stringr::str_flatten")
    eval(substitute(quote(fun(x, collapse = sep))))
  },
  string_agg = function(x, sep) {
    if (!nargs() == 2) {
      stop("Function STRING_AGG() requires two parameters", call. = FALSE)
    }
    fun <- str2lang("stringr::str_flatten")
    eval(substitute(quote(fun(x, collapse = sep))))
  }
)

r_aggregate_functions <- c(
  "max",
  "mean",
  "median",
  "min",
  "nrow",
  "sd",
  "sum",
  "quantile",
  "var",
  "dplyr::n",
  "dplyr::n_distinct",
  "stringr::str_flatten"
)
# paste() and paste0() can also be aggregate functions
# but only when !is.null(collapse)
