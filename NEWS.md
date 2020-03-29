# queryparser (development version)

* `ORDER BY` clause can include `NULLS FIRST` and `NULLS LAST` ([#12](https://github.com/ianmcook/queryparser/issues/12), [\@StevenHibble](https://github.com/StevenHibble))
* Translations of expressions in the `ORDER BY` clause now use `-xtfrm()` instead of the attribute `decreasing` to indicate descending order ([\@StevenHibble](https://github.com/StevenHibble))
* Parentheses can enclose table names and joins in the `FROM` clause ([#23](https://github.com/ianmcook/queryparser/issues/23))
* More Microsoft SQL Server functions now translate ([#26](https://github.com/ianmcook/queryparser/issues/26), [\@StevenHibble](https://github.com/StevenHibble))
* Minor bugfixes and improvements

# queryparser 0.2.0

* `BETWEEN` expressions with quotes in operands translate correctly ([#13](https://github.com/ianmcook/queryparser/issues/13))
* Line comments (`--`) and block comments (`/* */`) are removed from queries ([#14](https://github.com/ianmcook/queryparser/issues/14))
* `coalesce()` translates correctly when `tidyverse = FALSE` ([#15](https://github.com/ianmcook/queryparser/issues/15), [#17](https://github.com/ianmcook/queryparser/issues/17))
* `CASE` expressions translate ([#16](https://github.com/ianmcook/queryparser/issues/16), [#18](https://github.com/ianmcook/queryparser/issues/18))
* Table aliases can be used in queries ([#19](https://github.com/ianmcook/queryparser/issues/19))
* SQL-92-style (explicit) join queries translate ([#20](https://github.com/ianmcook/queryparser/issues/20))
* Continuous integration and coverage tests
* Minor bugfixes and improvements

# queryparser 0.1.1

* Output indicates when queries aggregate
  * List returned by `parse_query()` has attribute `aggregate` set to `TRUE` if query aggregates ([#8](https://github.com/ianmcook/queryparser/issues/8))
  * When translating an aggregate query, sublist `select` returned by by `parse_query()` has logical vector attribute `aggregate` indicating whether each expression in the `SELECT` list aggregates ([#9](https://github.com/ianmcook/queryparser/issues/9))
  * When translating an aggregate query with an `ORDER BY` clause, sublist `order_by` returned by by `parse_query()` has logical vector attribute `aggregate` indicating whether each expression in the `ORDER BY` clause aggregates ([#11](https://github.com/ianmcook/queryparser/issues/11))
* Translation of multiple `CAST` and `BETWEEN` expressions in a single expression no longer fails  ([#10](https://github.com/ianmcook/queryparser/issues/10))
* Minor bugfixes and improvements

# queryparser 0.1.0

* First CRAN release
* Column names that match SQL function names are not converted to lowercase ([#1](https://github.com/ianmcook/queryparser/issues/1))
* Outputted expressions use vectorized logical operators ([#2](https://github.com/ianmcook/queryparser/issues/2))
* Successive occurrences of binary symbolic operators are replaced ([#3](https://github.com/ianmcook/queryparser/issues/3))
* Column names that match function names are not replaced ([#4](https://github.com/ianmcook/queryparser/issues/4))
* Vulnerabilities that permitted use of disallowed functions are resolved ([#5](https://github.com/ianmcook/queryparser/issues/5), [#7](https://github.com/ianmcook/queryparser/issues/7))
* Column names can contain periods ([#6](https://github.com/ianmcook/queryparser/issues/6))
