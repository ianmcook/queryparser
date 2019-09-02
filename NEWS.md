# queryparser (development version)

* List returned by `parse_query()` has attribute `aggregate` set to `TRUE` if query aggregates ([#8](https://github.com/ianmcook/queryparser/issues/8))
* When translating an aggregate query, sublist `select` returned by by `parse_query()` has logical vector attribute `aggregate` indicating whether each expression aggregates ([#9](https://github.com/ianmcook/queryparser/issues/9))
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
