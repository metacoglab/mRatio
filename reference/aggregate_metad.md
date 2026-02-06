# Aggregate `data` by `response`, `confidence`, and other columns

Counts number of rows in `data` with unique combinations values in the
columns `response`, `confidence`, and any other columns in `...`.

## Usage

``` r
aggregate_metad(data, ..., .response = "N", K = NULL)
```

## Arguments

- data:

  The dataframe to aggregate

- ...:

  Grouping columns in `data`. These columns will be converted to
  factors.

- .response:

  The name of the resulting column containing trial counts

- K:

  The number of confidence levels in `data`. If `NULL`, this is
  estimated from `data`.

## Value

A tibble with one row per combination of the variables in `...`, and
another column named by the value of `.response` containing trial
counts. For `K` confidence levels, this will be an `N x K*4` matrix,
such that the columns represent:

    [N(stimulus=0, response=0, confidence=K), ..., N(stimulus=0, response=0, confidence=1),
     N(stimulus=0, response=1, confidence=1), ..., N(stimulus=0, response=1, confidence=K),
     N(stimulus=1, response=0, confidence=K), ..., N(stimulus=1, response=0, confidence=1),
     N(stimulus=1, response=1, confidence=1), ..., N(stimulus=1, response=1, confidence=K)]

## Examples

``` r
# aggregate a dataset without grouping factors
d <- sim_metad()
aggregate_metad(d)
#> # A tibble: 1 × 3
#>     N_0   N_1 N[,"N_0_1"] [,"N_0_2"] [,"N_0_3"] [,"N_0_4"] [,"N_0_5"] [,"N_0_6"]
#>   <int> <int>       <int>      <int>      <int>      <int>      <int>      <int>
#> 1    50    50           8          5          4         12         10          7
#> # ℹ 1 more variable: N[7:16] <int>

# aggregate a dataset with grouping factors
d2 <- sim_metad_condition()
aggregate_metad(d2, condition)
#> # A tibble: 2 × 4
#>   condition   N_0   N_1 N[,"N_0_1"] [,"N_0_2"] [,"N_0_3"] [,"N_0_4"] [,"N_0_5"]
#>   <fct>     <int> <int>       <int>      <int>      <int>      <int>      <int>
#> 1 1            50    50           7          4         10         16          5
#> 2 2            50    50           3          6         10         13          9
#> # ℹ 1 more variable: N[6:16] <int>

# can also aggregate ignoring grouping factors
aggregate_metad(d2)
#> # A tibble: 1 × 3
#>     N_0   N_1 N[,"N_0_1"] [,"N_0_2"] [,"N_0_3"] [,"N_0_4"] [,"N_0_5"] [,"N_0_6"]
#>   <int> <int>       <int>      <int>      <int>      <int>      <int>      <int>
#> 1   100   100          10         10         20         29         14          9
#> # ℹ 1 more variable: N[7:16] <int>
```
