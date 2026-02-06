# Generate a covariance matrix.

Generate a covariance matrix.

## Usage

``` r
cov_matrix(S, OMEGA)
```

## Arguments

- S:

  A vector of standard deviations

- OMEGA:

  A correlation matrix

## Value

a `[N x N]` covariance matrix, where `N = length(S)`.

## Examples

``` r
sds <- c(1, 2)
corrs <- matrix(c(1, .5, .5, 1), nrow = 2)
cov_matrix(sds, corrs)
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    1    4
```
