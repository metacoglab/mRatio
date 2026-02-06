# Sample from a matrix-normal distribution

Sample from a matrix-normal distribution

## Usage

``` r
rmatrixnorm(mu, L_sigma_rows, L_sigma_cols)
```

## Arguments

- mu:

  a matrix of means

- L_sigma_rows:

  the Cholesky-decomposed covariance matrix for the rows

- L_sigma_cols:

  the Cholesky-decomposed covariance matrix for the columns

## Value

A single sample from a matrix-normal distribution with mean `mu` (a
matrix), row-wise covariances `sigma_rows`, and column-wise covariances
`sigma_cols`, where `L_sigma_rows` and `L_sigma_cols` are the
Cholesky-decomposed covariance matrices

## Examples

``` r
mu <- matrix(rep(0, 8), nrow = 4)
sd_rows <- rep(1, 4)
sd_cols <- rep(1, 2)
r_rows <- corr_matrix(.25, 4)
r_cols <- corr_matrix(.75, 2)
L_sigma_rows <- chol(cov_matrix(sd_rows, r_rows))
L_sigma_cols <- chol(cov_matrix(sd_cols, r_cols))
rmatrixnorm(mu, L_sigma_rows, L_sigma_cols)
#>             [,1]       [,2]
#> [1,]  1.38449542  0.5960231
#> [2,]  0.06199199 -0.1553442
#> [3,] -0.24818627 -0.6175485
#> [4,]  1.42159078  0.6059229
```
