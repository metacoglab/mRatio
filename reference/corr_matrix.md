# Generate a correlation matrix with all off-diagonal values equal to `r`

Generate a correlation matrix with all off-diagonal values equal to `r`

## Usage

``` r
corr_matrix(r, nrow = 2)
```

## Arguments

- r:

  The correlation to fill in the matrix off-diagonals

- nrow:

  The number of rows (and columns) of the resulting matrix

## Value

An `[nrow x nrow]` matrix with values along the diagonal equal to `1`
and values off of the diagonal equal to `r`

## Examples

``` r
corr_matrix(0, nrow = 3)
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    0    1    0
#> [3,]    0    0    1

corr_matrix(-.5, nrow = 4)
#>      [,1] [,2] [,3] [,4]
#> [1,]  1.0 -0.5 -0.5 -0.5
#> [2,] -0.5  1.0 -0.5 -0.5
#> [3,] -0.5 -0.5  1.0 -0.5
#> [4,] -0.5 -0.5 -0.5  1.0
```
