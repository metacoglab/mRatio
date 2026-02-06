# Normal cumulative distribution functions

Normal cumulative distribution functions

## Usage

``` r
normal_lcdf(x, mu)

normal_lccdf(x, mu)
```

## Arguments

- x:

  The quantile to evaluate the l(c)cdf at

- mu:

  The mean of the normal distribution

## Value

\\log(P(X \< x))\\ (for `normal_lcdf`) or \\log(P(X \> x))\\ (for
`normal_lccdf`) where \\X\\ is sampled from a normal distribution with
mean `mu` and standard deviation of \\1\\

## Examples

``` r
normal_lcdf(0, mu = 1)
#> [1] -1.841022
normal_lccdf(0, mu = 1)
#> [1] -0.1727538
```
