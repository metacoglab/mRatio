# Convert binary variable `x` between `{0, 1}` and `{-1, 1}`

- `to_signed()` converts a variable from `{0, 1}` to `{-1, 1}`

- `to_unsigned()` converts a variable from `{-1, 1}` to `{0, 1}`

## Usage

``` r
to_signed(x)

to_unsigned(x)
```

## Arguments

- x:

  A binary variable

## Value

A signed (for `to_signed`) or unsigned (for `to_unsigned`) version of
`x`

## Examples

``` r
# should return `1`
to_signed(0)
#> [1] -1

# should return `1`
to_signed(1)
#> [1] 1

# should return `0`
to_unsigned(-1)
#> [1] 0

# should return `1`
to_unsigned(1)
#> [1] 1

# `to_signed` also works with objects `R` interprets as `0` or `1`
to_signed(10)
#> [1] 1

# `to_unsigned` also works with any signed integer
to_unsigned(-10)
#> [1] 0

# neither function works with factors
to_unsigned(factor(1))
#> Warning: ‘>’ not meaningful for factors
#> [1] NA
```
