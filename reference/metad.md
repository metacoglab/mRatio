# `brms` family for the metad' model

`brms` family for the metad' model

## Usage

``` r
metad(K, distribution = "normal", metac_absolute = TRUE)
```

## Arguments

- K:

  The number of confidence levels

- distribution:

  The noise distribution to use for the signal detection model

- metac_absolute:

  If `TRUE`, fix the type 2 criterion to be equal to the type 1
  criterion. Otherwise, equate the criteria relatively such that
  `meta_c/meta_dprime = c/dprime`.

## Value

A `brms` family for the metad' model with K confidence levels

## Examples

``` r
# create a family using the normal distribution and 3 levels of confidence
metad(3)
#> 
#> Custom family: metad__3__normal__absolute 
#> Link function: log 
#> Parameters: mu, dprime, c, metac2zero1diff, metac2zero2diff, metac2one1diff, metac2one2diff 
#> 

# create a family with meta_c = M * c
metad(3, metac_absolute = FALSE)
#> 
#> Custom family: metad__3__normal__relative 
#> Link function: log 
#> Parameters: mu, dprime, c, metac2zero1diff, metac2zero2diff, metac2one1diff, metac2one2diff 
#> 

# create a family with an alternative distribution
# note: cumulative distribution functions must be defined
# in R and in Stan using [brms::stanvar()]
metad(4, distribution = "gumbel_min")
#> 
#> Custom family: metad__4__gumbel_min__absolute 
#> Link function: log 
#> Parameters: mu, dprime, c, metac2zero1diff, metac2zero2diff, metac2zero3diff, metac2one1diff, metac2one2diff, metac2one3diff 
#> 
```
