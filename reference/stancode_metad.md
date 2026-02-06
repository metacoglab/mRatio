# Generate Stan code for the meta-d' model

Generate Stan code for the meta-d' model

## Usage

``` r
stancode_metad(K, distribution = "normal", metac_absolute = TRUE)
```

## Arguments

- K:

  The number of confidence levels

- distribution:

  The noise distribution to use. Should be a parameter-free
  distribution, i.e., one that is mean-centered without additional
  variance/shape parameters. If the distribution is not already
  available in stan, you must additionally provide two functions to Stan
  (one for `<distribution>_lcdf` and one for `<distribution>_lccdf`).

- metac_absolute:

  Should the type 2 criterion (metac) be fixed to the absolute type 1
  criterion (c)? If `TRUE`, the model will set `metac = c`. Otherwise,
  it will set `metac = M * c`, such that the type 2 criterion is
  *relatively* equal to the type 1 criterion (i.e.,
  `meta_c/meta_dprime = c/dprime`)

## Value

A single string containing Stan code defining the likelihood for the
metad' model with `K` confidence levels, signal distributed according to
the distribution `distribution`, and where `metac = c` if
`metac_absolute==TRUE`, and `metac = M*c` otherwise.
