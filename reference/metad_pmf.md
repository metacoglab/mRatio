# Generate (log) probability simplex over the joint type 1/type 2 responses

Generate (log) probability simplex over the joint type 1/type 2
responses

## Usage

``` r
metad_pmf(
  stimulus,
  dprime,
  c,
  meta_dprime,
  meta_c,
  meta_c2_0,
  meta_c2_1,
  lcdf = normal_lcdf,
  lccdf = normal_lccdf,
  log = FALSE
)
```

## Arguments

- stimulus:

  the stimulus (0 or 1)

- dprime:

  the type 1 sensitivity

- c:

  the type 1 response criterion

- meta_dprime:

  the type 2 sensitivity

- meta_c:

  the type 1 criteriom for generating confidence ratings

- meta_c2_0:

  the type 2 response criteria for `"0"` responses, indexed by
  increasing confidence levels

- meta_c2_1:

  the type 2 response criteria for `"1"` responses, indexed by
  increasing confidence levels

- lcdf:

  The log cumulative distribution function for the underlying
  distribution in the metad' model. By default, uses the normal
  distribution with a standard deviation of `1`.

- lccdf:

  The log complement cumulative distribution function for the underlying
  distribution in the metad' model. By default, uses the normal
  distribution with a standard deviation of `1`.

- log:

  if TRUE, return log probabilities instead of probabilities

## Value

A probability simplex \$\$\begin{bmatrix} P(R=0, C=K \vert S=0), \ldots,
P(R=0, C=1 \vert S=0), P(R=0, C=1 \vert S=1), \ldots, P(R=1, C=1 \vert
S=1)\end{bmatrix}\$\$ for response \\R\\ and confidence \\C\\ given
stimulus \\S\\, as defined by the meta-d' model.

## Examples

``` r
metad_pmf(
  stimulus = 0, dprime = 2, c = .5, meta_dprime = 1, meta_c = .5,
  meta_c2_0 = c(0, -.5), meta_c2_1 = c(1, 1.5)
)
#> [1] 0.554584077 0.212364065 0.166244657 0.038675753 0.018551730 0.009579718
```
