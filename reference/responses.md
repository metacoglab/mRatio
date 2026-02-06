# Convert between separate and joint type 1/type 2 responses

Confidence ratings and decisions are collected in one of two ways.

- For separate ratings, there will be a type 1 response (`0` or `1`) and
  a type 2 response (confidence in `1:K`).

- For joint ratings, there is instead a combined type 1/type 2 response
  (in `1:(2*K)`), with values in `1:K` indicating a type 1 response of
  `0` and values in `(K+1):(2*K)` indicating a type 1 response of `1`,
  with confident responses at the ends of the scale.

`joint_response` converts separate type 1 and type 2 responses into the
joint format

`type1_response` and `type2_response` convert the joint response into
separate responses.

## Usage

``` r
joint_response(response, confidence, K)

type1_response(joint_response, K)

type2_response(joint_response, K)
```

## Arguments

- response:

  A type 1 response (`0` or `1`)

- confidence:

  A type 2 response/confidence rating (in `1:K`)

- K:

  The number of confidence levels

- joint_response:

  A joint type 1/type 2 response

## Examples

``` r
# convert joint_response to separate responses
joint <- 1:8
K <- 4
type1_response(joint, K)
#> [1] 0 0 0 0 1 1 1 1
type2_response(joint, K)
#> [1] 4 3 2 1 1 2 3 4

# convert separate responses to a joint response
t1 <- rep(c(0, 1), each = 4)
t2 <- c(4:1, 1:4)
joint_response(t1, t2, K)
#> [1] 1 2 3 4 5 6 7 8
```
