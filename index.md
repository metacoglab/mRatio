# mRatio

The mRatio package is designed to fit the meta-d’ model for confidence
ratings ([Maniscalco and Lau 2012](#ref-maniscalco2012),
[2014](#ref-maniscalco2014)). Like the [Hmeta-d
toolbox](https://github.com/metacoglab/HMeta-d) ([Fleming
2017](#ref-fleming2017)), the mRatio package uses a Bayesian modeling
approach. The mRatio package builds on the Hmeta-d toolbox through
implementation as a custom family in the
[brms](https://paulbuerkner.com/brms/) package, which itself provides a
friendly interface to the probabilistic programming language
[Stan](https://mc-stan.org).

This provides major benefits:

- Model designs can be specified as simple `R` formulas
- Support for complex model designs (e.g., multilevel models,
  distributional models)
- Interfaces to other packages surrounding `brms` (e.g., `tidybayes`,
  `ggdist`, `bayesplot`, `loo`, `posterior`, `bridgesampling`)
- Computation of model-implied quantities (e.g., mean confidence, type 1
  and type 2 receiver operating characteristic curves, metacognitive
  bias)
- Increased sampling efficiency and better convergence diagnostics

## Installation

You can install the development version of mRatio from
[GitHub](https://github.com/metacoglab/mRatio) with:

``` r
# install.packages("pak")
pak::pak("metacoglab/mRatio")
```

## Get started

Let’s say you have some data from a binary decision task with ordinal
confidence ratings:

``` R
#> # A tibble: 1,000 × 5
#>    trial stimulus response correct confidence
#>    <int>    <int>    <int>   <int>      <int>
#>  1     1        0        0       1          4
#>  2     2        0        0       1          3
#>  3     3        1        0       0          1
#>  4     4        0        0       1          2
#>  5     5        1        1       1          3
#>  6     6        0        1       0          1
#>  7     7        0        0       1          2
#>  8     8        0        1       0          1
#>  9     9        1        0       0          3
#> 10    10        1        1       1          3
#> # ℹ 990 more rows
```

You can fit an intercepts-only meta-d’ model using `fit_metad`:

``` r
library(mRatio)

m <- fit_metad(N ~ 1, data=d, file="vignettes/models/readme1.rds")
```

``` R
#>  Family: metad__4__normal__absolute 
#>   Links: mu = log 
#> Formula: N ~ 1 
#>    Data: data.aggregated (Number of observations: 1) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     0.00      0.14    -0.28     0.26 1.00     3551     2906
#> 
#> Further Distributional Parameters:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> dprime              1.06      0.08     0.90     1.22 1.00     3934     3453
#> c                  -0.05      0.04    -0.13     0.03 1.00     3792     3534
#> metac2zero1diff     0.50      0.04     0.44     0.58 1.00     5036     3085
#> metac2zero2diff     0.44      0.04     0.37     0.52 1.00     5778     3389
#> metac2zero3diff     0.55      0.05     0.46     0.66 1.00     5543     3280
#> metac2one1diff      0.53      0.04     0.46     0.61 1.00     4765     3336
#> metac2one2diff      0.44      0.04     0.37     0.51 1.00     5370     2594
#> metac2one3diff      0.52      0.05     0.43     0.61 1.00     5880     3060
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

Now let’s say you have a more complicated design, such as a
within-participants manipulation:

``` R
#> # A tibble: 5,000 × 7
#> # Groups:   participant, condition [50]
#>    participant condition trial stimulus response correct confidence
#>          <int>     <int> <int>    <int>    <int>   <int>      <int>
#>  1           1         1     1        0        0       1          3
#>  2           1         1     2        1        1       1          3
#>  3           1         1     3        1        1       1          3
#>  4           1         1     4        0        1       0          1
#>  5           1         1     5        1        1       1          2
#>  6           1         1     6        1        1       1          3
#>  7           1         1     7        1        0       0          3
#>  8           1         1     8        0        0       1          2
#>  9           1         1     9        1        1       1          4
#> 10           1         1    10        0        0       1          4
#> # ℹ 4,990 more rows
```

To account for the repeated measures in this design, you can simply
adjust the formula to include participant-level effects:

``` r
m <- fit_metad(
  bf(N ~ condition + (condition | participant),
    dprime + c + 
      metac2zero1diff + metac2zero2diff + metac2zero3diff + 
      metac2one1diff + metac2one2diff + metac2one3diff ~
      condition + (condition | participant)), 
  data=d, init="0", file="vignettes/models/readme2.rds",
  prior = prior(normal(0, 1)) +
    prior(normal(0, 1), dpar = dprime) +
    prior(normal(0, 1), dpar = c) +
    prior(normal(0, 1), dpar = metac2zero1diff) +
    prior(normal(0, 1), dpar = metac2zero2diff) +
    prior(normal(0, 1), dpar = metac2zero3diff) +
    prior(normal(0, 1), dpar = metac2one1diff) +
    prior(normal(0, 1), dpar = metac2one2diff) +
    prior(normal(0, 1), dpar = metac2one3diff))
```

``` R
#>  Family: metad__4__normal__absolute 
#>   Links: mu = log; dprime = identity; c = identity; metac2zero1diff = log; metac2zero2diff = log; metac2zero3diff = log; metac2one1diff = log; metac2one2diff = log; metac2one3diff = log 
#> Formula: N ~ condition + (condition | participant) 
#>          dprime ~ condition + (condition | participant)
#>          c ~ condition + (condition | participant)
#>          metac2zero1diff ~ condition + (condition | participant)
#>          metac2zero2diff ~ condition + (condition | participant)
#>          metac2zero3diff ~ condition + (condition | participant)
#>          metac2one1diff ~ condition + (condition | participant)
#>          metac2one2diff ~ condition + (condition | participant)
#>          metac2one3diff ~ condition + (condition | participant)
#>    Data: data.aggregated (Number of observations: 50) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Multilevel Hyperparameters:
#> ~participant (Number of levels: 25) 
#>                                                           Estimate Est.Error
#> sd(Intercept)                                                 0.74      0.18
#> sd(condition2)                                                0.71      0.22
#> sd(dprime_Intercept)                                          0.39      0.08
#> sd(dprime_condition2)                                         0.49      0.11
#> sd(c_Intercept)                                               0.55      0.09
#> sd(c_condition2)                                              0.62      0.10
#> sd(metac2zero1diff_Intercept)                                 0.10      0.07
#> sd(metac2zero1diff_condition2)                                0.13      0.09
#> sd(metac2zero2diff_Intercept)                                 0.07      0.05
#> sd(metac2zero2diff_condition2)                                0.11      0.08
#> sd(metac2zero3diff_Intercept)                                 0.09      0.07
#> sd(metac2zero3diff_condition2)                                0.22      0.13
#> sd(metac2one1diff_Intercept)                                  0.09      0.06
#> sd(metac2one1diff_condition2)                                 0.08      0.06
#> sd(metac2one2diff_Intercept)                                  0.16      0.08
#> sd(metac2one2diff_condition2)                                 0.12      0.09
#> sd(metac2one3diff_Intercept)                                  0.10      0.07
#> sd(metac2one3diff_condition2)                                 0.15      0.11
#> cor(Intercept,condition2)                                    -0.41      0.29
#> cor(dprime_Intercept,dprime_condition2)                      -0.66      0.17
#> cor(c_Intercept,c_condition2)                                -0.54      0.15
#> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2)    -0.11      0.56
#> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2)    -0.17      0.58
#> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2)    -0.12      0.57
#> cor(metac2one1diff_Intercept,metac2one1diff_condition2)      -0.25      0.58
#> cor(metac2one2diff_Intercept,metac2one2diff_condition2)      -0.11      0.56
#> cor(metac2one3diff_Intercept,metac2one3diff_condition2)      -0.40      0.55
#>                                                           l-95% CI u-95% CI
#> sd(Intercept)                                                 0.45     1.17
#> sd(condition2)                                                0.33     1.20
#> sd(dprime_Intercept)                                          0.25     0.57
#> sd(dprime_condition2)                                         0.29     0.72
#> sd(c_Intercept)                                               0.41     0.76
#> sd(c_condition2)                                              0.46     0.86
#> sd(metac2zero1diff_Intercept)                                 0.01     0.25
#> sd(metac2zero1diff_condition2)                                0.01     0.32
#> sd(metac2zero2diff_Intercept)                                 0.00     0.20
#> sd(metac2zero2diff_condition2)                                0.00     0.29
#> sd(metac2zero3diff_Intercept)                                 0.00     0.26
#> sd(metac2zero3diff_condition2)                                0.01     0.50
#> sd(metac2one1diff_Intercept)                                  0.00     0.23
#> sd(metac2one1diff_condition2)                                 0.00     0.23
#> sd(metac2one2diff_Intercept)                                  0.02     0.33
#> sd(metac2one2diff_condition2)                                 0.01     0.33
#> sd(metac2one3diff_Intercept)                                  0.00     0.27
#> sd(metac2one3diff_condition2)                                 0.01     0.41
#> cor(Intercept,condition2)                                    -0.85     0.24
#> cor(dprime_Intercept,dprime_condition2)                      -0.91    -0.24
#> cor(c_Intercept,c_condition2)                                -0.79    -0.19
#> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2)    -0.95     0.92
#> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2)    -0.97     0.92
#> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2)    -0.96     0.93
#> cor(metac2one1diff_Intercept,metac2one1diff_condition2)      -0.98     0.92
#> cor(metac2one2diff_Intercept,metac2one2diff_condition2)      -0.95     0.93
#> cor(metac2one3diff_Intercept,metac2one3diff_condition2)      -0.99     0.86
#>                                                           Rhat Bulk_ESS
#> sd(Intercept)                                             1.00     2128
#> sd(condition2)                                            1.00     1433
#> sd(dprime_Intercept)                                      1.00     1971
#> sd(dprime_condition2)                                     1.00     1904
#> sd(c_Intercept)                                           1.00     1109
#> sd(c_condition2)                                          1.00     1257
#> sd(metac2zero1diff_Intercept)                             1.00     1562
#> sd(metac2zero1diff_condition2)                            1.00     1724
#> sd(metac2zero2diff_Intercept)                             1.00     2550
#> sd(metac2zero2diff_condition2)                            1.00     2206
#> sd(metac2zero3diff_Intercept)                             1.00     2208
#> sd(metac2zero3diff_condition2)                            1.00     1049
#> sd(metac2one1diff_Intercept)                              1.00     1698
#> sd(metac2one1diff_condition2)                             1.00     2373
#> sd(metac2one2diff_Intercept)                              1.00     1171
#> sd(metac2one2diff_condition2)                             1.00     1825
#> sd(metac2one3diff_Intercept)                              1.00     1512
#> sd(metac2one3diff_condition2)                             1.00     1348
#> cor(Intercept,condition2)                                 1.00     2947
#> cor(dprime_Intercept,dprime_condition2)                   1.00     2053
#> cor(c_Intercept,c_condition2)                             1.00     1295
#> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2) 1.00     3197
#> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2) 1.00     3720
#> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2) 1.00     1697
#> cor(metac2one1diff_Intercept,metac2one1diff_condition2)   1.00     3847
#> cor(metac2one2diff_Intercept,metac2one2diff_condition2)   1.00     3993
#> cor(metac2one3diff_Intercept,metac2one3diff_condition2)   1.00     2489
#>                                                           Tail_ESS
#> sd(Intercept)                                                 2302
#> sd(condition2)                                                2016
#> sd(dprime_Intercept)                                          2623
#> sd(dprime_condition2)                                         2945
#> sd(c_Intercept)                                               1865
#> sd(c_condition2)                                              1769
#> sd(metac2zero1diff_Intercept)                                 2262
#> sd(metac2zero1diff_condition2)                                1958
#> sd(metac2zero2diff_Intercept)                                 1963
#> sd(metac2zero2diff_condition2)                                2203
#> sd(metac2zero3diff_Intercept)                                 2455
#> sd(metac2zero3diff_condition2)                                2127
#> sd(metac2one1diff_Intercept)                                  1847
#> sd(metac2one1diff_condition2)                                 2271
#> sd(metac2one2diff_Intercept)                                  1832
#> sd(metac2one2diff_condition2)                                 2499
#> sd(metac2one3diff_Intercept)                                  2524
#> sd(metac2one3diff_condition2)                                 2271
#> cor(Intercept,condition2)                                     2971
#> cor(dprime_Intercept,dprime_condition2)                       2632
#> cor(c_Intercept,c_condition2)                                 1926
#> cor(metac2zero1diff_Intercept,metac2zero1diff_condition2)     3075
#> cor(metac2zero2diff_Intercept,metac2zero2diff_condition2)     2974
#> cor(metac2zero3diff_Intercept,metac2zero3diff_condition2)     2546
#> cor(metac2one1diff_Intercept,metac2one1diff_condition2)       2891
#> cor(metac2one2diff_Intercept,metac2one2diff_condition2)       2786
#> cor(metac2one3diff_Intercept,metac2one3diff_condition2)       3209
#> 
#> Regression Coefficients:
#>                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> Intercept                     -0.13      0.21    -0.56     0.25 1.00     2848
#> dprime_Intercept               0.86      0.10     0.67     1.05 1.00     3034
#> c_Intercept                   -0.04      0.11    -0.27     0.18 1.00      819
#> metac2zero1diff_Intercept     -1.01      0.06    -1.12    -0.89 1.00     7762
#> metac2zero2diff_Intercept     -1.05      0.06    -1.18    -0.93 1.00     8956
#> metac2zero3diff_Intercept     -0.96      0.07    -1.10    -0.83 1.00     5809
#> metac2one1diff_Intercept      -1.06      0.06    -1.18    -0.94 1.00     8443
#> metac2one2diff_Intercept      -1.01      0.07    -1.14    -0.88 1.00     5639
#> metac2one3diff_Intercept      -0.97      0.07    -1.11    -0.84 1.00     8469
#> condition2                    -0.08      0.23    -0.54     0.39 1.00     3760
#> dprime_condition2              0.17      0.13    -0.08     0.42 1.00     3170
#> c_condition2                   0.03      0.13    -0.23     0.29 1.00     1052
#> metac2zero1diff_condition2     0.02      0.08    -0.15     0.18 1.00     6954
#> metac2zero2diff_condition2    -0.03      0.09    -0.21     0.13 1.00     8356
#> metac2zero3diff_condition2    -0.07      0.10    -0.28     0.13 1.00     5252
#> metac2one1diff_condition2      0.07      0.08    -0.09     0.22 1.00     9424
#> metac2one2diff_condition2      0.06      0.09    -0.11     0.23 1.00     5787
#> metac2one3diff_condition2     -0.11      0.10    -0.30     0.08 1.00     6526
#>                            Tail_ESS
#> Intercept                      2927
#> dprime_Intercept               3275
#> c_Intercept                    1340
#> metac2zero1diff_Intercept      3106
#> metac2zero2diff_Intercept      2554
#> metac2zero3diff_Intercept      3236
#> metac2one1diff_Intercept       3238
#> metac2one2diff_Intercept       3499
#> metac2one3diff_Intercept       3035
#> condition2                     3271
#> dprime_condition2              2724
#> c_condition2                   1441
#> metac2zero1diff_condition2     3102
#> metac2zero2diff_condition2     2920
#> metac2zero3diff_condition2     3269
#> metac2one1diff_condition2      3087
#> metac2one2diff_condition2      2956
#> metac2one3diff_condition2      3186
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

## References

Fleming, Stephen M. 2017. “HMeta-d: Hierarchical Bayesian Estimation of
Metacognitive Efficiency from Confidence Ratings.” *Neuroscience of
Consciousness* 2017 (1): nix007.

Maniscalco, Brian, and Hakwan Lau. 2012. “A Signal Detection Theoretic
Approach for Estimating Metacognitive Sensitivity from Confidence
Ratings.” *Consciousness and Cognition* 21 (1): 422–30.

Maniscalco, Brian, and Hakwan Lau. 2014. “Signal Detection Theory
Analysis of Type 1 and Type 2 Data: Meta-d′, Response-Specific Meta-d′,
and the Unequal Variance SDT Model.” In *The Cognitive Neuroscience of
Metacognition*. Springer.
