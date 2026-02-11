# Parameterization of the meta-d' model

This vignette describes the parameterization of the meta-d’ model used
in the `mRatio` package. For an in-depth review, we refer readers to
([Maniscalco and Lau 2012](#ref-maniscalco2012),
[2014](#ref-maniscalco2014)).

## Introduction

In metacognition research, decision-making is often categorized into two
kinds.

The *type 1* decision is the primary decision at hand, for example
judging the orientation of a grating as left or right, or judging a word
as either having been presented before (old) or not (new). In the
context of the meta-d’ model, the type 1 decision is a binary decision
task (i.e., either a yes-no decision task or a two-alternative forced
choice task).

In contrast, the *type 2* decision is the task of rating confidence in
the type 1 decision. The meta-d’ model is applicable where the type 2
decision is categorical (i.e., confidence is rated on an ordinal scale
from `1` to `K`, where `1` indicates low confidence and `K` indicates
high confidence).

In line with the distinction between type 1 and type 2 decisions, the
meta-d’ model is a conjunction of two models (one for each decision).
Both models are based in Signal Detection Theory (SDT). However, the
insight of the meta-d’ model is that the information available for type
2 decisions might differ from the information available for type 1
decisions.

## Model for type 1 decisions

Under SDT, the observer is presented with a noisy signal depending on
the underlying stimulus x_1 \sim \mathcal{D}\_S(d') following a
distribution \mathcal{D} dependent on a stimulus S \in \\0, 1\\ and the
observer’s sensitivity d'. Typically, \mathcal{D} is chosen to be an
equal-variance normal distribution, such that

\mathcal{D}\_0(d') = \mathcal{N}\left(-\frac{d'}{2}, 1\right) and
\mathcal{D}\_1(d') = \mathcal{N}\left(\frac{d'}{2}, 1\right)

However, this decision is not arbitrary and other options are available.

Given the noisy encoding x_1, the observer is tasked with determining
the true value of S. To do so, they simply threshold x_1 such that their
response is

R = \[x_1 \> c\]

In this setup, the observer makes a correct response when R = S. More
importantly, trials can be categorized into *hits* (S = 1 and R = 1),
*misses* (S = 1 and R = 0), *false alarms* (FAs; S = 0 and R = 1), and
*correct rejections* (CRs; S = 0 and R = 0). The generative model for
type 1 decisions above implies the following response probabilities:

P(R=r \\\vert\\ S=s) = \begin{cases} 1 -
F\_{\mathcal{D}\_s(d')}\left(c\right) & \textrm{if } r=1 \\
F\_{\mathcal{D}\_s(d')}\left(c\right) & \textrm{if } r=0 \end{cases}

## Model for type 2 decisions

In classical SDT, type 2 decisions are treated just like type 1
decisions but with more stringent or more liberal response criteria.
However, this assumes that observers have access to the same information
when making type 1 and type 2 decisions. Relaxing this assumption, the
meta-d’ model assumes that type 2 decisions are derived from a separate
decision variable:

x_2 \sim \begin{cases} \mathcal{D}\_S^{(-\infty,
\textrm{meta-}c\]}\left(\textrm{meta-}d'\right) & \textrm{if } R=0 \\
\mathcal{D}\_S^{\[\textrm{meta-}c, \infty)}\left(\textrm{meta-}d'\right)
& \textrm{if } R=1 \end{cases}

Importantly, this decision variable follows the same distribution as for
the type 1 decision, with two differences. First, the distribution is
truncated either above or below at the type 1 criterion \textrm{meta-}c
depending on the initial type 1 response. This is so that the type 2
decision cannot contradict the type 1 decision (i.e., the meta-d’ model
does not allow for changes of mind). Second, the sensitivity for the
type 2 decision is \textrm{meta-}d' rather than d' to allow for
task-level sensitivity and metacognitive sensitivity to differ.

Then, to determine the confidence level C \in \\ 1 \ldots K\\, the
observer rates confidence using one of two sets of K-1 ordered
confidence criteria (\textrm{meta-}c_2^0 or \textrm{meta-}c_2^1):

\begin{align\*} C &= \begin{cases} 1+\Sigma\_{k=1}^{K-1}\[x_2 \<
\textrm{meta-}c\_{2,k}^0\] & \textrm{if } R=0 \\ 1+\Sigma\_{k=1}^{K-1}
\[x_2 \> \textrm{meta-}c\_{2,k}^1\] & \textrm{if } R=1 \end{cases}
\end{align\*}

This generative model implies that, conditional on the stimulus and the
type 1 response, the type 2 response probabilities are

\begin{align\*} P(C=c \\\vert\\ R=r,S=s) &= \begin{cases}
\frac{F\_{\mathcal{D}\_s(\textrm{meta-}d')}\left(\textrm{meta-}c\right) -
F\_{\mathcal{D}\_s(\textrm{meta-}d')}\left(\textrm{meta-}c\_{2,1}^0\right)}
{F\_{\mathcal{D}\_s(\textrm{meta-}d')}\left(\textrm{meta-}c\right)} &
\textrm{if } r=0, c=1 \\
\frac{F\_{\mathcal{D}\_s(\textrm{meta-}d')}\left(\textrm{meta-}c\_{2,k}^0\right) -
F\_{\mathcal{D}\_s(\textrm{meta-}d')}\left(\textrm{meta-}c\_{2,k+1}^0\right)}
{F\_{\mathcal{D}\_s(\textrm{meta-}d')}\left(\textrm{meta-}c\right)} &
\textrm{if } r=0, 1 \le c \le K \\
\frac{F\_{\mathcal{D}\_s(\textrm{meta-}d')}\left(\textrm{meta-}c\_{2,K}^0\right)}
{F\_{\mathcal{D}\_s(\textrm{meta-}d')}\left(\textrm{meta-}c\right)} &
\textrm{if } r=0, c = K \\
\frac{F\_{\mathcal{D}\_s(\textrm{meta-}d')}\left(\textrm{meta-}c\_{2,1}^0\right) -
F\_{\mathcal{D}\_s}\left(\textrm{meta-}c \right)} {1 -
F\_{\mathcal{D}\_s(\textrm{meta-}d')}\left(\textrm{meta-}c\right)} &
\textrm{if } r=1, c=1 \\
\frac{F\_{\mathcal{D}\_s(\textrm{meta-}d')}\left(\textrm{meta-}c\_{2,k+1}^0\right) -
F\_{\mathcal{D}\_s(\textrm{meta-}d')}\left(\textrm{meta-}c\_{2,k}^0\right)}
{1 - F\_{\mathcal{D}\_s(\textrm{meta-}d')}\left(\textrm{meta-}c\right)}
& \textrm{if } r=1, 1 \le c \le K \\ \frac{1 -
F\_{\mathcal{D}\_s(\textrm{meta-}d')}\left(\textrm{meta-}c\_{2,K}^0
\right)} {1 -
F\_{\mathcal{D}\_s(\textrm{meta-}d')}\left(\textrm{meta-}c\right)} &
\textrm{if } r=1, c = K \end{cases} \end{align\*}

In these formulas, the numerator is the probability that x_2 lies
between successive confidence thresholds and the denominator is the
probability of a type 1 response r given the type 2 parameters
\textrm{meta-}d' and \textrm{meta-}c to account for the truncation of
the type 2 signal distributions at \textrm{meta-}c.

## Joint model for type 1 and type 2 decisions

Ultimately, we are interested in the joint type 1 and type 2 response
probabilities for each stimulus. This can be conveniently decomposed
into the type 1 response probability and the conditional type 2 response
probability as follows:

P(R=r, C=c \\\vert\\ S=s) = P(R=r \\\vert\\ S=s) \\ P(C=c \\\vert\\ R=r,
S=s)

Given the joint response probabilities for each stimulus, we can
formulate the log likelihood of the meta-d’ in one of two ways. If
trial-level effects are of interest, one can model individual trials
using a categorical likelihood:

LL \\=\\ \sum_n
\textrm{categorical}\\\textrm{lpmf}\left(\textrm{joint}\\\textrm{response}(r_n,c_n)
\\\vert\\ P(R=r_n, C=c_n \\\vert\\ S=s_n)\right)

However, this formulation requires the likelihood to be evaluated once
per trial, which for well-powered experiments can take a long time. So,
by default the `mRatio` package uses a multinomial likelihood over the
aggregated data. Where N\_{s,r,c} is the number of trials with S=s, R=r,
and C=c:

\begin{align\*} LL \\&=\\
\textrm{multinomial}\\\textrm{lpmf}\left(N\_{0,r,c} \\\vert\\ P(R=r, C=c
\\\vert\\ S=0)\right) \\ &\\\quad+\\
\textrm{multinomial}\\\textrm{lpmf}\left(N\_{1,r,c} \\\vert\\ P(R=r, C=c
\\\vert\\ S=1)\right) \end{align\*}

This formulation only requires the model likelihood to be evaluated
twice (once per stimulus), dramatically increasing the efficiency of
model fitting.

## Fixing the type 1 threshold for type 2 responses

The meta-d’ model requires the parameter \textrm{meta-}c to be fixed to
be equal with respect to the type 1 criterion c. As discussed by
([Maniscalco and Lau 2014](#ref-maniscalco2014)), there are multiple
ways of fixing \textrm{meta-}c. The `mRatio` package implements two:

Under the fixed parameterization, \textrm{meta-}c = c. This
parameterization is used as the default, since it was also used in the
[Hmeta-d toolbox](https://github.com/metacoglab/HMeta-d) (see also
Fleming ([2017](#ref-fleming2017))).

Alternatively, under the relative parameterization,
\frac{\textrm{meta-}c}{\textrm{meta-}d'} = \frac{c}{d'}, which is
achieved by setting \textrm{meta-}c = M c. This parameterization was
used in ([Maniscalco and Lau 2012](#ref-maniscalco2012),
[2014](#ref-maniscalco2014)).

To switch between these two parameterizations, the `fit_metad` and
`sim_metad` functions have an argument `metac_absolute` which is `TRUE`
by default. To use the relative parameterization, simply set
`metac_absolute=FALSE` in your call to each function.

## Model parameterization

To further increase the efficiency model fitting and help with
convergence, the `mRatio` parameterizes the meta-d’ model so that all
parameters are unconstrained variables (i.e., they fall in the range
(-\infty, \infty)). The parameters for type 1 responses (d' and c) are
already unconstrained, so they are estimated normally. However, the
parameters for the type 2 parameters are bounded.

First, instead of fitting \textrm{meta-}d' directly, the `mRatio`
package models the M-ratio M = \frac{\textrm{meta-}d'}{d'}. While this
parameterization helps regularize against strong differences between
\textrm{meta-}d' and d', the M-ratio is still bounded by zero. So, the
`mRatio` package models the M-ratio on the logarithmic scale, i.e.,
\textrm{log }M = \textrm{log}\frac{\textrm{meta-}d'}{d'}. In this
parameterization, one can compute \textrm{meta-}d' as \textrm{meta-}d' =
e^{\textrm{log }M}d'.

Second, the confidence criteria \textrm{meta-}c\_{2,1:K}^0 and
\textrm{meta-}c\_{2,1:K}^1 each have two constraints. Namely,
\textrm{meta-}c\_{2,1:K}^0 must be strictly decreasing and less than
\textrm{meta-}c, whereas \textrm{meta-}c\_{2,1:K}^1 must be strictly
increasing and greater than \textrm{meta-}c. To deal with these
constraints, the `mRatio` package estimates the differences between
successive confidence criteria:

\textrm{dmeta-}c\_{2,k}^0 = \begin{cases} \textrm{meta-}c -
\textrm{meta-}c\_{2,1}^0 & \textrm{if } k = 1 \\
\textrm{meta-}c\_{2,k-1}^0 - \textrm{meta-}c\_{2,k}^0 & \textrm{if } 2
\le k \le K \end{cases} \textrm{dmeta-}c\_{2,k}^1 = \begin{cases}
\textrm{meta-}c\_{2,1}^0 - \textrm{meta-}c & \textrm{if } k = 1 \\
\textrm{meta-}c\_{2,k}^1 - \textrm{meta-}c\_{2,k-1}^1 & \textrm{if } 2
\le k \le K \end{cases}

Like for M-ratio, the differences between successive confidence criteria
are also modeled on the logarithmic scale (parameters are named
`metac2zero<k>diff` and `metac2one<k>diff`). Under this
parameterization, the confidence criteria can be computed as follows:

\begin{align\*} \textrm{meta-}c_2^0 &= \textrm{meta-}c -
\textrm{cumulative}\\\textrm{sum}\left(e^{\textrm{dmeta-}c_2^0}\right)
\\ \textrm{meta-}c_2^1 &= \textrm{meta-}c +
\textrm{cumulative}\\\textrm{sum}\left(e^{\textrm{dmeta-}c_2^1}\right)
\end{align\*}

## References

Fleming, Stephen M. 2017. “HMeta-d: Hierarchical Bayesian Estimation of
Metacognitive Efficiency from Confidence Ratings.” *Neuroscience of
Consciousness* 2017 (1): nix007.

Maniscalco, Brian, and Hakwan Lau. 2012. “A Signal Detection Theoretic
Approach for Estimating Metacognitive Sensitivity from Confidence
Ratings.” *Consciousness and Cognition* 21 (1): 422–30.

———. 2014. “Signal Detection Theory Analysis of Type 1 and Type 2 Data:
Meta-d′, Response-Specific Meta-d′, and the Unequal Variance SDT Model.”
In *The Cognitive Neuroscience of Metacognition*, 25–66. Springer.
