
<!-- README.md is generated from README.Rmd. Please edit that file -->

diftrans
========

<!-- badges: start -->

![status](https://img.shields.io/badge/status-under%20construction-yellow)
<!-- badges: end -->

Table of Contents
-----------------

-   [Introduction](#introduction)
-   [Installation](#installation)
-   [Attribution](#attribution)
-   [Example](#example)

Introduction
------------

The `diftrans` package applies the novel methods described in Daljord,
Pouliot, Hu, and Xiao (2021) to compute the transport costs between two
univariate distributions and the differences-in-transports estimator.

Appealing to optimal transport theory, the `diftrans` package builds off
of the [`transport`
package](https://cran.r-project.org/web/packages/transport/index.html)
to compute the change between the distributions of a univariate variable
of interest. Extending this application, the `diftrans` package allows
users to compute the *before-and-after estimator* (Daljord et al.,
2021), which controls for sampling uncertainty by trivializing small
changes in the distributions.

The `diftrans` package also computes the *differences-in-transports
estimator*, which controls for unobservable reasons that the
distributions may change by comparing the transport costs to those from
another source of measurement.

The only function in the `diftrans` package is `diftrans`, which is
explained below by way of a toy example.

The documentation file for `diftrans::diftrans` can be found by running
the following in an R console:

    ?diftrans::diftrans

Installation
------------

Install the `diftrans` from
[GitHub](https://github.com/omkarakatta/diftrans) with:

    # install.packages("devtools")
    devtools::install_github("omkarakatta/diftrans")

To install the package with the vignette installed, set the
`build_vignettes` argument to `TRUE`. Note that the installation will
take more time this way.

    # install.packages("devtools")
    devtools::install_github("omkarakatta/diftrans", build_vignettes = TRUE)

Attribution
-----------

To cite the `diftrans` package, use the BibTeX entry provided by:

    citation("diftrans")

Example
-------

The workhorse function in the `diftrans` package is also called
`diftrans`, which serves two purposes:

-   compute the transport cost between two univariate distributions, and
-   compute the differences-in-transports estimator (see Daljord et
    al. (2021)).

### Setup

    library(dplyr)
    library(magrittr)
    library(diftrans)

We begin by computing the transport cost between two distributions of
some variable `x`. These distributions should be represented as tibbles
with two columns:

-   column 1 contains the full support of the distribution, and
-   column 2 (labeled “count”) contains the mass/counts associated with
    each value in the support.

Suppose that the shift in the distribution is due to some treatment. We
refer to the first and second distributions of some random variable `x`
as the pre-distribution and post-distribution for the treated group,
respectively.

Below are the tibbles for the pre- and post-distributions as well as the
corresponding plots. Both tibbles contain the full support of our
variable of interest, i.e., their `support` column is the same.

*Pre-Distribution for Treated Group*

|  x  | count |
|:---:|:-----:|
|  1  |   9   |
|  2  |   7   |
|  3  |   7   |
|  4  |   6   |
|  5  |   9   |
|  6  |   0   |
|  7  |   0   |
|  8  |   0   |
|  9  |   0   |
| 10  |   0   |

*Post-Distribution for Treated Group*

|  x  | count |
|:---:|:-----:|
|  1  |   0   |
|  2  |   0   |
|  3  |   0   |
|  4  |   0   |
|  5  |   0   |
|  6  |   9   |
|  7  |   7   |
|  8  |   7   |
|  9  |   6   |
| 10  |   9   |

Observe that all the mass from the pre-distribution was shifted by 5
units of the support to form the post-distribution. That is, the
treatment resulted in all the mass to change. For instance, the mass
that was given given to 1 in the pre-distribution is now given to 6.

In this toy example, we should expect that the transport cost is 100%
because all the mass was transported due to the treatment.

### Compute Transport Cost

We can compute the transport cost as follows:

    tc <- diftrans(pre_main = pre_treated, post_main = post_treated,
                      estimator = "tc", var = x,
                      bandwidth = 0)

    #> Computing Transport Costs...

    #> ================================================================================

    #> The transport cost for the specified bandwidths have been computed.

    tc

    #>   bandwidth main
    #> 1         0    1

**The transport cost is 100%** as expected. The `estimator` argument is
specified to be `"tc"`, which stands for transport cost. (Since we only
have two distributions, estimator will take on the value of `"tc"` by
default.)

Since the post-distribution is a 5-unit shift in the pre-distribution,
any bandwidth at least 5 must result in a transport cost of 0. We verify
this by computing the transport cost for a sequence of bandwidths from 0
to 10:

    tc <- diftrans(pre_main = pre_treated, post_main = post_treated,
                      estimator = "tc", var = x,
                      bandwidth = seq(0, 10))

    #> Computing Transport Costs...

    #> ================================================================================

    #> The transport cost for the specified bandwidths have been computed.

    tc

    #>    bandwidth      main
    #> 1          0 1.0000000
    #> 2          1 0.7631579
    #> 3          2 0.6052632
    #> 4          3 0.4210526
    #> 5          4 0.2368421
    #> 6          5 0.0000000
    #> 7          6 0.0000000
    #> 8          7 0.0000000
    #> 9          8 0.0000000
    #> 10         9 0.0000000
    #> 11        10 0.0000000

### Setup for Difference-in-Transports

Above, we assumed that the treatment is sole reason why any shifts
between the pre- and post-distributions of `x` might occur. Now suppose
there are unobserved trends in our variable that might also explain this
shift. To elicit how much of this shift is due to the treatment and how
much of this shift is due to unobserved trends, we can compare our
distributions above with distributions of the same variable measured in
some control group that did not receive the treatment but was also
subjected to the same unobserved trends.

The novel differences-in-transports estimator described in Daljord et
al. (2021) quantifies the change in distribution due to some treatment
relative to the change in distribution of some control group.

First, we need to set up the distributions of our variable for our
control group, both before and after the treatment was administered to
the treatment group. For our purposes, we let the pre-distribution of
our control group be the same as that of our treated group. Then, let
our post-distribution for our control group be a 2-unit shift of our
pre-distribution. We therefore have the following tibbles that represent
our control group distributions:

*Pre-Distribution for Control Group*

|  x  | count |
|:---:|:-----:|
|  1  |   9   |
|  2  |   7   |
|  3  |   7   |
|  4  |   6   |
|  5  |   9   |
|  6  |   0   |
|  7  |   0   |
|  8  |   0   |
|  9  |   0   |
| 10  |   0   |

*Post-Distribution for Control Group*

|  x  | count |
|:---:|:-----:|
|  1  |   0   |
|  2  |   0   |
|  3  |   9   |
|  4  |   7   |
|  5  |   7   |
|  6  |   6   |
|  7  |   9   |
|  8  |   0   |
|  9  |   0   |
| 10  |   0   |

### Compute Differences-in-Transports Estimator <sup id="a1">[1](#f1)</sup>

Above, we used `estimator = "tc"` and specified our pre- and
post-distributions for our treated group. Now, we use
`estimator = "dit"` (which stands for differences-in-treatments
estimator) while also specifying our pre- and post-distributions for our
control group.

    dit <- diftrans(pre_main = pre_treated, post_main = post_treated,
                       pre_control = pre_control, post_control = post_control,
                       estimator = "dit", var = x,
                       bandwidth_seq = seq(0, 10, 1),
                       save_dit = TRUE)

    #> Computing Differences-in-Transports Estimator...

    #> ================================================================================

    #> The non-conservative diff-in-transports estimator is 0.605263157894737 at d = 2

    dit$out

    #>    bandwidth      main   control      diff
    #> 1          0 1.0000000 0.4736842 0.5263158
    #> 2          1 0.7631579 0.2368421 0.5263158
    #> 3          2 0.6052632 0.0000000 0.6052632
    #> 4          3 0.4210526 0.0000000 0.4210526
    #> 5          4 0.2368421 0.0000000 0.2368421
    #> 6          5 0.0000000 0.0000000 0.0000000
    #> 7          6 0.0000000 0.0000000 0.0000000
    #> 8          7 0.0000000 0.0000000 0.0000000
    #> 9          8 0.0000000 0.0000000 0.0000000
    #> 10         9 0.0000000 0.0000000 0.0000000
    #> 11        10 0.0000000 0.0000000 0.0000000

    dit$dit

    #> [1] 0.6052632

    dit$optimal_bandwidth

    #> [1] 2

Another difference between computing the transport cost and the
differences-in-differences estimator is that the
differences-in-transports estimator is printed as a message. To save
this result in `dit`, we use `save_dit = TRUE`.

Thus, **the differences-in-transports estimator is 61% at an optimal
bandwidth of 2**.

#### Sampling Variability

Often times, the data will be a sample from a population, and sample
variability is enough to cause differences between distributions. To
account for this discrepancy, we can increase the bandwidth to ignore
transfers of mass between nearby values of the support.

While Daljord et al. (2021) offer a disciplined way to determine what
the appropriate bandwidths are, for simplicity, we will suppose that
there will not be any sampling variation with a bandwidth greater than
1.

    d_a <- 1

    dit <- diftrans(pre_main = pre_treated, post_main = post_treated,
                       pre_control = pre_control, post_control = post_control,
                       estimator = "dit", var = x,
                       bandwidth_seq = seq(d_a, 10, 1), # smallest bandwidth will be d_a
                       save_dit = TRUE)

    #> Computing Differences-in-Transports Estimator...

    #> ================================================================================

    #> The non-conservative diff-in-transports estimator is 0.605263157894737 at d = 2

    dit$out

    #>    bandwidth      main   control      diff
    #> 1          1 0.7631579 0.2368421 0.5263158
    #> 2          2 0.6052632 0.0000000 0.6052632
    #> 3          3 0.4210526 0.0000000 0.4210526
    #> 4          4 0.2368421 0.0000000 0.2368421
    #> 5          5 0.0000000 0.0000000 0.0000000
    #> 6          6 0.0000000 0.0000000 0.0000000
    #> 7          7 0.0000000 0.0000000 0.0000000
    #> 8          8 0.0000000 0.0000000 0.0000000
    #> 9          9 0.0000000 0.0000000 0.0000000
    #> 10        10 0.0000000 0.0000000 0.0000000

    dit$dit

    #> [1] 0.6052632

    dit$optimal_bandwidth

    #> [1] 2

Note that this restriction is not binding, so our result is unaffected.
**Accounting for sampling variability, our differences-in-transports
estimate is still 61%**.

#### Conservative Differences-in-Transports

If we want to be more conservative, we can use `conservative = TRUE`,
which essentially uses twice the bandwidth for computing the treated
group’s transport costs relative to the bandwidth for computing the
control group’s transport costs.

    dit <- diftrans(pre_main = pre_treated, post_main = post_treated,
                       pre_control = pre_control, post_control = post_control,
                       estimator = "dit", var = x,
                       bandwidth_seq = seq(d_a, 10, 1),
                       save_dit = TRUE,
                       conservative = TRUE)

    #> Computing Differences-in-Transports Estimator...

    #> Note: you are using `conservative = T`.

    #> ================================================================================

    #> The conservative diff-in-transports estimator is 0.368421052631579 at d = 1

    dit$out

    #>    bandwidth      main    main2d   control      diff    diff2d
    #> 1          1 0.7631579 0.6052632 0.2368421 0.5263158 0.3684211
    #> 2          2 0.6052632 0.2368421 0.0000000 0.6052632 0.2368421
    #> 3          3 0.4210526 0.0000000 0.0000000 0.4210526 0.0000000
    #> 4          4 0.2368421 0.0000000 0.0000000 0.2368421 0.0000000
    #> 5          5 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
    #> 6          6 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
    #> 7          7 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
    #> 8          8 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
    #> 9          9 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
    #> 10        10 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000

    dit$dit

    #> [1] 0.3684211

    dit$optimal_bandwidth

    #> [1] 1

Finally, we have that the **conservative differences-in-transports
estimator is 37% at an optimal bandwidth of 1**.

Note that `estimator = "dit"` is not necessary. Since we have two sets
of pre- and post-distributions, `diftrans` is smart enough to return the
differences-in-transports estimator by default.

------------------------------------------------------------------------

<b id="f1">1</b> There is more nuance to computing the
differences-in-transports estimator than is presented in this expository
document. See Daljord et al. (2021) for a more complete treatment on how
to compute the differences-in-transports estimator. [↩](#a1)

<!-- https://stackoverflow.com/a/32119820 -->
