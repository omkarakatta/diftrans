
<!-- README.md is generated from README.Rmd. Please edit that file -->
<style> 
  /* https://stackoverflow.com/questions/31753897/2-column-section-in-r-markdown */
  .col2 {
    columns: 2 200px;         /* number of columns and width in pixels*/
    -webkit-columns: 2 200px; /* chrome, safari */
    -moz-columns: 2 200px;    /* firefox */
  }
  .col3 {
    columns: 3 100px;
    -webkit-columns: 3 100px;
    -moz-columns: 3 100px;
  }
  .column-left{
    float: left;
    width: 30%;
    text-align: left;
  }
  .column-right{
    float: right;
    width: 70%;
    text-align: right;
  }
  /* https://gist.github.com/jennybc/e9e9aba6ba18c72cec26 */
  .clearer {clear: both}
  
  .center{
    text-align: center
  }
</style>

diftrans
========

<!-- badges: start -->

![status](https://img.shields.io/badge/status-under%20construction-yellow)
<!-- badges: end -->

The goal of the `diftrans` package is to compute the
differences-in-transports estimator introduced in Daljord, Pouliot, Hu,
and Xiao (2020). Currently, the package is in development.

Installation
------------

You can install the `diftrans` from [GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("omkarakatta/diftrans")

Example
-------

The workhorse function in the `diftrans` package is `get_results`, which
serves two purposes:

-   compute transport cost between two univariate distributions, and
-   compute the differences-in-transports estimator (see Daljord et
    al. (2020)).

We begin by computing the transport cost between two distributions,
which we shall call the pre-distribution and post-distribution. They
should be represented as tibbles with two columns:

-   column 1 contains the full support of the distribution and
-   column 2 (labeled “count”) contains the mass/counts associated with
    each value in the support.

### Setup

Below are the tibbles for the pre- and post-distributions as a well as
corresponding plot. Both tibbles contain the full support of our
variable of interest, that is, their `support` column is the same.

<div class="center">

<strong> Pre-Distribution </strong>

</div>

<div class="column-left">

| support | count |
|--------:|------:|
|       1 |     9 |
|       2 |     7 |
|       3 |     7 |
|       4 |     6 |
|       5 |     9 |
|       6 |     0 |
|       7 |     0 |
|       8 |     0 |
|       9 |     0 |
|      10 |     0 |

</div>

<div class="column-right">

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

</div>

<div class="clearer">

</div>

<div class="center">

<strong> Post-Distribution </strong>

</div>

<div class="column-left">

| support | count |
|--------:|------:|
|       1 |     0 |
|       2 |     0 |
|       3 |     0 |
|       4 |     0 |
|       5 |     0 |
|       6 |     9 |
|       7 |     7 |
|       8 |     7 |
|       9 |     6 |
|      10 |     9 |

</div>

<div class="column-right">

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

</div>

<div class="clearer">

</div>

Observe that the post-distribution is simply the result of shifting the
mass in the pre-distribution by 5 units. For instance, all the mass that
was given given to 1 in the pre-distribution is now given to 6.

### Compute Transport Cost

We can compute the transport cost as follows:

    tc <- get_results(pre_main = pre_distribution, post_main = post_distribution,
                      estimator = "tc", var = support,
                      bandwidth = 0)
    #> Computing Transport Costs...
    #> ================================================================================
    #> The transport cost for the specified bandwidths have been computed.

    tc
    #>   bandwidth main
    #> 1         0    1

The transport cost is 100%, which makes sense since all the mass in the
pre-distribution moves to values of the support that did not have any
mass in the post-distribution, i.e. each unit of mass was transported.
The `estimator` argument is specified to be `"tc"`, which stands for
transport cost. (Since we only have two distributions, estimator will
take on the value of `"tc"` by default.)

Since the post-distribution is a 5-unit shift in the pre-distribution,
any bandwidth at least 5 must result in a transport cost of 0. We verify
this by computing the transport cost for a sequence of bandwiths from 0
to 10:

    tc <- get_results(pre_main = pre_distribution, post_main = post_distribution,
                      estimator = "tc", var = support,
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

To provide a bit more context to our synthetic example, suppose that the
shift in the post-distribution of our variable of interest is considered
to be the result of some treatment. Perhaps there are unobserved trends
in our variable that might also explain this shift. To elicit how much
of this shift is due to the treatment and how much of this shift is due
to unobserved trends, we can compare our distributions above with
distributions of the same variable measured in some control group that
did not receive the treatment but was also subjected to the same
unobserved trends.

The novel differences-in-transports estimator described in Daljord et
al. (2020) quantifies the change in distribution due to some treatment
relative to the change in distribution of some control group.

First, we need to set up the distributions of our variable for our
control group, both before and after the treatment was administered to
the treatment group. For our purposes, we let the pre-distribution of
our control group be the same as that of our treated group. Then, let
our post-distribution for our control group be a 2-unit shift of our
pre-distribution. We therefore have the following tibbles that represent
our control group distributions:

<div class="center">

<strong> Pre-Distribution </strong>

</div>

<div class="column-left">

| support | count |
|--------:|------:|
|       1 |     9 |
|       2 |     7 |
|       3 |     7 |
|       4 |     6 |
|       5 |     9 |
|       6 |     0 |
|       7 |     0 |
|       8 |     0 |
|       9 |     0 |
|      10 |     0 |

</div>

<div class="column-right">

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

</div>

<div class="clearer">

</div>

<div class="center">

<strong> Post-Distribution </strong>

</div>

<div class="column-left">

| support | count |
|--------:|------:|
|       1 |     0 |
|       2 |     0 |
|       3 |     9 |
|       4 |     7 |
|       5 |     7 |
|       6 |     6 |
|       7 |     9 |
|       8 |     0 |
|       9 |     0 |
|      10 |     0 |

</div>

<div class="column-right">

<img src="man/figures/README-unnamed-chunk-13-1.png" width="100%" />

</div>

<div class="clearer">

</div>

### Compute Differences-in-Transports Estimator

There is more nuance to computing the differences-in-transports
estimator than is presented in this expository document. See Daljord et
al. (2020) for a more complete treatment on how to compute the
differences-in-transports estimator.

Above, we used `estimator = "tc"` and specified our pre- and
post-distributions for our treated group. Now, we use
`estimator = "dit"` (which stands for differences-in-treatments
estimator) while also specifying our pre- and post-distributions for our
control group.

    dit <- get_results(pre_main = pre_distribution, post_main = post_distribution,
                       pre_control = pre_control, post_control = post_control,
                       estimator = "dit", var = support,
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
this result, we use `save_dit = TRUE`.

Thus, the differences-in-transports estimator at an optimal bandwidth of
2 is 61%. Often times, the data will be a sample from a population, and
sample variability is enough to cause differences between distributions.
To account for this discrepancy, we can increase the bandwidth to ignore
transfers of mass between nearby values of the support.

    d_a <- 3

While Daljord et al. (2020) offer a disciplined way to determine what
the appropriate bandwidths are, for simplicity, we will suppose that
there will not be any sampling variation with a bandwidth greater than
3.

    dit <- get_results(pre_main = pre_distribution, post_main = post_distribution,
                       pre_control = pre_control, post_control = post_control,
                       estimator = "dit", var = support,
                       bandwidth_seq = seq(d_a, 10, 1), # smallest bandwidth will be 1
                       save_dit = TRUE)
    #> Computing Differences-in-Transports Estimator...
    #> ================================================================================
    #> The non-conservative diff-in-transports estimator is 0.421052631578947 at d = 3
    dit$out
    #>   bandwidth      main control      diff
    #> 1         3 0.4210526       0 0.4210526
    #> 2         4 0.2368421       0 0.2368421
    #> 3         5 0.0000000       0 0.0000000
    #> 4         6 0.0000000       0 0.0000000
    #> 5         7 0.0000000       0 0.0000000
    #> 6         8 0.0000000       0 0.0000000
    #> 7         9 0.0000000       0 0.0000000
    #> 8        10 0.0000000       0 0.0000000
    dit$dit
    #> [1] 0.4210526
    dit$optimal_bandwidth
    #> [1] 3

Accounting for sampling variability, our differences-in-transports
estimate is 42%. If we want to be more conservative, we can use
`conservative = TRUE`, which essentially uses twice the bandwidth for
computing the treated group’s transport costs relative to the bandwidth
for computing the control group’s transport costs.

    dit <- get_results(pre_main = pre_distribution, post_main = post_distribution,
                       pre_control = pre_control, post_control = post_control,
                       estimator = "dit", var = support,
                       bandwidth_seq = seq(d_a, 10, 1), # smallest bandwidth will be 1
                       save_dit = TRUE,
                       conservative = TRUE)
    #> Computing Differences-in-Transports Estimator...
    #> Note: you are using `conservative = T`.
    #> ================================================================================
    #> The conservative diff-in-transports estimator is 0 at d = 3
    dit$out
    #>   bandwidth      main main2d control      diff diff2d
    #> 1         3 0.4210526      0       0 0.4210526      0
    #> 2         4 0.2368421      0       0 0.2368421      0
    #> 3         5 0.0000000      0       0 0.0000000      0
    #> 4         6 0.0000000      0       0 0.0000000      0
    #> 5         7 0.0000000      0       0 0.0000000      0
    #> 6         8 0.0000000      0       0 0.0000000      0
    #> 7         9 0.0000000      0       0 0.0000000      0
    #> 8        10 0.0000000      0       0 0.0000000      0
    dit$dit
    #> [1] 0
    dit$optimal_bandwidth
    #> [1] 3

Finally, we have that the conservative differences-in-transports
estimator is 0% at an optimal bandwidth of 3.

Note that `estimator = "dit"` is not necessary. Since we have two sets
of pre- and post-distributions, `get_results` is smart enough to return
the differences-in-transports estimator by default.
