
<!-- README.md is generated from README.Rmd. Please edit that file -->

# KMeansStability

<!-- badges: start -->
<!-- badges: end -->

The goal of KMeansStability is to implement the functions necessary to
conduct a small simulation study comparing the efficacy of a set of
stability-based methods for specifying for $K$-means clustering. In
brief, stability-based specification methods are based on the principle
that a correct specification of should induce the most reproducible
clusters: if one were to repeatedly sample from the population, the
clusterings tuned to the correct value of would be more similar compared
to clusterings with inappropriate values of $K$. To leverage this
intuition into a method, one emulates repeated samplings through
resampling from a given dataset and defines a distance metric over the
resampled clusterings. The value of $K$ which yields the smallest
average distance between the clusters is deemed the most stable tuning
of $K$. The choice of distance metric is essential to defining the most
stable value of $K$. KMeansStability implements two common stability
metrics, Minimum Matching Distance and Comembership Distance, along with
three novel distances based on the Wasserstein distance between
different components of the clusterings, within a resampling framework
which accomodates either cross-validation or the bootstrap for
resampling.

## Installation

You can install the development version of KMeansStability from GitHub
with:

``` r
# install.packages("devtools")
devtools::install_github("wolfgangkbri/KMeansStability")
#> Downloading GitHub repo wolfgangkbri/KMeansStability@HEAD
#> 
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file ‘/private/var/folders/0x/clg_bd7156z9wp_60zk8tdkr0000gn/T/RtmpzqGMII/remotes9d2e5ed99db8/wolfgangkbri-KMeansStability-9e58d05/DESCRIPTION’ ... OK
#> * preparing ‘KMeansStability’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> Omitted ‘LazyData’ from DESCRIPTION
#> * building ‘KMeansStability_0.1.0.tar.gz’
#> Installing package into '/private/var/folders/0x/clg_bd7156z9wp_60zk8tdkr0000gn/T/RtmpwjVOEF/temp_libpath9a99b936719'
#> (as 'lib' is unspecified)
```

## Example

The main function in KMeansStability is stability_analysis. This
function evaluates a set of five different stability metrics across a
range of $K$-means clusterings.

``` r
library(KMeansStability)
## basic example code
set.seed(3)

# samples from a mixture distribution with three clusters
df <- toy_datagen() 

results <- stability_analysis(df, Kmax = 6, method = "CV", nreps = 4)
results$mean_stabilities
#>    stab_metric
#> K         wd     wd_cb     wd_cp   mmd     cd
#>   2 5.281103 2.5619288 2.2666139 0.340 0.3676
#>   3 1.417943 0.2391401 0.5920472 0.005 0.0074
#>   4 5.024423 1.1161138 0.9185044 0.240 0.1242
#>   5 8.838908 1.6673312 1.0441221 0.370 0.1862
#>   6 5.030682 0.6964552 0.7962646 0.160 0.0676
```

With this dataset, we have that all five of the stability metrics agree
that setting $K=3$ induces the most stable clustering, correctly
identifying the number of components of the mixture model forming the
dataset.
