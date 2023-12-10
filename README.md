
<!-- README.md is generated from README.Rmd. Please edit that file -->

# KMeansStability

<!-- badges: start -->
<!-- badges: end -->

The goal of KMeansStability is to

## Installation

You can install the development version of KMeansStability from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("wolfgangkbri/KMeansStability")
```

## Example

The main function in `KMeansStability` is `stability_analysis`. This
function evaluates a set of five stability metrics across a range of
Kmeans clusterings. Smaller values for these stability metrics indicate
a more stable clustering. One can use stability as a means of specifing
$K$.

``` r
library(KMeansStability)
## basic example code
set.seed(3)
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

In this setting, we have that all five of the stability metrics agree
that setting K=3 induces the most stable clustering.

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
