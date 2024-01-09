
<!-- README.md is generated from README.Rmd. Please edit that file -->

# verbose

<!-- badges: start -->
<!-- badges: end -->

The goal of verbose package is to manage the verbosity of R packages.

## Installation

You can install the development version of verbose from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pawelqs/verbose")
```

## Example

Let’s assume that we want to implement a foo package and be able to
globally set the verbosity level for all foo functions. We can use the
verbose package to set the verbosity level for the foo package.

``` r
library(verbose)

verbose(foo = 1)
```

Now let’s develop a foo function which will get the default verbosity
level using th `verbose()` function.

``` r
foo <- function(verbose = verbose::verbose("foo")) {
  if (verbose > 0) {
    message("Hello world!")
  }
}

foo()
#> Hello world!
```

Now, let’s turn off the verbosity for the foo package.

``` r
verbose(foo = 0)
foo()
```
