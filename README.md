# epiworldRShiny: An RShiny Application for the epiworldR Package

<!-- badges: start -->
[![R-CMD-check](https://github.com/UofUEpiBio/epiworldRShiny/actions/workflows/r.yml/badge.svg)](https://github.com/UofUEpiBio/epiworldRShiny/actions/workflows/r.yml)
<!-- badges: end -->

This R package provides a user-friendly application for 
<a href="https://github.com/UofUEpiBio/epiworldR"
target="_blank">epiworldR</a>,
a wrapper of the C++ library
<a href="https://github.com/UofUEpiBio/epiworld"
target="_blank">epiworld</a>. It provides a general framework for
modeling disease transmission using <a
href="https://en.wikipedia.org/w/index.php?title=Agent-based_model&amp;oldid=1153634802"
target="_blank">agent-based models</a>. Some of the main features
include:

- Fast simulation with an average of 30 million agents/day per second.
- 9 different epidemiological models to choose from.
- Built-in capability for user-defined interventions. 
- Built-in capability to define population and disease parameters.
- Informative visualizations and tables provided after running each simulation.

## Installation

You can install the development version of epiworldRShiny from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("UofUEpiBio/epiworldRShiny")
```

Or from CRAN

``` r
install.packages("epiworldRShiny")
```
To run this ShinyApp, you need to type the following:

```r
epiworldRShiny()
```

## Examples
