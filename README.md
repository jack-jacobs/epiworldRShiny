# epiworldRShiny: An RShiny Application for the epiworldR Package

<!-- badges: start -->
<!-- badges: end -->

To run this ShinyApp, you need to type the following:

```r
epiworldRShiny()
```

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
- One model can include multiple diseases.
- Policies (tools) can be multiple and user-defined.
- Transmission can be a function of agents’ features.
- Out-of-the-box parallelization for multiple simulations.

From package description of <a href="https://github.com/UofUEpiBio/epiworldR"
target="_blank">epiworldR</a>:

> A flexible framework for Agent-Based Models (ABM), the epiworldR
> package provides methods for prototyping disease outbreaks and
> transmission models using a C++ backend, making it very fast. It
> supports multiple epidemiological models, including the
> Susceptible-Infected-Susceptible (SIS), Susceptible-Infected-Removed
> (SIR), Susceptible-Exposed-Infected-Removed (SEIR), and others,
> involving arbitrary mitigation policies and multiple-disease models.
> Users can specify infectiousness/susceptibility rates as a function of
> agents’ features, providing great complexity for the model dynamics.
> Furthermore, epiworldR is ideal for simulation studies featuring large
> populations.


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
