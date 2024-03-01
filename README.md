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
### Example #1
This first example demonstrates how to run the Shiny app, run a simulation, and observe results. Notice the sidebar 
contains many disease and model parameters which can be altered. Changing these parameters will affect the spread of the infectious 
disease in the simulated population. After running the simulation, a plot of the distribution of states over time, a plot 
of the disease's reproductive number over time, a model summary, and table of states counts over time are displayed.

This example features:
- SEIR network model for COVID-19
- Day of peak infections occurs on day 12, maxing at about 18,000 infections.
- The disease spreads rapidly at the beginning of the simulation, and drastically decreased over the first 10 days. 
- Model summary
- State counts table

![ex1](https://github.com/UofUEpiBio/epiworldRShiny/assets/105825983/1f35d22c-c8d7-420b-8a02-f5c4cf77f971)

### Example #2
This example features the implementation of the vaccine and school closure interventions to curb disease spread. All model 
output can be interpreted using the same logic from example #1.

Key features:
- SEIRD network model for COVID-19
- Vaccine prevalence = 70%
- School closure prevalance = 50%
- Day of school closure implementation = 7
- Significantly decreased number of infections and deaths.
- Majority of population recovered or susceptible by day 30.

![ex2](https://github.com/UofUEpiBio/epiworldRShiny/assets/105825983/784bfe51-68fd-4ab4-a401-080f5e6e087f)

### Example #3




