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

The model of choice is a SEIR Network model. Notice the day of peak infections occurs on day 12, maxing at about 18,000 infections. After
roughly 40 days, the state's curves taper off, meaning that the majority of the population has recovered from the disease. The reproductive
number plot demonstrates that the disease spread rapidly at the beginning of the simulation, and drastically decreased over the first 10 days. 
The model summary returns important information about the simulation such as the model choice, population size, simulation speed, disease(s)
present, any tool(s) present, and model parameters. The distribution table displays the counts for each state at baseline and conclusion. 
The transition probabilities table displays the probability of moving between states. For example, the probability that a susceptible 
agent remains in the susceptible state is 0.62, with a probability of moving to the exposed state 0.38. Lastly, the counts table 
shows the state's counts over time, marking the peak infection count in bold letters.

<center>
![ex1](https://github.com/UofUEpiBio/epiworldRShiny/assets/105825983/1f35d22c-c8d7-420b-8a02-f5c4cf77f971)
</center>




