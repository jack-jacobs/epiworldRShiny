
#' pop_generator Function
#'
#' This function generates a population matrix with specified characteristics.
#'
#' @param n The number of individuals in the population.
#' @param prop_hispanic The proportion of individuals who are Hispanic. Default is 0.5.
#' @param prop_female The proportion of individuals who are female. Default is 0.5.
#' @param prop_19_59_60plus A vector of length 3 representing the proportions of individuals in the age groups 0-19, 20-59, and 60+. Default is c(0.5, 0.3, 0.2).
#'
#' @return A matrix representing the generated population, with columns for age groups (0-19, 20-59, 60+), NotHispanic, and Female.
#'
#' @examples
#' pop_generator(100)
#' pop_generator(100, prop_hispanic = 0.3, prop_female = 0.6, prop_19_59_60plus = c(0.4, 0.4, 0.2))
#'
#' @export
pop_generator <- function(
  n, 
  prop_hispanic = .5,
  prop_female   = .5,
  prop_19_59_60plus = c(.3, .6)
  ) {

  prop_19_59_60plus <- c(
    prop_19_59_60plus[1],
    diff(prop_19_59_60plus),
    1 - prop_19_59_60plus[2]
  )

  agegroups <- sample.int(
    3, size = n, replace = TRUE,
    prob = prop_19_59_60plus
    )

  X <- matrix(0, nrow = n, ncol = 3)
  X[cbind(1:n, agegroups)] <- 1
  colnames(X) <- c("0-19", "20-59", "60+")

  cbind(
    X,
    NotHispanic = sample.int(
      2, size = n, replace = TRUE,
      prob = c(1 - prop_hispanic, prop_hispanic)
      ) - 1,
    Female   = sample.int(
      2, size = n, replace = TRUE,
      prob = c(1 - prop_female, prop_female)
      ) - 1
  )

}