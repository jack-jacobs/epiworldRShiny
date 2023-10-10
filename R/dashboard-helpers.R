text_input_disease_name <- function(model_name) {
  textInput(
    inputId     = paste0(model_name, "_disease_name"),
    label       = "Disease",
    value       = "",
    placeholder = "Please enter a disease name"
  )
}

slider_prevalence <- function(model_name) {
  sliderInput(
    paste0(model_name, "_prevalence"),
    label = "Disease Prevalence",
    value = "0.1",
    min = 0, 
    max = 1,
    step = 0.01,
    ticks = FALSE
    )
}

numeric_input_ndays <- function(model_name) {
  numericInput(
    inputId = paste0(model_name, "_n_days"),
    label   = "Simulation Time (Days)",
    value   = "100",
    min     = 0, 
    max     = NA,
    step    = 1
    )
}

slider_input_rate <- function(
  model_name, rate_name, value, maxval = 1
  ) {
  sliderInput(
    inputId = paste(
      model_name, gsub("\\s+", "_", tolower(rate_name)),
      sep = "_"
    ),
    label = rate_name,
    value = value,
    min   = 0, 
    max   = maxval,
    step  = 0.01,
    ticks = FALSE
  )
}

network_input <- function(model_name) {

  c(
    headerPanel(h3("Small World Population")) |> as.character(),
    sliderInput(
      inputId = paste0(model_name, "_population_size"),
      label   = "Population Size",
      min     = 0, 
      max     = 100000, 
      value   = 50000, 
      step    = 1000,
      ticks   = FALSE
      ) |> as.character(),
    numericInput(
      inputId = paste0(model_name, "_k"),
      label   = "Number of Ties", 
      min     = 0, 
      max     = 500, 
      step    = 1,
      value   = 20
      ) |> as.character(),
    selectInput(
      inputId  = paste0(model_name, "_directed"),
      label    = "Directed",
      choices  = c("TRUE", "FALSE"),
      selected = "FALSE"
      ) |> as.character(), 
    sliderInput(
      inputId = paste0(model_name, "_prob_rewiring"),
      label   = "Probability of Rewiring",
      value   = "0.20",
      min     = 0, 
      max     = 1,
      step    = 0.01,
      ticks   = FALSE
      ) |> as.character()
    ) |> paste(collapse = "\n") |> HTML()
}

seed_input <- function(model_name) {
  numericInput(
    inputId = paste0(model_name, "_seed"),
    label   = "Seed (Optional)", 
    min     = 0, 
    max     = NA, 
    step    = 1,
    value   = 2023
    )
}