library(shiny)
library(tidyverse)
library(shinydashboard)

# library(fresh)
# my_theme = create_theme(
#   adminlte_color(
#     light_blue = "#4898a8"
#   )
# )

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
  model_name, rate_name, value
  ) {
  sliderInput(
    inputId = paste(
      model_name, gsub("\\s+", "_", tolower(rate_name)),
      sep = "_"
    ),
    label = rate_name,
    value = value,
    min   = 0, 
    max   = 1,
    step  = 0.01,
    ticks = FALSE
  )
}

network_and_seed_input <- function(model_name) {

  c(
    headerPanel(h3("Small World Population")),
    sliderInput(
      inputId = paste0(model_name, "_population_size"),
      label   = "Population Size",
      min     = 0, 
      max     = 100000, 
      value   = 50000, 
      step    = 1000,
      ticks   = FALSE
      ),
    numericInput(
      inputId = paste0(model_name, "_k"),
      label   = "Number of Ties", 
      min     = 0, 
      max     = NA, 
      step    = 1,
      value   = 5
      ),
    selectInput(
      inputId  = paste0(model_name, "_directed"),
      label    = "Directed",
      choices  = c("TRUE", "FALSE"),
      selected = "FALSE"
      ), 
    sliderInput(
      inputId = paste0(model_name, "_prob_rewiring"),
      label   = "Probability of Rewiring",
      value   = "0.01",
      min     = 0, 
      max     = 1,
      step    = 0.01,
      ticks   = FALSE
      ),
    numericInput(
      inputId = paste0(model_name, "_seed"),
      label   = "Seed (Optional)", 
      min     = 0, 
      max     = NA, 
      step    = 1,
      value   = 2023
      )
      )

}

header <- dashboardHeader(title="epiworldR")

sidebar <- dashboardSidebar(
  width = 273,
  tags$style(
      "#sidebarItemExpanded {
          overflow: auto;
          max-height: 100vh;
      }"
  ),
  selectInput("model",
    label = h3("Model"),
    choices = c("SEIR", "SIR", "SIS", "SEIRCONNECTED", "SIRCONNECTED",
      "SEIRD", "SIRD", "SISD", "SEIRDCONNECTED", "SIRDCONNECTED", "SURV"
      ),
    ),
  
  # Only show this panel is Model is SEIR
  conditionalPanel(
    condition = "input.model == 'SEIR'",
    text_input_disease_name("seir"),
    slider_prevalence("seir"),
    slider_input_rate("seir", "Transmission Rate", "0.5"),
    slider_input_rate("seir", "Recovery Rate", "0.14"),
    numericInput(
      inputId = "seir_incubation_days",
      label   = "Incubation Days",
      value   = "7",
      min     = 0, 
      max     = NA,
      step    = 1
      ),
    numeric_input_ndays("seir"),
    network_and_seed_input("seir")
  ),
  # Only show this panel is Model is SIR
  conditionalPanel(
    condition = "input.model == 'SIR'",
    text_input_disease_name("sir"),
    slider_prevalence("sir"),
    slider_input_rate("sir", "Transmission Rate", "0.5"),
    slider_input_rate("sir", "Recovery Rate", "0.14"),
    numeric_input_ndays("sir"),
    network_and_seed_input("sir")
  ),
  # Only show this panel is Model is SIS
  conditionalPanel(
    condition = "input.model == 'SIS'",
    text_input_disease_name("sis"),
    slider_prevalence("sis"),
    slider_input_rate("sis", "Transmission Rate", "0.5"),
    slider_input_rate("sis", "Recovery Rate", "0.14"),
    numeric_input_ndays("sis"),
    network_and_seed_input("sis")
  ),
  actionButton("simulate", "Run Simulation")
)

body <- dashboardBody(
  plotOutput("model_plot"),
  verbatimTextOutput("model_summary") 
)

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "black",
)


