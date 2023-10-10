
library(shiny)
library(tidyverse)
library(shinydashboard)
library(DT)

# library(fresh)
# my_theme = create_theme(
#   adminlte_color(
#     light_blue = "#4898a8"
#   )
# )

source("R/dashboard-helpers.R")

header <- dashboardHeader(title="epiworldR")

sidebar <- dashboardSidebar(
  width = 273,
  tags$style(
      "#sidebarItemExpanded {
          overflow: auto;
          max-height: 100vh;
      }"
  ),

  # Sidebar panel
  selectInput(
    inputId = "model",
    label   = h3("Model"),
    choices = c("SEIR", "SIR", "SIS", "SEIRCONNECTED", "SIRCONNECTED",
      "SEIRD", "SIRD", "SISD", "SEIRDCONNECTED", "SIRDCONNECTED", "SURV"
      )
    ),
  
  # Only show this panel is Model is SEIR
  conditionalPanel(
    condition = "input.model == 'SEIR'",
    text_input_disease_name("seir"),
    slider_prevalence("seir"),
    slider_input_rate("seir", "Transmission Rate", "0.05"),
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
    network_input("seir"),
    seed_input("seir")
  ),
  # Only show this panel is Model is SIR
  conditionalPanel(
    condition = "input.model == 'SIR'",
    text_input_disease_name("sir"),
    slider_prevalence("sir"),
    slider_input_rate("sir", "Transmission Rate", "0.05"),
    slider_input_rate("sir", "Recovery Rate", "0.14"),
    numeric_input_ndays("sir"),
    network_input("sir"),
    seed_input("sir")
  ),
  # Only show this panel is Model is SIS
  conditionalPanel(
    condition = "input.model == 'SIS'",
    text_input_disease_name("sis"),
    slider_prevalence("sis"),
    slider_input_rate("sis", "Transmission Rate", "0.05"),
    slider_input_rate("sis", "Recovery Rate", "0.14"),
    numeric_input_ndays("sis"),
    network_input("sis"),
    seed_input("sis")
  ),
  # SEIRCONN panel
  conditionalPanel(
    condition = "input.model == 'SEIRCONNECTED'",
    text_input_disease_name("seirconn"),
    slider_prevalence("seirconn"),
    slider_input_rate("seirconn", "Transmission Rate", "0.1"),
    slider_input_rate("seirconn", "Recovery Rate", "0.14"),
    slider_input_rate("seirconn", "Contact Rate", "4", maxval = 20),
    numericInput(
      inputId = "seirconn_incubation_days",
      label   = "Incubation Days",
      value   = "7",
      min     = 0, 
      max     = NA,
      step    = 1
      ),
    sliderInput(
      inputId = "seirconn_population_size",
      label   = "Population Size",
      min     = 0, 
      max     = 100000, 
      value   = 50000, 
      step    = 1000,
      ticks   = FALSE
    ),
    numeric_input_ndays("seirconn"),
    seed_input("seirconn")
  ),
  actionButton("simulate", "Run Simulation")
)

body <- dashboardBody(
  fluidRow(
    column(6, plotOutput("model_plot")),
    column(6, plotOutput("model_reproductive_plot"))
  ),
  HTML("<br>"),
  fluidRow(
    column(6, verbatimTextOutput("model_summary")) #,
    #column(6, dataTableOutput("model_table"))
  )

)

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "black"
)