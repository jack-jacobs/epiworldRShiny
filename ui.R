
library(shiny)
library(tidyverse)
library(shinydashboard)
library(DT)
library(ggplot2)
library(shinyjs)
library(epiworldR)
library(shinycssloaders)

# library(fresh)
# my_theme = create_theme(
#   adminlte_color(
#     light_blue = "#4898a8"
#   )
# )

# Creating an environment available for the simulation.
# This will serve in case we need to store anything that should
# be available globaly.
assign("epiworldR_env", new.env(parent = .GlobalEnv), envir = .GlobalEnv)

# Models are individually defined in the models folder
source("R/ui-functions.R")

# Loads the panels and the model names
models_setup()

# Loads the Non-pharmacological interventions are defined in the npi folder
source("R/npi.R")

# Functions to generate population
source("R/server-functions.R")


header <- dashboardHeader(
  title=
    HTML('epiworldR <text style="color: gray; font-size:50%">(alpha)</text>')
  )

# Preparing CSS for cursor pointer
cursor_header_pointer <-
  sprintf(
    "#npis_header_%1$s, #network_header_%1$s, #population_header_%1$s",
    epiworldR_env$models
    ) |>
  paste0(collapse = ", ") |>
  paste("{\n  cursor: pointer;\n}\n")

sidebar <- do.call(
  "dashboardSidebar",
  c(
    list(
      width = NULL,
      tags$style(
        paste0(
          "#sidebarItemExpanded {overflow: auto;max-height: 100vh;}",
          cursor_header_pointer
        )
      ),
      # To activate shinyJs
      useShinyjs(),
      # Sidebar panel
      selectInput(
        inputId = "model",
        label   = h3("Model"),
        choices = unname(epiworldR_env$models_names)
        )
    ),
    # Loads the panels for all existing models
    mget(paste0(epiworldR_env$models, "_panel"), envir = .GlobalEnv)
  )
)

body <- dashboardBody(
  fluidRow(
    column(12, htmlOutput("model_description"))
  ),
  fluidRow(
    column(6, plotOutput("model_plot") %>% withSpinner(color="#009bff")),
    column(6, plotOutput("model_reproductive_plot") %>% withSpinner(color="#009bff"))
  ),
  HTML("<br>"),
  fluidRow(
    column(6, verbatimTextOutput("model_summary") %>% withSpinner(color="#009bff")),
    column(6, dataTableOutput("model_table") %>% withSpinner(color="#009bff"))
  ),
  htmlOutput("download_button"),
  tags$style(type='text/css', "#downloadData {position: fixed; bottom: 20px; right: 20px; }"),
  fluidRow(
    column(6, markdown("epiworldRShiny app version 0.0-1 (alpha)")),
    column(6, markdown("**The University of Utah**"))
  )
)

ui <- dashboardPage(
  header  = header,
  sidebar = sidebar,
  body    = body,
  skin    = "black"
)
