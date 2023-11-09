
library(shiny)
library(tidyverse)
library(shinydashboard)
library(DT)
library(ggplot2)
library(shinyjs)
library(epiworldR)

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

header <- dashboardHeader(
  title="epiworldR"
  )

sidebar <- do.call(
  "dashboardSidebar",
  c(
    list(
      width = NULL,
      tags$style(
        "#sidebarItemExpanded {
            overflow: auto;
            max-height: 100vh;
        }
        #npis_header, #network_header {
          cursor: pointer;
        }
        "
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
    column(6, plotOutput("model_plot")),
    column(6, plotOutput("model_reproductive_plot"))
  ),
  HTML("<br>"),
  fluidRow(
    column(6, verbatimTextOutput("model_summary")),
    column(6, dataTableOutput("model_table"))
  ),
  downloadButton("downloadData", "Download Data"),
  tags$style(type='text/css', "#downloadData { margin-bottom: 100px; float: right}")
  

)

ui <- dashboardPage(
  header  = header,
  sidebar = sidebar,
  body    = body,
  skin    = "black"
)
