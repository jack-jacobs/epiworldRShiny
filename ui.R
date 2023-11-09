
library(shiny)
library(tidyverse)
library(shinydashboard)
library(DT)
library(ggplot2)

# library(fresh)
# my_theme = create_theme(
#   adminlte_color(
#     light_blue = "#4898a8"
#   )
# )

# Creating an environment available for the simulation.
# This will serve in case we need to store anything that should
# be available globaly.
epiworldR_env <- new.env(parent = .GlobalEnv)
assign("epiworldR_env", epiworldR_env, envir = .GlobalEnv)

# Models are individually defined in the models folder
source("R/dashboard-helpers.R")

models <- list.files("models", full.names = TRUE)
for (f in models) {
  source(f)
}

# Capturing alt names
models_names <- sapply(models, \(f) {
  
  # Only on the first line
  altname <- readLines(f, n = 1)

  # Is there any alt name?
  altname <- gsub("^#\\s*alt-name[:]\\s*(.+)\\s*$", "\\1", altname, perl = TRUE)

  # If there is no alt name, use the file name
  if (altname == "") {
    altname <- toupper(gsub("shiny_([^.]+).R", "\\1", basename(f)))
  }
  altname

})

models <- gsub("^.+shiny_([^.]+).R$", "\\1", models)
names(models_names) <- models
names(models) <- models_names

# Saving in the global environment
epiworldR_env$models_names <- models_names
epiworldR_env$models <- models

# Doing some hacking
models_panels <- mget(paste0(models, "_panel"), envir = .GlobalEnv)
invisible({
  Map(\(pfun, nam, id) {
    assign(paste0(id, "_panel"), pfun(nam), envir = .GlobalEnv)
  }, pfun = models_panels, nam = models_names, id = models
  )
})

# Non-pharmacological interventions are defined in the npi folder
source("R/npi.R")

header <- dashboardHeader(
  title="epiworldR"
  )

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
    choices = unname(models_names)
    ),
  # Models conditional panels
  seir_panel,
  sir_panel,
  sis_panel,
  seirconn_panel,
  sirconn_panel,
  sird_panel,
  sisd_panel,
  seirconnequity_panel,
  actionButton("simulate", "Run Simulation")
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