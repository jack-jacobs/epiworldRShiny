library(shiny)
library(epiworldR)
library(shinyjs)

# Models are individually defined in the models folder
for (f in list.files("models", full.names = TRUE)) {
  source(f)
}

# Non-pharmacological interventions are defined in the npi folder
source("R/npi.R")

# Creating an environment available for the simulation.
# This will serve in case we need to store anything that should
# be available globaly.
epiworldR_env <- new.env(parent = .GlobalEnv)

function(input, output) {
  
  model_output <- eventReactive(
    eventExpr = input$simulate, 
    valueExpr = {

      model_output <- switch(input$model,
        "SEIR"           = shiny_seir(input),
        "SIR"            = shiny_sir(input),
        "SIS"            = shiny_sis(input),
        "SEIRCONNECTED"  = shiny_seirconn(input),
        "SIRCONNECTED"   = shiny_sirconn(input),
        "SIRD"           = shiny_sird(input),
        "SISD"           = shiny_sisd(input),
        "SEIRCONNEQUITY" = shiny_seirconnequity(input),
        stop("No model selected")
        )

  })

  # Displaying Plots and Model Summary
  output$model_plot <- renderPlot({
    model_output()$epicurves_plot()
  })
  output$model_reproductive_plot <- renderPlot({
    model_output()$reproductive_plot()
  })
  output$model_summary <- renderPrint({
    model_output()$model_summary()
  })
  output$model_table <- renderDataTable({
    model_output()$model_table()
  }, options = list(
       scrollY = '550px',
       lengthMenu = c(16, 25, 50), # Set the default value to 15
       pageLength = 16))
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(model_output()$model_table(), file)
    }
  )
}
