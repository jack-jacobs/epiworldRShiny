
library(shiny)
library(epiworldR)

# Models are individually defined in the models folder
for (f in list.files("models", full.names = TRUE)) {
  source(f)
}

function(input, output) {
  
    model_output <- eventReactive(
      eventExpr = input$simulate, 
      valueExpr = {

        if(input$model == "SEIR") {

            return(shiny_seir(input))
            
        } else if(input$model == "SIR") {

            return(shiny_sir(input))

        } else if (input$model == "SIS") {

            return(shiny_sis(input))
            
        } else if (input$model == "SEIRCONNECTED") {

            return(shiny_seirconn(input))

        } else {
          stop("No model selected")
        }
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
    # output$model_table <- renderPrint({
    #       model_output()$data_table_seir()
    # })
}
