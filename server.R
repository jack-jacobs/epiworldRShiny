function(input, output) {

  # Function to make it easier to hide some options
  onclick(id = "npis_header", toggle(id = "npis_inputs", anim = TRUE))
  onclick(id = "network_header", toggle(id = "network_inputs", anim = TRUE))
        
  model_output <- eventReactive(
    eventExpr = input$simulate, 
    valueExpr = {

      eval(parse(text = paste0("shiny_", globalenv()$epiworldR_env$models[input$model], "(input)")))

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
       pageLength = 16), escape = FALSE)
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(model_output()$model_table(), file)
    }
  )
}
