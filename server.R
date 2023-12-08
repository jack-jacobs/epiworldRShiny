function(input, output) {

  # Function to make it easier to hide some options
  # This is a workaround for the fact that a simpler for loop (without the local)
  # was not working.
  # See the following issue:
  # https://github.com/daattali/shinyjs/issues/167#issuecomment-409475023
  for (i in c("npis", "network", "population")) {
    for (m in epiworldR_env$models) {
      local({
        id0 <- paste0(i, "_header_", m)
        id1 <- paste0(i, "_inputs_", m)
        onclick(id = id0, toggle(id = id1, anim = TRUE))
      })
    }
  }

  # Get model id
  model_id <- reactive(globalenv()$epiworldR_env$models[input$model])
  
  # Running the model
  model_output <- eventReactive(
    eventExpr = input[[paste0("simulate_", model_id())]],
    valueExpr = {

      eval(parse(text = paste0("shiny_", model_id(), "(input)")))

  })

  # Displaying Model Description
  # Only if the file exists
  output$model_description <- renderText({

    fn <- paste0("models/shiny_", model_id(), ".md")
    contents <- if (file.exists(fn))
      readLines(fn, warn = FALSE)
    else
      "No description available."

    markdown(contents)

  })

  # Displaying Plots and Model Summary
  output$model_plot <- renderPlotly({
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
       scrollY = '600px',
       lengthMenu = c(16, 25, 50), # Set the default value to 16
       pageLength = 16), escape = FALSE)
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(model_output()$model_table(), file)
    }
  )

  output$download_button <- renderUI({
    downloadButton("downloadData", "Download Data")
  })
}
