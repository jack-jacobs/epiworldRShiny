shiny_sirconn <- function(input) {

  model_sirconn <- ModelSIRCONN(
    name              = input$sirconn_disease_name,
    prevalence        = input$sirconn_prevalence,
    transmission_rate = input$sirconn_transmission_rate,
    recovery_rate     = input$sirconn_recovery_rate,
    contact_rate      = input$sirconn_contact_rate, 
    n                 = input$sirconn_population_size
    )

  # Running and printing
  verbose_off(model_sirconn)
  run(model_sirconn, ndays = input$sirconn_n_days, seed = input$sirconn_seed)
  
  # Plot, summary, and reproductive number
  plot_sirconn <- function() plot(model_sirconn, main = "SIR connected Model")
  
  summary_sirconn <- function() summary(model_sirconn)
  
  reproductive_sirconn <- function()
    plot_reproductive_number(
      model_sirconn,
      main = "SIR connected Model Reproductive Number"
      )

  # Output list
  return(
    list(
      epicurves_plot    = plot_sirconn,
      reproductive_plot = reproductive_sirconn,
      model_summary     = summary_sirconn
    )
  )
}