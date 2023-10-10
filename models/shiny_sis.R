shiny_sis <- function(input) {
  
  # Creating model
  model_sis <- ModelSIS(
    name              = input$sis_disease_name,
    prevalence        = input$sis_prevalence,
    transmission_rate = input$sis_transmission_rate,
    recovery_rate     = input$sis_recovery_rate
    )

  # Creating network
  agents_smallworld(
      model_sis,
      n = input$sis_population_size,
      k = input$sis_k,
      d = as.logical(input$sis_directed),
      p = input$sis_prob_rewiring
  )

  # Running and printing
  verbose_off(model_sis)
  run(model_sis, ndays = input$sis_n_days, seed = input$sis_seed)
  
  # Plot, summary, and reproductive number
  plot_sis <- function() plot(model_sis, main = "SIS Model")
  
  summary_sis <- function() summary(model_sis)
  
  reproductive_sis <- function()
    plot_reproductive_number(
      model_sis,
      main = "SIS Model Reproductive Number"
      )

  # Output list
  return(list(
    epicurves_plot     = plot_sis,
    reproductive_plot  = reproductive_sis,
    model_summary      = summary_sis
    )
  )

}