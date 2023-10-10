shiny_sir <- function(input) {
  # Creating model
  model_sir <- ModelSIR(
    name              = input$sir_disease_name,
    prevalence        = input$sir_prevalence,
    transmission_rate = input$sir_transmission_rate,
    recovery_rate     = input$sir_recovery_rate
    )

  # Generating random graph
  agents_smallworld(
      model_sir,
      n = input$sir_population_size,
      k = input$sir_k,
      d = as.logical(input$sir_directed),
      p = input$sir_prob_rewiring
  )

  # Running and printing
  verbose_off(model_sir)
  run(model_sir, ndays = input$sir_n_days, seed = input$sir_seed)

  # Plot, summary and repnum
  plot_sir <- function() plot(model_sir, main = "SIR Model")
  summary_sir <- function() summary(model_sir)
  reproductive_sir <- function()
    plot_reproductive_number(
      model_sir,
      main = "SIR Model Reproductive Number"
    )

  # Output list
  return(
    list(
      epicurves_plot     = plot_sir,
      reproductive_plot  = reproductive_sir,
      model_summary      = summary_sir
      )
    )
}