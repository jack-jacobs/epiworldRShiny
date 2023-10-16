shiny_sird <- function(input) {
  
  # Creating model
  model_sird <- ModelSIRD(
    name              = input$sird_disease_name,
    prevalence        = input$sird_prevalence,
    transmission_rate = input$sird_transmission_rate,
    recovery_rate     = input$sird_recovery_rate,
    death_rate        = input$sird_death_rate
    )

  # Generating random graph
  agents_smallworld(
      model_sird,
      n = input$sird_population_size,
      k = input$sird_k,
      d = as.logical(input$sird_directed),
      p = input$sird_prob_rewiring
  )

  # Running and printing
  verbose_off(model_sird)
  run(model_sird, ndays = input$sird_n_days, seed = input$sir_seed)

  # Plot, summary, and repnum
  plot_sird <- function() plot(model_sird, main = "SIRD Model")
  summary_sird <- function() summary(model_sird)
  reproductive_sird <- function()
    plot_reproductive_number(
      model_sird,
      main = "SIRD Model Reproductive Number"
    )

  # Output list
  return(
    list(
      epicurves_plot     = plot_sird,
      reproductive_plot  = reproductive_sird,
      model_summary      = summary_sird
      )
    )
}