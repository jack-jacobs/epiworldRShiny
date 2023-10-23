shiny_sisd <- function(input) {
  
  # Creating model
  model_sisd <- ModelSISD(
    name              = input$sisd_disease_name,
    prevalence        = input$sisd_prevalence,
    transmission_rate = input$sisd_transmission_rate,
    recovery_rate     = input$sisd_recovery_rate,
    death_rate        = input$sisd_death_rate
    )

  # Generating random graph
  agents_smallworld(
      model_sisd,
      n = input$sisd_population_size,
      k = input$sisd_k,
      d = as.logical(input$sisd_directed),
      p = input$sisd_prob_rewiring
  )

  # Running and printing
  verbose_off(model_sisd)
  run(model_sisd, ndays = input$sisd_n_days, seed = input$sir_seed)

  # Plot, summary, and repnum
  plot_sisd <- function() plot(model_sisd, main = "SISD Model")
  summary_sisd <- function() summary(model_sisd)
  reproductive_sisd <- function()
    plot_reproductive_number(
      model_sisd,
      main = "SISD Model Reproductive Number"
    )

  # Table 
  table_sisd <- function() as.data.frame(get_hist_total(model_sisd))
  # Output list
  return(
    list(
      epicurves_plot     = plot_sisd,
      reproductive_plot  = reproductive_sisd,
      model_summary      = summary_sisd,
      model_table        = table_sisd
    )
  )
}