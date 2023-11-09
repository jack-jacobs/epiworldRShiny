# alt-name: SISD

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
   
  # NPIs -----------------------------------------------------------------------
  npi_add_all(model_sisd, "sisd", input)

  # Running and printing
  verbose_off(model_sisd)
  run(model_sisd, ndays = input$sisd_n_days, seed = input$sisd_seed)

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

sisd_panel <- function(model_alt) {

  conditionalPanel(
    condition = sprintf("input.model == '%s'", model_alt),
    text_input_disease_name("sisd"),
    slider_prevalence("sisd"),
    slider_input_rate("sisd", "Transmission Rate", "0.05"),
    slider_input_rate("sisd", "Recovery Rate", "0.14"),
    slider_input_rate("sisd", "Death Rate", "0.01"),
    numeric_input_ndays("sisd"),
    seed_input("sisd"),
    simulate_button,
    network_input("sisd"),
    npis_input("sisd")
  )

}