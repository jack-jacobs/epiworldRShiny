# alt-name: SIRD

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
  
  # NPIs -----------------------------------------------------------------------
  npi_add_all(model_sird, "sird", input)

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

  # Table 
  table_sird <- function() as.data.frame(get_hist_total(model_sird))
  # Output list
  return(
    list(
      epicurves_plot     = plot_sird,
      reproductive_plot  = reproductive_sird,
      model_summary      = summary_sird,
      model_table        = table_sird
    )
  )
}

sird_panel <- function(model_alt) {

  conditionalPanel(
    condition = sprintf("input.model == '%s'", model_alt),
    text_input_disease_name("sird"),
    slider_prevalence("sird"),
    slider_input_rate("sird", "Transmission Rate", "0.05"),
    slider_input_rate("sird", "Recovery Rate", "0.14"),
    slider_input_rate("sird", "Death Rate", "0.01"),
    numeric_input_ndays("sird"),
    seed_input("sird"),
    simulate_button,
    network_input("sird"),
    npis_input("sird")
  )

}

