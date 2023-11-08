# alt-name: SIR Network

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

  # NPIs -----------------------------------------------------------------------
  npi_add_all(model_sir, "sir", input)
  
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

  # Table 
  table_sir <- function() as.data.frame(get_hist_total(model_sir))
  # Output list
  return(
    list(
      epicurves_plot     = plot_sir,
      reproductive_plot  = reproductive_sir,
      model_summary      = summary_sir,
      model_table        = table_sir
    )
  )
}

sir_panel <- function(model_alt) {

  conditionalPanel(
    condition = sprintf("input.model == '%s'", model_alt),
    text_input_disease_name("sir"),
    slider_prevalence("sir"),
    slider_input_rate("sir", "Transmission Rate", "0.05"),
    slider_input_rate("sir", "Recovery Rate", "0.14"),
    numeric_input_ndays("sir"),
    network_input("sir"),
    tools_input("sir"),
    seed_input("sir")
  )

}