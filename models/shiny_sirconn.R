# alt-name: SIR
shiny_sirconn <- function(input) {

  model_sirconn <- ModelSIRCONN(
    name              = input$sirconn_disease_name,
    prevalence        = input$sirconn_prevalence,
    transmission_rate = input$sirconn_transmission_rate,
    recovery_rate     = input$sirconn_recovery_rate,
    contact_rate      = input$sirconn_contact_rate, 
    n                 = input$sirconn_population_size
    )
    
  # NPIs -----------------------------------------------------------------------
  npi_add_all(model_sirconn, "sirconn", input)
  
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

  # Table 
  table_sirconn <- function() as.data.frame(get_hist_total(model_sirconn))
  # Output list
  return(
    list(
      epicurves_plot     = plot_sirconn,
      reproductive_plot  = reproductive_sirconn,
      model_summary      = summary_sirconn,
      model_table        = table_sirconn
    )
  )
}

sirconn_panel <- function(model_alt) {
  conditionalPanel(
    condition = sprintf("input.model == '%s'", model_alt),
    text_input_disease_name("sirconn"),
    slider_prevalence("sirconn"),
    slider_input_rate("sirconn", "Transmission Rate", 0.1),
    slider_input_rate("sirconn", "Recovery Rate", 0.14),
    slider_input_rate("sirconn", "Contact Rate", 4, maxval = 20),
    sliderInput(
      inputId = "sirconn_population_size",
      label   = "Population Size",
      min     = 0, 
      max     = 100000, 
      value   = 50000, 
      step    = 1000,
      ticks   = FALSE
    ),
    numeric_input_ndays("sirconn"),
    seed_input("sirconn"),
    simulate_button("sirconn"),
    npis_input("sirconn")
  )
}

