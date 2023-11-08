# alt-name: SEIR

shiny_seirconn <- function(input) {

  model_seirconn <- ModelSEIRCONN(
    name              = input$seirconn_disease_name,
    prevalence        = input$seirconn_prevalence,
    transmission_rate = input$seirconn_transmission_rate,
    recovery_rate     = input$seirconn_recovery_rate,
    contact_rate      = input$seirconn_contact_rate,
    n                 = input$seirconn_population_size,
    incubation_days   = input$seirconn_incubation_days
    )
  
  # NPIs -----------------------------------------------------------------------
  npi_add_all(model_seirconn, "seirconn", input)

  # Running and printing
  verbose_off(model_seirconn)
  run(model_seirconn, ndays = input$seirconn_n_days, seed = input$seirconn_seed)
  
  # Plot, summary, and reproductive number
  plot_seirconn <- function() plot(model_seirconn, main = "SEIRCONNECTED Model")
  
  summary_seirconn <- function() summary(model_seirconn)
  
  reproductive_seirconn <- function()
    plot_reproductive_number(
      model_seirconn,
      main = "SEIRCONNECTED Model Reproductive Number"
      )

  # Table 
  table_seirconn <- function() as.data.frame(get_hist_total(model_seirconn))
  # Output list
  return(
    list(
      epicurves_plot     = plot_seirconn,
      reproductive_plot  = reproductive_seirconn,
      model_summary      = summary_seirconn,
      model_table        = table_seirconn
    )
  )
}

seirconn_panel <- function(model_alt) {
  conditionalPanel(
    condition = sprintf("input.model == '%s'", model_alt),
    text_input_disease_name("seirconn"),
    slider_prevalence("seirconn"),
    slider_input_rate("seirconn", "Transmission Rate", "0.1"),
    slider_input_rate("seirconn", "Recovery Rate", "0.14"),
    slider_input_rate("seirconn", "Contact Rate", "4", maxval = 20),
    numericInput(
      inputId = "seirconn_incubation_days",
      label   = "Incubation Days",
      value   = "7",
      min     = 0, 
      max     = NA,
      step    = 1
      ),
    sliderInput(
      inputId = "seirconn_population_size",
      label   = "Population Size",
      min     = 0, 
      max     = 100000, 
      value   = 50000, 
      step    = 1000,
      ticks   = FALSE
    ),
    numeric_input_ndays("seirconn"),
    tools_input("seirconn"),
    seed_input("seirconn")
  )
}

