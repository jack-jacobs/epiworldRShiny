# alt-name: Network SEIR

# Creating model
shiny_seir <- function(input) {

  model_seir <- ModelSEIR(
    name              = input$seir_disease_name, 
    prevalence        = input$seir_prevalence,
    transmission_rate = input$seir_transmission_rate, 
    recovery_rate     = input$seir_recovery_rate, 
    incubation_days   = input$seir_incubation_days
    )

  # Generating random graph
  agents_smallworld(
    model_seir,
    n = input$seir_population_size,
    k = input$seir_k,
    d = as.logical(input$seir_directed),
    p = input$seir_prob_rewiring
  )
  
  # NPIs -----------------------------------------------------------------------
  npi_add_all(model_seir, "seir", input)
  
  # Running and printing
  verbose_off(model_seir)
  run(model_seir, ndays = input$seir_n_days, seed = input$seir_seed)
  # Plot
  plot_seir <- function() plot(model_seir, main = "SEIR Model")
  # Summary
  summary_seir <- function() summary(model_seir)
  # Reproductive Number
  reproductive_seir <- function() 
    plot_reproductive_number(
      model_seir,
      main = "SEIR Model Reproductive Number"
    )
  # Table 
  table_seir <- function() as.data.frame(get_hist_total(model_seir))
  # Output list
  return(
    list(
      epicurves_plot     = plot_seir,
      reproductive_plot  = reproductive_seir,
      model_summary      = summary_seir,
      model_table        = table_seir
    )
  )

}

seir_panel <- function(model_alt) {
  conditionalPanel(
    condition = sprintf("input.model == '%s'", model_alt),
    text_input_disease_name("seir"),
    slider_prevalence("seir"),
    slider_input_rate("seir", "Transmission Rate", "0.05"),
    slider_input_rate("seir", "Recovery Rate", "0.14"),
    numericInput(
      inputId = "seir_incubation_days",
      label   = "Incubation Days",
      value   = "7",
      min     = 0, 
      max     = NA,
      step    = 1
      ),
    numeric_input_ndays("seir"),
    network_input("seir"),
    tools_input("seir"),
    seed_input("seir")
  )
}