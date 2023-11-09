# alt-name: SIS

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
  
  # NPIs -----------------------------------------------------------------------
  npi_add_all(model_sis, "sis", input)
  
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

  # Table 
  table_sis <- function() {
    df <- as.data.frame(get_hist_total(model_sis))
    df$counts[1] <- sprintf("<strong>%s</strong>", df$counts[1])
    df
  }
  # Output list
  return(
    list(
      epicurves_plot     = plot_sis,
      reproductive_plot  = reproductive_sis,
      model_summary      = summary_sis,
      model_table        = table_sis
    )
  )

}

sis_panel <- function(model_alt) {

  conditionalPanel(
    condition = sprintf("input.model == '%s'", model_alt),
    text_input_disease_name("sis"),
    slider_prevalence("sis"),
    slider_input_rate("sis", "Transmission Rate", "0.05"),
    slider_input_rate("sis", "Recovery Rate", "0.14"),
    numeric_input_ndays("sis"),
    network_input("sis"),
    tools_input("sis"),
    seed_input("sis")
  )

}