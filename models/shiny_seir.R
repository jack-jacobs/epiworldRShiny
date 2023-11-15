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
  plot_seir <- function() {
    #plot(model_seir, main = "SEIR Model") 
    df_seir <- get_hist_total(model_seir)[get_hist_total(model_seir)$state 
                                          == "Infected",]
    peak_time <- which.max(df_seir$counts) - 1

    # Plotting  
    plot(model_seir, main = "SEIR Model")
    points(peak_time, max(df_seir$counts), pch = 20, col = "red")
    segments(x0 = peak_time, y0 = 0, x1 = peak_time, 
             y1 = max(df_seir$counts), col = "red", lty = 2)
  }
  # Summary
  summary_seir <- function() summary(model_seir)
  # Reproductive Number
  reproductive_seir <- function() 
    plot_reproductive_number(
      model_seir,
      main = "SEIR Model Reproductive Number"
    )
  # Table 
  table_seir <- function() {
    df <- as.data.frame(get_hist_total(model_seir))
    # Subset to only include "infection" state
    infection_data <- df[df$state == "Infected", ]
    # Row with the maximum count
    max_infection_row <- infection_data[which.max(infection_data$count), ]
    # Row number of the maximum count in the original data frame
    max_row_number <- which(df$date == max_infection_row$date & 
                              df$state == "Infected")
    df[max_row_number,] <- sprintf("<strong>%s</strong>", 
                                       df[max_row_number,])
    df
  }
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
    slider_input_rate("seir", "Probability of exposure (daily)", "0.05", input_label = "transmission_rate"),
    slider_input_rate("seir", "Recovery probability (daily)", "0.14", input_label = "recovery_rate"),
    numericInput(
      inputId = "seir_incubation_days",
      label   = "Incubation Days",
      value   = "7",
      min     = 0, 
      max     = NA,
      step    = 1
      ),
    numeric_input_ndays("seir"),
    seed_input("seir"),
    simulate_button("seir"),
    network_input("seir"),
    npis_input("seir")
  )
}