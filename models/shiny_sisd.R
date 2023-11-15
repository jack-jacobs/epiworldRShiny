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
  plot_sisd <- function() {
    df_sisd <- get_hist_total(model_sisd)[get_hist_total(model_sisd)$state 
                                          == "Infected",]
    peak_time <- which.max(df_sisd$counts) - 1

    # Plotting  
    plot(model_sisd, main = "SISD Model")
    points(peak_time, max(df_sisd$counts), pch = 20, col = "red")
    segments(x0 = peak_time, y0 = 0, x1 = peak_time, 
             y1 = max(df_sisd$counts), col = "red", lty = 2)
  }
  summary_sisd <- function() summary(model_sisd)
  reproductive_sisd <- function()
    plot_reproductive_number(
      model_sisd,
      main = "SISD Model Reproductive Number"
    )

  # Table 
  table_sisd <- function() {
    df <- as.data.frame(get_hist_total(model_sisd))
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
    slider_input_rate("sisd", "Probability of exposure (daily)", 0.05, input_label = "transmission_rate"),
    slider_input_rate("sisd", "Recovery probability (daily)", 0.14, input_label = "recovery_rate"),
    slider_input_rate("sisd", "Death Rate", "0.01"),
    numeric_input_ndays("sisd"),
    seed_input("sisd"),
    simulate_button("sisd"),
    network_input("sisd"),
    npis_input("sisd")
  )

}