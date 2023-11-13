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
  plot_sirconn <- function() {
    df_sirconn <- 
      get_hist_total(model_sirconn)[get_hist_total(model_sirconn)$state 
                                          == "Infected",]
    peak_time <- which.max(df_sirconn$counts) - 1

    # Plotting  
    plot(model_sirconn, main = "SIRCONNNECTED Model")
    points(peak_time, max(df_sirconn$counts), pch = 20, col = "black")
    segments(x0 = peak_time, y0 = 0, x1 = peak_time, 
             y1 = max(df_sirconn$counts), col = "black", lty = 2)
  }
  
  summary_sirconn <- function() summary(model_sirconn)
  
  reproductive_sirconn <- function()
    plot_reproductive_number(
      model_sirconn,
      main = "SIRCONNECTED Model Reproductive Number"
      )

  # Table 
  table_sirconn <- function() {
    df <- as.data.frame(get_hist_total(model_sirconn))
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
    tools_input("sirconn"),
    seed_input("sirconn")
  )
}

