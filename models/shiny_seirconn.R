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
  plot_seirconn <- function() {
    df_seirconn <- 
      get_hist_total(model_seirconn)[get_hist_total(model_seirconn)$state 
                                          == "Infected",]
    peak_time <- which.max(df_seirconn$counts) - 1

    # Plotting  
    plot(model_seirconn, main = "SEIRCONNECTED Model")
    points(peak_time, max(df_seirconn$counts), pch = 20, col = "red")
    segments(x0 = peak_time, y0 = 0, x1 = peak_time, 
             y1 = max(df_seirconn$counts), col = "red", lty = 2)
  }
  
  summary_seirconn <- function() summary(model_seirconn)
  
  reproductive_seirconn <- function()
    plot_reproductive_number(
      model_seirconn,
      main = "SEIRCONNECTED Model Reproductive Number"
      )

  # Table 
  table_seirconn <- function() {
    df <- as.data.frame(get_hist_total(model_seirconn))
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
    slider_input_rate("seirconn", "Transmission Rate", 0.1),
    slider_input_rate("seirconn", "Recovery Rate", 0.14),
    slider_input_rate("seirconn", "Contact Rate", 4, maxval = 20),
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
    seed_input("seirconn"),
    simulate_button("seirconn"),
    npis_input("seirconn")
  )
}

