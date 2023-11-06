shiny_sirconn <- function(input) {

  model_sirconn <- ModelSIRCONN(
    name              = input$sirconn_disease_name,
    prevalence        = input$sirconn_prevalence,
    transmission_rate = input$sirconn_transmission_rate,
    recovery_rate     = input$sirconn_recovery_rate,
    contact_rate      = input$sirconn_contact_rate, 
    n                 = input$sirconn_population_size
    )
  # Tools
  sirconn_vaccine_tool <- tool(
    name = "Vaccine",
    susceptibility_reduction = .9,
    transmission_reduction = .5,
    recovery_enhancer = .5,
    death_reduction = .9
  )
  sirconn_masking_tool <- tool(
    name = "Masking",
    susceptibility_reduction = 0,
    transmission_reduction = 0.5,
    recovery_enhancer = 0,
    death_reduction = 0
  )
  add_tool(model_sirconn, sirconn_vaccine_tool, input$sirconn_vaccine_prevalence)
  add_tool(model_sirconn, sirconn_masking_tool, input$sirconn_masking_prevalence)
  # Creating a tool
  sirconn_school_closure_tool <- tool(
    name = "School Closure",
    susceptibility_reduction = 0,
    transmission_reduction = 0.5,
    recovery_enhancer = 0,
    death_reduction = 0
  )
  # Adding a global action
  sirconn_school_closure_ga <- 
    globalaction_tool(sirconn_school_closure_tool, 
                      input$sirconn_school_closure_prevalence, 
                      day = input$sirconn_school_closure_day)
  add_global_action(model_sirconn, sirconn_school_closure_ga)
  
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