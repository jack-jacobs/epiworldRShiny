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
  # Tools
  seirconn_vaccine_tool <- tool(
    name = "Vaccine",
    susceptibility_reduction = .9,
    transmission_reduction = .5,
    recovery_enhancer = .5,
    death_reduction = .9
  )
  seirconn_masking_tool <- tool(
    name = "Masking",
    susceptibility_reduction = 0,
    transmission_reduction = 0.5,
    recovery_enhancer = 0,
    death_reduction = 0
  )
  add_tool(model_seirconn, seirconn_vaccine_tool, input$seirconn_vaccine_prevalence)
  add_tool(model_seirconn, seirconn_masking_tool, input$seirconn_masking_prevalence)
  # Creating a tool
  seirconn_school_closure_tool <- tool(
    name = "School Closure",
    susceptibility_reduction = 0,
    transmission_reduction = 0.5,
    recovery_enhancer = 0,
    death_reduction = 0
  )
  # Adding a global action
  seirconn_school_closure_ga <- 
    globalaction_tool(seirconn_school_closure_tool, 
                      input$seirconn_school_closure_prevalence, 
                      day = input$seirconn_school_closure_day)
  add_global_action(model_seirconn, seirconn_school_closure_ga)
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