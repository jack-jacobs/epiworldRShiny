shiny_sir <- function(input) {
  
  # Creating model
  model_sir <- ModelSIR(
    name              = input$sir_disease_name,
    prevalence        = input$sir_prevalence,
    transmission_rate = input$sir_transmission_rate,
    recovery_rate     = input$sir_recovery_rate
    )

  # Generating random graph
  agents_smallworld(
      model_sir,
      n = input$sir_population_size,
      k = input$sir_k,
      d = as.logical(input$sir_directed),
      p = input$sir_prob_rewiring
  )

  # Tools
  sir_vaccine_tool <- tool(
    name = "Vaccine",
    susceptibility_reduction = .9,
    transmission_reduction = .5,
    recovery_enhancer = .5,
    death_reduction = .9
  )
  sir_masking_tool <- tool(
    name = "Masking",
    susceptibility_reduction = 0,
    transmission_reduction = 0.5,
    recovery_enhancer = 0,
    death_reduction = 0
  )
  add_tool(model_sir, sir_vaccine_tool, input$sir_vaccine_prevalence)
  add_tool(model_sir, sir_masking_tool, input$sir_masking_prevalence)
  # Creating a tool
  sir_school_closure_tool <- tool(
    name = "School Closure",
    susceptibility_reduction = 0,
    transmission_reduction = 0.5,
    recovery_enhancer = 0,
    death_reduction = 0
  )
  # Adding a global action
  sir_school_closure_ga <- 
    globalaction_tool(sir_school_closure_tool, 
                      input$sir_school_closure_prevalence, 
                      day = input$sir_school_closure_day)
  add_global_action(model_sir, sir_school_closure_ga)
  
  # Running and printing
  verbose_off(model_sir)
  run(model_sir, ndays = input$sir_n_days, seed = input$sir_seed)

  # Plot, summary and repnum
  plot_sir <- function() plot(model_sir, main = "SIR Model")
  summary_sir <- function() summary(model_sir)
  reproductive_sir <- function()
    plot_reproductive_number(
      model_sir,
      main = "SIR Model Reproductive Number"
    )

  # Table 
  table_sir <- function() as.data.frame(get_hist_total(model_sir))
  # Output list
  return(
    list(
      epicurves_plot     = plot_sir,
      reproductive_plot  = reproductive_sir,
      model_summary      = summary_sir,
      model_table        = table_sir
    )
  )
}