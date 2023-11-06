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
   
    # Tools
  sisd_vaccine_tool <- tool(
    name = "Vaccine",
    susceptibility_reduction = .9,
    transmission_reduction = .5,
    recovery_enhancer = .5,
    death_reduction = .9
  )
  sisd_masking_tool <- tool(
    name = "Masking",
    susceptibility_reduction = 0,
    transmission_reduction = 0.5,
    recovery_enhancer = 0,
    death_reduction = 0
  )
  add_tool(model_sisd, sisd_vaccine_tool, input$sisd_vaccine_prevalence)
  add_tool(model_sisd, sisd_masking_tool, input$sisd_masking_prevalence)
  # Creating a tool
  sisd_school_closure_tool <- tool(
    name = "School Closure",
    susceptibility_reduction = 0,
    transmission_reduction = 0.5,
    recovery_enhancer = 0,
    death_reduction = 0
  )
  # Adding a global action
  sisd_school_closure_ga <- 
    globalaction_tool(sisd_school_closure_tool, 
                      input$sisd_school_closure_prevalence, 
                      day = input$sisd_school_closure_day)
  add_global_action(model_sisd, sisd_school_closure_ga)

  # Running and printing
  verbose_off(model_sisd)
  run(model_sisd, ndays = input$sisd_n_days, seed = input$sisd_seed)

  # Plot, summary, and repnum
  plot_sisd <- function() plot(model_sisd, main = "SISD Model")
  summary_sisd <- function() summary(model_sisd)
  reproductive_sisd <- function()
    plot_reproductive_number(
      model_sisd,
      main = "SISD Model Reproductive Number"
    )

  # Table 
  table_sisd <- function() as.data.frame(get_hist_total(model_sisd))
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