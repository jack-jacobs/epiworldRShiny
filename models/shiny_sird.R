# alt-name: SIRD

shiny_sird <- function(input) {
  
  # Creating model
  model_sird <- ModelSIRD(
    name              = input$sird_disease_name,
    prevalence        = input$sird_prevalence,
    transmission_rate = input$sird_transmission_rate,
    recovery_rate     = input$sird_recovery_rate,
    death_rate        = input$sird_death_rate
    )

  # Generating random graph
  agents_smallworld(
      model_sird,
      n = input$sird_population_size,
      k = input$sird_k,
      d = as.logical(input$sird_directed),
      p = input$sird_prob_rewiring
  ) 
  
  # Tools
  sird_vaccine_tool <- tool(
    name = "Vaccine",
    susceptibility_reduction = .9,
    transmission_reduction = .5,
    recovery_enhancer = .5,
    death_reduction = .9
  )
  sird_masking_tool <- tool(
    name = "Masking",
    susceptibility_reduction = 0,
    transmission_reduction = 0.5,
    recovery_enhancer = 0,
    death_reduction = 0
  )
  add_tool(model_sird, sird_vaccine_tool, input$sird_vaccine_prevalence)
  add_tool(model_sird, sird_masking_tool, input$sird_masking_prevalence)
  # Creating a tool
  sird_school_closure_tool <- tool(
    name = "School Closure",
    susceptibility_reduction = 0,
    transmission_reduction = 0.5,
    recovery_enhancer = 0,
    death_reduction = 0
  )
  # Adding a global action
  sird_school_closure_ga <- 
    globalaction_tool(sird_school_closure_tool, 
                      input$sird_school_closure_prevalence, 
                      day = input$sird_school_closure_day)
  add_global_action(model_sird, sird_school_closure_ga)
  # Running and printing
  verbose_off(model_sird)
  run(model_sird, ndays = input$sird_n_days, seed = input$sir_seed)

  # Plot, summary, and repnum
  plot_sird <- function() plot(model_sird, main = "SIRD Model")
  summary_sird <- function() summary(model_sird)
  reproductive_sird <- function()
    plot_reproductive_number(
      model_sird,
      main = "SIRD Model Reproductive Number"
    )

  # Table 
  table_sird <- function() as.data.frame(get_hist_total(model_sird))
  # Output list
  return(
    list(
      epicurves_plot     = plot_sird,
      reproductive_plot  = reproductive_sird,
      model_summary      = summary_sird,
      model_table        = table_sird
    )
  )
}

sird_panel <- function(model_alt) {

  conditionalPanel(
    condition = sprintf("input.model == '%s'", model_alt),
    text_input_disease_name("sird"),
    slider_prevalence("sird"),
    slider_input_rate("sird", "Transmission Rate", "0.05"),
    slider_input_rate("sird", "Recovery Rate", "0.14"),
    slider_input_rate("sird", "Death Rate", "0.01"),
    numeric_input_ndays("sird"),
    network_input("sird"),
    tools_input("sird"),
    seed_input("sird")
  )

}

