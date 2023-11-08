shiny_seirconnequity <- function(input) {

  n <- input$seirconnequity_population_size

  model_seirconnequity <- ModelSEIRCONN(
    name              = input$seirconnequity_disease_name,
    prevalence        = input$seirconnequity_prevalence,
    transmission_rate = input$seirconnequity_transmission_rate,
    recovery_rate     = input$seirconnequity_recovery_rate,
    contact_rate      = input$seirconnequity_contact_rate,
    n                 = n,
    incubation_days   = input$seirconnequity_incubation_days
    )

  set_name(model_seirconnequity, "SEIR Equity Model")

  # Generating artificial pop data ---------------------------------------------
  agegroups <- sample.int(3, size = n, replace = TRUE)
  X <- matrix(0, nrow = n, ncol = 3)
  X[cbind(1:n, agegroups)] <- 1
  colnames(X) <- c("0-19", "20-59", "60+")

  X <- cbind(
    X,
    Hispanic = sample.int(2, size = n, replace = TRUE) - 1,
    Female   = sample.int(2, size = n, replace = TRUE) - 1
  )

  # Saving the data to the global environment (this way we make sure it is
  # available to the model)
  epiworldR_env <- globalenv()$epiworldR_env
  epiworldR_env$X <- X  

  # Adding population data
  set_agents_data(
    model = model_seirconnequity,
    data  = epiworldR_env$X
    )

  # Creating immune system to add the difference in susceptibility -------------
  immune <- tool(
    "Immune system",
    susceptibility_reduction = 0, 
    transmission_reduction = 0,
    recovery_enhancer = 0,
    death_reduction = 0
    )

  # Setting logit function for the virus to be more infectious for some groups
  # This is done with susceptibility reduction. The smallest the value, the
  # more infectious the virus is for that group.
  tfun <- tool_fun_logit(
    vars = 0:4,
    coefs = log(c( # Defined in terms of odds
      0.250, # 0-19
      0.500, # 20-59
      0.125, # 60+
      0.500, # Hispanic
      0.500  # Female
    )),
    model = model_seirconnequity
  )

  # Setting the virus
  set_susceptibility_reduction_fun(
    tool  = immune,
    model = model_seirconnequity,
    tfun  = tfun
    )

  # Adding the tool to the model
  add_tool(
    model      = model_seirconnequity,
    tool       = immune,
    proportion = 1
    )

  # effective trate (user defined) = t rate virus x (1 - suscept redux)
  # we use the middle as a reference
  set_prob_infecting(
    virus = get_virus(model_seirconnequity, 0), 
    prob  = min(1, input$seirconnequity_transmission_rate/
      (1 - plogis(1)))
  )
  
  # NPIs -----------------------------------------------------------------------
  npi_add_vaccine(
    model  = model_seirconnequity,
    preval = input$seirconnequity_vaccine_prevalence
    )

  npi_add_masking(
    model = model_seirconnequity,
    preval = input$seirconnequity_masking_prevalence
    )

  npi_add_school_closure(
    model  = model_seirconnequity,
    preval = input$seirconnequity_school_closure_prevalence,
    day    = input$seirconnequity_school_closure_day
  )

  # Running and printing
  verbose_off(model_seirconnequity)
  run(model_seirconnequity, ndays = input$seirconnequity_n_days, seed = input$seirconnequity_seed)
  
  # Plot, summary, and reproductive number
  plot_seirconnequity <- function() plot(model_seirconnequity, main = "SEIRCONNECTED Model")
  
  summary_seirconnequity <- function() summary(model_seirconnequity)
  
  reproductive_seirconnequity <- function()
    plot_reproductive_number(
      model_seirconnequity,
      main = "SEIRCONNECTED Model Reproductive Number"
      )

  # Table 
  table_seirconnequity <- function() as.data.frame(get_hist_total(model_seirconnequity))
  # Output list
  return(
    list(
      epicurves_plot     = plot_seirconnequity,
      reproductive_plot  = reproductive_seirconnequity,
      model_summary      = summary_seirconnequity,
      model_table        = table_seirconnequity
    )
  )
}

