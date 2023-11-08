npi_add_vaccine <- function(model, preval) {

  if (preval > 0) {

    tool_ <- tool(
      name = "Vaccine",
      susceptibility_reduction = .9,
      transmission_reduction = .5,
      recovery_enhancer = .5,
      death_reduction = .9
    )

    add_tool(
      model      = model,
      tool       = tool_,
      proportion = preval
      )

  }

}

npi_add_masking <- function(model, preval) {

  if (preval > 0) {

    tool_ <- tool(
      name = "Masking",
      susceptibility_reduction = 0,
      transmission_reduction = 0.5,
      recovery_enhancer = 0,
      death_reduction = 0
    )

    add_tool(
      model      = model,
      tool       = tool_,
      proportion = preval
      )

  }

}

npi_add_school_closure <- function(model, preval, day) {
  
  if (preval > 0) {
    
    # Creating a tool
    tool_ <- tool(
      name = "School Closure",
      susceptibility_reduction = 0,
      transmission_reduction = 0.5,
      recovery_enhancer = 0,
      death_reduction = 0
    )

    # Adding a global action
    action_ <- globalaction_tool(
        tool = tool_, 
        prob = preval, 
        day = day
        )

    add_global_action(
      model,
      action_
      )

  }

}