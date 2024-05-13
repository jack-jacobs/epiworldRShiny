#' Vaccine definition function
#' @param model epiworldR model.
#' @param preval Initial prevalence of the vaccine.
#' @param sus_red Reduction in susceptibility probability due
#' to vaccine.
#' @param trans_red Reduction in transmission probability due to
#' vaccine.
#' @param rec_enhan Probability increase in recovery due to vaccine.
#' @param death_red Reduction in death probability due to vaccine.
#' @return Returns an object of class epiworld_model, where model is
#' substituted with the model name.
#' @examples
#' library(epiworldR) # for ModelSEIRCONN function
#' model <- ModelSEIRCONN("COVID-19", n = 1000, prevalence = 0.05,
#'                        contact_rate = 4, transmission_rate = 0.1,
#'                        incubation_days = 7, recovery_rate = 0.14)
#' run(model, ndays = 100, seed = 123)
#' npi_add_vaccine(model, preval = .8, susceptibility_reduction = .9,
#'                 transmission_reduction = .5, recovery_enhancer = .5,
#'                 death_reduction = .9)
#' @export
#' @family npis
npi_add_vaccine <- function(
  model, preval,
  sus_red,
  trans_red,
  rec_enhan,
  death_red
  ) {

  if (preval > 0) {

    tool_ <- epiworldR::tool(
      name = "Vaccine",
      susceptibility_reduction = sus_red,
      transmission_reduction = trans_red,
      recovery_enhancer = rec_enhan,
      death_reduction = death_red
    )

    epiworldR::add_tool(
      model      = model,
      tool       = tool_,
      proportion = preval
      )

  }

}

#' Masking definition function
#' @param model epiworldR model.
#' @param preval Prevalence of masking within the population.
#' @param trans_red Reduction in transmission probability due to masking
#' @return Returns an object of class epiworld_model, where model is
#' substituted with the model name.
#' @examples
#' library(epiworldR) # for ModelSEIRCONN function
#' model <- ModelSEIRCONN("COVID-19", n = 1000, prevalence = 0.05,
#'                        contact_rate = 4, transmission_rate = 0.1,
#'                        incubation_days = 7, recovery_rate = 0.14)
#' run(model, ndays = 100, seed = 123)
#' npi_add_masking(model, preval = .8)
#' @export
#' @family npis
npi_add_masking <- function(model, preval, trans_red) {

  if (preval > 0) {

    tool_ <- epiworldR::tool(
      name = "Masking",
      susceptibility_reduction = 0,
      transmission_reduction = trans_red,
      recovery_enhancer = 0,
      death_reduction = 0
    )

    epiworldR::add_tool(
      model      = model,
      tool       = tool_,
      proportion = preval
      )

  }

}

#' School closure definition function
#' @param model epiworldR model.
#' @param preval Prevalence of school closure within the population.
#' @param day Day in the simulation where school closure goes into effect.
#' @param trans_red Reduction in transmission probability due to school closure.
#' @return Returns an object of class epiworld_model, where model is
#' substituted with the model name.
#' @examples
#' library(epiworldR) # for ModelSEIRCONN function
#' model <- ModelSEIRCONN("COVID-19", n = 1000, prevalence = 0.05,
#'                        contact_rate = 4, transmission_rate = 0.1,
#'                        incubation_days = 7, recovery_rate = 0.14)
#' run(model, ndays = 100, seed = 123)
#' npi_add_school_closure(model, preval = .8, day = 10)
#' @export
#' @family npis
npi_add_school_closure <- function(model, preval, day,
                                   trans_red) {

  if (preval > 0) {

    # Creating a tool
    tool_ <- epiworldR::tool(
      name                     = "School Closure",
      susceptibility_reduction = 0,
      transmission_reduction   = trans_red,
      recovery_enhancer        = 0,
      death_reduction          = 0
    )

    # Adding a global action
    action_ <- epiworldR::globalevent_tool(
      tool = tool_,
      prob = preval,
      day  = day
      )

    epiworldR::add_globalevent(
      model,
      action_
      )

  }

}

#' NPI adding function
#' @param model epiworldR model.
#' @param modelname Specified model.
#' @param input User epiworldR model selection.
#' @returns Returns an object of class epiworld_model, where model is
#' substituted with the model name.
#' @family npis
#' @export
npi_add_all <- function(model, modelname, input) {

  npi_add_vaccine(
    model  = model,
    preval = input[[paste0(modelname, "_vaccine_prevalence")]],
    sus_red = input[[paste0(modelname, "_vaccine_susceptibility_reduction")]],
    trans_red = input[[paste0(modelname, "_vaccine_transmission_reduction")]],
    rec_enhan = input[[paste0(modelname, "_vaccine_recovery_enhancer")]],
    death_red = input[[paste0(modelname, "_vaccine_death_reduction")]]
    )

  npi_add_masking(
    model = model,
    preval = input[[paste0(modelname, "_masking_prevalence")]],
    trans_red = input[[paste0(modelname, "_masking_transmission_reduction")]]
    )

  npi_add_school_closure(
    model     = model,
    preval    = input[[paste0(modelname, "_school_closure_prevalence")]],
    day       = input[[paste0(modelname, "_school_closure_day")]],
    trans_red = input[[paste0(modelname, "_school_closure_transmission_reduction")]]
    )

}
