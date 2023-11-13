text_input_disease_name <- function(model_name) {
  textInput(
    inputId     = paste0(model_name, "_disease_name"),
    label       = "Disease",
    value       = "",
    placeholder = "Please enter a disease name"
  )
}

slider_prevalence <- function(model_name) {
  sliderInput(
    paste0(model_name, "_prevalence"),
    label = "Disease Prevalence",
    value = "0.1",
    min = 0, 
    max = 1,
    step = 0.01,
    ticks = FALSE
    )
}

numeric_input_ndays <- function(model_name) {
  numericInput(
    inputId = paste0(model_name, "_n_days"),
    label   = "Simulation Time (Days)",
    value   = "100",
    min     = 0, 
    max     = NA,
    step    = 1
    )
}

slider_input_rate <- function(
  model_name, rate_name, value, maxval = 1
  ) {
  sliderInput(
    inputId = paste(
      model_name, gsub("\\s+", "_", tolower(rate_name)),
      sep = "_"
    ),
    label = rate_name,
    value = value,
    min   = 0, 
    max   = maxval,
    step  = 0.01,
    ticks = FALSE
  )
}

network_input <- function(model_name) {

  tagList(
    div(
      id = paste0("network_header_", model_name),
      headerPanel(h4("Population structure"))
      ),
    hidden(
      div(
        id = paste0("network_inputs_", model_name),
        sliderInput(
          inputId = paste0(model_name, "_population_size"),
          label   = "Population Size",
          min     = 0, 
          max     = 100000, 
          value   = 50000, 
          step    = 1000,
          ticks   = FALSE
          ),
        numericInput(
          inputId = paste0(model_name, "_k"),
          label   = "Number of Ties", 
          min     = 0, 
          max     = 500, 
          step    = 1,
          value   = 20
          ),
        selectInput(
          inputId  = paste0(model_name, "_directed"),
          label    = "Directed",
          choices  = c("TRUE", "FALSE"),
          selected = "FALSE"
          ), 
        sliderInput(
          inputId = paste0(model_name, "_prob_rewiring"),
          label   = "Probability of Rewiring",
          value   = "0.20",
          min     = 0, 
          max     = 1,
          step    = 0.01,
          ticks   = FALSE
          )
        )
      )
    )
}

npis_input <- function(model_name) {
  tagList(
    div(
      id = paste0("npis_header_", model_name),
      headerPanel(h4("Non-pharmaceutical interventions"))
      ),
    # By default hidden
    hidden(
      div(id = paste0("npis_inputs_", model_name),
        sliderInput(
          inputId = paste0(model_name, "_vaccine_prevalence"),
          label   = "Vaccine Prevalence",
          min     = 0, 
          max     = 1, 
          value   = 0, 
          step    = 0.01,
          ticks   = FALSE
          ),
        sliderInput(
          inputId = paste0(model_name, "_masking_prevalence"),
          label   = "Masking Prevalence",
          value   = "0",
          min     = 0, 
          max     = 1,
          step    = 0.01,
          ticks   = FALSE
          ),
        headerPanel(h4("School Closure")),
        sliderInput(
          inputId = paste0(model_name, "_school_closure_prevalence"),
          label   = "Prevalence",
          value   = "0",
          min     = 0, 
          max     = 1,
          step    = 0.01,
          ticks   = FALSE
          ),
        numericInput(
          inputId = paste0(model_name, "_school_closure_day"),
          label   = "Day of Implementation",
          value = "0",
          min = 0,
          max = 100,
          step = 1
        )
      )
    )
  )
}

seed_input <- function(model_name) {
  numericInput(
    inputId = paste0(model_name, "_seed"),
    label   = "Seed (Optional)", 
    min     = 0, 
    max     = NA, 
    step    = 1,
    value   = 2023
    )
}

# Function to set up models
models_setup <- function() {

  # Getting the environment
  env <- parent.frame()

  eval({

    # Get a list of all model files in the "models" directory
    models <- list.files("models", full.names = TRUE)

    # Source each model file
    for (f in models) {
      source(f)
    }

    # Capturing alt names
    models_names <- sapply(models, \(f) {
      
      # Only on the first line
      altname <- readLines(f, n = 1)

      # Is there any alt name?
      altname <- gsub("^#\\s*alt-name[:]\\s*(.+)\\s*$", "\\1", altname, perl = TRUE)

      # If there is no alt name, use the file name
      if (altname == "") {
        altname <- toupper(gsub("shiny_([^.]+).R", "\\1", basename(f)))
      }
      altname

    })

    # Get the model names from the file names
    models <- gsub("^.+shiny_([^.]+).R$", "\\1", models)

    # Set the names of the models to their alt names (if available)
    names(models_names) <- models
    names(models) <- models_names

    # Get the epiworldR environment
    epiworldR_env <- get("epiworldR_env", envir = .GlobalEnv)

    # Add the model names and models to the epiworldR environment
    epiworldR_env$models_names <- models_names
    epiworldR_env$models <- models

    # Doing some hacking
    models_panels <- mget(paste0(models, "_panel"), envir = .GlobalEnv)
    invisible({
      Map(\(pfun, nam, id) {
        assign(paste0(id, "_panel"), pfun(nam), envir = .GlobalEnv)
      }, pfun = models_panels, nam = models_names, id = models
      )
    })

  }, envir = env)

}

simulate_button <- function(model_name) {
  actionButton(
    inputId = paste0("simulate_", model_name),
    label   = "Run Simulation"
    )
}