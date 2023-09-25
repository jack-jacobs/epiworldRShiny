library(shiny)
library(epiworldR)

function(input, output) {

  
    model_plot <- eventReactive(input$simulate, {
        if(input$model == "SEIR"){
            model_seir <- ModelSEIR(name = input$seir_disease_name, 
                                    prevalence = input$seir_prevalence,
                                    transmission_rate = input$seir_transmission_rate, 
                                    recovery_rate = input$seir_recovery_rate, 
                                    incubation_days = input$seir_incubation_days)
            agents_smallworld(
                model_seir,
                n = input$seir_population_size,
                k = input$seir_k,
                d = as.logical(input$seir_directed),
                p = input$seir_prob_rewiring
            )
            # Running and printing
            verbose_off(model_seir)
            run(model_seir, ndays = input$seir_n_days, seed = input$seir_seed)
            return(plot(model_seir, main = "SEIR Model"))
        }
        if(input$model == "SIR"){
            model_sir <- ModelSIR(name = input$sir_disease_name, 
                                  prevalence = input$sir_prevalence,
                                  transmission_rate = input$sir_transmission_rate, 
                                  recovery_rate = input$sir_recovery_rate)
            agents_smallworld(
                model_sir,
                n = input$sir_population_size,
                k = input$sir_k,
                d = as.logical(input$sir_directed),
                p = input$sir_prob_rewiring
            )
            # Running and printing
            verbose_off(model_sir)
            run(model_sir, ndays = input$sir_n_days, seed = input$sir_seed)
            return(plot(model_sir, main = "SIR Model"))
        }
        if(input$model == "SIS"){
            model_sis <- ModelSIS(name = input$sis_disease_name, 
                                  prevalence = input$sis_prevalence,
                                  transmission_rate = input$sis_transmission_rate, 
                                  recovery_rate = input$sis_recovery_rate)
            agents_smallworld(
                model_sis,
                n = input$sis_population_size,
                k = input$sis_k,
                d = as.logical(input$sis_directed),
                p = input$sis_prob_rewiring
            )
            # Running and printing
            verbose_off(model_sis)
            run(model_sis, ndays = input$sis_n_days, seed = input$sis_seed)
            return(plot(model_sis, main = "SIS Model"))          
        }
    })
  
    model_summary <- eventReactive(input$simulate, {
        if(input$model == "SEIR"){
            model_seir <- ModelSEIR(name = input$seir_disease_name, 
                                    prevalence = input$seir_prevalence,
                                    transmission_rate = input$seir_transmission_rate, 
                                    recovery_rate = input$seir_recovery_rate, 
                                    incubation_days = input$seir_incubation_days)
            agents_smallworld(
                model_seir,
                n = input$seir_population_size,
                k = input$seir_k,
                d = as.logical(input$seir_directed),
                p = input$seir_prob_rewiring
            )
            # Running and printing
            verbose_off(model_seir)
            run(model_seir, ndays = input$seir_n_days, seed = input$seir_seed)
            return(summary(model_seir))
        }
        if(input$model == "SIR"){
            model_sir <- ModelSIR(name = input$sir_disease_name, 
                                    prevalence = input$sir_prevalence,
                                    transmission_rate = input$sir_transmission_rate, 
                                    recovery_rate = input$sir_recovery_rate)
            agents_smallworld(
                model_sir,
                n = input$sir_population_size,
                k = input$sir_k,
                d = as.logical(input$sir_directed),
                p = input$sir_prob_rewiring
            )
            # Running and printing
            verbose_off(model_sir)
            run(model_sir, ndays = input$sir_n_days, seed = input$sir_seed)
            return(summary(model_sir))
       }
       if(input$model == "SIS"){
            model_sis <- ModelSIS(name = input$sis_disease_name, 
                                    prevalence = input$sis_prevalence,
                                    transmission_rate = input$sis_transmission_rate, 
                                    recovery_rate = input$sis_recovery_rate)
            agents_smallworld(
                model_sis,
                n = input$sis_population_size,
                k = input$sis_k,
                d = as.logical(input$sis_directed),
                p = input$sis_prob_rewiring
            )
            # Running and printing
            verbose_off(model_sis)
            run(model_sis, ndays = input$sis_n_days, seed = input$sis_seed)
            return(summary(model_sis))
        }
    })
    
    # Displaying Plot and Model Summary
    output$model_plot <- renderPlot({
            model_plot()
    })
    output$model_summary <- renderPrint({
            model_summary()
    })
}
