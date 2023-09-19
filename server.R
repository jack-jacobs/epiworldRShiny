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
    })
    
    # Displaying Plot and Model Summary
    output$model_plot <- renderPlot({
            model_plot()
    })
    output$model_summary <- renderPrint({
            model_summary()
    })
}
