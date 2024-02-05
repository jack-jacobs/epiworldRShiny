#' @import epiworldR
#' @import shinydashboard
#' @importFrom DT dataTableOutput renderDataTable
#' @import ggplot2
#' @import shinycssloaders
#' @importFrom plotly plot_ly add_markers add_segments layout
#' @import shiny
#' @importFrom shinyjs hidden useShinyjs toggle
#'
#' @return Loads and opens the RShiny app for the epiworldR package
#' @export
epiworldRShiny <- function(...) {

    assign("epiworldR_env", new.env(parent = .GlobalEnv), envir = .GlobalEnv)

    models_setup()

    header <- dashboardHeader(
      title = HTML('epiworldR <text style="color: gray; font-size:50%">(alpha)</text>')
    )

    cursor_header_pointer <-
      sprintf(
        "#npis_header_%1$s, #network_header_%1$s, #population_header_%1$s",
        epiworldR_env$models
      ) |>
      paste0(collapse = ", ") |>
      paste("{\n  cursor: pointer;\n}\n")

    sidebar <- do.call(
      "dashboardSidebar",
      c(
        list(
          width = NULL,
          tags$style(
            paste0(
              "#sidebarItemExpanded {overflow: auto;max-height: 100vh;}",
              cursor_header_pointer
            )
          ),
          shinyjs::useShinyjs(),
          shiny::selectInput(
            inputId = "model",
            label = h3("Model"),
            choices = unname(epiworldR_env$models_names)
          )
        ),
        mget(paste0(epiworldR_env$models, "_panel"), envir = .GlobalEnv)
      )
    )

    body <- dashboardBody(
      fluidRow(
        column(12, htmlOutput("model_description"))
      ),
      fluidRow(
        column(6, plotly::plotlyOutput("model_plot") |> shinycssloaders::withSpinner(color = "#009bff")),
        column(6, plotly::plotlyOutput("model_reproductive_plot") |> shinycssloaders::withSpinner(color = "#009bff"))
      ),
      HTML("<br>"),
      fluidRow(
        column(6, verbatimTextOutput("model_summary") |> shinycssloaders::withSpinner(color = "#009bff")),
        column(6, DT::dataTableOutput("model_table") |> shinycssloaders::withSpinner(color = "#009bff"))
      ),
      htmlOutput("download_button"),
      tags$style(type = 'text/css', "#downloadData {position: fixed; bottom: 20px; right: 20px; }"),
      fluidRow(
        column(6, markdown("epiworldRShiny app version 0.0-1 (alpha)")),
        column(6, markdown("**The University of Utah**"))
      )
    )

     ui <- dashboardPage(
        header = header,
        sidebar = sidebar,
        body = body,
        skin = "black"
     )

   server <- function(input, output, session) {

     for (i in c("npis", "network", "population")) {
       for (m in epiworldR_env$models) {
         local({
           id0 <- paste0(i, "_header_", m)
           id1 <- paste0(i, "_inputs_", m)
           shinyjs::onclick(id = id0, shinyjs::toggle(id = id1, anim = TRUE))
         })
       }
     }

     model_id <- shiny::reactive(globalenv()$epiworldR_env$models[input$model])

     model_output <- shiny::eventReactive(
       eventExpr = input[[paste0("simulate_", model_id())]],
       valueExpr = {
         eval(parse(text = paste0("shiny_", model_id(), "(input)")))
       }
     )

     output$model_description <- renderText({

       # Reading the model description from the package
       fn <- system.file(
          "models", paste0("shiny_", model_id(), ".md"),
          package = "epiworldRShiny"
        )

       contents <- if (file.exists(fn))
         readLines(fn, warn = FALSE)
       else
         "No description available."

       markdown(contents)
     })

     output$model_plot <- plotly::renderPlotly({
       model_output()$epicurves_plot()
     })
     output$model_reproductive_plot <- plotly::renderPlotly({
       model_output()$reproductive_plot()
     })
     output$model_summary <- shiny::renderPrint({
       model_output()$model_summary()
     })
     output$model_table <- DT::renderDataTable({
       model_output()$model_table()
     }, options = list(
       scrollY = '600px',
       lengthMenu = c(16, 25, 50),
       pageLength = 16
     ), escape = FALSE)

     output$downloadData <- shiny::downloadHandler(
       filename = function() {
         paste("data", ".csv", sep = "")
       },
       content = function(file) {
         write.csv(model_output()$model_table(), file)
       }
     )

     output$download_button <- shiny::renderUI({
       shiny::downloadButton("downloadData", "Download Data")
     })
   }
   shiny::shinyApp(ui=ui, server=server)
}

