# UI function for the warning message

tmpt_warning_ui <- function(id) {
    ns <- NS(id)
    
    uiOutput(NS(id, "timepoint_comparison_warning"))
 
}

# Server function for the warning message
tmpt_warning_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        output$timepoint_comparison_warning <- renderText({
                includeMarkdown("./text_mds/tmpt-comparison-warning.md")
        })
    })
}


tmpt_warning_demo <- function() {
    ui <- fluidPage(tmpt_warning_ui("x"))
    server <- function(input, output, session) {
        tmpt_warning_server("x")
    }
    shinyApp(ui, server)
}