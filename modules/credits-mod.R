# Credits tab module

credits_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    tagList(
        
        br(),
        
        div(
            class = "d-flex justify-content-center",
        
            accordion(
                id = "credits_accordion",
                open = TRUE,
                multiple = TRUE,
                width = "75%",
                margin = "0 auto",
                
                accordion_panel(
                    "About this dashboard app",
                    value = "credits_about",
                    uiOutput(ns("credits_about"))
                ),
                
                accordion_panel(
                    "About the study",
                    value = "credits_study",
                    uiOutput(ns("credits_about_study"))
                ), 
                
                accordion_panel(
                    "Citation information",
                    value = "credits_citation",
                    uiOutput(ns("credits_citation"))
                )

            )
        )
#         
#     
#     card(
#         card_header(
#             "About this app"
#         ),
#         card_body(
#             markdown("This app was created by Jennifer Pokorny, PhD: <https://www.linkedin.com/in/jenpokorny/>."), 
#             markdown("For questions pertaining to the app, please write to Jen at [jenpokorny@gmail.com](mailto:jenpokorny@gmail.com).")
#         )),
#     
#     card(
#         card_header(
#             "Citation information"
#         ),
#         card_body(
#             markdown("Pokorny, J., Conklin, Q., and Saron, C., (2023). shiny: Exploring relationships between meditation practice and well-being during the
# COVID-19 pandemic: Data from the CCC Study. R version 4.1.3 (2022-03-10).")
#             
#         )
#         
#     ),
# 
# 
#     card(
#         card_header(
#             "About the study"
#         ),
#         card_body(
#             p("This CCC study is based out of the University of California Davis
#               and is led by Principal Investigators Quinn Conklin, PhD and
#               Clifford Saron, PhD."),
#             p("People who contributed to the design and implementation of this
#               study include Quinn Conklin, Clifford Saron, Kamilah Majied,
#               Jennifer Pokorny, Savannah VandenBoss, Brandon King, Jue Lin, 
#               Alea Skwara, Amber Davis, Prerana Dewan, Jacob Fernandez, Arielle
#               Limberis, Cavan Patterson, Alex Norman, Anthony Zanesco, Matthew
#               Goodman, Harleen Gill, and Serigne Diaw."),
#             p("Funding for the project was provided by the Yoga Science
#               Foundation, the Mind and Life Institute, the Fetzer Institute 
#               and other anonymous donors."),
#             markdown("To learn more about the study, please visit our study webpage at <https://www.contemplative-coping-covid-19.org/>. For questions about the study, please write to Quinn at [qconklin@ucdavis.edu](qconklin@ucdavis.edu) or Cliff at [cdsaron@ucdavis.edu](cdsaron@ucdavis.edu). ")
#         )
#         
#     )

        #includeMarkdown("./text_mds/credits-page.md")
    )

}


credits_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        ns <- session$ns
        
        output$credits_about <- renderUI({
            tagList(
                markdown("This app was created by Jennifer Pokorny, PhD: <https://www.linkedin.com/in/jenpokorny/>."), 
                markdown("For questions pertaining to the app, please write to Jen at [jenpokorny@gmail.com](mailto:jenpokorny@gmail.com).")
            ) 
        })
        
        
        output$credits_citation <- renderUI({
            tagList(
                markdown("Pokorny, J.J., Conklin, Q., and Saron, C., (2023). shiny: Contemplative Coping during COVID-19: Exploring relationships between stress, meditation and well-being during the COVID-19 pandemic. R version 4.3.1 (2023-06-16).")
            ) 
        })
        
        output$credits_about_study <- renderUI({
            tagList(
                p("The CCC study is based out of the University of California Davis
              and is led by Principal Investigators Quinn Conklin, PhD and
              Clifford Saron, PhD."),
              p("People who contributed to the design and implementation of this
              study include Quinn Conklin, Clifford Saron, Kamilah Majied,
              Jennifer Pokorny, Savannah VandenBoss, Brandon King, Jue Lin, 
              Alea Skwara, Amber Davis, Prerana Dewan, Jacob Fernandez, Arielle
              Limberis, Cavan Patterson, Alex Norman, Anthony Zanesco, Matthew
              Goodman, Harleen Gill, Serigne Diaw, and Ami Singh."),
              p("Funding for the project was provided by the Yoga Science
              Foundation, the Mind and Life Institute, the Fetzer Institute,
              the Nancy Driscoll Foundation, the National Institutes of Health,
              and other anonymous donors."),
              markdown("To learn more about the study, please visit our study webpage at <https://www.contemplative-coping-covid-19.org/>."),
              markdown("For questions about the study, please write to Quinn at [qconklin@ucdavis.edu](qconklin@ucdavis.edu) or Cliff at [cdsaron@ucdavis.edu](cdsaron@ucdavis.edu).")
            ) 
        })
        
        
        
    })
    
    
}
