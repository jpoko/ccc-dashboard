# Home tab module

home_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    
    tagList(
        
#         
#         
#         div(
#             class="clearfix",
#             style = "width:94%; margin-left: auto; margin-right: auto;",
#             tags$img(src='home-page-image-2.jpg', class="col-md-6 float-md-end mb-3 ms-md-3"),
#             #style="margin-bottom:0px; width:75%; margin: 0 auto;",
#             h4(strong("Contemplative Coping during COVID-19"), class="text-center"),
#             p(strong("Exploring relationships between stress, meditation, and well-being
#                during the COVID-19 pandemic"), class="text-center"),
#             p("Contemplative Coping during COVID-19 (CCC) is a multidisciplinary
#                  research study designed to learn about the lived experiences of 
#                  meditation practitioners in the United States during the COVID-19 
#                  pandemic."),
#             p("This dashboard allows you to explore relationships between 
#                  many of the stress, meditation and well-being variables measured 
#                  during the study."),
#             markdown("You can view the ‘About’ page for an overview of the 
#                         study design, procedure, and measures before exploring 
#                         the data. You can also learn more about the study and 
#                         research team by visiting our website at: <a href='https://contemplative-coping-covid-19.org/' target='_blank'>https://contemplative-coping-covid-19.org</a>.", .noWS = "outside"),
#             
#             p(strong("Tips for using this dashboard")),
#             tags$ul(
#                 tags$li("View the dashboard on a computer or laptop for the best user experience."),
#                 tags$li("Use the main navigation bar at the top of the screen to navigate between pages."),
#                 tags$li("Some pages include additional sections indicated by another row of tabs that appear below the main navigation bar."),
#                 tags$li("The dashboard is dynamic, so you will be able to select the variables you want to investigate and apply different options for how to view those data."),
#                 tags$li("When you see a vertical blue bar with a white arrow, this indicates that there is another panel you can open and close to see additional options.")
#             )
#             ),
#         
#         div(
#             style = "width:94%; margin-left: auto; margin-right: auto;",
#             p(strong("Helpers")),
#             p("Most pages include drop-down panels and pop-up windows that include additional information about what is being presented."),
#             p("If you have questions about the visualizations you’re viewing, the data being presented, or which settings or options are available to you, 
# look for the following item and icons:")
# ),
#             div(
#                 class="clearfix align-items-center",
#                 style = "width:94%; margin-left: auto; margin-right: auto;",
#                 
#                 tags$img(src='about-helper.png', style = "width:35%; max-width: 400px; min-width: 300px; flex-shrink: 0; flex-grow: 1;", class="col-md-4 float-md-start mb-1 ms-md-3"),
#                 
#                 div(
#                     class="text ms-5",
#                     p("Click on the 'About' bars to open drop-down panels describing the information presented.")
#                 )
#                 
#             ),
# div(
#     style = "width:88%; margin-left: auto; margin-right: auto;",
#     class="mb-3",
#             span(
#                 tooltip(
#                     bs_icon("question-square", size = "1.5rem", class="me-2"),
#                     "This is information that appears when you hover over the icon."
#                 ),
#                 "Hover over the question mark icon for more information."
#             ),
#             
#             br(),br(),
#             
#             span(
#                 popover(
#                     bs_icon("info-circle", size = "1.5rem", class="me-2"),
#                     "This is information that appears when you click on the icon."
#                 ),
#                 "Click on the information icon for more information - this will open a popup panel with additional information."
#             )
# 
#         ),

    div(
        class="container text",
        style="width:75%",
        h4(strong("Contemplative Coping during COVID-19"),class="text-center"),
        p(strong("Exploring relationships between stress, meditation, and well-being
               during the COVID-19 pandemic"),class="text-center")
    ),

        
    # Image/logo ----
    div(
        class = "row justify-content-center align-items-center",
        div(
            class = "col-8",
            img(
                src='shiny-home-art.png',
                class = 'img-fluid',
                style = "max-width: 100%; max-height: 300px; height: auto; display: block; margin-left: auto; margin-right: auto;"
            )
        )
        # style="width:75%",
        # tags$img(src='shiny-home-art.png')
        
    ),
    
    div(
        #class = "d-flex justify-content-center",
        class="container text",
        style="width:75%",
        p("Contemplative Coping during COVID-19 (CCC) is a multidisciplinary research study designed to learn about the lived experiences of meditation practitioners in the United States during the COVID-19 pandemic."),
        p("This dashboard allows you to explore relationships between many of the stress, meditation and well-being variables measured during the study."),
        markdown("You can view the ‘About’ page for an overview of the study design, procedure, and measures before exploring the data. You can also learn more about the study and research team by visiting our website at: <a href='https://contemplative-coping-covid-19.org/' target='_blank'>https://contemplative-coping-covid-19.org</a>.", .noWS = "outside"),
    
    ),

    # Accordion----
    # Formatted: centered, width is 75% of screen width
    div(
        class = "d-flex justify-content-center",
        style = "width: 75%; margin: 0 auto;",
        
        accordion(
            id = "dashboard_accordion",
            open = TRUE,
            multiple = TRUE,
            width = "95%",
            margin = "0 auto",
    
            ## How to use -----
            accordion_panel(
                "Tips for using this dashboard",
                value = "dashboard_how_use",
                uiOutput(ns("how_use_fill"))
            ),
    
            ## Helpers ----
            accordion_panel(
                "Helpers",
                value = "dashboard_helpers",
                uiOutput(ns("helpers"))
            )
        )
    ) # close accordion div


    # div(
    #     class = "d-flex justify-content-center",
    #     style = "width: 75%; margin: 0 auto;",
    #     accordion(
    #         id = "about_study_accordion",
    #         multiple = FALSE,
    #         width = "95%",
    #         margin = "0 auto",
    #         
    #         #### Aim ----
    #         accordion_panel(
    #             "Aim",
    #             value = "aim_panel",
    #             htmlOutput(ns("aim_accordion_text"))
    #         ),
    #         
    #         #### Study design ----
    #         accordion_panel(
    #             "Study Design",
    #             value = "study_design_panel",
    #             uiOutput(ns("study_design_accordion_fill"))
    #         ),
    # 
    #     )
    #     
    # )
    
    






    )
        
    
    
    

        # Accordion----
        # Formatted: centered, width is 75% of screen width
        # div(
        #     class = "d-flex justify-content-center",
        #     accordion(
        #         id = "dashboard_accordion",
        #         open = TRUE,
        #         multiple = TRUE,
        #         width = "75%",
        #         margin = "0 auto",
        #     
        #         ## How to use -----
        #         accordion_panel(
        #             "Tips for using this dashboard",
        #             value = "dashboard_how_use",
        #             uiOutput(ns("how_use_fill"))
        #         ),
        #     
        #         ## Helpers ----
        #         accordion_panel(
        #             "Helpers",
        #             value = "dashboard_helpers",
        #             uiOutput(ns("helpers"))
        #         )
        #     )
        # ) # close accordion div
        # 
     # close taglist

}


home_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        ns <- session$ns
        

        output$how_use_fill <- renderUI({
            
            tags$ul(
                tags$li("View the dashboard on a computer or laptop for the best user experience."),
                tags$li("Use the main navigation bar at the top of the screen to navigate between pages."),
                tags$li("Some pages include additional sections indicated by another row of tabs that appear below the main navigation bar."),
                tags$li("The dashboard is dynamic, so you will be able to select the variables you want to investigate and apply different options for how to view those data."),
                tags$li("When you see a vertical blue bar with a white arrow, this indicates that there is another panel you can open and close to see additional options.")
            )
         })
         
        
        output$helpers <- renderUI({

            tagList(
                
                p("Most pages include drop-down panels and pop-up windows that include additional information about what is being presented."),
                p("If you have questions about the visualizations you’re viewing, the data being presented, or which settings or options are available to you, 
look for the following item and icons:"),

                div(
                    class="clearfix align-items-center",
                    style = "width:94%; margin-left: auto; margin-right: auto;",
                    tags$img(src='about-helper.png', style = "width:35%; max-width: 400px; min-width: 200px; flex-shrink: 0; flex-grow: 1;", class="col-md-4 float-md-start mb-1 ms-md-3 me-2"),
                    
                    div(
                        class="text ms-5",
                        p("Click on the 'About' bars to open drop-down panels describing the information presented. (Image here is provided as an example and will not open if you click on it.)")
                    )
                ),

                div(
                    style = "width:88%; margin-left: auto; margin-right: auto;",
                    class="mb-3",
                    span(
                        tooltip(
                            bs_icon("question-square", size = "1.5rem", class="me-2"),
                            "This is information that appears when you hover over the icon."
                        ),
                        "Hover over the question mark icon for more information."
                    ),
                    
                    br(),br(),
                    
                    span(
                        popover(
                            bs_icon("info-circle", size = "1.5rem", class="me-2"),
                            "This is information that appears when you click on the icon."
                        ),
                        "Click on the information icon for more information - this will open a popup panel with additional information."
                    ),
                    
                    br(),br(),
                    
                    span(
                        popover(
                            bs_icon("gear", size = "1.5rem", class="me-2"),
                            "This will contain additional options for you to adjust.",
                            sliderInput(
                                inputId = "example_slider_input",
                                label = tags$strong("Select level"),
                                min = 0,
                                max = 100,
                                value = 10,
                                step = 5
                            )
                        ),
                        "Click on the gear icon for additional input options - this will open a popup panel with options to make adjustments to the plots (currently only used on the 'Correlation matrices' page)."
                    )
                    
                )
                
                # 
                # 
                # p("On most pages, there is information to help users
                #   understand what is presented, typically under a light blue
                #   header named 'About' that opens when you click on it."),
                # hr(),
                # 
                # p("You may also see some symbols - these will provide
                #   additional information about either what is presented or
                #   about options available to you."),
                # p("Below are the icons you will see and how you can interact
                #   with them when you come across them in the dashboard:"),
                # 
                # span(
                #     tooltip(
                #         bs_icon("question-square", size = "1.5rem"),
                #         "This is information that appears when you hover over the icon."
                #     ),
                #     "Hover over the icon for more information."
                # ),
                # 
                # br(), br(),
                # 
                # span(
                #     popover(
                #         bs_icon("info-circle", size = "1.5rem"),
                #         "This is information that appears when you click on the icon."
                #     ),
                #     "Click on the icon for more information."
                # )

            )
        })

    
        
        
    })
    
    
    
}
