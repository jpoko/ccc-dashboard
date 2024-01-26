# Demographics tab module

# Define UI ----
demographics_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    tagList(
        
        #h5("General demographic information about participants"),
        
        uiOutput(ns("about_accordion")),
        
        
        navset_card_pill(
            id = ns("demographic_view_info_tabs"),
            height = "1500px",
            
            sidebar = sidebar(
                radioButtons(
                    inputId = ns("demo_category"),
                    label = tags$strong("Select a demographic characteristic"),
                    choices = c(
                        "Age"              = "Age",
                        "Education"        = "Education",
                        "Ethnicity"        = "Ethnicity",
                        "Gender"           = "Gender",
                        "Household income" = "Income",
                        "Location"         = "Location",
                        "Sexual orientation"      = "Orientation"
                    )
                )
            ),
            
            
            nav_panel(
                title = "Overview",
                uiOutput(ns("view_demographic_data"))
            ),
            nav_panel(
                title = "Description",
                uiOutput(ns("demographic_category_description"))
            ),
            nav_panel(title = "Categorization",
                      value = "categorizationDemoTab",
                      uiOutput(ns("sankey_diagram_page"))
                      )
        )
        

    )
 
} 


# Define server ----
demographics_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        ns <- session$ns
        
        
        ## About accordion ----
        demographic_about_accordion <- accordion(
            id = "about_demographics",
            open = FALSE,
            
            accordion_panel(
                title = "About",
                p("Here you can find information about the demographics of the
                  participants in the study, including their age, education level,
                  ethnicity, gender, household income, location, and sexual
                  orientation. Use the sidebar to select the characteristic of
                  interest."),
                hr(),
                p(strong("Overview tab")),
                p("For Location, you will see a map that shows the locations of
                  participants using zip code information."),
                p("For all characteristics except Location, you will see a treemap
                  and table that provides the number of participants in each
                  category within the demographic characteristic."),

                p(strong("Description tab")),
                p("The descriptions included in the second tab provide more detailed information about how we obtained the information presented for a given characteristic, including the specific question that was asked of participants, the response options available to them, and what kind of data preparation was done by the research team to present the data as you see it. In many cases demographic categories were combined to preserve participants’ privacy."),
                
                p("For example, we asked participants to provide their age as a whole number, but here we present participants' age in bins with a 10-year range (e.g., 31-40 years old). However, the oldest age bin spans 17 years (71-88 years old). In this case, there may have only been one or two 86-year-old participants, making them easy to identify, but there were 37 participants in the 71–88-year-old range."),
                p(strong("Categorization tab")),
                p("Ethnicity, Gender, and Sexual orientation each have an additional 'Categorization' tab that provides a visualization depicting how we combined participant responses into broader categories. Detailed information on how and why this was done is provided under the corresponding Description tab.
")
            )
        )
        
        
        output$about_accordion <- renderUI({
            demographic_about_accordion
        })
        
        
        ## Overview tab ----
        
        ### Tree maps ----
        
        output$demographic_treemap_plots <- renderPlotly({
            req(input$demo_category)
            
            if (input$demo_category != "Location") {
                plot_treemap_demo(dat_mini_subscale, input$demo_category)
                
            }
            
            # validate(
            #     need(input$demo_category, "Please wait...")
            # )
            #plot_treemap_demo(dat_mini_subscale, input$demo_category)
        })
        
        
        ### Table of demographic counts & percentages ----
        
        output$demographic_table <- DT::renderDataTable({
            dat_mini_subscale |>
                dplyr::group_by_at(input$demo_category) |>
                dplyr::summarize(Count = n()) |>
                dplyr::mutate(Percent = Count/sum(Count),
                              Percent = round(Percent * 100, 2))
        }, options = list(paging = FALSE, dom = "t"), rownames = FALSE, fillContainer = TRUE)
        
        
        ### Zip code map ----
        
        output$zipcodeMap <- renderLeaflet({
            req(input$demo_category == "Location")
            leaflet(zipcode_dat) |>
                addTiles() |>
                setView(-93.65, 42.0285, zoom = 3) |>
                addMarkers(clusterOptions = markerClusterOptions())
        })
        
        
        ## Create Overview panel ----
        ## Create UI dynamically based on demographic variable selected
        
        output$view_demographic_data <- renderUI({
            req(input$demo_category)
            
            # If 'Location' is selected, show map
            if (input$demo_category == "Location") {
                tagList(
                    card(
                        full_screen = TRUE,
                        card_header(
                            "Map of locations",
                            popover(
                                trigger = bs_icon("info-circle", title = "Map location info"),
                                title = "Map info",
                                p("Interactive map showing where our participants 
                                  are from. You can zoom in/out and around to see 
                                  more specifically the area where the 
                                  participants reside. You can also expand the
                                  map by clicking on the expand button that
                                  appears on the bottom right corner when you 
                                  hover over the map."),
                                p(strong("NOTE:"), "the locations presented are 
                                  from participants' zip code, not actual address 
                                  location.")
                            )
                        ),
                        card_body(
                            # remove card padding
                            class = "p-0",
                            # Map of zipcode locations
                            leafletOutput(ns("zipcodeMap"))
                        ),
                        card_footer(
                            p("NOTE: locations are centroids of the zip code provided
                      by participants, NOT the actual address of any participant.")
                        )
                    )
                )
                
            } else {
                
                # If not 'Location', show treemap and summary table
                tagList(
                    
                    card(
                        full_screen = TRUE,
                        card_header(
                            "Treemap",
                            popover(
                                trigger = bs_icon("info-circle", title = "Treemap info"),
                                title = "Treemap info",
                                markdown("
                                    The figure (treemap) shows the relative proportion of participants in the demographic category selected.  
                                    
                                    The figure is interactive. You can:  
                                    
                                    * Hover over each box to see the number of people in that particular group    
                                    * Click on a box to zoom in and out to better read the category label  
                                    * Expand the figure by clicking on the ‘Expand’ button that appears on the bottom right corner when you hover over the treemap. ![expand](expand-button.png)  
                                    ")
                            )),
                            card_body(
                                # remove card padding
                                class = "p-0",
                                plotlyOutput(ns("demographic_treemap_plots"))
                            ),
                        card_footer(
                            p("Click on the smaller boxes to zoom in and read the labels.")
                        )
                        ),
                    
                    card(
                        card_header(
                            "Table of demographic information",
                            popover(
                                trigger = bs_icon("info-circle", title = "Demo table info"),
                                title = "Table info",
                                p("Table shows the number of participants in each group of the demographic category selected. The table can be sorted in ascending or descending order by clicking on the table headings.")
                            )
                        ),
                        card_body(
                            DT::dataTableOutput(ns("demographic_table")) 
                        )
                    )    
                )
            }
        })
        
        
        
        
        ## Description tab ----
        
        ### Demographic category descriptions ----
        
        demographic_description_file <- reactive({
            # req(input$demo_category)
            switch(input$demo_category,
                   "Age" = "./text_mds/demographics_descriptions/age-description.md",
                   "Gender" = "./text_mds/demographics_descriptions/gender-description.md",
                   "Ethnicity" = "./text_mds/demographics_descriptions/ethnicity-description.md",
                   "Orientation" = "./text_mds/demographics_descriptions/orientation-description.md",
                   "Education" = "./text_mds/demographics_descriptions/education-description.md",
                   "Income" = "./text_mds/demographics_descriptions/income-description.md",
                   "Location" = "./text_mds/demographics_descriptions/location-description.md")
        })
        
        ## Create Description panel ---- 
        output$demographic_category_description <- renderUI({
            
            includeMarkdown(demographic_description_file())
        })
        
        
        
        ## Categorization tab ----
        
        ### Show tab selectively ----
        ## Show tab only if select gender, ethnicity, or orientation 
        
        observe({
            req(input$demo_category)
            
            if (input$demo_category %in% c("Gender", "Ethnicity", "Orientation")) {
                nav_show(id = "demographic_view_info_tabs", 
                         target = "categorizationDemoTab")
            } else {
                nav_hide(id = "demographic_view_info_tabs", 
                         target = "categorizationDemoTab")
            }
        })
        
        
        ### Sankey stage input options ----
        
        output$sankey_diagram_stage <- renderUI({
            req(input$demo_category)
            
            if (input$demo_category %in% c("Ethnicity", "Gender", "Orientation")) {
                radioButtons(
                    inputId = ns("sankey_diagram_stage"),
                    label = span(tags$strong("Select stage:"),popover(bs_icon("info-circle"), title = "Recruitment stage info", p("You can select different recruitment stages to see
                      participant responses from different stages of the
                      recruitment process."),
                      
                      h5("Stage descriptions:"),
                      tags$ul(
                          tags$li(tags$strong("applied:"), " all people who applied to participate in the study"),
                          tags$li(tags$strong("screened:"), " people who passed our initial screening"),
                          tags$li(tags$strong("admitted:"), " people who were admitted into the study"),
                          tags$li(tags$strong("consented:"), " people who provided consent to participate"),
                          tags$li(tags$strong("started:"), " people who started at the baseline data collection time point")))),
                    
                    #label = tags$strong("Select stage:"),
                    choices = c("applied", 
                                "screened",
                                "admitted", 
                                "consented",
                                "started"),
                    selected = "applied")
            } else {
                br()
            }
        })
        
        
        
        ### Sankey diagrams ----

        output$sankey_diagram_demographic_category_plotly <- renderPlotly({
            
            req(input$demo_category, input$sankey_diagram_stage)
            
            # get data file based on what category selected
            if (input$demo_category == "Ethnicity") {
                get_sankey_data <- nested_ethnicity_levels
            } else if (input$demo_category == "Gender") {
                get_sankey_data <- nested_gender_levels
            } else if (input$demo_category == "Orientation") {
                get_sankey_data <- nested_orientation_levels
            } 
            
            
            # get index of which stage to examine
            index_data <- which(sapply(get_sankey_data,
                                       function(x) x$name == input$sankey_diagram_stage))
            
            # get that specific data (selected demo category and selected stage)
            sankey_data <- get_sankey_data[[index_data]][["data"]]
            
            
            # get color categories for that data
            color_category_df <- get_sankey_data[[which(sapply(get_sankey_data, function(x) x$name == "color_categories"))]][["data"]]
            
            # create nodes with appropriate colors
            nodes <- data.frame(
                name = c(sankey_data$source, color_category_df$categories),
                colors = c(sankey_data$color, color_category_df$color_list)
            )
            
            sankey_data$IDsource = match(sankey_data$source, nodes$name) - 1
            
            sankey_data$IDtarget = match(sankey_data$target, nodes$name) - 1
            
            #create sankey plot using plotly
            sankey_fig <- plot_ly(
                type = "sankey",
                orientation = "h",

                node = list(
                    label = nodes$name,
                    color = nodes$colors,
                    pad = 25,
                    thickness = 30,
                    line = list(
                        color = "black", width = .5
                    )),
                link = list(
                    source = sankey_data$IDsource,
                    target = sankey_data$IDtarget,
                    value = sankey_data$value
                )
            )

            sankey_fig <- sankey_fig |>
                plotly::layout(
                    font = list(size = 12),
                    margin = list(t = 10, b = 10, l = 0, r = 0)
                    ) |>
                plotly::config(displayModeBar = FALSE)

            sankey_fig
            
        })


        ### Make sankey plot card ----
        sankey_plot_card <- card(
            full_screen = TRUE,
            height = 1500,
            card_header(
                "Sankey plot",
                # div(class = "float-left","Sankey plot"),
                # div(class = "float-right",
                # popover(
                #     trigger = bs_icon("info-circle", title = "Sankey stage info"),
                #     title = "Recruitment stage",
                #     p("You can select different recruitment stages to see
                #       participant responses from different stages of the
                #       recruitment process."),
                # 
                #     h5("Stage descriptions:"),
                #     tags$ul(
                #         tags$li(tags$strong("applied:"), " all people who applied to participate in the study"),
                #         tags$li(tags$strong("screened:"), " people who passed our initial screening"),
                #         tags$li(tags$strong("admitted:"), " people who were admitted into the study"),
                #         tags$li(tags$strong("consented:"), " people who provided consent to participate"),
                #         tags$li(tags$strong("started:"), " people who started at the baseline data collection time point")), 
                # )
                ),
                
                #class = "d-flex align-items-center gap-1"
            #),
        
            layout_sidebar(
                fillable = TRUE,
                sidebar = sidebar(
                    title = tags$strong("Recruitment stage"),
                    # 
                    # title = popover(
                    #     trigger = bs_icon("info-circle",
                    #                       title = "Recruitment stage info"),
                    #     "Recruitment stage",
                    #     p("You can select different recruitment stages to see
                    #   participant responses from different stages of the
                    #   recruitment process."),
                    #   
                    #   h5("Stage descriptions:"),
                    #   tags$ul(
                    #       tags$li(tags$strong("applied:"), " all people who applied to participate in the study"),
                    #       tags$li(tags$strong("screened:"), " people who passed our initial screening"),
                    #       tags$li(tags$strong("admitted:"), " people who were admitted into the study"),
                    #       tags$li(tags$strong("consented:"), " people who provided consent to participate"),
                    #       tags$li(tags$strong("started:"), " people who started at the baseline data collection time point"))
                    # ),
                    # 
                    #tags$strong("Recruitment stage"),
                                  position = "right",
                                  open = TRUE,
                                  uiOutput(ns("sankey_diagram_stage"))
                                  ),
                
            plotlyOutput(ns("sankey_diagram_demographic_category_plotly"))
        )
        )
        
        
        #### About sankey -----
        
        output$sankey_diagram_page <- renderUI({
            
            tagList(
                h4("Sankey diagrams"),
                accordion(
                    id = "about_demo_categorization_accordion",
                    open = FALSE,
                    
                    accordion_panel(
                        "About",
                        p("For Ethnicity, Gender, and Sexual orientation, participants
                          were able to select as many options as were applicable
                          and/or provide their own response. This resulted
                          in a large number of possible combinations, some of
                          which only had 1 person with that particular
                          combination. We decided to reduce the number of 
                          categories. Details about how and why this was done
                          can be found under the Description tab."),
                        p("The Sankey diagram below is a visualization of how we grouped participant responses into fewer categories."),
                        p("The original responses provided by the participants are shown on the left and the categories that they were grouped into are on the right."),
                        markdown("
                                 The diagram is interactive. You can...   
                                 
                                 * change the 'stage' to see participant responses across the different stages of recruitment using the options to the right of the diagram.   
                                 * hover over parts of the diagram for more information.  
                                 * expand the diagram by clicking on the 'Expand' button that appears in the bottom right corner when you hover over the diagram.  
                                 ")
                    )
                ),
                
                br(), 
                sankey_plot_card
            )
        })
        
        
    })
    

}