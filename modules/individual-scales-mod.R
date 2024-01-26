# Individual scales tab module

# Define UI ----
individual_scales_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    tagList(
        
        layout_sidebar(
            sidebar = sidebar(
                
                width = "25%",
                
                selectInput(
                    inputId = ns("scale_main_group_single"),
                    label = tags$strong("Select scale"),
                    choices = unique(individual_scale_variables$scale.main.group)
                ),
                
                selectInput(
                    inputId = ns("scale_sub_group_single"),
                    label = span(tags$strong("Select score"), tooltip(bs_icon("question-square"), "Some scales have total scores and subscales, some have total scores only, and some have only subscales. The options here will update based on which scale is selected.")),
                    choices = NULL
                ),
                
                pickerInput(
                    inputId = ns("single_scale_view"),
                    label = tags$strong("What to view"),
                    choices = list(
                        General = c("Description" = "description",
                                    "Descriptives" = "descriptives"),
                        Distribution = c("Raincloud plot" = "raincloud_plot",
                                         "Histogram" = "histogram_plot",
                                         "Estimated density plot" = "estimated_density_plot",
                                         "Box plot" = "box_plot",
                                         "Violin plot" = "violin_plot"),
                        `Over time` = c("Changes between time points" = "change_time_point_corset",
                                        "Scale levels by time point" = "levels_by_time_point_alluvial",
                                        "Scores by date" = "scores_by_date",
                                        "Scores by date and time point" = "scores_by_date_by_time_point"),
                        `Individual items` = c("Item response distributions" = "item_response_distribution"),
                        
                        `Missing data` = c("Missing all items" = "missing_all_items",
                                           "Missing individual items" = "missing_individual_items",
                                           "Missing some or all items" = "missing_some_all_items")
                    )
                ),
                
                hr(style = "border-top: 1px solid #494d4a;"),
                
                # Lay description of scale
                uiOutput(ns("lay_scale_description_single"))
            
                ), # close sidebar
            
            
            ### Description ----
            conditionalPanel(
                ns = ns,
                condition = c("input.single_scale_view=='description'"),
                
                # show description of scale
                uiOutput(ns("long_scale_description"))
            ),
            
            ### Descriptives ----
            conditionalPanel(
                ns = ns,
                condition = "input.single_scale_view=='descriptives'",
                
                # show descriptives page (table)
                uiOutput(ns("descriptives_panel"))
            ),
            
            ### Response distribution ----
            conditionalPanel(
                ns = ns,
                condition = "input.single_scale_view == 'item_response_distribution'",
                
                h4("Feature forthcoming")
            ),
            
            ### Raincloud plots ----
            conditionalPanel(
                ns = ns,
                condition = "input.single_scale_view == 'raincloud_plot'",
                
                # show raincloud page
                uiOutput(ns("raincloud_panel"))
                
            ),
            
            ### Histogram ----
            conditionalPanel(
                ns = ns,
                condition = "input.single_scale_view == 'histogram_plot'",
                
                # show histogram page
                uiOutput(ns("histogram_panel"))
            ),
            
            ### Estimated density ----
            conditionalPanel(
                ns = ns,
                condition = "input.single_scale_view == 'estimated_density_plot'",
                
                # show estimated density page
                uiOutput(ns("estimated_density_panel"))
            ),
            
            ### Box plot ----
            conditionalPanel(
                ns = ns,
                condition = "input.single_scale_view == 'box_plot'",
                
                # show boxplot page
                uiOutput(ns("boxplot_panel"))
            ),
            
            ### Violin plot ----
            conditionalPanel(
                ns = ns,
                condition = "input.single_scale_view == 'violin_plot'",
                
                # show violin plot page
                uiOutput(ns("violin_plot_panel"))
            ),
            
            
            
            ### Changes between time points (corset plots) ----
            conditionalPanel(
                ns = ns,
                condition = "input.single_scale_view == 'change_time_point_corset'",
                
                h4("Change in scores between time points"),
                
                # show corset plot page
                uiOutput(ns("corset_panel"))
              
            ),
            
            
            ### Scores by time point (alluvials) ----
            conditionalPanel(
                ns = ns,
                condition = "input.single_scale_view == 'levels_by_time_point_alluvial'",
                
                h4("Change in scale levels between time points"),
                
                # show alluvial diagram page
                uiOutput(ns("alluvial_panel"))
                
            ),
            
            ### Scores by time point (ORIGINAL CODE - corset and alluvial) ----
            # conditionalPanel(
            #     ns = ns,
            #     condition = "input.single_scale_view == 'scores_by_time_point'",
            #     
            #     h4("Change in scores between time points"),
            #     
            #     navset_card_pill(
            #         id = ns("scores_by_time_point_card_tabs"),
            #         
            #         nav_panel(
            #             title = "Corset",
            #             value = "corsetTab",
            #             uiOutput(ns("corset_tab"))
            #         ),
            #         
            #         nav_panel(
            #             title = "Alluvial",
            #             value = "alluvialTab",
            #             uiOutput(ns("alluvial_tab"))
            #         )
            #     )
            #     
            # ),
            
            
            ### Scores by date ----
            conditionalPanel(
                ns = ns,
                condition = "input.single_scale_view == 'scores_by_date'",
                
                # show scores by date page
                uiOutput(ns("scores_by_date_panel"))
            ),
            
            
            ### Scores by date by time point ----
            conditionalPanel(
                ns = ns,
                condition = "input.single_scale_view == 'scores_by_date_by_time_point'",
                
                # show scores by date by time point page
                uiOutput(ns("scores_by_date_by_time_point_panel"))
            ),
            
            ### Missing all items ----
            conditionalPanel(
                ns = ns,
                condition = "input.single_scale_view == 'missing_all_items'",
                
                h4("Feature forthcoming")
                
            ),
            
            ### Missing individual items ----
            conditionalPanel(
                ns = ns,
                condition = "input.single_scale_view == 'missing_individual_items'",
                
                h4("Feature forthcoming")
                
            ),
            
            ### Missing some/all items ----
            conditionalPanel(
                ns = ns,
                condition = "input.single_scale_view == 'missing_some_all_items'",
                
                h4("Feature forthcoming")
                
            )
            
            
        ) # close layout_sidebar
    ) # close tagList

} 


# Define server ----
individual_scales_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        ns <- session$ns
        
        ## Prepare data ----
       
        ### General ----
        
        # initialize the selected_subscale_info object - 
        # will be row of data from variable information of the scale selected
        
        selected_subscale_info <- reactiveValues(data = NULL)
        
        
        # Filter data to subscales for selected main scale
        filtered_subscale_options <- reactive({
            selected_category <- input$scale_main_group_single
            
            subscale_options <- individual_scale_variables |>
                dplyr::filter(
                    scale.main.group == selected_category
                )
            
            updateSelectInput(session, "scale_sub_group_single", choices = subscale_options$scale.sub.group)
            
            return(subscale_options)
        })
        
        
        # Get selected subscale input, save row of variable information to use elsewhere in selected_subscale_info
        observe({
            # Get the selected subscale input
            selected_subscale_value <- input$scale_sub_group_single
            
            # Filter to just variable of interest
            filtered_subscale <- filtered_subscale_options() |>
                dplyr::filter(scale.sub.group == selected_subscale_value)
            
            # set variable info
            selected_subscale_info$data <- filtered_subscale
            
        })
        
        
        
        # get the index of the total columns for the scale selected, used in dat_mini_subscale
        index_individual_scale_selected_explore <- reactive({
            
            col_interest <- paste("^", selected_subscale_info$data$variable.name, "_total_", sep = "")
            total_cols_interest <- grep(col_interest, colnames(dat_mini_subscale))
            return(total_cols_interest)
        })

        
        # Get the data used for most of the plots - long data

        data_individual_scale_selected_explore <- reactive({
            selected_dat <- dat_subscale |>
                dplyr::filter(measure %in% selected_subscale_info$data$variable.name)
            return(selected_dat)
        })
      
        
        # get abbreviation of scale to use in plots
        abbreviation_individual_scale_selected_explore <- reactive({
            
            abbreviation <- selected_subscale_info$data$variable
            
            return(abbreviation)
        })
        
        
        #### Colors to use ----
        
        color_palette_tmpt <- reactive({
            
            tmpt_pattern <-  selected_subscale_info$data$tmpt_pattern
            
            if (tmpt_pattern == "tmpt_pre_post") {
                c("T1" = "#fde725", # yellow
                  "T4" = "#472c7a" # purple
                )
            } else if (tmpt_pattern == "tmpt_t4") {
                c(
                    "T4" = "#472c7a" # purple
                )
            } else if (tmpt_pattern == "tmpt_t1") {
                c("T1" = "#fde725" # yellow
                )
            } else if (tmpt_pattern == "tmpt_all") {
                c(
                    "T1" = "#fde725", # yellow
                    "T2" = "#35b779", # green
                    "T3" = "#316883", # blue
                    "T4" = "#472c7a" # purple
                )
            }
          
        })
        
        
        #### Height of plots ----
        
        plot_height_tmpt <- reactive({

            tmpt_pattern <-  selected_subscale_info$data$tmpt_pattern
            
            if (tmpt_pattern == "tmpt_pre_post") {
                plot_height = 600
            } else if (tmpt_pattern %in% c("tmpt_t1", "tmpt_t4")) {
               plot_height = 400
            } else if (tmpt_pattern == "tmpt_all") {
                plot_height = 800
            }
            return(plot_height)
        })
        
        
        ## Build components to show
        
        ### Short scale descriptions----
        scale_single_lay_description_file <- reactive({
            req(selected_subscale_info$data$lay.filename)
            
            # get file path and file name from variable information data frame
            file_path <- selected_subscale_info$data$lay.filename
            return(file_path)
          
        })
        
        output$lay_scale_description_single <- renderUI({
            includeMarkdown(scale_single_lay_description_file())
        })
        
        
        ### Time point comparison warning ----

        tmpt_warning_server("warning_descriptives")
        tmpt_warning_server("warning_raincloud")
        tmpt_warning_server("warning_histogram")
        tmpt_warning_server("warning_estimated_density")
        tmpt_warning_server("warning_boxplot")
        tmpt_warning_server("warning_violin_plot")
        tmpt_warning_server("warning_scores_by_time_point")
        tmpt_warning_server("warning_scores_by_date")
        tmpt_warning_server("warning_scores_by_date_by_time_point")

        
        
        
        
        ### Detailed scale descriptions----
        scale_detailed_description_file <- reactive({
            req(selected_subscale_info$data$detailed.filename)
            
            # get file path and file name from variable information data frame
            file_path <- selected_subscale_info$data$detailed.filename
            
            return(file_path)
            
        })
        
        output$long_scale_description <- renderUI({
            includeMarkdown(scale_detailed_description_file())
        })

        
        
        ### Descriptives page ----
        
        ## Heading info 
        
        descriptives_heading_text <- reactive({
            req(selected_subscale_info$data$variable.name)
            
            heading <- str_glue("Descriptive stats of  {abbreviation_individual_scale_selected_explore()} scores")
            return(heading)
        })
       
        output$descriptives_heading <- renderText({
            descriptives_heading_text()
        })
        
        
        ## Table info 
        
        descriptives_scale_selected <- reactive({ 
            req(selected_subscale_info$data$variable.name)
            
            # num NAs for total scores of interest at each time point
            freq_nas <- dat_mini_subscale %>% 
                dplyr::select(all_of(index_individual_scale_selected_explore())) %>% 
                dplyr::summarize(across(everything(), ~sum(is.na(.)))) %>% 
                t(.) %>% 
                as.data.frame()
            
            # summary stats
            total_summary <- as.data.frame(psych::describe(dat_mini_subscale[index_individual_scale_selected_explore()]))
            
            # add num NAs and round numbers
            total_summary <- total_summary %>% 
                mutate(NAs = freq_nas[,1]) %>% 
                mutate(across(where(is.numeric), round, 2))
            
            # add time point
            total_summary <- total_summary %>% 
                rownames_to_column(var = "timept") %>% 
                mutate(timept = timept %>% str_extract(".{2}$") %>% str_to_upper()) %>% 
                remove_rownames() %>% 
                dplyr::select(-c(vars, trimmed, mad))
            
            return(total_summary) 
        })
        
        
        ## Print table 
        
        output$descriptives_table <- DT::renderDataTable({
            descriptives_scale_selected()
        }, options = list(paging = FALSE, dom = "t"), rownames = FALSE)
        
        

        #### Compose descriptives page ----
        
        output$descriptives_panel <- renderUI({
            tagList(
                h4(textOutput(ns("descriptives_heading"))),
                br(),
                # show descriptives table
                DT::dataTableOutput(ns("descriptives_table")),
                br(),
                
                # show warning about comparing time points for particular scales 
                if (input$scale_main_group_single %in% c('everyday discrimination (EDISC)', 'major discrimination (MDISC)', 'race-related experiences (RaLES)', 'cumulative adversity (CAM)')) {
                    tmpt_warning_ui(NS(id, "warning_descriptives"))
            }
            )
        })
        
      
        
        
        
        
        ### Item response distributions ----
        
        item_response_distribution_data_selected <- reactive({
            
            # TODO: read in item-level response data to do this
        })
        
        output$plot_item_response_distribution_selected <- renderPlot({
            
        })
        
        
        
        ### Distributions ----
        
        #### Raincloud plots ----
        
        ##### Plot ----
        output$single_scale_raincloudPlot <- renderPlot(height = function() plot_height_tmpt(), {
            req(selected_subscale_info$data$variable.name, data_individual_scale_selected_explore())
            
            raincloud_data <- data_individual_scale_selected_explore() |>
                dplyr::mutate(
                    time.pt = factor(time.pt,
                                     levels = c("T4", "T3", "T2", "T1")))
            
            raincloud_plot <- ggplot(raincloud_data,
                                     aes(x = time.pt,
                                         y = value,
                                         fill = time.pt)) +
                
                # add half violin
                ggdist::stat_halfeye(
                    # custom bandwidth
                    adjust = 0.5,
                    # adjust height
                    width = 0.6,
                    # move geom to the right
                    justification = -0.2,
                    # remove slab interval
                    .width = 0,
                    point_color = NA
                ) +
                
                # add boxplot
                geom_boxplot(
                    # make narrow
                    width = 0.12,
                    # make semi-transparent
                    alpha = 0.5,
                    # outlier - diamond shape
                    outlier.shape = 18,
                    # outlier size
                    outlier.size = 2,
                    notch = FALSE
                ) +
                
                # add scatter/dots
                gghalves::geom_half_point(
                    aes(fill = time.pt),
                    side = "l",
                    range_scale = .4,
                    alpha = .5,
                    shape = 21,
                    # transformation_params =
                    #   list(height = 0, width = 0.001)
                ) +
                
                # add mean and CI
                stat_summary(fun.data = mean_cl_boot,
                             #geom = "errorbar",
                             linewidth = 0.9,
                             position = position_nudge(x = 0, y = 0),
                             size = .8,
                             color = mean_color) +
                
                # add labels
                labs(x = "Time point",
                     y = str_glue("Total {abbreviation_individual_scale_selected_explore()} scores")
                     ) +
                
                # apply color
                scale_fill_manual(values = color_palette_tmpt()) +
                scale_color_manual(values = color_palette_tmpt()) +
                
                theme_bw() +
                
                # remove legend
                theme(legend.position = "none",
                      axis.title = element_text(size = 13),
                      axis.text = element_text(size = 13),
                      # make all text Roboto
                      text = element_text(
                          family = "Roboto"
                      )
                      
                      ) +
                
                # flip, add limit for y (now x)
                coord_flip(
                    #ylim = c(y.lim.min, y.lim.max)
                ) 

            return(raincloud_plot)
        })

        
        ##### Heading ----
        raincloud_heading_text <- reactive({
            req(selected_subscale_info$data$variable.name)
            
            heading <- str_glue("Raincloud plots of {abbreviation_individual_scale_selected_explore()} scores")
            return(heading)
        })
        
        output$raincloud_heading <- renderText({
            raincloud_heading_text()
        })
        
        
        ##### About accordion ----
        raincloud_about_accordion <- accordion(
            id = "about_raincloud_plots",
            open = FALSE,
            
            accordion_panel(
                title = "About",
                p("Raincloud plots are hybrid plots that visualize the distribution of the data (halved density plot), the summary statistics (box plot), and the raw data (individual points as a scatter). The plot is constructed with the density plot (the 'cloud') on the top, the box plot in the middle, and the individual data points (the 'rain') scattered below."),
              p("The plots also show the mean value (peach circle) and bootstrapped
              confidence intervals (peach lines extending from peach circle)."),
              p(strong("How to read raincloud plots: ")),
              card(
                  full_screen = TRUE,
                  tags$img(src='raincloud-plot-how-to.png'))
            )
        ) # close accordion
        
        
        ##### Descriptives table ----
        output$descriptives_table_raincloud <- renderTable({
            descriptives_scale_selected()
        })
        
        ##### Compose page ----
        output$raincloud_panel <- renderUI({
 
            if (input$scale_main_group_single %in% c('everyday discrimination (EDISC)', 'major discrimination (MDISC)', 'race-related experiences (RaLES)', 'cumulative adversity (CAM)')) {
                
                tagList(
                    h4("Raincloud plots"),
                    raincloud_about_accordion,
                    br(),
                    tmpt_warning_ui(NS(id, "warning_raincloud")),
                    br(),
                    h5(textOutput(ns("raincloud_heading"))),
                    card(
                        height = (plot_height_tmpt() + 50),
                        plotOutput(ns("single_scale_raincloudPlot")),
                    ),
                    br(),
                    card(
                        tableOutput(ns("descriptives_table_raincloud"))
                    )
                )
                
            } else {
                tagList(
                    h4("Raincloud plots"),
                    raincloud_about_accordion,
                    br(),
                    h5(textOutput(ns("raincloud_heading"))),
                    card(
                        height = (plot_height_tmpt() + 50),
                        plotOutput(ns("single_scale_raincloudPlot")),
                    ),
                    br(),
                    card(
                        tableOutput(ns("descriptives_table_raincloud"))
                    )
                )
            }

        })
        
 
        
        
        #### Histograms ----           
        
        ##### Heading ----
        
        histogram_heading_text <- reactive({
            req(selected_subscale_info$data$variable.name)
            
            heading <- str_glue("Histogram - frequency of {abbreviation_individual_scale_selected_explore()} scores")
            return(heading)
        })
        
        output$histogram_heading <- renderText({
            histogram_heading_text()
        })
        
        
        ##### Plot -----
        
        output$single_scale_histogramPlot <- renderPlotly({
            req(selected_subscale_info$data$variable.name)
            
            p <- data_individual_scale_selected_explore() |>
                
                ggplot(aes(value)) +
                
                # add histogram
                geom_histogram(binwidth = input$histo_bin_size,
                               color = 'white',
                               fill = 'black') +
                
                # x- and y-axis labels
                labs(
                    x = str_glue("Total {abbreviation_individual_scale_selected_explore()} scores"), 
                    y = "Frequency") +
                
                # apply theme
                theme_bw() +
                
                theme(
                    # remove legend
                    legend.position = "none",
                    
                    # make all text Roboto
                    text = element_text(
                        family = "Roboto"
                    ),
                    
                    axis.text = element_text(
                        size = 10
                    ),
                    
                    # color of facet strip and facet text
                    strip.background = element_rect(
                        color = "black",
                        fill = "#58758b"),
                    strip.text = element_text(
                        color = "white",
                        face = "bold",
                        size = 12)) +
                
                # facet according to time point
                facet_wrap( ~ time.pt)
            
            
            gp <- ggplotly(p) |> 
                plotly::config(displayModeBar = FALSE)
            
            gp
        })                       
        
        ##### About ----
        histogram_about_accordion <- accordion(
            id = "about_histograms",
            open = FALSE,
            
            accordion_panel(
                title = "About",
                p("The histograms below have the total scale scores plotted along the x-axis and the count/frequency of individuals with the corresponding total score along the y-axis. Each panel represents a single time point."),
                p("The plots are interactive—you can hover over the plots for more information. You can also zoom in by drawing a box over the area to zoom in on, and zoom out by double clicking on the plot area."),
                p("Use the slider to change the bin size of the histograms."),
                p(strong("How to read histograms: ")),
                card(
                    full_screen = TRUE,
                    #height = 300,
                    card_image(
                        tags$img(src='histogram-how-to.png'),
                        file = NULL
                    )
            )
            )
        ) # close accordion
        
        
        
        ##### Descriptives table ----
        output$descriptives_table_histogram <- renderTable({
            descriptives_scale_selected()
        })
        

        ##### Compose histogram page -----
        output$histogram_panel <- renderUI({
            
            if (input$scale_main_group_single %in% c('everyday discrimination (EDISC)', 'major discrimination (MDISC)', 'race-related experiences (RaLES)', 'cumulative adversity (CAM)')) {
                tagList(
                    h4("Histograms"),
                    histogram_about_accordion,
                    br(),
                    tmpt_warning_ui(NS(id, "warning_histogram")),
                    br(),
                    h5(textOutput(ns("histogram_heading"))),
                    
                    # increase size of text on slider
                    tagList(
                        tags$style(type = 'text/css', '#text_slider .irs-grid-text {font-size: 15px}'), 
                        div(id = 'text_slider',
                            sliderInput(ns("histo_bin_size"), "Select bin size:",
                                        min = ifelse(
                                            selected_subscale_info$data$scale.sub.group == "chronicity",
                                            10,
                                            1
                                        ), 
                                        max = ifelse(
                                            selected_subscale_info$data$scale.sub.group == "chronicity",
                                            300,
                                            30
                                        ), 
                                        width = '50%',
                                        value = ifelse(
                                            selected_subscale_info$data$scale.sub.group == "chronicity",
                                            10,
                                            1
                                        ), 
                                        step = ifelse(
                                            selected_subscale_info$data$scale.sub.group == "chronicity",
                                            10,
                                            1
                                        ), 
                                        ticks = FALSE)
                        )
                    ),
                    
                    br(),
                    plotlyOutput(ns("single_scale_histogramPlot")),
                    br(),
                    card(
                        tableOutput(ns("descriptives_table_histogram"))
                    )
                    
                )
            } else {
                    tagList(
                        h4("Histograms"),
                        histogram_about_accordion,
                        br(),
                        h5(textOutput(ns("histogram_heading"))),
                        # increase size of text on slider
                        tagList(
                            tags$style(type = 'text/css', '#text_slider .irs-grid-text {font-size: 15px}'), 
                            div(id = 'text_slider',
                                sliderInput(ns("histo_bin_size"), "Select bin size:",
                                            min = 1, max = 30, width = '50%',
                                            value = 1, ticks = FALSE)
                            )
                        ),
                        br(),
                        layout_columns(
                            plotlyOutput(ns("single_scale_histogramPlot"))
                        ),
                        br(),
                        card(
                            tableOutput(ns("descriptives_table_histogram"))
                        )
                        
                    )
            }
        })
        
        
        
        
        
        #### Estimated density plot ----
        
        
        ##### Heading ----
        estimated_density_heading_text <- reactive({
            req(selected_subscale_info$data$variable.name)
            
            heading <- str_glue("Estimated density plots of {abbreviation_individual_scale_selected_explore()} scores")
            return(heading)
        })
        
        output$estimated_density_heading <- renderText({
            estimated_density_heading_text()
        })
        
        
        ##### Plot -----
        output$single_scale_estimateddensityPlot <- renderPlotly({    
            req(selected_subscale_info$data$variable.name)
            
            density_plot <- data_individual_scale_selected_explore() |>
                
                ggplot(aes(value)) +
                
                geom_density(
                    aes(color = time.pt,
                        fill = time.pt),
                    alpha = .3,
                    linewidth = .5) +
                
                theme_bw() +
                
                theme(
                    # make all text Roboto
                    text = element_text(
                        family = "Roboto"
                    ),
                    
                    axis.text = element_text(
                        size = 10
                    )
                ) +
                
                # apply color
                scale_fill_manual(
                    name = "Time point", 
                    values = color_palette_tmpt()) +
                
                scale_color_manual(
                    name = "Time point", 
                    values = color_palette_tmpt()) +
                
                labs(
                    title = "",
                    x = str_glue("Total {abbreviation_individual_scale_selected_explore()} scores"),
                    y = "Estimated density"
                )
            
            # option to view by time point
            if (input$estimated_density_single_faceted == "Multiple") {
                density_plot <- density_plot + 
                    facet_wrap(~ time.pt) +
                    theme(
                        # color of facet strip and facet text
                        strip.background = element_rect(
                            color = "black",
                            fill = "#58758b"),
                        strip.text = element_text(
                            color = "white",
                            face = "bold",
                            size = 12)) 
                    
            }
            
            gp <- ggplotly(density_plot) |> 
                plotly::config(displayModeBar = FALSE)
            gp
        })
        
        
        ##### About accordion ----
        estimated_density_about_accordion <- accordion(
            id = "about_estimated_density",
            open = FALSE,
            
            accordion_panel(
                title = "About",
                p("A density plot visualizes the distribution of data over a continuous interval. This chart is a variation of a histogram that uses kernel smoothing to plot values, generating a smoother distribution."),
                p("The peaks of a density plot help display where values are concentrated over the interval. The advantage of density plots over histograms is that they are better at determining the distribution shape because they are not affected by the number of bins used (i.e., the bars used in a typical histogram)."),
                p("The plots are interactive—you can hover over the plots for more information. You can also zoom in by drawing a box over the area to zoom in on, and zoom out by double clicking on the plot area."),
                p("Use the radio button option to view the estimated densities for each time point separately ('single') or together, overlaid on top of one another ('multiple')."),
              p(strong("How to read estimated density plots: ")),
              card(
                  full_screen = TRUE,
                  tags$img(src='estimated-density-how-to.png'))
            )
        ) # close accordion
        
        
        ##### Descriptives table ----
        output$descriptives_table_density <- renderTable({
            descriptives_scale_selected()
        })
        
        
        
        ##### Compose estimated density page -----
        output$estimated_density_panel <- renderUI({
            
            if (input$scale_main_group_single %in% c('everyday discrimination (EDISC)', 'major discrimination (MDISC)', 'race-related experiences (RaLES)', 'cumulative adversity (CAM)')) {
                
                tagList(
                    h4("Estimated density plots"),
                    estimated_density_about_accordion,
                    br(),
                    tmpt_warning_ui(NS(id, "warning_estimated_density")),
                    br(),
                    h5(textOutput(ns("estimated_density_heading"))),
                    br(),
                    
                    radioGroupButtons(
                        inputId = ns("estimated_density_single_faceted"),
                        label = "View as single plot or multiple (by time point)",
                        choiceNames = list(
                            span(bs_icon("square-fill"), "Single"), 
                            span(bs_icon("grid-fill"), "Multiple")),
                        choiceValues = list("Single", "Multiple"),
                        direction = "horizontal",
                        selected = "Single"
                    ),
                    
                    
                    plotlyOutput(ns("single_scale_estimateddensityPlot")),
                    br(),
                    card(
                        tableOutput(ns("descriptives_table_density"))
                    )
                    
                )
            } else {
                tagList(
                    h4("Estimated density plots"),
                    estimated_density_about_accordion,
                    br(),
                    h5(textOutput(ns("estimated_density_heading"))),
                    br(),
                    
                    radioGroupButtons(
                        inputId = ns("estimated_density_single_faceted"),
                        label = "View as single plot or multiple (by time point)",
                        choiceNames = list(
                            span(bs_icon("square-fill"), "Single"), 
                            span(bs_icon("grid-fill"), "Multiple")),
                        choiceValues = list("Single", "Multiple"),
                        direction = "horizontal",
                        selected = "Single"
                    ),
                   
                    plotlyOutput(ns("single_scale_estimateddensityPlot")),
                    br(),
                    card(
                        tableOutput(ns("descriptives_table_density"))
                    )
                )
            }
        })
        
        
        
        #### Box plot ----
        
        
        ##### Heading ----
        boxplot_heading_text <- reactive({
            req(selected_subscale_info$data$variable.name)
            
            heading <- str_glue("Box plots of {abbreviation_individual_scale_selected_explore()} scores")
            return(heading)
        })
        
        output$boxplot_heading <- renderText({
            boxplot_heading_text()
        })
        
        
        ##### Plot ----
        output$single_scale_boxPlot <- renderPlotly({    
            req(selected_subscale_info$data$variable.name)
            
            box_plot <- data_individual_scale_selected_explore() |>
                
                ggplot(aes(
                    x = time.pt, 
                    y = value,
                    fill = time.pt)) + 
                
                # Add individual points 
                geom_jitter(
                    # if want color of points to be same color as box plots
                    #aes(color = time.pt),
                    color = "black",
                    size = 0.5, 
                    alpha = 0,
                    width = .1) + 
                
                scale_fill_manual(values = color_palette_tmpt()) +
                
                geom_boxplot() + 
                
                geom_jitter(
                    # if want color of points to be same color as box plots
                    #aes(color = time.pt),
                    color = "black",
                    size = 0.5, 
                    alpha = 0.75,
                    width = .1) + 
                
                labs(
                    title = "",
                    x = "Time point", 
                    y = str_glue("Total {abbreviation_individual_scale_selected_explore()} scores")
                ) +
                
                # apply theme
                theme_bw() +
                
                # remove legend
                theme(legend.position = "none",
                      
                      # make all text Roboto
                      text = element_text(
                          family = "Roboto"
                      ),
                      
                      axis.text = element_text(
                          size = 10
                      )
                      ) 
            
            ggplotly(box_plot) |> 
                plotly::config(displayModeBar = FALSE)
            
        })
        
        
        ##### About accordion ----
        boxplot_about_accordion <- accordion(
            id = "about_box_plots",
            open = FALSE,
            
            accordion_panel(
                title = "About",
                p("Boxplots are used to visualize the summary statistics for a particular variable. The horizontal black line in each box represents the median. The bottom and top of the box correspond with the first and third quartiles, respectively. The whiskers indicate the relative min and max scores, not including outliers. The bottom whisker represents the 1st quartile - 1.5 * interquartile range (IQR). The top whisker represents the 3rd quartile + 1.5 * IQR. Outliers are indicated as solid black dots outside of the whiskers."),
                p("The plots are interactive—you can hover over the plots for more information. You can also zoom in by drawing a box over the area to zoom in on, and zoom out by double clicking on the plot area."),
                p(strong("How to read box plots: ")),
                div(
                    tags$img(src='box-plot-how-to.png', style="display: block; margin-left: auto; margin-right: auto; height: 60vh;")
                )
            )
        ) # close accordion
        
        
        ##### Descriptives table ----
        output$descriptives_table_boxplot <- renderTable({
            descriptives_scale_selected()
        })
        
        
        ##### Compose page ----
        output$boxplot_panel <- renderUI({
            
            if (input$scale_main_group_single %in% c('everyday discrimination (EDISC)', 'major discrimination (MDISC)', 'race-related experiences (RaLES)', 'cumulative adversity (CAM)')) {
                tagList(
                    h4("Box plots"),
                    boxplot_about_accordion,
                    br(),
                    tmpt_warning_ui(NS(id, "warning_boxplot")),
                    br(),
                    h5(textOutput(ns("boxplot_heading"))),
                    plotlyOutput(ns("single_scale_boxPlot")),
                    br(),
                    card(
                        tableOutput(ns("descriptives_table_boxplot"))
                    )
                )
            } else {
                tagList(
                    h4("Box plots"),
                    boxplot_about_accordion,
                    br(),
                    h5(textOutput(ns("boxplot_heading"))),
                    plotlyOutput(ns("single_scale_boxPlot")),
                    br(),
                    card(
                        tableOutput(ns("descriptives_table_boxplot"))
                    )
                )
            }
        })
        
        
        
        #### Violin plot ----
        
        ##### Heading ----
        violin_heading_text <- reactive({
            req(selected_subscale_info$data$variable.name)
            
            heading <- str_glue("Violin plots of {abbreviation_individual_scale_selected_explore()} scores")
            return(heading)
        })
        
        output$violin_heading <- renderText({
            violin_heading_text()
        })
        
        
        ##### Plot -----
        output$single_scale_violinPlot <- renderPlotly({    
            req(selected_subscale_info$data$variable.name)
            
            p <- data_individual_scale_selected_explore() |>
                
                ggplot(aes(x = time.pt, y = value, fill = time.pt)) + 
                
                geom_sina(
                    # size of point
                    size = 1,
                    # make semi-transparent
                    alpha = 0,
                    # control range/width of points
                    maxwidth = .7) +
                
                # color palette to use
                scale_fill_manual(values = c(
                    "T1" = "#fde725", # yellow
                    "T2" = "#35b779", # green
                    "T3" = "#316883", # blue
                    "T4" = "#472c7a" # purple
                )) +
                
                # make violin plot
                geom_violin(
                    # scale violin area proportionate to number of observations
                    scale = "count",
                    # bandwidth adjustment - SD of smoothing kernel
                    bw = .7) + 
                
                # add individual points
                geom_sina(
                    # size of point
                    size = 1,
                    # make semi-transparent
                    alpha = .75,
                    # control range/width of points
                    maxwidth = .7) +
                
                # apply theme
                theme_bw() +
                
                # remove legend
                theme(legend.position = "none",
                      # make all text Roboto
                      text = element_text(
                          family = "Roboto"
                      ),
                      
                      axis.text = element_text(
                          size = 10
                      )
                ) +
                
                # add axis labels
                labs(
                    title = "",
                    x = "Time point", 
                    y = str_glue("Total {abbreviation_individual_scale_selected_explore()} scores")
                ) 
            
            # calculate the mean and SD to add to the plot  
            violin_plot <- p + stat_summary(
                fun.data = mean_sdl,
                fun.args = list(mult = 1),
                geom = "pointrange", 
                color = '#ff9966',
                size = 2)
            
            ggplotly(violin_plot) |> plotly::config(displayModeBar = FALSE)
        })
        
        
        ##### About accordion ----
        violin_about_accordion <- accordion(
            id = "about_violin_plots",
            open = FALSE,
            
            accordion_panel(
                title = "About",
                p("Violin plots are similar to box plots, except that they show the kernel probability density of the data at different values. The width of the violin plots here are proportional to the number of observations. The sina plot overlaid on top of the box plot adds the individual data points, and uses the normalized density to constrain the jitter of those points along the x-axis. This results in the sina points following the same contour as a violin plot."),
                p("The plots are interactive—you can hover over the plots for more information. You can also zoom in by drawing a box over the area to zoom in on, and zoom out by double clicking on the plot area."),
                p(strong("How to read violin plots: ")),
                card(
                    full_screen = TRUE,
                    tags$img(src='violin-plot-how-to.png'))
            )
        ) # close accordion
        
        
        ##### Descriptives table ----
        output$descriptives_table_violin <- renderTable({
            descriptives_scale_selected()
        })
        
        
        ##### Compose violin plot page ----
        
        output$violin_plot_panel <- renderUI({
            
            if (input$scale_main_group_single %in% c('everyday discrimination (EDISC)', 'major discrimination (MDISC)', 'race-related experiences (RaLES)', 'cumulative adversity (CAM)')) {
                tagList(
                    h4("Violin plots"),
                    violin_about_accordion,
                    br(),
                    tmpt_warning_ui(NS(id, "warning_violin_plot")),
                    br(),
                    h5(textOutput(ns("violin_heading"))),
                    plotlyOutput(ns("single_scale_violinPlot")),
                    br(),
                    card(
                        tableOutput(ns("descriptives_table_violin"))
                    )
                )
            } else {
                tagList(
                    h4("Violin plots"),
                    violin_about_accordion,
                    br(),
                    h5(textOutput(ns("violin_heading"))),
                    plotlyOutput(ns("single_scale_violinPlot")),
                    br(),
                    card(
                        tableOutput(ns("descriptives_table_violin"))
                    )
                )
            }
        })
        
        
        
        
        ### Scores by time point ----
        
        #### Corset plots ----
        
        ## 2 time points - T1 vs T4
        # If scale variable only has pre & post time points, compare option only T1 vs T4, otherwise
        # default options are: T1 vs T2, T2 vs T3, T3 vs T4, T1 vs T4
        
        ##### Compare tmpt input options ----
        output$corset_tmpt_compare_options <- renderUI({
            req(selected_subscale_info$data$variable.name)
            
            #req(selected_subscale_info$data$variable.name %in% c(list_scales_all, list_scales_two))
            
            validate(
                need(selected_subscale_info$data$variable.name, "Please wait...")
            )

            tmpt_pattern <- selected_subscale_info$data$tmpt_pattern 
            
            if (tmpt_pattern == "tmpt_pre_post") {
                radioGroupButtons(
                    inputId = ns("corset_plot_tmpts"),
                    label = "Time points to compare",
                    choices = c("T1 vs T4"),
                    selected = "T1 vs T4",
                    direction = "horizontal",
                    )

            } else if (tmpt_pattern == "tmpt_t4" | tmpt_pattern == "tmpt_t1") {
                includeHTML(path = "./text_mds/corset-plot.md")
                
            } else if (tmpt_pattern == "tmpt_all") {
                radioGroupButtons(
                    inputId = ns("corset_plot_tmpts"),
                    label = "Time points to compare",
                    choices = c("T1 vs T2", "T2 vs T3",
                                "T3 vs T4", "T1 vs T4"),
                    selected = "T1 vs T2",
                    direction = "horizontal"
                )

            }
            
           
        }) #|> bindCache(input$scale_type_var_single)
        
        
        
        ##### Get corset data ----
        # Extract time point A
        change_timepoint_a <- reactive({
            req(input$corset_plot_tmpts, selected_subscale_info$data$variable.name)
            split_string <- paste(input$corset_plot_tmpts) |> strsplit(" ")
            split_string[[1]][1]
        })
        
        # Extract time point B
        change_timepoint_b <- reactive({
            req(input$corset_plot_tmpts, selected_subscale_info$data$variable.name)
            split_string <- paste(input$corset_plot_tmpts) |> strsplit(" ")
            split_string[[1]][3]
        })
        
        # Extract difference score and direction of change between 2 time points 
        data_individual_scale_change_timepoints <- reactive({
            req(selected_subscale_info$data$variable.name)
            
            time_a <- tolower(change_timepoint_a())
            time_b <- tolower(change_timepoint_b())
            
            # get the columns of interest (score at the 2 time points, change, 
            # and direction values)
            col_interest_a <- paste(selected_subscale_info$data$variable.name, "_total_", time_a, sep = "")
            col_interest_b <- paste(selected_subscale_info$data$variable.name, "_total_", time_b, sep = "")
            col_change <- paste(selected_subscale_info$data$variable.name, ".change.", time_a, ".", time_b, sep = "")
            col_direction <- paste(selected_subscale_info$data$variable.name, ".direction.", time_a, ".", time_b, sep = "")
            
            
            vars_select <- c(col_interest_a, col_interest_b, 
                             col_change, col_direction)
            
            corset_data <- dat_mini_subscale %>% 
                dplyr::select(vars_select,
                              ccc.id)
            
            colnames(corset_data)[3:4] <- c("change", "direction")
            
            corset_data
        })
        
        
        # Get data for corset plot - count by each direction type
        select_corset_data <- reactive({
            req(selected_subscale_info$data$variable.name)
            
            dat_corset_select <- data_individual_scale_change_timepoints() |>
                dplyr::filter(!is.na(change)) |>
                dplyr::group_by(direction) |> 
                mutate(Count = n()) |> 
                dplyr::ungroup() |> 
                mutate(direction_updated = paste0(direction, ": n=", Count))
            
            return(dat_corset_select)
        })
        
        
        ##### Create corset plot ----
        # Create corset plot for data of interest
        output$single_scale_corsetPlot <- renderPlot({
            req(selected_subscale_info$data$variable.name)
            
            if (selected_subscale_info$data$tmpt_pattern %in% c("tmpt_all", "tmpt_pre_post")) {
                
                corset_plot <- select_corset_data() |>
                    gg_corset(
                        y_var1 = 1,
                        y_var2 = 2,
                        c_var = "direction_updated",
                        group = "ccc.id",
                        eyelets = T,
                        faceted = T,
                        e_type = "SE",
                        line_size = 1
                    ) +
                    
                    scale_color_manual(name = "Direction of change:", 
                                       values = c("#46327e", "#4ac16d", "#fde725")) +
                    
                    theme(
                        # make all text Roboto
                        text = element_text(
                            family = "Roboto"
                        ),
                        axis.title.y = element_text(size = 15, hjust = 0),
                        axis.text.y = element_text(size = 15),
                        axis.text.x = element_text(size = 15),
                        legend.position = "none",
                        plot.title = element_text(hjust = 0.5, size = 18),
                        strip.text.x = element_text(
                            size = 17, face = "italic"
                        ),
                        axis.title.y.left = element_text(
                            hjust = .5,
                            size = 15)
                    ) +
                    
                    labs(
                        title = str_glue("Change in {abbreviation_individual_scale_selected_explore()} scores - {change_timepoint_a()} vs {change_timepoint_b()}"),
                        x = "",
                        y = ""
                    ) +
                    
                    scale_y_continuous(
                        name = str_glue("{abbreviation_individual_scale_selected_explore()} Scores")) +
                    
                    scale_x_discrete(
                        labels = c(change_timepoint_a(), change_timepoint_b())) 
                
                corset_plot
                
            } else {
                
                plot(0, type = "n", axes = FALSE, ann = FALSE, xlim = c(0, 1), ylim = c(0, 1))
                text(0.5, 0.5, "Data do not exist at\n multiple time points", cex = 1.5, adj = c(0.5, 0.5))
            }

        })    
        
        
        ##### Get change histogram data ----
        # Get counts and avg by direction change type 
        data_corset_change_counts <- reactive({
            req(selected_subscale_info$data$variable.name)
            
            count_change <- select_corset_data() |>
                dplyr::group_by(direction) |>
                dplyr::summarize(Count = n(),
                                 Avg = round(mean(change, na.rm = TRUE), 3))
            count_change
        })
        
        ##### Create change histogram ----
        # Create histogram of change scores 
        output$single_scale_corset_change_histoPlot <- renderPlotly({
            req(selected_subscale_info$data$variable.name)
            
            if (selected_subscale_info$data$tmpt_pattern %in% c("tmpt_all", "tmpt_pre_post")) {
                
            corset_histo_plot <- select_corset_data() |> 
                
                ggplot(aes(change)) +
                
                geom_histogram(
                    binwidth = 1,
                    color = 'black',
                    fill = histo_color
                ) + 
                
                theme_bw() +
                
                theme(
                    # make all text Roboto
                    text = element_text(
                        family = "Roboto"
                    ),
                    
                    axis.text = element_text(
                        size = 10
                    )
                ) +
                
                # add vertical lines for mean of decrease and increase
                geom_vline(
                    xintercept = data_corset_change_counts()$Avg[data_corset_change_counts()$direction == 'Decrease'][1],
                    size = 1,
                    color = mean_color,
                    linetype = 'dashed'
                ) +
                
                geom_vline(
                    xintercept = data_corset_change_counts()$Avg[data_corset_change_counts()$direction == 'Increase'][1],
                    size = 1,
                    color = mean_color,
                    linetype = 'dashed'
                ) +
                
                # add vertical line for 0
                geom_vline(
                    xintercept = 0,
                    size = 1,
                    linetype = 'dashed'
                ) +
                labs(
                    title = str_glue("Distribution of change in {abbreviation_individual_scale_selected_explore()} scores {change_timepoint_a()} vs {change_timepoint_b()}"),
                    x = str_glue("Change in {abbreviation_individual_scale_selected_explore()} scores"),
                    y = "Frequency") 
            
            
            # center the histogram on 0
            buffer_value <- 2
            corset_histo_plot <- ggplotly(corset_histo_plot) |> 
                plotly::layout(xaxis = list(
                    range = c(-max(abs(select_corset_data()$change)) - buffer_value, 
                              max(abs(select_corset_data()$change)) + buffer_value)))
            
            
            ggplotly(corset_histo_plot) |> plotly::config(displayModeBar = FALSE)
            
            } else {
            
                empty_plot <- plot_ly(
                    x = 0.5,
                    y = 0.5,
                    type = "scatter",
                    mode = "text",
                    text = "Data do not exist at\n multiple time points",
                    textfont = list(size = 15),
                    showlegend = FALSE
                ) %>%
                    plotly::layout(
                        xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE, showline = FALSE, visible = FALSE),
                        yaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE, showline = FALSE, visible = FALSE)
                        )
                    
                # Display the empty plot
                empty_plot
            }

        })
        
        

        ##### Corset About accordion ----
        corset_about_accordion <- accordion(
            id = "about_corset_plots",
            open = FALSE,
            
            accordion_panel(
                title = "About corset plots",
                # p("Corset plots and accompanying change-score histograms 
                #   depicting the change of each individual's score on the 
                #   selected scale between the 2 time points selected."),
                # hr(),
#                p(strong("Corset plots")),
                p("Corset plots show the proportion of individuals whose scores increased, decreased, or stayed the same between time points, as well as the distribution of scores for participants whose score increased, decreased, or stayed the same. They also allow you to compare the size of the increases and decreases. For instance, those who increased may have increased by large amounts, while those who decreased may have only decreased by small amounts."),
                p(strong("How to read corset plots: ")),
                card(
                    full_screen = TRUE,
                    tags$img(src='corset-how-to.png')),
                p("Corset plots show changes in individual scores between the 2 time points. The time points are plotted on the x-axis and the scale scores are plotted on the y-axis."),
                p("Each line represents a participant. Separate panels are plotted for scores that decreased, scores that increased, and scores that did not change between the 2 time points, with colors also indicating the direction of change (Purple = Decrease, Green = Increase, Yellow = No change)."),
                p("At the top of each plot there is a label indicating which group is plotted (i.e., scores decreased, scores increased, or no change) and how many individuals are represented in that group."),
                p("The black shape on each side of the plot shows the distribution of scores for individuals in that group (decrease, increase, no change) at that time point (T1, T2, T3, T4). The dot in the middle of the distribution is the mean score for individuals in that grouping at that time point. The vertical lines off the dot indicate the SE (standard error) value. If the lines are difficult to see, the SE is likely small.")
            ),
            
            accordion_panel(
                title = "About change histograms",
                p("These histograms correspond to the corset plots, and show the distribution of change scores (i.e., the number of people who had each possible change in their scale scores) between the 2 time points selected."),
                p("The plots are interactive—you can hover over the plots for more information. You can also zoom in by drawing a box over the area to zoom in on, and zoom out by double clicking on the plot area."),
                
                p(strong("How to read the histograms: ")),
                card(
                    full_screen = TRUE,
                    tags$img(src='corset-histogram-how-to.png')),
                p("The range of the change scores is plotted along the x-axis, with the number of individuals in each bin plotted along the y-axis."),
                tags$ul(
                    tags$li("Positive values = scores increased"),
                    tags$li("Negative values = scores decreased"),
                    tags$li("0 value = scores stayed the same (no change)")),
                p("The black vertical line = no change"),
                p("The peach vertical lines = mean increase (to the right of 
                  the black line, positive value), and mean decrease (to the 
                  left of the black line, negative value)."),
                p("Bin size = 1. A bin reflects the change in score (reflected 
                  on the x-axis) and the number of people who had that 
                  change score (reflected on the y-axis)."),
                p("NOTE: In a few cases, the frequency for 0 (no change) differs between the histogram and the corset plot. This is because the change scores are less than 1, and thus smaller than the bin size of the histogram. Therefore, participants with change scores < 1, are included in the 0 bin in the histogram, whereas 'no change' in the corset plot only includes those with absolutely no difference in their scores between the two time points.")
            )
 
        ) # close accordion
           
        
        ##### Create corset panel ----
        output$corset_panel <- renderUI({
            
            req(selected_subscale_info$data$variable.name)
            
            validate(
                need(input$scale_main_group_single, "Please wait...")
            )
            
            if (input$scale_main_group_single %in% c('everyday discrimination (EDISC)', 'major discrimination (MDISC)', 'race-related experiences (RaLES)', 'cumulative adversity (CAM)')) {
                
                tagList(
                    #h5("Corset plots and histogram of change"),
                    corset_about_accordion,
                    
                    br(),
                    tmpt_warning_ui(NS(id, "warning_scores_by_time_point")),
                    br(),
                    
                    uiOutput(ns("corset_tmpt_compare_options")),
                    
                    br(),
                    h5("Corset plots"),
                    plotOutput(ns("single_scale_corsetPlot")),
                    
                    br(),
                    h5("Histogram of distribution of change"),
                    plotlyOutput(ns("single_scale_corset_change_histoPlot")),
                    br()
                    
                )
            } else {
                
                tagList(
                    #h5("Corset plots and histogram of change"),
                    corset_about_accordion,
                    
                    br(),
                    
                    uiOutput(ns("corset_tmpt_compare_options")),
                    
                    br(),
                    h5("Corset plots"),
                    plotOutput(ns("single_scale_corsetPlot")),
                    
                    br(),
                    h5("Histogram of distribution of change"),
                    plotlyOutput(ns("single_scale_corset_change_histoPlot")),
                    br()
                )
            }
        })
        
        
        
        
        
        
         
        #     
        #     
        #     
        #     accordion_panel(
        #         title = "About",
        #         p("Corset plots and accompanying change-score histograms 
        #           depicting the change of each individual's score on the 
        #           selected scale between the 2 time points selected.")),
        #     accordion_panel(
        #         title = "Corset plots",
        #         p("Corset plots show the proportion of individuals whose scores increased, decreased, or stayed the same between time points, as well as the distribution of scores for participants whose score increased, decreased, or stayed the same. They also allow you to compare the size of the increases and decreases. For instance, those who increased may have increased by large amounts, while those who decreased may have only decreased by small amounts."),
        #         p(strong("How to read corset plots: ")),
        #         card(
        #             full_screen = TRUE,
        #             tags$img(src='corset-how-to.png')),
        #         p("Corset plots show changes in individual scores between the 2 time points. The time points are plotted on the x-axis and the scale scores are plotted on the y-axis."),
        #         p("Each line represents a participant. Separate panels are plotted for scores that decreased, scores that increased, and scores that did not change between the 2 time points, with colors also indicating the direction of change (Purple = Decrease, Green = Increase, Yellow = No change)."),
        #         p("At the top of each plot there is a label indicating which group is plotted (i.e., scores decreased, scores increased, or no change) and how many individuals are represented in that group."),
        #         p("The black shape on each side of the plot shows the distribution of scores for individuals in that group (decrease, increase, no change) at that time point (T1, T2, T3, T4). The dot in the middle of the distribution is the mean score for individuals in that grouping at that time point. The vertical lines off the dot indicate the SE (standard error) value. If the lines are difficult to see, the SE is likely small.")
        #     ),
        #     accordion_panel(
        #         title = "Change histograms",
        #         p("These histograms correspond to the corset plots, and show the distribution of change scores (i.e., the number of people who had each possible change in their scale scores) between the 2 time points selected."),
        #         p("The plots are interactive—you can hover over the plots for more information. You can also zoom in by drawing a box over the area to zoom in on, and zoom out by double clicking on the plot area."),
        #         
        #         p(strong("How to read the histograms: ")),
        #         card(
        #             full_screen = TRUE,
        #             tags$img(src='corset-histogram-how-to.png')),
        #         p("The range of the change scores is plotted along the x-axis, with the number of individuals in each bin plotted along the y-axis."),
        #         tags$ul(
        #             tags$li("Positive values = scores increased"),
        #             tags$li("Negative values = scores decreased"),
        #             tags$li("0 value = scores stayed the same (no change)")),
        #         p("The black vertical line = no change"),
        #         p("The peach vertical lines = mean increase (to the right of 
        #           the black line, positive value), and mean decrease (to the 
        #           left of the black line, negative value)."),
        #         p("Bin size = 1. A bin reflects the change in score (reflected 
        #           on the x-axis) and the number of people who had that 
        #           change score (reflected on the y-axis)."),
        #         p("NOTE: In a few cases, the frequency for 0 (no change) differs between the histogram and the corset plot. This is because the change scores are less than 1, and thus smaller than the bin size of the histogram. Therefore, participants with change scores < 1, are included in the 0 bin in the histogram, whereas 'no change' in the corset plot only includes those with absolutely no difference in their scores between the two time points.")
        #     )
        # 
        # ) # close accordion
        # 
        # 
        
        
        #### Alluvial diagrams ----
        
        ##### Alluvial diagram input options ----
        
        output$alluvial_participant_option <- renderUI({
            req(selected_subscale_info$data$variable.name)
            
            if (selected_subscale_info$data$variable.name %in% c("pss", "stai")) {
                radioButtons(
                    inputId = ns("alluvial_participant_option"),
                    label = span("Participants", tooltip(bs_icon("question-square"), "All: all participants, includes missing data. Complete: only participants who have scores at every time point.")),
                    choices = c("all", "complete"),
                    selected = "all",
                    inline = T
                )
            } else {
                br()
            }
            
        })
        
        output$alluvial_color_option <- renderUI({
            req(selected_subscale_info$data$variable.name)
            
            if (selected_subscale_info$data$variable.name %in% c("pss", "stai")) {
                radioButtons(
                    inputId = ns("alluvial_color_option"),
                    label = span("Color by", tooltip(bs_icon("question-square"), "T1: Color is based on level at T1 (baseline), where participants started, and carried through all time points. T4: Color is based on level at T4 (1-year follow-up), where participants ended.")),
                    choices = c("T1", "T4"),
                    selected = "T1",
                    inline = T
                )
            } else {
                br()
            }
            
        })
        
        
        ##### Heading ----
        alluvial_heading_text <- reactive({
            req(selected_subscale_info$data$variable.name)
            
            heading <- str_glue("Alluvial diagram of {abbreviation_individual_scale_selected_explore()} levels across time points")
            return(heading)
        })
        
        output$alluvial_heading <- renderText({
            alluvial_heading_text()
        })
        
        
        
        ##### Create alluvials ----
        output$alluvial_plot <- render_parcats({
            
            req(selected_subscale_info$data$variable.name, input$alluvial_participant_option, input$alluvial_color_option)
            
            # how to color traces - by first or last levels
            if (input$alluvial_color_option == "T1") {
                color_by_var <- "first_variable"
                sort_direction <- "forward"
            } else if (input$alluvial_color_option == "T4") {
                color_by_var <- "last_variable"
                sort_direction <- "backward"
            }
            
            
            # get data depending on inputs (all or complete participants, PSS or STAI)
            if (input$alluvial_participant_option == "all") {
                
                if (selected_subscale_info$data$variable.name == "pss") {
                    alluvial_data <- pss_alluvial_dat
                    
                } else if (selected_subscale_info$data$variable.name == "stai") {
                    alluvial_data <- stai_alluvial_dat
                }
                
            } else if (input$alluvial_participant_option == "complete") {
                
                if (selected_subscale_info$data$variable.name == "pss") {
                    alluvial_data <- pss_alluvial_dat %>% 
                        dplyr::filter(complete.cases(.))
                    
                } else if (selected_subscale_info$data$variable.name == "stai") {
                    alluvial_data <- stai_alluvial_dat %>% 
                        dplyr::filter(complete.cases(.))
                }
            }
            
            names(alluvial_data) <- c("T1", "T2", "T3", "T4")
            
            
            # create alluvial
            # make alluvial using easyalluvial
            g <- alluvial_wide(alluvial_data, 
                               order_levels = c("Low", "Moderate", "High"),
                               NA_label = "Missing", 
                               col_vector_value = alluvial_colors, 
                               col_vector_flow = alluvial_colors,
                               fill_by = color_by_var
            )
            
            # make interactive using parcats
            gg <- parcats(g, 
                          marginal_histograms = FALSE, 
                          sortpaths = sort_direction,
                          bundlecolors = FALSE
            )
            
            # show 'count' on hover tooltip
            gg$x$traces$parcats$hoverinfo <- "count"
            
            # change size of level labels
            gg$x$traces$parcats$tickfont <- list(size = 18)
            
            # show count of category on hover
            gg$x$traces$parcats$hoveron <- "category"
            
            gg
            
        })
        
        
        
        
        ##### Alluvial About accordion ----
 
        alluvial_about_accordion <- accordion(
            id = "about_alluvials",
            open = FALSE,
            
            accordion_panel(
                title = "About",
                p("Alluvial diagrams showing the flow of participants across 
                  data collection time points based on their scale level (low, 
                  moderate, or high) at each time point. The width of the flow 
                  bands and the height of the boxes are scaled according to the 
                  number of people in each level. The vertical boxes/bars are 
                  essentially stacked bar charts showing the proportion of 
                  individuals who have low, moderate, and high levels at each 
                  time point. The bands flowing between time points show changes
                  between levels from one time point to the next, and correspond
                  to the number of people in that category, such as the number
                  of people with a High level at Time 1 who then had a Moderate
                  level at Time 2."),
                p("The diagram is interactive - you can hover over the plot
                  for more information, such as the number of people in each
                  flow band from one time point to another."),
                p("Use the 'Participants' option to select whether to show all
                  participants ('all'), which will include missing data, or 
                  only participants who have a score at every time point
                  ('complete')."),
                p("Use the 'Color by' option to select how to view the flow of
                  levels over time.")
                )
        )
        
        
        

        
        
        
        ##### Create alluvial panel ----
        
        output$alluvial_panel <- renderUI({
            
            req(selected_subscale_info$data$variable.name)
            
            validate(
                need(input$scale_main_group_single, "Please wait...")
            )
            
            # Create alluvial page for PSS/STAI
            if (input$scale_main_group_single %in% c('perceived stress (PSS)', 'anxiety (STAI)')) {
            
            tagList(
                #h5("Alluvial diagram"),
                alluvial_about_accordion,
                
                br(),
                h5(textOutput(ns("alluvial_heading"))),
                br(),
                card(
                    full_screen = TRUE,
                    height = 800,
                    card_header(
                        "Alluvial diagram",
                        popover(
                            trigger = bs_icon("info-circle", title = "Alluvial diagram info"),
                            title = "Alluvial options info",
                            p("Use the 'Participants' option to select whether to show all
                  participants ('all'), which will include missing data, or 
                  only participants who have a score at every time point
                  ('complete')."),
                  p("Use the 'Color by' option to select how to view the flow of
                  levels over time.")
                        )
                    ),
                  layout_sidebar(
                      fillable = TRUE,
                      sidebar = sidebar(
                          tags$strong("Options:"),
                          position = "right",
                          open = TRUE,
                          uiOutput(ns("alluvial_participant_option")),
                          uiOutput(ns("alluvial_color_option"))
                      ),
                      parcatsOutput(ns("alluvial_plot")),
                  )
                )
            )
            } else {
                tagList(
                    alluvial_about_accordion,
                    br(),
                    div(
                        class="container text",
                        style="width:85%",
                        p(
                            HTML(paste("Alluvial diagrams showing the change in scale levels between time points is only available for scales in which the total score can be interpreted as a 'level' (low, moderate, high). The scales that have levels are: ",
                            "<strong>anxiety (STAI)</strong> and <strong>perceived stress (PSS)</strong>. Select either of those scales to see the corresponding alluvial diagram.")),
                            style = "color: red;"
                    )
                    )   
                )
            }
        })
        
        
        
        
        
        
        #### Create corset/alluvial tabs ORIGINAL----

        ##### Corset tab ORIGINAL ----
        # output$corset_tab <- renderUI({
        #     
        #     req(selected_subscale_info$data$variable.name)
        #     
        #     validate(
        #         need(input$scale_main_group_single, "Please wait...")
        #     )
        #     
        #     if (input$scale_main_group_single %in% c('everyday discrimination (EDISC)', 'major discrimination (MDISC)', 'race-related experiences (RaLES)', 'cumulative adversity (CAM)')) {
        #     
        #     tagList(
        #         h5("Corset plots and histogram of change"),
        #         corset_about_accordion,
        #         
        #         br(),
        #         tmpt_warning_ui(NS(id, "warning_scores_by_time_point")),
        #         br(),
        #         
        #         uiOutput(ns("corset_tmpt_compare_options")),
        #         
        #         br(),
        #         h5("Corset plots"),
        #         plotOutput(ns("single_scale_corsetPlot")),
        #         
        #         br(),
        #         h5("Histogram of distribution of change"),
        #         plotlyOutput(ns("single_scale_corset_change_histoPlot")),
        #         br()
        #         
        #     )
        #     } else {
        #         
        #         tagList(
        #             h5("Corset plots and histogram of change"),
        #             corset_about_accordion,
        #             
        #             br(),
        #             
        #             uiOutput(ns("corset_tmpt_compare_options")),
        #             
        #             br(),
        #             h5("Corset plots"),
        #             plotOutput(ns("single_scale_corsetPlot")),
        #             
        #             br(),
        #             h5("Histogram of distribution of change"),
        #             plotlyOutput(ns("single_scale_corset_change_histoPlot")),
        #             br()
        #         )
        #     }
        # })
        
        
        ##### Alluvial tab ORIGINAL ----
        # output$alluvial_tab <- renderUI({
        #     
        #     tagList(
        #         h5("Alluvial diagram"),
        #         alluvial_about_accordion,
        #         
        #         br(),
        #         h5(textOutput(ns("alluvial_heading"))),
        #         br(),
        #         card(
        #             full_screen = TRUE,
        #             height = 800,
        #             card_header(
        #                 "Alluvial diagram",
        #                 popover(
        #                     trigger = bs_icon("info-circle", title = "Alluvial diagram info"),
        #                     title = "Alluvial options info",
        #                     p("Use the 'Participants' option to select whether to show all
        #           participants ('all'), which will include missing data, or 
        #           only participants who have a score at every time point
        #           ('complete')."),
        #           p("Use the 'Color by' option to select how to view the flow of
        #           levels over time.")
        #                 )
        #             ),
        #             layout_sidebar(
        #                 fillable = TRUE,
        #                 sidebar = sidebar(
        #                     tags$strong("Options:"),
        #                     position = "right",
        #                     open = TRUE,
        #                     uiOutput(ns("alluvial_participant_option")),
        #                     uiOutput(ns("alluvial_color_option"))
        #                 ),
        #                 parcatsOutput(ns("alluvial_plot")),
        #             )
        #         )
        #     )
        # })
        
        
        
        #### Show alluvial tab selectively ORIGINAL ----
        
        # observe({
        #     req(selected_subscale_info$data$variable.name)
        #     
        #     if (selected_subscale_info$data$variable.name %in% c("pss", "stai")) {
        #         nav_show(id = "scores_by_time_point_card_tabs",
        #                  target = "alluvialTab")
        #         
        #     } else {
        #         nav_hide(id = "scores_by_time_point_card_tabs",
        #                  target = "alluvialTab")
        #         nav_select(id = "scores_by_time_point_card_tabs",
        #                    selected = "corsetTab")
        #     }
        # })
        
  
        
        ### Scores by date ---- 
        
        # Prepare scores by month data:
        # Select scale of interest
        # Round date to 1st of month & group by month
        # Calculate count, mean, and median 
        
        scores_by_date_data <- reactive({
            req(selected_subscale_info$data$variable.name)
            
            scores_by_month_dat <- dat_subscale |>
                dplyr::filter(measure %in% selected_subscale_info$data$variable.name) |>
                mutate(
                    month_date = lubridate::floor_date(recorded.date, "month")) |>
                dplyr::group_by(month_date) |>
                
                dplyr::summarize(
                    avg_score = round(mean(value, na.rm = TRUE), 2),
                    median_score = median(value, na.rm = TRUE),
                    n = n()) |>
                dplyr::filter(month_date < '2022-01-31') |>
                dplyr::ungroup()
            
        })
        
        
        
        ##### Text for plot title and caption ----
        
        score_by_date_title <- reactive({
            req(selected_subscale_info$data$variable.name)
            
            if (input$scores_by_date_type == "avg_score") {
                score_type_text <- "average"
            } else if (input$scores_by_date_type == "median_score") {
                score_type_text <- "median"
            }
            
            score_type_title_text <- str_glue("Line plot of {score_type_text} {abbreviation_individual_scale_selected_explore()} scores by month")
            
            return(score_type_title_text)
            
        })
        
        score_by_date_caption <- reactive({
            
            if (input$covid_cases_deaths == "weekly.cases") {
                covid_type_text <- "cases"
            } else if (input$covid_cases_deaths == "weekly.deaths") {
                covid_type_text <- "deaths"
            }
            
            covid_type_caption_text <- str_glue("Weekly number of U.S. Covid-19 {covid_type_text} overlaid in red. Value is reflected on the right y-axis.")
            
            return(covid_type_caption_text)
        })
        
        
        ##### About accordion -----
        score_date_accordion <- accordion(
            id = "about_scores_by_date_lines",
            open = FALSE,
            
            accordion_panel(
                title = "About",
                p("This line plot shows the scores for a particular variable over the data-collection window. Study participants enrolled and participated over a six-month period, so these plots allow us to look for potential variations across this timeframe.  Date is plotted along the x-axis and scores are plotted along the left y-axis. Black dots depict the average or median scores by month. The size of the circle indicates the number of participants included in the calculation (e.g., average or median) for that month. If selected, the right y-axis will represent weekly Covid-19 cases or deaths in the U.S."),
                p("Use the radio button options to 1) select whether to show average or median scores, and 2) to select whether to add weekly U.S. Covid-19 cases or deaths to the plot."),
                p("The plot is interactive. Hover over parts of the plot for more information.")
            )
        ) # close accordion
        
        
        
        ##### Line plot (highcharter) -----
        
        output$hc_single_scale_date_linePlot <- renderHighchart({
            req(selected_subscale_info$data$variable.name)
            
            hc <- highchart(type = "chart") |> 
                
                # set up axes
                hc_yAxis_multiples(
                    list(title = list(text = input$scores_by_date_type)),
                    list(title = list(text = input$covid_cases_deaths),
                         labels = list(style = list(color = "red")),
                         showLastLabel = FALSE, 
                         opposite = TRUE)) %>% 
                
                hc_xAxis(type = "datetime") %>% 
                
                # add line for scores
                hc_add_series(name = "Score",
                              data = scores_by_date_data(),
                              color = "black",
                              id = "score",
                              'line',
                              hcaes(x = month_date,
                                    y = !!input$scores_by_date_type),
                              tooltip = list(
                                  pointFormat = "Score={point.y}, n={point.n}")) %>% 
                
                # add circles, sized for number of people
                hc_add_series(name = input$scores_by_date_type,
                              data = scores_by_date_data(),
                              'scatter',
                              linkedTo = "score",
                              color = 'black',
                              hcaes(x = month_date,
                                    y = !!input$scores_by_date_type,
                                    size = n),
                              tooltip = list(
                                  pointFormat = "{point.x:%b %Y}<br>Score={point.y}<br>n={point.n}")) %>%
                
                hc_title(text = score_by_date_title()) %>% 
                
                # add tooltip formatting
                hc_tooltip(crosshairs = TRUE)
            
            
            
            # add weekly covid deaths/cases if selected
            if (input$covid_cases_deaths != "none") {
                
                hc <- hc %>%
                    hc_add_series(name = input$covid_cases_deaths,
                                  data = covid_cases_deaths_dat, 
                                  "spline", 
                                  yAxis = 1,
                                  color = "red",
                                  hcaes(x = date, y = !!input$covid_cases_deaths)) %>% 
                    hc_caption(text = score_by_date_caption()) %>% 
                    
                    hc_credits(enabled = TRUE,
                               text = "Covid-19 data downloaded from CDC COVID Data Tracker", 
                               href = "https://covid.cdc.gov/covid-data-tracker/#trends_weeklycases_select_00")
                
            }
            
            hc
            
        })
        
        
        
        ##### Compose scores by date page -----
        
        output$scores_by_date_panel <- renderUI({
            
            if (input$scale_main_group_single %in% c('everyday discrimination (EDISC)', 'major discrimination (MDISC)', 'race-related experiences (RaLES)', 'cumulative adversity (CAM)')) {
                tagList(
                    h4("Scores by date"),
                    score_date_accordion,
                    br(),
                    
                    tmpt_warning_ui(NS(id, "warning_scores_by_date")),
                    
                    layout_columns(
                        width = 1/2,
                        radioButtons(
                            inputId = ns("scores_by_date_type"),
                            label = "What should be plotted?",
                            choices = c(
                                "average scores" = "avg_score",
                                "median scores" = "median_score"),
                            selected = "avg_score"),
                        
                        radioButtons(
                            inputId = ns("covid_cases_deaths"),
                            label = "Add average weekly Covid-19 cases or deaths?",
                            choices = c(
                                "none" = "none", 
                                "cases" = "weekly.cases", 
                                "deaths" = "weekly.deaths"),
                            selected = "none"
                        )
                    ),

                    br(),
                    
                    highchartOutput(ns("hc_single_scale_date_linePlot"), height = "500px"),
                    
                    br(), br()
                    
                )
            } else {
                tagList(
                    h4("Scores by date"),
                    score_date_accordion,
                    br(),
                    
                    layout_columns(
                        width = 1/2,
                        radioButtons(
                            inputId = ns("scores_by_date_type"),
                            label = "What should be plotted?",
                            choices = c(
                                "average scores" = "avg_score",
                                "median scores" = "median_score"),
                            selected = "avg_score"),
                        
                        radioButtons(
                            inputId = ns("covid_cases_deaths"),
                            label = "Add average weekly Covid-19 cases or deaths?",
                            choices = c(
                                "none" = "none", 
                                "cases" = "weekly.cases", 
                                "deaths" = "weekly.deaths"),
                            selected = "none"
                        )
                    ),
                    br(),
                    
                    highchartOutput(ns("hc_single_scale_date_linePlot"), height = "500px"),
                    
                    br(), br()
                    
                )
            }
        })
        
        
        
        
        
        ### Scores by date by time point---- 
        
        # Prepare scores by month data:
        # Select scale of interest
        # Round date to 1st of month & group by time point & month
        # Calculate count, mean, and median 
        
        scores_by_date_by_timepoint_data <- reactive({
            req(selected_subscale_info$data$variable.name)
            
            scores_by_month_dat <- dat_subscale |>
                dplyr::filter(measure %in% selected_subscale_info$data$variable.name) |>
                mutate(
                    month_date = lubridate::floor_date(recorded.date, "month")) |>
                dplyr::group_by(time.pt, month_date) |>
                
                dplyr::summarize(
                    avg_score = round(mean(value, na.rm = TRUE), 2),
                    median_score = median(value, na.rm = TRUE),
                    n = n()) |>
                
                dplyr::filter(month_date < '2022-01-31') |>
                
                mutate(color_group = case_when(
                    time.pt == "T1" ~ "#fde725",
                    time.pt == "T2" ~ "#35b779",
                    time.pt == "T3" ~ "#316883",
                    time.pt == "T4" ~ "#472c7a"
                ),
                tmpt = time.pt) |>
                
                dplyr::ungroup()
            
        })
        
        
        ##### Text for plot title and caption ----
        
        score_by_date_by_timepoint_title <- reactive({
            req(selected_subscale_info$data$variable.name)
            
            if (input$scores_by_date_by_timepoint_type == "avg_score") {
                score_type_text <- "average"
            } else if (input$scores_by_date_by_timepoint_type == "median_score") {
                score_type_text <- "median"
            }
            
            score_type_title_text <- str_glue("Line plot of {score_type_text} {abbreviation_individual_scale_selected_explore()} scores by month by time point")
            
            return(score_type_title_text)
            
        })
        
        score_by_date_by_timepoint_caption <- reactive({
            
            if (input$covid_cases_deaths_tmpt == "weekly.cases") {
                covid_type_text <- "cases"
            } else if (input$covid_cases_deaths_tmpt == "weekly.deaths") {
                covid_type_text <- "deaths"
            }
            
            covid_type_caption_text <- str_glue("Weekly number of U.S. Covid-19 {covid_type_text} overlaid in red. Value is reflected on the right y-axis.")
            
            return(covid_type_caption_text)
        })
        
        
        ##### Line plot (highcharter) -----
        
        output$hc_single_scale_date_tmpt_linePlot <- renderHighchart({
            req(selected_subscale_info$data$variable.name)
            
            hc <- highchart(type = "chart") |> 
                
                # set up axes
                hc_yAxis_multiples(
                    list(title = list(text = input$scores_by_date_by_timepoint_type)),
                    list(title = list(text = input$covid_cases_deaths_tmpt),
                         labels = list(style = list(color = "red")),
                         showLastLabel = FALSE, 
                         opposite = TRUE)) %>% 
                
                hc_xAxis(type = "datetime") %>% 
                
                # add line for scores
                hc_add_series(
                    data = scores_by_date_by_timepoint_data(),
                    color = color_palette_tmpt(),
                    id = "score",
                    'line',
                    hcaes(x = month_date,
                          y = !!input$scores_by_date_by_timepoint_type,
                          group = time.pt),
                    tooltip = list(
                        pointFormat = "Score={point.y}, n={point.n}<br>Time point: {point.tmpt}")) %>% 
                
                # add circles, sized for number of people
                hc_add_series(name = input$scores_by_date_by_timepoint_type,
                              data = scores_by_date_by_timepoint_data(),
                              'scatter',
                              linkedTo = "score",
                              hcaes(x = month_date,
                                    y = !!input$scores_by_date_by_timepoint_type,
                                    color = color_group,
                                    size = n),
                              tooltip = list(
                                  pointFormat = "{point.y}, n={point.n}<br>{point.x:%b %Y}<br>Time point: {point.tmpt}")) %>%
                
                hc_title(text = score_by_date_by_timepoint_title()) %>% 
                
                # add tooltip formatting
                hc_tooltip(crosshairs = TRUE)
            
            
            
            # add weekly covid deaths/cases if selected
            if (input$covid_cases_deaths_tmpt != "none") {
                
                hc <- hc %>%
                    hc_add_series(name = input$covid_cases_deaths_tmpt,
                                  data = covid_cases_deaths_dat, 
                                  "spline", 
                                  yAxis = 1,
                                  color = "red",
                                  hcaes(x = date, y = !!input$covid_cases_deaths_tmpt)) %>% 
                    hc_caption(text = score_by_date_by_timepoint_caption()) %>% 
                    
                    hc_credits(enabled = TRUE,
                               text = "Covid-19 data downloaded from CDC COVID Data Tracker", 
                               href = "https://covid.cdc.gov/covid-data-tracker/#trends_weeklycases_select_00")
            }
            
            hc
        })
        
        
        #### About accordion ----
        
        score_date_tmpt_accordion <- accordion(
            id = "about_scores_by_date_by_timept_lines",
            open = FALSE,
            
            accordion_panel(
                title = "About",
                p("This line plot shows the scores for a particular variable over time color coded by time point. Date is along the x-axis and score is on the left y-axis. Data are plotted as average or median scores by month. The size of the circle indicates the number of participants included in the calculation (average or median) for that month.  If selected, the right y-axis will represent weekly Covid-19 cases or deaths in the U.S."),
                p("Use the radio button options to 1) select whether to show average or median scores, and 2) to select whether to add weekly U.S. Covid-19 cases or deaths to the plot."),
                p("The plot is interactive. Hover over parts of the plot for more information.")
            )
        ) # close accordion
        
        
        #### Compose scores by date by time point page ----
        
        output$scores_by_date_by_time_point_panel <- renderUI({
            
            if (input$scale_main_group_single %in% c('everyday discrimination (EDISC)', 'major discrimination (MDISC)', 'race-related experiences (RaLES)', 'cumulative adversity (CAM)')) {
                tagList(
                    h4("Scores by date by time point"),
                    score_date_tmpt_accordion,
                    br(),
                    
                   tmpt_warning_ui(NS(id, "warning_scores_by_date_by_time_point")),
                   
                   layout_columns(
                       width = 1/2,
                       
                       radioButtons(
                           inputId = ns("scores_by_date_by_timepoint_type"),
                           label = "What should be plotted?",
                           choices = c(
                               "average scores" = "avg_score",
                               "median scores" = "median_score"),
                           selected = "avg_score"),
                       
                       radioButtons(
                           inputId = ns("covid_cases_deaths_tmpt"),
                           label = "Add average weekly Covid-19 cases or deaths?",
                           choices = c(
                               "none" = "none", 
                               "cases" = "weekly.cases", 
                               "deaths" = "weekly.deaths"),
                           selected = "none"
                       )
                   ),
                   
                   br(),
                   
                   highchartOutput(ns("hc_single_scale_date_tmpt_linePlot"), height = "500px"),
                   
                   br(), br()
                )
            } else {
                tagList(
                    h4("Scores by date by time point"),
                    score_date_tmpt_accordion,
                    br(),
                    
                    layout_columns(
                        width = 1/2,
                        
                        radioButtons(
                            inputId = ns("scores_by_date_by_timepoint_type"),
                            label = "What should be plotted?",
                            choices = c(
                                "average scores" = "avg_score",
                                "median scores" = "median_score"),
                            selected = "avg_score"),
                        
                        radioButtons(
                            inputId = ns("covid_cases_deaths_tmpt"),
                            label = "Add average weekly Covid-19 cases or deaths?",
                            choices = c(
                                "none" = "none", 
                                "cases" = "weekly.cases", 
                                "deaths" = "weekly.deaths"),
                            selected = "none"
                        )
                    ),
                    
                    br(),
                    
                    highchartOutput(ns("hc_single_scale_date_tmpt_linePlot"), height = "500px"),
                    
                    br(), br()
                )
            }
        })
        
        
        
 
        
    })
    

}