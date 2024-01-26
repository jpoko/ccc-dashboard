# test scatter plot x & y var inputs from prepared list

# Define UI ----
scatter_plots_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    tagList(
        
        #h5("Explore relationships among various measures"),
        
        uiOutput(ns("about_scatter_plots")),
        
        layout_sidebar(
            sidebar = sidebar(
                width = "25%",
                
                h5("Options:"),
                
                # Input: x-axis
                virtualSelectInput(
                    inputId = ns("x_var_scatter"),
                    label = tags$strong("x-axis variable"),
                    choices = prepare_choices(variable_information_grouped,
                                              label = dropdown.description,
                                              value = variable.name,
                                              group_by = grouping_var),
                    multiple = FALSE, 
                    selected = "stai"
                ),
                
                materialSwitch(
                    inputId = ns("hide_show_x_var_descriptions"),
                    label = span(tags$strong("Show/hide x-variable description"), tooltip(bs_icon("question-square"), "Description appears below the main scatter plot.")),
                    value = FALSE,
                    status = "success"
                ),
                
                # Input: y-axis
                virtualSelectInput(
                    inputId = ns("y_var_scatter"),
                    label = tags$strong("y-axis variable"),
                    choices = prepare_choices(variable_information_grouped,
                                              label = dropdown.description,
                                              value = variable.name,
                                              group_by = grouping_var),
                    multiple = FALSE, 
                    selected = "meditation.years"
                ),
                
                materialSwitch(
                    inputId = ns("hide_show_y_var_descriptions"),
                    label = span(tags$strong("Show/hide y-variable description"), tooltip(bs_icon("question-square"), "Description appears below the main scatter plot.")),
                    value = FALSE,
                    status = "success"
                ),
                
                # Input: time point
                radioButtons(
                    inputId = ns("timepoint_var"),
                    label = span(tags$strong("Time point"), tooltip(bs_icon("question-square"), "The main plot shows the time point selected. The faceted plots at the bottom show the data at all of the time points it was collected.")),
                    #label = "Time point",
                    choices = timepoint_label,
                    selected = "T1"
                ),
                
                # Input: demographic category
                radioButtons(
                    inputId = ns("demographic_var"),
                    label = span(tags$strong("Demographic characteristic"), tooltip(bs_icon("question-square"), "If a demographic category is selected, the dots will be colored according to the 
groupings within the demographic category. You can select certain groups to be 
shown or not shown by selecting the name of the group in the legend to the right of the main plot.")),
                    #label = "Demographic characteristic",
                    choices = c(
                        #"None" = "none",
                        "None" = "demographic",
                        "Age" = "age",
                        "Education" = "education",
                        "Ethnicity" = "ethnicity",
                        "Gender" = "gender",
                        "Household income" = "income",
                        "Sexual orientation" = "orientation"
                    ),
                    selected = "demographic"
                ),
                
                # Input: all or complete participants
                radioButtons(
                    inputId = ns("scale_complete"),
                    label = span(tags$strong("Participants"), tooltip(bs_icon("question-square"), "Select which participants to show in the plots. 'All' = all participants who have data for the variables selected at the time point selected 'Complete' = participants who have data at every time point the data were collected for both variables selected")),
                    choices = c("All", "Complete"),
                    selected = "All"
                ),
                

                # Input: marginal plot type
                radioButtons(
                    inputId = ns("marginal_plot_type"),
                    label = span(tags$strong("Marginal plot type"), tooltip(bs_icon("question-square"), "Select the plot type for the marginal plots.")),
                    choices = c("violin", "box", "histogram"),
                    selected = "box"
                ),


                # Input: correlation type
                radioButtons(
                    inputId = ns("corr_type"),
                    label = span(tags$strong("Correlation type"), tooltip(bs_icon("question-square"), "Select the type of correlation for the correlation table.")),
                    choices = c("Pearson" = "pearson",
                                "Spearman" = "spearman"),
                    selected = "pearson"
                ), 



# 
# 
# 
# 
#                 # Input: x & y marginal plot types
#                 
#                 radioButtons(
#                     inputId = ns("marginal_x_plot_type"),
#                     label = span(tags$strong("X-axis marginal plot type"), tooltip(bs_icon("question-square"), "Select the plot type for the marginal plot of the x-axis data. This is shown above the main scatter plot.")),
#                     choices = c("violin", "box", "histogram"),
#                     selected = "box"
#                 ),
# 
#                 radioButtons(
#                     inputId = ns("marginal_y_plot_type"),
#                     label = span(tags$strong("Y-axis marginal plot type"), tooltip(bs_icon("question-square"), "Select the plot type for the marginal plot of the x-axis data. This is shown to the right of the main scatter plot.")),
#                     choices = c("violin", "box", "histogram"),
#                     selected = "box"
#                 ),
                
                # Input: regression line or no
                checkboxInput(
                    inputId = ns("fit"),
                    label = span(tags$strong("Add best fit (regression) line to scatterplot"), tooltip(bs_icon("question-square"), "Select to add best fit (regression) line to scatter plot. If a demographic category is also selected, individual regression lines for each demographic category will be drawn on the scatter plots.")),
                    #"Add best fit (regression) line to scatterplot",
                    value = FALSE
                )
                
                
                
            ), # close sidebar
        
            
            # conditional panels - created in server
            uiOutput(ns("dynamic_scatter_panel"))
            
            ) # close layout_sidebar
        

        ) # close tagList
        
} 


# Define server ----
scatter_plots_server  <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        ns <- session$ns
      
        
        ## General data prep ----
        
        
        ### Variables at each time point ----

        # When time point changes, update the available variables
        get_available_variables_for_time_point <- reactive({
            req(input$timepoint_var)
            
            available_vars[[input$timepoint_var]]
        })
        
        
        
        ### Update scatter plots' heading text ----
        
        # formula for the text - scatter plot heading
        txt_scatterplot_heading <- reactive({
            req(input$x_var_scatter, input$y_var_scatter)
            
            paste(
                "Relationship between ",
                variable_information_grouped$text.name[variable_information_grouped$variable.name==input$x_var_scatter], 
                " and ",
                variable_information_grouped$text.name[variable_information_grouped$variable.name==input$y_var_scatter],
                " at the ",
                names(timepoint_label)[timepoint_label == input$timepoint_var],
                " time point."
            )
        })
        
        # return formula text for printing as a caption
        output$caption <- renderText({
            txt_scatterplot_heading()
        })
        
        
        # scatter plot heading of faceted plots
        txt_scatterplot_faceted <- reactive({
            req(input$x_var_scatter, input$y_var_scatter)
            
            paste(
                "Relationship between ",
                variable_information_grouped$text.name[variable_information_grouped$variable.name==input$x_var_scatter], 
                " and ",
                variable_information_grouped$text.name[variable_information_grouped$variable.name==input$y_var_scatter],
                " at each time point"
            )
        })
        
        # return formula text for printing as a caption
        output$caption_facets <- renderText({
            txt_scatterplot_faceted()
        })
        
        
        
        
        ### Data for counts of n, correlations, scatter plots ----
        
        # Filter for selected x & y vars, make data wide
        scatter_x_y <- reactive({
            req(input$x_var_scatter, input$y_var_scatter)
            
            selected_x_y <- dat_subscale |>
                # only x and y variables
                dplyr::filter(measure %in% c(input$x_var_scatter, input$y_var_scatter)) |>
                
                # remove date (different dates for different measures)
                dplyr::select(-recorded.date) |>
                
                # make data wide
                pivot_wider(
                    names_from = "measure",
                    values_from = "value"
                )
            
            return(selected_x_y)
        })
        
        
        # Filter for selected time point
        scatter_x_y_tmpt <- reactive({
            req(input$x_var_scatter, input$y_var_scatter)
            
            selected_x_y_tmpt <- scatter_x_y() |>
                # only time point selected
                dplyr::filter(time.pt %in% input$timepoint_var)
            
            return(selected_x_y_tmpt)
        })
        
        
        
        # Tmpt data for all participants
        scatter_x_y_tmpt_all <- reactive({
            req(input$x_var_scatter, input$y_var_scatter, input$timepoint_var)
            
            selected_x_y_tmpt_all <- scatter_x_y_tmpt() |>
                # remove where NA in either x or y variable (keep only if have x and y)
                dplyr::filter(!is.na(.data[[input$x_var_scatter[[1]]]]) & !is.na(.data[[input$y_var_scatter[[1]]]]))
            
            return(selected_x_y_tmpt_all)
            
        })
        
        
        
        # Tmpt data for complete participants
        scatter_x_y_tmpt_complete <- reactive({
            req(input$x_var_scatter, input$y_var_scatter, input$timepoint_var)
            
            dat_all <- scatter_x_y_tmpt()
            
            # get name of 'complete' variable for x input
            x_var_name <- paste0("^", input$x_var_scatter, "_complete", sep = "")
            # get index of 'complete' variable for x input
            x_var_col <- grep(x_var_name, colnames(dat_all))
            
            
            # get name of 'complete' variable for y input
            y_var_name <- paste0("^", input$y_var_scatter, "_complete", sep = "")
            # get index of 'complete' variable for y input
            y_var_col <- grep(y_var_name, colnames(dat_all))
            
            
            selected_x_y_tmpt_complete <- dat_all |>
                # filter data where x and y variables are complete
                dplyr::filter(dat_all[x_var_col]==1 & dat_all[y_var_col]==1) |>
                
                # probably not needed:
                # remove where NA in either x or y variable (keep only if have x and y)
                dplyr::filter(!is.na(.data[[input$x_var_scatter[[1]]]]) & !is.na(.data[[input$y_var_scatter[[1]]]]))
            
            return(selected_x_y_tmpt_complete)
        })
        
        
        
        # Data for all tmpts, complete or all (depends on input selected) 
        scatter_x_y_all_tmpts <- reactive({
            req(input$x_var_scatter, input$y_var_scatter, input$timepoint_var)
            
            dat_all_tmpts <- scatter_x_y()
            
            if (input$scale_complete == "Complete") {
                
                # get name of 'complete' variable for x input
                scatter_x_var_name <- paste0("^", input$x_var_scatter, "_complete", sep = "")
                # get index of 'complete' variable for x input
                scatter_x_var_col <- grep(scatter_x_var_name, colnames(dat_all_tmpts))
                
                # get name of 'complete' variable for y input
                scatter_y_var_name <- paste0("^", input$y_var_scatter, "_complete", sep = "")
                # get index of 'complete' variable for y input
                scatter_y_var_col <- grep(scatter_y_var_name, colnames(dat_all_tmpts))
                
                # get the complete data
                selected_x_y_all_tmpts <- dat_all_tmpts |>
                    
                    # filter where x and y variables are complete
                    dplyr::filter(dat_all_tmpts[scatter_x_var_col]==1 & dat_all_tmpts[scatter_y_var_col]==1) |>
                    
                    # remove any rows where there is an NA in the 2 variables selected - not needed because we are using 'complete' data here, meaning they have data at all time points, so they will have data here.
                    dplyr::filter(!is.na(.data[[input$x_var_scatter[[1]]]]) & !is.na(.data[[input$y_var_scatter[[1]]]]))
                
                return(selected_x_y_all_tmpts)
                
            } else {
                # all data but NAs removed for x and y
                selected_x_y_all_tmpts <- dat_all_tmpts |>
                    
                    # remove any rows where there is an NA in the 2 variables selected
                    dplyr::filter(!is.na(.data[[input$x_var_scatter[[1]]]]) & !is.na(.data[[input$y_var_scatter[[1]]]]))
                
                return(selected_x_y_all_tmpts)
            }
            
        })
        
        
        ### Counts for n in scatter plots ----
        
        # Extract count of all participants who have x & y data at selected time point
        count_all_n <- reactive({
            req(input$x_var_scatter, input$y_var_scatter, input$timepoint_var)
            
            count_all_n <- nrow(scatter_x_y_tmpt_all())
            paste0("All participants: n = ", count_all_n)
        })
        
        output$counts_scatterplots_all <- renderText({
            count_all_n()
        })
        
        
        # Extract count of participants who have x & y data at all time points
        count_complete_n <- reactive({
            req(input$x_var_scatter, input$y_var_scatter, input$timepoint_var)
            
            dat_all_complete <- nrow(scatter_x_y_tmpt_complete())
            paste0("Complete participants: n = ", dat_all_complete)
        })
        
        output$counts_scatterplots_complete <- renderText({
            count_complete_n()
        })
        
        
        ## Build components ----
        
        
        ### Variable descriptions ----
        
        ### Short descriptions - x variable----
        x_var_lay_description_file <- reactive({
            req(input$x_var_scatter, input$y_var_scatter, input$timepoint_var)
            
            # get file path and file name from variable information data frame
            file_path <- variable_information_grouped$lay.filename[variable_information_grouped$variable.name == input$x_var_scatter]
            
            return(file_path)
        })
        
        output$x_var_lay_description <- renderUI({
            if (input$hide_show_x_var_descriptions) {
                includeMarkdown(x_var_lay_description_file())
            } else {
                NULL
            }
        })
        
        observeEvent(input$hide_show_x_var_descriptions, {
            if (input$hide_show_x_var_descriptions) {
                shinyjs::enable("x_var_lay_description")  # Show the description
            } else {
                shinyjs::disable("x_var_lay_description")  # Hide the description
            }
        })
        
        
        
        ### Short descriptions - y variable ----
        y_var_lay_description_file <- reactive({
            # get file path and file name from variable information data frame
            file_path <- variable_information_grouped$lay.filename[variable_information_grouped$variable.name == input$y_var_scatter]
            
            return(file_path)
        })
        
        output$y_var_lay_description <- renderUI({
            if (input$hide_show_y_var_descriptions) {
                includeMarkdown(y_var_lay_description_file())
            } else{
                NULL
            }
        })
        
        observeEvent(input$hide_show_y_var_descriptions, {
            if (input$hide_show_y_var_descriptions) {
                shinyjs::enable("y_var_lay_description")  # Show the description
            } else {
                shinyjs::disable("y_var_lay_description")  # Hide the description
            }
        })
        
        
        ### Main scatter plot & marginal distribution plots ----
        
        # Filter data for scatter plot so only includes complete cases
        scatter_data <- reactive({
            req(input$x_var_scatter, input$y_var_scatter, input$timepoint_var)
            
            if (input$scale_complete == "Complete") {
                scatter_dat <- scatter_x_y_tmpt_complete()
            } else if (input$scale_complete == "All") {
                scatter_dat <- scatter_x_y_tmpt_all()
            }
            return(scatter_dat)
        })
        
        
        # Set up x-axis marginal plot - violin, box, histogram
        # NOTE: violin plot requires additional spanmode setting, so is separate
        marginal_x_plot <- reactive({
            req(input$x_var_scatter, input$y_var_scatter, input$timepoint_var)
            
            if (input$marginal_plot_type == "violin") {
                plot_ly(data = scatter_data(), 
                        x = ~get(input$x_var_scatter), 
                        type = 'violin', 
                        alpha = .5, 
                        color = ~get(input$demographic_var), 
                        colors = "viridis", 
                        showlegend = FALSE,
                        spanmode = "hard")
            } else {
                plot_ly(data = scatter_data(), 
                        x = ~get(input$x_var_scatter), 
                        type = input$marginal_plot_type, 
                        alpha = .5, 
                        color = ~get(input$demographic_var), 
                        colors = "viridis", 
                        showlegend = FALSE)
            }
        })
        
        # Set up y-axis marginal plot - violin, box, histogram
        # NOTE: violin plot requires additional spanmode setting, so is separate
        marginal_y_plot <- reactive({ 
            req(input$x_var_scatter, input$y_var_scatter, input$timepoint_var)
            
            if (input$marginal_plot_type == "violin") {
                
                plot_ly(data = scatter_data(), 
                        y = ~get(input$y_var_scatter), 
                        type = 'violin', 
                        alpha = .5, 
                        color = ~get(input$demographic_var), 
                        colors = "viridis", 
                        showlegend = FALSE,
                        spanmode = "hard")
            } else {
                plot_ly(data = scatter_data(), 
                        y = ~get(input$y_var_scatter), 
                        type = input$marginal_plot_type, 
                        alpha = .5, 
                        color = ~get(input$demographic_var), 
                        colors = "viridis", 
                        showlegend = FALSE)
            }
        })
        
        # Set up main scatter plot
        marginal_scatter_plot <- reactive({
            req(input$x_var_scatter, input$y_var_scatter, input$timepoint_var)
            
            # Add regression line if option is selected
            if (input$fit) {
                
                # Resources:
                # https://rpubs.com/cfong32/Plotly-Tutorial (search: regression)
                # https://stackoverflow.com/questions/53836414/r-plotly-plotting-multiple-regression-lines
                # how to pass variables: https://stackoverflow.com/questions/18762962/passing-variable-names-to-model-in-shiny-app
                
                # x variable as predictor
                model_predictors <- paste(input$x_var_scatter, collapse = "+")
                model_response <- paste(input$y_var_scatter, collapse = "+")
                
                # Determine model formula depending on whether demographic category
                # is selected or not (if demographic category is selected, separate
                # regression lines are calculated for each demographic category group)
                # Formula: response ~ predictor / response ~ predictor * predictor 2
                # score ~ meditation variable / score ~ meditation variable * demographic
                if (input$demographic_var == "demographic") {
                    model_formula <- as.formula(sprintf('%s ~ %s', 
                                                        model_response, 
                                                        model_predictors))
                } else {
                    model_category <- paste(input$demographic_var, collapse = "+")
                    model_formula <- as.formula(sprintf('%s ~ %s*%s', 
                                                        model_response, 
                                                        model_predictors, 
                                                        model_category))
                }
                
                # Run regression
                model_fit <- lm(model_formula, data = scatter_data())
                
                # Scatter plot of selected meditation variable (x-axis) and 
                # score on selected well-being scale (y-axis), with regression
                # line(s)
                plot_ly(data = scatter_data(), 
                        x = ~get(input$x_var_scatter), 
                        y = ~get(input$y_var_scatter), 
                        type = 'scatter', 
                        color = ~get(input$demographic_var), 
                        colors = "viridis", 
                        mode = "markers") %>%
                    add_trace(
                        data = scatter_data(),
                        x = ~get(input$x_var_scatter),
                        y = model_fit$fitted.values,
                        mode = "lines",
                        showlegend = FALSE) 
            } else {
                # Scatter plot of selected meditation variable (x-axis) and 
                # score on selected well-being scale (y-axis)
                plot_ly(data = scatter_data(), 
                        x = ~get(input$x_var_scatter), 
                        y = ~get(input$y_var_scatter), 
                        type = 'scatter', 
                        color = ~get(input$demographic_var), 
                        colors = "viridis", 
                        mode = "markers") 
            }
        })
        
        
        # Layout main scatter plot with marginal distribution plots
        output$scatPlot <- renderPlotly({
            req(input$x_var_scatter, input$y_var_scatter, input$timepoint_var)
            
                marg_plot <- plotly::subplot(marginal_x_plot(), 
                                             plotly_empty(), 
                                             marginal_scatter_plot(), 
                                             marginal_y_plot(),
                                             nrows = 2, 
                                             heights = c(.2, .8), widths = c(.8,.2), 
                                             margin = 0,
                                             shareX = TRUE, shareY = TRUE, 
                                             titleY = TRUE)
                
                # TODO: remove the rangemode stuff here - not what desire - replace with min and max of scale
                
                plotly::layout(marg_plot, 
                               barmode = 'overlay', 
                               
                               yaxis2 = list(title = variable_information_grouped$variable[variable_information_grouped$variable.name==input$y_var_scatter],
                                             rangemode = "normal", range = c(-0.1, NA)), 
                               xaxis = list(title = variable_information_grouped$variable[variable_information_grouped$variable.name==input$x_var_scatter],
                                            rangemode = "normal", range = c(-0.1, NA))
                               
                ) |>
                    plotly::config(displayModeBar = FALSE)
                
            #}
        })
        
        
        
        
        
        ### Faceted scatter plots ----
        
        output$scatfacetPlot <- renderPlot({
            req(input$x_var_scatter, input$y_var_scatter, input$timepoint_var)
            
            facet_plot <- scatter_x_y_all_tmpts() |>    
                ggplot(
                    aes_string(
                        x = input$x_var_scatter,
                        y = input$y_var_scatter,
                        color = input$demographic_var)) +
                
                geom_count(alpha = 0.65) +
                scale_size_area() +
                
            # FORMATTING
                theme_bw() +
                scale_color_viridis_d() +
                
                theme(
                    
                    # make all text Roboto
                    text = element_text(
                        family = "Roboto"
                    ),
                    
                    axis.text = element_text(
                        size = 12
                    ),
                    
                    axis.title = element_text(
                        size = 14
                    ),

                    legend.position = "bottom",
                    legend.text = element_text(size = 15),
                    legend.title = element_text(size = 16),
                    legend.direction = "vertical",
                    
                    strip.background = element_rect(
                        color = "black",
                        fill = "#58758b"),
                    strip.text = element_text(
                        color = "white",
                        face = "bold",
                        size = 16)

                    ) +
                    
 
                # Add labels to plot
                labs(
                    x = variable_information_grouped$variable[variable_information_grouped$variable.name==input$x_var_scatter],
                    y = variable_information_grouped$variable[variable_information_grouped$variable.name==input$y_var_scatter],
                    fill = "Demographic"
                ) +
                
                # Facet by time point
                facet_wrap( ~ time.pt,
                            nrow = 1)
            
            # Add regression line(s) if option is selected 
            if (input$fit) {
                facet_plot <- facet_plot + 
                    geom_smooth(
                        alpha = 0.3,
                        method = "lm",
                        se = FALSE,
                        fill = NA
                    )
            }
            facet_plot
        })
        
        
        ### Correlations ----
        
        
        # ## Get correlation type ----
        # corr_type <- reactive({
        #     input$corr_type
        # })
        
        # Calculate correlations and make table
        calculate_correlations <- reactive({
            req(input$x_var_scatter, 
                input$y_var_scatter, 
                input$timepoint_var,
                input$corr_type)
            
            # Partial correlation all participants - inputs here should be the columns corresponding to: x var, y var, age
            
            # need column indices for x and y vars
            x_var_name <- paste0("^", input$x_var_scatter, "$", sep = "")
            x_var_col_all <- grep(x_var_name, colnames(scatter_x_y_tmpt_all()))
            
            y_var_name <- paste0("^", input$y_var_scatter, "$", sep = "")
            y_var_col_all <- grep(y_var_name, colnames(scatter_x_y_tmpt_all()))
            
            age_col_all <- grep("^ages", colnames(scatter_x_y_tmpt_all()))
            
            partial_corr_all <- pcor.test(
                scatter_x_y_tmpt_all()[, x_var_col_all],
                scatter_x_y_tmpt_all()[, y_var_col_all],
                scatter_x_y_tmpt_all()[, age_col_all],
                method = input$corr_type)
            
            # Correlation all participants
            orig_corr_all <-
                corr.test(scatter_x_y_tmpt_all()[, x_var_col_all], 
                          scatter_x_y_tmpt_all()[, y_var_col_all], 
                          method = input$corr_type)
            
            
            
            # Partial correlation complete participants
            x_var_col_complete <- grep(x_var_name, colnames(scatter_x_y_tmpt_complete()))
            y_var_col_complete <- grep(y_var_name, colnames(scatter_x_y_tmpt_complete()))
            age_col_complete <- grep("^ages", colnames(scatter_x_y_tmpt_complete()))
            
            
            partial_corr_complete <- pcor.test(scatter_x_y_tmpt_complete()[, x_var_col_complete],
                                               scatter_x_y_tmpt_complete()[, y_var_col_complete],
                                               scatter_x_y_tmpt_complete()[, age_col_complete],
                                               method = input$corr_type)
            
            # Correlation of complete participants
            orig_corr_complete <-
                corr.test(scatter_x_y_tmpt_complete()[, x_var_col_complete], 
                          scatter_x_y_tmpt_complete()[, y_var_col_complete], 
                          method = input$corr_type)
            
            # Make table of correlation results (type, estimate, p, n)
            correlation_table <- data.frame(
                Correlation = c(
                    "All participants",
                    "All participants controlling for age",
                    "Complete participants",
                    "Complete participants controlling for age"
                ),
                Estimate = c(
                    orig_corr_all$r,
                    partial_corr_all$estimate,
                    orig_corr_complete$r,
                    partial_corr_complete$estimate
                ),
                p = c(
                    orig_corr_all$p,
                    partial_corr_all$p.value,
                    orig_corr_complete$p,
                    partial_corr_complete$p.value
                ),
                n = c(
                    orig_corr_all$n,
                    partial_corr_all$n,
                    orig_corr_complete$n,
                    partial_corr_complete$n
                )
            )
            
            # Round estimate and p values
            correlation_table <- correlation_table |>
                mutate(Estimate = round(Estimate, 3),
                       p = round(p, 3))
            
            # 'fix' p value in table - if '0' after rounding, replace with p < 0.001
            correlation_table <- correlation_table |>
                mutate(p = case_when(
                    p == 0 ~ '< 0.001', 
                    TRUE ~ as.character(p)
                ))
            
            # Return correlation table
            correlation_table
        })
        
        
        
        
        # Correlation table  
        output$dt_correlation_table <- DT::renderDataTable(
            calculate_correlations(),
            options = list(paging = FALSE, dom = "t"),
            rownames = FALSE
        )
        
        # Correlation table heading
        correlation_table_heading <- reactive({
            req(input$x_var_scatter, input$y_var_scatter, input$timepoint_var, input$corr_type)
            
            
            corr_type <- str_to_title(input$corr_type)
            
            
            paste(
                corr_type, 
                " correlations between ",
                variable_information_grouped$text.name[variable_information_grouped$variable.name==input$x_var_scatter], 
                " and ",
                variable_information_grouped$text.name[variable_information_grouped$variable.name==input$y_var_scatter], 
                " at the ",
                names(timepoint_label)[timepoint_label == input$timepoint_var],
                " time point."
            )
        })
        
        output$correlation_table_txt <- renderText({
            correlation_table_heading()
        })
        
        
        
        ## Create dynamic UI ----
        
        # create panels based on what variables are selected 
        output$dynamic_scatter_panel <- renderUI({
            req(input$x_var_scatter, input$y_var_scatter, input$timepoint_var)
            
            # get the variables available for time point selected
            available_vars_tmpt <- get_available_variables_for_time_point()
            
            # get the x and y variables selected
            x_var <- input$x_var_scatter
            y_var <- input$y_var_scatter
            
            
            
            if (x_var %in% available_vars_tmpt && y_var %in% available_vars_tmpt)  {
                
                tagList(
                    
                    h5(textOutput(ns("caption"))),
                    br(),
                    card(
                        full_screen = TRUE,
                        card_header(
                            "Scatter plot",
                            popover(
                                trigger = bs_icon("info-circle",
                                                  title = "Scatter plot info"),
                                title = "Scatter plot info",
                                p("Scatter plot showing the relationship
                                  between the selected x and y variables."),
                                p("This plot is interactive. You can hover over areas of the plot to reveal more information. You can also zoom in by drawing a box around an area to zoom in on, and you can zoom out by double clicking on the plot area. You can expand the entire plot by clicking on the expand button that appears at the bottom right when you hover over the plot."),
                                p("If a demographic category is selected, you can choose whether or not a certain group is shown by selecting the name of that group(s) in the legend on the right (note: this legend is only visible if a demographic category is selected).")
                            )
                        ),
                        card_body(
                            plotlyOutput(ns("scatPlot"),
                                         height = "500px")
                        )
                    ),
                    
                    uiOutput(ns("x_var_lay_description")),
                    uiOutput(ns("y_var_lay_description")),

                    hr(),
                    
                    h5("Number of participants"),
                    p(textOutput(ns("counts_scatterplots_all"))),
                    p(textOutput(ns("counts_scatterplots_complete"))),
                    
                    hr(),
                    
                    accordion(
                        open = TRUE,
                        
                        accordion_panel(
                            value = "correlations",
                            title = span("Correlation table", tooltip(bs_icon("question-square"), "Table showing the correlation between the variables selected. Correlations are Pearson or Spearman correlations for all participants and for 'complete' participants (those who completed the scale at every time point), as well as partial correlations for all and complete participants controlling for age.")),
                            # p(strong(span("Correlation table"), popover(bs_icon("info-circle"), p("Table showing the correlation between the 
                            #       variables selected. Correlations are Spearman
                            #       correlations for all participants and for 
                            #       'complete' participants (those who completed 
                            #       the scale at every time point), as well as 
                            #       partial correlations for all and complete 
                            #       participants controlling for age.")))),
                            # 
                            # 
                            # 
                            # title = "Correlations",
                            # p(strong(span("Correlation table"), popover(bs_icon("info-circle"), p("Table showing the correlation between the 
                            #       variables selected. Correlations are Spearman
                            #       correlations for all participants and for 
                            #       'complete' participants (those who completed 
                            #       the scale at every time point), as well as 
                            #       partial correlations for all and complete 
                            #       participants controlling for age.")))),
                            
                            
                            p(textOutput(ns("correlation_table_txt"))),
                            DT::dataTableOutput(ns("dt_correlation_table"))

                        )
                    ),
 
                    
                    hr(),

                    card(
                        full_screen = TRUE,
                        card_header(
                            "Scatter plots by time point (faceted plots)",
                            popover(
                                trigger = bs_icon("info-circle"),
                                p("Scatter plots showing the relationship
                                  between the x and y variables selected at each of
                                  the time points they were assessed."),
                                p("The size of the circle indicates the number
                                  of people who provided the same response."),
                                p("You can expand the entire plot by clicking 
                                  on the expand button that appears at the 
                                  bottom right when you hover over the plot.")
                            )
                        ),
                        card_body(
                            # heading for faceted scatter plots
                            h5(textOutput(ns("caption_facets"))),
                            
                            # faceted scatter plots
                            plotOutput(ns("scatfacetPlot"),
                                       height = "700px")
                        )
                        
                    )
                    
                    
                    
                    
                    
                )
                
                
            } else if (x_var %in% available_vars_tmpt) {
                return(HTML('<span style="color:red; font-size:20px; font-style:italic">Selected y-axis variable is not available at this time point</span>'))
            } else if (y_var %in% available_vars_tmpt) {
                return(HTML('<span style="color:red; font-size:20px; font-style:italic">Selected x-axis variable is not available at this time point</span>'))
            } else {
                return(HTML('<span style="color:red; font-size:20px; font-style:italic">Neither the x- or y-axis variables are available at this time point</span>'))
            }
            
        })
        
  
        
        
        
        
        ## About accordion ----
        
        output$about_scatter_plots <- renderUI({
            
            tagList(
                accordion(
                    id = "about_scatter_plot_accordion",
                    open = FALSE,
                    
                    accordion_panel(
                        "About",
                        value = "about_scatter_plot",
                        p("Here you can generate scatter plots to explore specific relationships between variables of interest across different time points in the study."),
                        p("From the options in the sidebar, you will select a variable to be plotted on the x-axis and a variable to be plotted on the y-axis. You will also choose which time point to view."),
                        p("After selecting variables of interest from the dropdown menus, a scatter plot depicting the relationship between the variables for the given time point will appear in the top panel on the right. This scatter plot is interactive—click on the info icon above the plot to learn more."),
                        p("Below this plot, you will see 1) the number of participants who provide data for those variables; 2) a correlation table presenting a series of correlations between the two variables selected; and 3) a series of faceted scatter plots that show the relationship between the variables selected at all of the time points at which they were measured-this panel allows you to quickly view general trends over time."),
                        p("You can choose to add different demographic characteristics, which will color code the data points according to different categories of the demographic characteristic selected. You may also choose to include  ", strong("‘all participants’"), " who provided data for those variables, or ", strong("‘complete participants,’"), " which will present only participants who provided data at all time points. This will give you a comparable view of the relationship between those two variables across timepoints."),
                        p("Hover over the question mark icons to learn more about each of the options available to you.")
                    )
                )
            )
        })
        
        
        
        
    })
    
    
}