# Correlation matrices tab module

# Define UI ----
correlation_matrices_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    tagList(

        #h5("Correlation matrices of selected variables at the 4 data collection time points"),
        
        uiOutput(ns("about_corr_matrices")),
        
        uiOutput(ns("select_vars_inputs")),
        
        conditionalPanel(
            ns = ns,
            condition = "input.run_corr > 0",
            
            uiOutput(ns("correlation_matrices_plots"))
            
        )

     
    ) # close tagList
        
 
} 


# Define server ----
correlation_matrices_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        ns <- session$ns
        
  
        ## Set up data by time point object ----
        # will store the data for each time point
        data_by_time <- reactiveValues(
            t1 = NULL,
            t2 = NULL,
            t3 = NULL,
            t4 = NULL
        )
        
       
        ## Get the selected data ----
        selected_data <- eventReactive(input$run_corr, {
            selected_vars <- input$corr_vars
            #print(selected_vars)
            
            # Function to generate column names based on selected variable name
            generate_column_names <- function(var) {
                
                # variables that are: name_total_t[1-4]
                total_pattern <- paste0(var, "_total_t", 1:4)
                # variables that are: name_t[1-4] (meditation variables)
                t_pattern <- paste0(var, "_t", 1:4)
                
                # get column indexes for each possible pattern
                total_cols <- total_pattern[total_pattern %in% colnames(dat_corr)]
                t_cols <- t_pattern[t_pattern %in% colnames(dat_corr)]
                
                # return columns that match pattern in data
                if (length(total_cols) > 0) {
                    return(total_cols)
                } else if (length(t_cols) > 0) {
                    return(t_cols)
                } else {
                    stop("No matching columns found for variable: ", var)
                }
            }
            
            # Apply the function to each selected variable
            column_names <- sapply(selected_vars, generate_column_names)
            
            # Convert column_names to a character vector
            column_names <- unlist(column_names)
            
            # get data frame of those variables
            selected_data_df <- dat_corr[, colnames(dat_corr) %in% column_names, drop = FALSE]
            return(selected_data_df)

        })

        
        ## Update data_by_time with the data for each time point ----
        observeEvent(selected_data(), {

            data_by_time$t1 <- dplyr::select(selected_data(), contains("_t1")) %>%
                purrr::set_names(names(.) |> str_remove("_t1") |> str_remove("_total"))
            #print(head(data_by_time$t1, 10))

            data_by_time$t2 <- dplyr::select(selected_data(), contains("_t2")) %>%
                purrr::set_names(names(.) |> str_remove("_t2") |> str_remove("_total"))
            #print(head(data_by_time$t2, 10))
            
            data_by_time$t3 <- dplyr::select(selected_data(), contains("_t3")) %>%
                purrr::set_names(names(.) |> str_remove("_t3") |> str_remove("_total"))
            #print(head(data_by_time$t3, 10))
            
            data_by_time$t4 <- dplyr::select(selected_data(), contains("_t4")) %>%
                purrr::set_names(names(.) |> str_remove("_t4") |> str_remove("_total"))
            #print(head(data_by_time$t4, 10))
        })
        
        
        ## Get significance level input ----
        significant_p_value <- reactive({
            as.numeric(input$corr_matrix_sig_p_val)
        })
        
        
        ## Get ordering method ----
        corr_order <- reactive({
            input$corr_matrix_order
        })
        
        ## Get correlation type ----
        corr_type <- reactive({
            input$corr_matrix_type
        })


        ## Make the correlation heatmaps ----
        
        output$correlation_heatmap_t1 <- renderPlot({
            
            # check if enough data exist
            if (ncol(data_by_time$t1) >= 2) {
                # if yes, plot
                #corr <- round(cor(data_by_time$t1, use = "complete.obs"), 2)
                corr <- round(cor(data_by_time$t1, use = "complete.obs", method = corr_type()), 2)
                #print(head(corr, 10))
                
                #p_mat <- ggcorrplot::cor_pmat(data_by_time$t1)
                p_mat <- corrplot::cor.mtest(data_by_time$t1, method = corr_type())
                
                #correlation_heatmap(corr, p_mat, significant_p_value(), corr_order(), "Baseline")
                correlation_heatmap(corr, p_mat$p, significant_p_value(), corr_order(), "Baseline")
                
            } else {
                # if not, create empty plot and provide message
                plot(0, type = "n", axes = FALSE, ann = FALSE, xlim = c(0, 1), ylim = c(0, 1))
                text(0.5, 0.5, "At least 2 selected variables\n must exist at this time point", cex = 1.5, adj = c(0.5, 0.5))
            }
           
        })
        
        output$correlation_heatmap_t2 <- renderPlot({
            
            # check if enough data exist
            if (ncol(data_by_time$t2) >= 2) {
                # if yes, plot
                #corr <- round(cor(data_by_time$t2, use = "complete.obs"), 2)
                corr <- round(cor(data_by_time$t2, use = "complete.obs", method = corr_type()), 2)
                #print(head(corr, 10))
                #p_mat <- ggcorrplot::cor_pmat(data_by_time$t2)
                p_mat <- corrplot::cor.mtest(data_by_time$t2, method = corr_type())
                
                #correlation_heatmap(corr, p_mat, significant_p_value(), corr_order(), "4-month follow-up")
                correlation_heatmap(corr, p_mat$p, significant_p_value(), corr_order(), "4-month follow-up")
                
            } else {
                # if not, create empty plot and provide message
                plot(0, type = "n", axes = FALSE, ann = FALSE, xlim = c(0, 1), ylim = c(0, 1))
                text(0.5, 0.5, "At least 2 selected variables\n must exist at this time point", cex = 1.5, adj = c(0.5, 0.5))
            }
            
        })
       
        
        output$correlation_heatmap_t3 <- renderPlot({
            
            # check if enough data exist
            if (ncol(data_by_time$t3) >= 2) {
                # if yes, plot
                #corr <- round(cor(data_by_time$t3, use = "complete.obs"), 2)
                corr <- round(cor(data_by_time$t3, use = "complete.obs", method = corr_type()), 2)
                #print(head(corr, 10))
                #p_mat <- ggcorrplot::cor_pmat(data_by_time$t3)
                p_mat <- corrplot::cor.mtest(data_by_time$t3, method = corr_type())
                
                #correlation_heatmap(corr, p_mat, significant_p_value(), corr_order(), "8-month follow-up")
                correlation_heatmap(corr, p_mat$p, significant_p_value(), corr_order(), "8-month follow-up")
                
            } else {
                # if not, create empty plot and provide message
                plot(0, type = "n", axes = FALSE, ann = FALSE, xlim = c(0, 1), ylim = c(0, 1))
                text(0.5, 0.5, "At least 2 selected variables\n must exist at this time point", cex = 1.5, adj = c(0.5, 0.5))
            }
          
        })
        
        output$correlation_heatmap_t4 <- renderPlot({
            
            # check if enough data exist
            if (ncol(data_by_time$t4) >= 2) {
                # if yes, plot
                #corr <- round(cor(data_by_time$t4, use = "complete.obs"), 2)
                corr <- round(cor(data_by_time$t4, use = "complete.obs", method = corr_type()), 2)
                #print(head(corr, 10))
                #p_mat <- ggcorrplot::cor_pmat(data_by_time$t4)
                p_mat <- corrplot::cor.mtest(data_by_time$t4, method = corr_type())
                
                #correlation_heatmap(corr, p_mat, significant_p_value(), corr_order(), "1-year follow-up")
                correlation_heatmap(corr, p_mat$p, significant_p_value(), corr_order(), "1-year follow-up")
                
            } else {
                # if not, create empty plot and provide message
                plot(0, type = "n", axes = FALSE, ann = FALSE, xlim = c(0, 1), ylim = c(0, 1))
                text(0.5, 0.5, "At least 2 selected variables\n must exist at this time point", cex = 1.5, adj = c(0.5, 0.5))
            }
           
        })
        
        gear_heatmap_adjustments <- popover(
            bs_icon("gear"),
            title = "Heatmap adjustments",
            
            radioButtons(
                inputId = ns("corr_matrix_type"),
                label = "Select correlation type",
                choices = c("Pearson" = "pearson",
                            "Spearman" = "spearman"),
                selected = "pearson"
            ), 
            
            sliderInput(
                inputId = ns("corr_matrix_sig_p_val"),
                label = "Select significance level",
                min = 0,
                max = 0.1,
                value = 0.05,
                step = 0.01
            ),
            radioButtons(
                inputId = ns("corr_matrix_order"),
                label = "Select variable ordering",
                choices = c("Grouped" = "hclust",
                            "Alphabetical" = "alphabet"),
                # choices = c("Default" = "original",
                #             "Alphabetical" = "alphabet",
                #             "First principal component" = "FPC",
                #             "Hierarchical clustering" = "hclust"),
                selected = "hclust"
            ) 
        )
        
        
        matrices_heatmap_info <- popover(
            bs_icon("info-circle"),
            title = "Heatmap info",
            p("Additional options available under the gear icon to the right."),
            p("You can select the type of correlation to run, Pearson (default) or Spearman."),
            p("You can also select the significance level (0-0.1, default 0.05), to 
              use as the cutoff in the correlation matrix."),
            p("And you can select the type of ordering for the variables
              in the matrix, grouped (default) or alphabetical."),
            #p("Variable ordering options:"),
            tags$ul(
                tags$li("Grouped - Hierarchical clustering order, using complete 
                        agglomeration method (see reference below for more
                        information)"),
                tags$li("Alphabetical")
            ),
            p("Details: Wei, T., & Simko, V. (2021). R package 'corrplot': Visualization of a Correlation Matrix. https://taiyun.github.io/corrplot/")
            )
        
        
        correlation_matrices_card <- card(
            full_screen = TRUE,
            #height = 1500,
            card_header(
                div(class = "float-left", "Correlation heatmap matrices"),
                div(class = "float-right", matrices_heatmap_info, gear_heatmap_adjustments),
            ),
            card_body(
                layout_column_wrap(
                    width = 1/2,
                    plotOutput(ns("correlation_heatmap_t1")),
                    plotOutput(ns("correlation_heatmap_t2")),
                    plotOutput(ns("correlation_heatmap_t3")),
                    plotOutput(ns("correlation_heatmap_t4"))
                )
            )
                )
            
        
        
        
        
        output$correlation_matrices_plots <- renderUI({
            #req(input$run_corr > 0)
            
            correlation_matrices_card

        })

        
        ## About accordion ----
        output$about_corr_matrices <- renderUI({
            
            tagList(
                accordion(
                    id = "about_correlation_matrices_accordion",
                    open = FALSE,
                    
                    accordion_panel(
                        "About",
                        p("Here you can generate correlation matrices to explore the relationships among variables of interest."),
                        p("Select up to 8 variables from the dropdown menu and press the 'Run correlations' button. Correlation matrices will be generated for each of the 4 time points."),
                        p("Only significant correlations will appear in the matrices—if a square is missing (aside from the diagonal), it is because there is no significant correlation between the two variables given the significance level selected. The value shown in the square is the correlation coefficient value for significant correlations. The colors of the squares reflect the correlation coefficient, with darker colors representing negative correlations and lighter colors representing positive correlations."),
                        p("Additional adjustments to the matrices can be made by clicking on the gear icon in the top right of the correlation heatmap matrices section. You can change 1) the type of correlation to run (Pearson or Spearman), 2) the significance level used as the cutoff to show correlations, and 3) the order in which the variables appear in the matrix. Click on the info icon for more information about these options."),
                        p("NOTE: correlations are not adjusted for multiple comparisons.")
                    )
                )
            )
        })
        
        
        
        ## Select variables ----
        select_vars_card <- card(
            card_header("Select variables"),
            card_body(
                
                layout_columns(
                    col_widths = c(4, -2, 4, -2),
                   # col_widths = c(6, -2, 4),
                    #width = 1/2,
                    #height = 300,
                    
                
                ### Input ----
                virtualSelectInput(
                    inputId = ns("corr_vars"),
                    label = tags$strong("Select variables (8 max)"),
                    choices = prepare_choices(
                        variable_information_grouped,
                        label = dropdown.description,
                        value = variable.name,
                        group_by = grouping_var),
                    # can select multiple options
                    multiple = TRUE, 
                    # option to select all removed
                    disableSelectAll = TRUE,
                    # max number to select
                    maxValues = 8,
                    # min number to select
                    minValues = 2,
                    # show selected as tags in box
                    showValueAsTags = TRUE,
                    # allow searching
                    search = TRUE,
                    # highlights when match search
                    markSearchResults = TRUE,
                    # make dropbox popup
                    showDropboxAsPopup = TRUE,
                    # make max screen width for making popup large so always shows as popup
                    popupDropboxBreakpoint = '30000px',
                    popupPosition = 'center'
                    # allow searching by group title - include this if have more groups
                    # and can select group title to select all in group
                    #searchGroup = TRUE
                ),
                
                actionButton(inputId = ns("run_corr"), 
                             label = "Run correlations!",
                             class = "btn-primary",
                             width = '50%',
                             icon = icon("play"))
                
                )
             
            )
        )
        

        output$select_vars_inputs <- renderUI({
            select_vars_card
        }) 
        

        })
        
        
}