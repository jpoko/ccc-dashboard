# Alluvial tab module

# Define UI ----
alluvial_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    tagList(
        
        radioButtons(
            inputId = ns("alluvial_data"),
            label = "Scale",
            choices = c("PSS", "STAI"),
            selected = "PSS",
            inline = T
        ),
        
        radioButtons(
            inputId = ns("alluvial_participant_option"),
            label = "Participants",
            choices = c("all", "complete"),
            selected = "all",
            inline = T
        ),
        
        radioButtons(
            inputId = ns("alluvial_color_option"),
            label = "Color by",
            choices = c("T1", "T4"),
            selected = "T1",
            inline = T
        ),
        
        
        parcatsOutput(
            ns("alluvial_plot")
        )
  
           
        )
        

    
 
} 


# Define server ----
alluvial_server <- function(id) {
    
    moduleServer(id, function(input, output, session) {
        
        ns <- session$ns
        
        output$alluvial_plot <- render_parcats({
            
            
            # how to color traces - first or last variables
            if (input$alluvial_color_option == "T1") {
                color_by_var <- "first_variable"
            } else if (input$alluvial_color_option == "T4") {
                color_by_var <- "last_variable"
            }
            
            
            
            # get data & set colors depending on inputs (all or complete participants, PSS or STAI)
            if (input$alluvial_participant_option == "all") {
                #alluvial_colors <- color_palette_alluvial_3_levels_na
                if (input$alluvial_data == "PSS") {
                    alluvial_data <- pss_alluvial_dat
                    
                } else if (input$alluvial_data == "STAI") {
                    alluvial_data <- stai_alluvial_dat
                }
            } else if (input$alluvial_participant_option == "complete") {
                #alluvial_colors <- color_palette_alluvial_3_levels
                if (input$alluvial_data == "PSS") {
                    alluvial_data <- pss_alluvial_dat %>% 
                        dplyr::filter(complete.cases(.))
                } else if (input$alluvial_data == "STAI") {
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
            gg <- parcats(g, marginal_histograms = FALSE)
            
            # show 'count' on hover tooltip
            gg$x$traces$parcats$hoverinfo <- "count"
            
            # change size of level labels
            gg$x$traces$parcats$tickfont <- list(size = 18)
            
            # show count of category on hover (not overall total - want it to be what is in the category that is hovered on/over)
            gg$x$traces$parcats$hoveron <- "category"
            
            gg
            
            
        })
        
        
        
        
        
        
    })
    

}