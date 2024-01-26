# app.R

# LOAD PACKAGES ----
source("./modules/load-packages.R")

# SOURCE MODULES ----
dir(path = "modules", full.names = TRUE) |> map(~ source(.))

assignInNamespace(
    "collapse_icon", 
    function() {
        bsicons::bs_icon(
            "caret-down-fill", class = "collapse-icon", size = NULL
        ) 
    },
    ns = "bslib"
)

thematic::thematic_shiny()

# UI ----
ui <- page_navbar(
    title = "Contemplative Coping during Covid-19",
    
    theme = bs_theme(
        version = 5,
        bg = "#FFFFFB",
        fg = "rgba(0, 0, 0, 0.87)",
        secondary = "#172A3A", 
        primary = "#58758b", 
        success = "#a7bfcc",
        info = "#6FE7FF", 
        warning = "#FFD65B", 
        #warning = "#a7bfcc",
        danger = "#C8553D",
        
        base_font = font_google("Roboto"), 
        heading_font = font_google("Roboto"),
        code_font = font_google("Noto Sans Mono"),
        "accordion-button-active-bg" = "#58758b",
        "accordion-button-active-color" = "#FFFFFF",
        "accordion-button-color" = "#FFFFFF",
        "accordion-button-bg" = "#58758b",
        "accordion-icon-active-color" = "#FFFFFF",
        "navbar-light-bg" = "#172A3A",
        "navbar-dark-bg" = "#172A3A",
        "card-cap-bg" = "#172A3A",
        "card-cap-color" = "#FFFFFF",
        "nav-link-hover-color" = "#E6E6E2",
        "nav-link-color" = "#bdc9d1"
        ), 
    
    includeCSS("styles.css"),

    nav_panel("Home", home_ui("home")),
    nav_panel("About", about_ui("about")),
    nav_panel("Demographics", demographics_ui("demographics")),
    nav_panel(HTML("Individual scales"),
               individual_scales_ui("scales")),
    
    nav_panel(HTML("Correlation matrices"), 
              correlation_matrices_ui("correlations")),
    nav_panel(HTML("Scatter plots"), scatter_plots_ui("scatter")),
    nav_panel("Credits", credits_ui("credits"))

    
)


# SERVER ----
server <- function(input, output, session) {
    
    #bs_themer()
    #bs_theme_preview()

    # server modules
    home_server("home")
    demographics_server("demographics")
    about_server("about")
    correlation_matrices_server("correlations")
    individual_scales_server("scales")
    scatter_plots_server("scatter")
    credits_server("credits")
    
}


# Run the application 
shinyApp(ui = ui, server = server)
