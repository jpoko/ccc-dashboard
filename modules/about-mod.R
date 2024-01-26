# About tab module

about_ui <- function(id) {
    
    ns <- shiny::NS(id)
    tagList(
        
        navset_card_pill(
            height = "1500px",
            
            
            ## Overview tab ----
            nav_panel(
                title = "Overview",

                ### Study at a glance ----
                
                #### Header ----
                # layout: width 75% of viewport
                div(
                    class="container text",
                    style="width:75%",
                    h4("Study at a glance"),
                ),

                #### Study duration boxes ----
                # layout: width 75% of viewport
                div(
                    class="container",
                    style="width:75%",
                    
                    layout_columns(
                        fill = FALSE,
                        value_box(
                            title = "Study duration",
                            value = "1 year",
                            showcase = bs_icon("calendar-range"),
                            theme_color = "secondary"
                        ),
                        value_box(
                            title = "Number of data collection time points",
                            value = "4",
                            showcase = bs_icon("calendar-event"),
                            theme_color = "secondary"
                        )
                    )
                ),
                
                #### Participants boxes ----
                # layout: width 75% of viewport
                div(
                    class="container",
                    style="width:75%",
                    
                    layout_columns(
                        fill = FALSE,
                        value_box(
                            title = "Number of applicants",
                            value = "836",
                            showcase = bs_icon("people"),
                            p("who were eligible for the study")
                        ),
                        value_box(
                            title = "Number admitted",
                            value = "608",
                            showcase = bs_icon("person-check"),
                        ),
                        value_box(
                            title = "Number who started",
                            value = "389",
                            showcase = bs_icon("person-video3"),
                            p("participated in the baseline assessment")
                        ),
                        
                        value_box(
                            title = "Number who finished",
                            value = "236",
                            showcase = bs_icon("check-all"),
                            p("participated in the 1-year assessment")
                        )
                    )
                ),

                br(),
                
                ### Learn more ----
                
                #### Header ----
                # layout: width 75% of viewport
                div(
                    class="container text",
                    style="width:75%",
                    h4("Learn more"),
                ),
                
                #### Accordion ----
                div(
                    class = "d-flex justify-content-center",
                    style = "width: 75%; margin: 0 auto;",
                    accordion(
                        id = "about_study_accordion",
                        multiple = FALSE,
                        width = "95%",
                        margin = "0 auto",
                        
                        #### Aim ----
                        accordion_panel(
                            "Aim",
                            value = "aim_panel",
                            htmlOutput(ns("aim_accordion_text"))
                        ),
                        
                        #### Study design ----
                        accordion_panel(
                            "Study Design",
                            value = "study_design_panel",
                            uiOutput(ns("study_design_accordion_fill"))
                        ),
                        
                        #### Recruitment ----
                        accordion_panel(
                            "Recruitment",
                            value = "recruitment_panel",
                            htmlOutput(ns("recruitment_accordion_text"))
                        ),
                        
                        #### Eligibility ----
                        accordion_panel(
                            "Eligibility",
                            value = "eligibility_panel",
                            uiOutput(ns("eligibility_accordion_fill"))
                        ),
                        
                        #### Enrollment ----
                        accordion_panel(
                            "Enrollment",
                            value = "enrollment_panel",
                            uiOutput(ns("enrollment_accordion_fill"))
                        ),
                        
                        #### Data collection ----
                        accordion_panel(
                            "Data Collection",
                            value = "data_collection_panel",
                            htmlOutput(ns("data_collection_accordion_fill"))
                        )
                    )
                    
                )
  
            ),
            
            
            ## Timeline tab ----
            
            nav_panel(
                title = "Timeline",
                
                card(
                    full_screen = TRUE,
                    height = 1500,
                    card_body(
                        plotlyOutput(ns("timeline_by_timepoint"))
                    )
                )

            ),  # close timeline tab
        
            
            ## Self-report measures ----
            nav_panel(
                title = "Measures",
             
                # ### Heading ----
                # div(
                #     class="container text",
                #     style="width:75%",
                #     h5("Self-report measures collected in the questionnaires"),
                # ),
                
                ### Content ----
                div(
                    class="container",
                    style="width:75%",
                    
                    layout_column_wrap(
                        width = 1/3,
                        uiOutput(ns("primary_questions")),
                        uiOutput(ns("secondary_questions")),
                        layout_column_wrap(
                            width = 1,
                            heights_equal = "row",
                            uiOutput(ns("weekly_questions")),
                            uiOutput(ns("blood_measures")),
                        )
                        
                    )
                )
            ), # close measures tab
        
            
            ## Participants flowchart tab ----
            nav_panel(
                title = "Participants",

                ### Heading ----
                div(
                    class="container text",
                    style="width:75%",
                    h5("Number of participants at each stage of the study"),
                ),

                ### About ----
                div(
                    class = "d-flex justify-content-center",
                    style = "width: 75%; margin: 0 auto;",
                    accordion(
                        id = "flowchart_accordion",
                        open = FALSE,
                        width = "95%",
                        margin = "0 auto",
                
                    accordion_panel(
                        "About",
                        
                        p("Flowchart depicting the number of participants at
                          each stage of the study. Click on the 'expand'
                          button that appears on the bottom right hand corner
                          to make the flowchart larger."),
                        p("The flowchart describes the number of people who participated in each stage of the recruitment and enrollment process (boxes shown in white). Below that, the four timepoints of the study are depicted with branches representing all of the components included in each of those assessments (these can also be seen on the ‘Timeline’ tab). For these components, the number of participants is written as 'n = x1 of x2'. The first number is the number of people who completed everything that was collected as part of that component. The second number is the number of people who completed any part of what was collected in that component."),
                          p("For
                          instance, the Primary Questionnaire at Baseline 
                          consisted of 9 standardized questionnaires and
                          several other sets of questions that
                          covered various topics (meditation experience,
                          experiences during COVID, etc.). There were 389 
                          people who completed some portion of the Primary
                          Questionnaire at Baseline. Of those 389 people, 355
                          completed all sections.")
                    )
                )
                ),
                
                card(
                    full_screen = TRUE,
                    card_header(
                        "Flowchart", popover(trigger = bs_icon("info-circle", title = "Flowchart info"), p("Click on the 'expand' button that appears on the bottom right hand corner to make the flowchart larger."))
                    ),
                    card_body(
                        grVizOutput(ns("participants_full_flowchart"))
                    )
                )
               
            ) # close flowchart tab
            
    ) # close tabs

    ) # close tagList
    
}


about_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        
        ## Build components ----
        
        ### Accordion panels ----
        
        #### Aim ----
        
        aim_text <- "To understand if and how people used meditation and other contemplative practices to cope with stressors during the COVID-19 pandemic, and whether this practice was related to better mental and physical health outcomes.<br><br>The pandemic's disparate impacts on communities cannot be ignored. Thus, our research also aims to understand how these inequities may have influenced the consequences of the pandemic and the role of contemplative practices for different communities as they navigated the unfolding circumstances of the pandemic."
            
        output$aim_accordion_text <- renderUI({
            HTML(aim_text)
        })
        
        
        #### Study design ----
        
        output$study_design_accordion_fill <- renderUI({
            
            tagList(
                
                # layout_columns(
                #     fill = FALSE,
                #     value_box(
                #         title = "Study duration",
                #         value = "1 year",
                #         showcase = bs_icon("calendar-range"),
                #         theme_color = "success"
                #     ),
                #     value_box(
                #         title = "Number of data collection time points",
                #         value = "4",
                #         showcase = bs_icon("calendar-event"),
                #         theme_color = "success"
                #     )
                # ),
                # 
                p("This was a year-long longitudinal study with data collected at 4 time points. We assessed changes in life stressors, coping mechanisms, psychological well-being, and telomere length—a biological marker of cellular aging—over a one year period. We also measured prior life challenges and adversities to better understand the participants' experiences within the context of their lives."),
                p("Questionnaire data was collected every 4 months, and blood samples were collected for telomere measurements at the baseline and 1-year follow-up assessments."),
                
                layout_column_wrap(
                    fill = FALSE,
                    width = "200px",
                    value_box(
                        title = "Stressors, coping, well-being, adversity",
                        # value = "4 sets of questionnaires",
                        # showcase = bs_icon("check-square"),
                        value = HTML("<span style='font-size: calc(.9rem + .9vw);'>Questionnaires</span>"),
                        showcase = bs_icon("check-square", size = ".8em"),
                        theme_color = "success"
                    ),
                    value_box(
                        title = "Telomere length",
                        value = "2 blood samples",
                        showcase = bs_icon("clipboard2-pulse", size = ".8em"),
                        theme_color = "success"
                    )
                ),
            )
            
        })
        
        #### Recruitment ----
        recruitment_text <- "Participants were recruited via web-based announcements (emails, social media posts, etc.) by meditation teachers, meditation centers, and meditation research groups from across the US.<br><br>Interested individuals completed a screening form to determine their eligibility and provide demographic information to maximize the diversity of the study cohort."
        
        output$recruitment_accordion_text <- renderUI({
            HTML(recruitment_text)
        })
        
        
        #### Eligibility ----
        output$eligibility_accordion_fill <- renderUI({
          tagList(
              
              layout_column_wrap(
                  fill = FALSE,
                  width = "200px",
                  
                  value_box(
                      title = "Age",
                      #value = "18+ years",
                      value = HTML("<span style='font-size: calc(.9rem + .9vw);'>18+ years</span>"),
                      showcase = bs_icon("person-vcard", size = ".8em"),
                      max_height = "150px",
                      theme_color = "success"
                  ),
                  value_box(
                      title = "Location",
                      #value = "US",
                      value = HTML("<span style='font-size: calc(.9rem + .9vw);'>US</span>"),
                      showcase = bs_icon("geo-alt", size = ".8em"),
                      max_height = "150px",
                      theme_color = "success"
                  ),
                  value_box(
                      title = "Meditation",
                      #value = HTML("<span style='font-size: 20px;'>Some previous experience</span>"),
                      value = HTML("<span style='font-size: calc(.8rem + .9vw);'>Some experience</span>"),
                      showcase = bs_icon("clock-history", size = ".8em"),
                      max_height = "150px",
                      theme_color = "success"
                  )
              ),
              
              p("Participants had to be 18 years or older, currently residing in the United States, and have some previous meditation experience."),

              p("Meditators were included if they currently or previously had a semi-regular or regular home practice, practiced using a meditation app, had taken a meditation class or course, attended a meditation retreat, or practiced meditation in another format.")
              
          )
        })
        
        
        #### Enrollment ----
        output$enrollment_accordion_fill <- renderUI({
            tagList(

                layout_column_wrap(
                    fill = FALSE,
                    width = "200px",
                    value_box(
                        title = "Enrollment began",
                        value = "June 2020",
                        showcase = bs_icon("calendar-check"),
                        theme_color = "success"
                    ),
                    value_box(
                        title = "Enrollment ended",
                        value = "December 2020",
                        showcase = bs_icon("calendar-x"),
                        theme_color = "success"
                    )
                ),
                
                p("People enrolled in the study on a rolling basis between June and December of 2020.")
                
            )
        })
        
        #### Data collection ----
        data_collection_text <- "This was a year-long longitudinal study with data collected at 4 time points.<br><br>Questionnaire data was collected every 4 months and blood samples were collected for telomere measurements at the baseline and 1-year follow-up assessments.<br><br>People enrolled in the study on a rolling basis between June and December of 2020.<br><br>Data collection took place between June of 2020 and February of 2022."
        
        output$data_collection_accordion_text <- renderUI({
            HTML(data_collection_text)
        })
        
        
        output$data_collection_accordion_fill <- renderUI({
            tagList(
              
                layout_column_wrap(
                    fill = FALSE,
                    width = "200px",
                    
                    value_box(
                        title = "Data collection began",
                        value = "June 2020",
                        showcase = bs_icon("calendar-check-fill"),
                        theme_color = "success"
                    ),
                    value_box(
                        title = "Data collection ended",
                        value = "February 2022",
                        showcase = bs_icon("calendar-x-fill"),
                        theme_color = "success"
                    )
                ),
                
                p("Data collection took place between June of 2020 and February of 2022."),

            )
        })
        
        
        
        ### Timeline ----
        
        output$timeline_by_timepoint <- renderPlotly({
            
            g <- vistime(timeline_dat,
                         linewidth = 25)

            g <- plotly::config(g, displaylogo = FALSE,
                                modeBarButtonsToRemove = c('lasso2d', 'select2d', 'hoverClosestCartesian', 'hoverCompareCartesian')) |>
                plotly::layout(margin = list(l = 1, r = 1, b = 1, t = 25))
            
            # add a bit of space at beginning and end of x-axis
            g <- plotly::layout(g, xaxis = list(
                range = c(min(timeline_dat$start) - 7,
                          max(timeline_dat$end) + 7)))

            g
            
            })
        
 

        ### Self-report measures ----
        
        output$primary_questions <- renderUI({
            tagList(
                
                card(
                    style = "background-color: #91d6a2;",
                    
                    h5(strong("Primary Questionnaires"), align = "center"),
                    p(strong("Covid Questions")),
                    tags$ul(
                        tags$li("Exposure & concern"),
                        tags$li("Disruption/changes"),
                        tags$li("Stress coping")),
                    
                    p(strong("Meditation Experience")),
                    tags$ul(
                        tags$li("Amounts of practice"),
                        tags$li("Types of practice"),
                        tags$li("Motivations for practice"),
                        tags$li("Experiences with practice")
                    ),
                    
                    p(strong("Mental Health & Well-being")),
                    tags$ul(
                        tags$li("Perceived stress"),
                        tags$li("Anxiety"),
                        tags$li("Depression"),
                        tags$li("PTSD"),
                        tags$li("Loneliness"),
                        tags$li("Social connectedness"),
                        tags$li("Resilience"),
                        tags$li("Flourishing"),
                        tags$li("Post-traumatic growth")
                    )
                )
            )
        })
        
        
        output$secondary_questions <- renderUI({
            tagList(
                
                card(
                    style = "background-color: #FFB133;",
                    
                    h5(strong("Secondary Questionnaires"), align = "center"),
                    p(strong("Individual Differences Measures")),
                    tags$ul(
                        tags$li("Gratitude"),
                        tags$li("Mindfulness"),
                        tags$li("Interoception"),
                        tags$li("Reappraisal"),
                        tags$li("Rumination/reflection"),
                        tags$li("Distress tolerance"),
                        tags$li("Personality")
                    ),
                    
                    p(strong("Interpersonal Measures")),
                    tags$ul(
                        tags$li("Attachment"),
                        tags$li("Interpersonal emotion regulation"),
                        tags$li("Empathic concern/personal distress")
                    ),
                    
                    p(strong("Adversity")),
                    tags$ul(
                        tags$li("Parental bonding"),
                        tags$li("Childhood trauma"),
                        tags$li("Cumulative adversity"),
                        tags$li("Discrimination"),
                        tags$li("Racism")
                    ),
                )
            )
        })
        
        output$weekly_questions <- renderUI({
            tagList(
                
                card(
                    style = "background-color: #6FE7FF;",
                    
                    h5(strong("Weekly Inventory/Journaling"), align = "center"),
                    p(strong("Weekly Inventory")),
                    tags$ul(
                        tags$li("Emotions"),
                        tags$li("Diet/alcohol"),
                        tags$li("Exercise"),
                        tags$li("Sleep"),
                        tags$li("Self-care")
                    ),
                    
                    p(strong("Journaling Exercise")),
                    tags$ul(
                        tags$li("Top 3 stressors"),
                        tags$li("Top 3 supports"),
                        tags$li("Long stress prompt"),
                        tags$li("Long positive prompt")
                    ),
                )
                )
        })
        
        output$blood_measures <- renderUI({
            tagList(
                
                card(
                    style = "background-color: #C8553D;",
                    
                    h5(strong("Blood Collection"), align = "center"),
                    p("Participants were shipped an at-home blood collection kit that included a lancet to prick their finger and a microtube coated with an EDTA preservative to collect a few drops of blood.")
                )
            )
        })
        
        ## Participant flowcharts -----
#         
#         ### General overview -----
#         output$general_flowchart_overview <- renderGrViz({
# 
#             DiagrammeR::grViz("digraph flowchart {
#                   graph [layout = dot, rankdir = TB]
# 
#                   node [fontname = Helvetica, shape = rectangle]
# 
#                   tab1 [label = '@@1']
#                   tab2 [label = '@@2']
#                   tab3 [label = '@@3']
#                   tab4 [label = '@@4']
#                   tab5 [label = '@@5']
# 
# 
#                   tab1 -> tab2 -> tab3 -> tab4 -> tab5;
#                   }
#                   
#                   [1]: paste0('Number of applicants (n = ', general.overview.counts$n.applied, ')')
#                   [2]: paste0('Number who passed screening (n = ', general.overview.counts$n.screened.in, ')')
#                   [3]: paste0('Number admitted (n = ', general.overview.counts$n.admitted, ')')
#                   [4]: paste0('Number consented (n = ', general.overview.counts$n.consented, ')')
#                   [5]: paste0('Number started the baseline (n = ', general.overview.counts$n.baseline, ')')
#                   "
#             )
# 
#         })
#             
#             
#             ### Participants by stage ----
#         output$participants_by_stage_flowchart <- renderGrViz({
#           
#             DiagrammeR::grViz("digraph flowchart {
#                   graph [layout = dot, rankdir = TB]
# 
#                   node [fontname = Helvetica, shape = rectangle]
# #C8553D
#                   
#                   tab1 [label = '@@1', tooltip = 'Started baseline']
#                   tab2 [label = '@@2', fillcolor = '#172A3A', style = 'filled', fontcolor = 'white', tooltip = 'Baseline']
#                   tab3 [label = '@@3', fillcolor = '#588B8B', style = 'filled', tooltip = 'Primary']
#                   tab4 [label = '@@4', fillcolor = '#FFB133', style = 'filled', tooltip = 'Secondary']
#                   tab5 [label = '@@5', fillcolor = '#172A3A', style = 'filled', fontcolor = 'white', tooltip = '4-month follow-up']
#                   tab6 [label = '@@6', fillcolor = '#588B8B', style = 'filled', tooltip = 'Primary']
#                   tab7 [label = '@@7', fillcolor = '#172A3A', style = 'filled', fontcolor = 'white', tooltip = '8 month follow up']
#                   tab8 [label = '@@8', fillcolor = '#588B8B', style = 'filled', tooltip = 'Primary']
#                   tab9 [label = '@@9', fillcolor = '#6FE7FF', style = 'filled', tooltip = 'Weekly inventory']
#                   tab10 [label = '@@10', fillcolor = '#6FE7FF', style = 'filled', tooltip = 'Weekly inventory']
#                   tab11 [label = '@@11', fillcolor = '#6FE7FF', style = 'filled', tooltip = 'Weekly inventory']
#                   tab12 [label = '@@12', fillcolor = '#6FE7FF', style = 'filled', tooltip = 'Weekly inventory']
#                   tab13 [label = '@@13', fillcolor = '#C8553D', style = 'filled', tooltip = 'Blood sample']
#                   tab14 [label = '@@14', fillcolor = '#172A3A', style = 'filled', fontcolor = 'white', tooltip = '1 year follow up']
#                   tab15 [label = '@@15', fillcolor = '#588B8B', style = 'filled', tooltip = 'Primary']
#                   tab16 [label = '@@16', fillcolor = '#FFB133', style = 'filled', tooltip = 'Secondary']
#                   tab17 [label = '@@17', fillcolor = '#6FE7FF', style = 'filled', tooltip = 'Weekly inventory']
#                   tab18 [label = '@@18', fillcolor = '#C8553D', style = 'filled', tooltip = 'Blood sample']
#                   tab19 [label = '@@19', fillcolor = '#6FE7FF', style = 'filled', tooltip = 'Weekly inventory']
#                   tab20 [label = '@@20', fillcolor = '#6FE7FF', style = 'filled', tooltip = 'Weekly inventory']
#                   
#                   
#                   
#                   tab1 -> tab2;
#                   tab2 -> tab3 -> tab4;
#                   tab1 -> tab5 -> tab6 -> tab19;
#                   tab1 -> tab7 -> tab8 -> tab20;
#                   tab2 -> tab9 -> tab10 -> tab11 -> tab12;
#                   tab2 -> tab13;
#                   tab1 -> tab14;
#                   tab14 -> tab15 -> tab16;
#                   tab14 -> tab17;
#                   tab14 -> tab18;
# }
#                   
#                   
#                   
#                   [1]: paste0('Started the baseline \\n(n = ', general.overview.counts$n.baseline, ')')
#                   [2]: paste0('Baseline')
#                   [3]: paste0('Primary \\n(n = ', n.stage.count[n.stage.count$stage == 'primary.progress_t1', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'primary.progress_t1', 'n.any'], ')')
#                   [4]: paste0('Secondary \\n(n = ', n.stage.count[n.stage.count$stage == 'secondary.progress_t1', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'secondary.progress_t1', 'n.any'], ')')
#                   [5]: paste0('4-month follow-up')
#                   [6]: paste0('Primary \\n(n = ', n.stage.count[n.stage.count$stage == 'x4m.progress_t2', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'x4m.progress_t2', 'n.any'], ')')
#                   [7]: paste0('8-month follow-up')
#                   [8]: paste0('Primary \\n(n = ', n.stage.count[n.stage.count$stage == 'x8m.progress_t3', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'x8m.progress_t3', 'n.any'], ')')
#                   [9]: paste0('Week 1 \\n(n = ', n.stage.count[n.stage.count$stage == 'weekly.1.progress_t1', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'weekly.1.progress_t1', 'n.any'], ')')
#                   [10]: paste0('Week 2 \\n(n = ', n.stage.count[n.stage.count$stage == 'weekly.2.progress_t1', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'weekly.2.progress_t1', 'n.any'], ')')
#                   [11]: paste0('Week 3 \\n(n = ', n.stage.count[n.stage.count$stage == 'weekly.3.progress_t1', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'weekly.3.progress_t1', 'n.any'],')')
#                   [12]: paste0('Week 4 \\n(n = ', n.stage.count[n.stage.count$stage == 'weekly.4.progress_t1', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'weekly.4.progress_t1', 'n.any'], ')')
#                   [13]: paste0('Sample \\n(n = ', n.stage.count[n.stage.count$stage == 'sample.status_xt1', 'n.any'], ')')
#                   [14]: paste0('1-year follow-up')
#                   [15]: paste0('Primary \\n(n = ', n.stage.count[n.stage.count$stage == 'primary.progress_t4', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'primary.progress_t4', 'n.any'],')')
#                   [16]: paste0('Secondary \\n(n = ', n.stage.count[n.stage.count$stage == 'secondary.progress_t4', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'secondary.progress_t4', 'n.any'], ')')
#                   [17]: paste0('Week 1 \\n(n = ', n.stage.count[n.stage.count$stage == 'weekly.progress_t4', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'weekly.progress_t4', 'n.any'], ')')
#                   [18]: paste0('Sample \\n(n = ', n.stage.count[n.stage.count$stage == 'sample.status_xt4', 'n.any'], ')')
#                   [19]: paste0('Weekly \\n(n = ', n.stage.count[n.stage.count$stage == 'x4m.weekly.progress_t2', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'x4m.weekly.progress_t2', 'n.any'], ')')
#                   [20]: paste0('Weekly \\n(n = ', n.stage.count[n.stage.count$stage == 'weekly.progress_t4', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'weekly.progress_t4', 'n.any'], ')')
#                   ")
#             
#               
#             
#         })
       
        
        ### All participants flowchart ----
        output$participants_full_flowchart <- renderGrViz({
            
            DiagrammeR::grViz("digraph flowchart {
                  graph [layout = dot, rankdir = TB]

                  node [fontname = Helvetica, shape = rectangle]
#C8553D
                  tab1 [label = '@@1', tooltip = 'Applicants']
                  tab2 [label = '@@2', tooltip = 'Passed screening']
                  tab3 [label = '@@3', tooltip = 'Admitted']
                  tab4 [label = '@@4', tooltip = 'Consented']
                  tab5 [label = '@@5', tooltip = 'Started baseline']
                  tab6 [label = '@@6', fillcolor = '#172A3A', style = 'filled', fontcolor = 'white', tooltip = 'Baseline']
                  tab7 [label = '@@7', fillcolor = '#91d6a2', style = 'filled', tooltip = 'Primary'] 
                  tab8 [label = '@@8', fillcolor = '#FFB133', style = 'filled', tooltip = 'Secondary']
                  tab9 [label = '@@9', fillcolor = '#172A3A', style = 'filled', fontcolor = 'white', tooltip = '4-month follow-up']
                  tab10 [label = '@@10', fillcolor = '#91d6a2', style = 'filled', tooltip = 'Primary']
                  tab11 [label = '@@11', fillcolor = '#172A3A', style = 'filled', fontcolor = 'white', tooltip = '8 month follow up']
                  tab12 [label = '@@12', fillcolor = '#91d6a2', style = 'filled', tooltip = 'Primary']
                  tab13 [label = '@@13', fillcolor = '#6FE7FF', style = 'filled', tooltip = 'Weekly inventory']
                  tab14 [label = '@@14', fillcolor = '#6FE7FF', style = 'filled', tooltip = 'Weekly inventory']
                  tab15 [label = '@@15', fillcolor = '#6FE7FF', style = 'filled', tooltip = 'Weekly inventory']
                  tab16 [label = '@@16', fillcolor = '#6FE7FF', style = 'filled', tooltip = 'Weekly inventory']
                  tab17 [label = '@@17', fillcolor = '#C8553D', style = 'filled', tooltip = 'Blood sample']
                  tab18 [label = '@@18', fillcolor = '#172A3A', style = 'filled', fontcolor = 'white', tooltip = '1 year follow up']
                  tab19 [label = '@@19', fillcolor = '#91d6a2', style = 'filled', tooltip = 'Primary']
                  tab20 [label = '@@20', fillcolor = '#FFB133', style = 'filled', tooltip = 'Secondary']
                  tab21 [label = '@@21', fillcolor = '#6FE7FF', style = 'filled', tooltip = 'Weekly inventory']
                  tab22 [label = '@@22', fillcolor = '#C8553D', style = 'filled', tooltip = 'Blood sample']
                  tab23 [label = '@@23', fillcolor = '#6FE7FF', style = 'filled', tooltip = 'Weekly inventory']
                  tab24 [label = '@@24', fillcolor = '#6FE7FF', style = 'filled', tooltip = 'Weekly inventory']
                  
                  tab1 -> tab2 -> tab3 -> tab4 -> tab5;
                  tab5 -> tab6;
                  tab6 -> tab7 -> tab8;
                  tab5 -> tab9 -> tab10 -> tab23;
                  tab5 -> tab11 -> tab12 -> tab24;
                  tab6 -> tab13 -> tab14 -> tab15 -> tab16;
                  tab6 -> tab17;
                  tab5 -> tab18;
                  tab18 -> tab19 -> tab20;
                  tab18 -> tab21;
                  tab18 -> tab22;
            }

                  [1]: paste0('Applicants \\n(n = ', general.overview.counts$n.applied, ')')
                  [2]: paste0('Passed screening \\n(n = ', general.overview.counts$n.screened.in, ')')
                  [3]: paste0('Admitted \\n(n = ', general.overview.counts$n.admitted, ')')
                  [4]: paste0('Consented \\n(n = ', general.overview.counts$n.consented, ')')
                  [5]: paste0('Started the baseline \\n(n = ', general.overview.counts$n.baseline, ')')
                  [6]: paste0('Baseline')
                  [7]: paste0('Primary \\n(n = ', n.stage.count[n.stage.count$stage == 'primary.progress_t1', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'primary.progress_t1', 'n.any'], ')')
                  [8]: paste0('Secondary \\n(n = ', n.stage.count[n.stage.count$stage == 'secondary.progress_t1', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'secondary.progress_t1', 'n.any'], ')')
                  [9]: paste0('4-month follow-up')
                  [10]: paste0('Primary \\n(n = ', n.stage.count[n.stage.count$stage == 'x4m.progress_t2', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'x4m.progress_t2', 'n.any'], ')')
                  [11]: paste0('8-month follow-up')
                  [12]: paste0('Primary \\n(n = ', n.stage.count[n.stage.count$stage == 'x8m.progress_t3', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'x8m.progress_t3', 'n.any'], ')')
                  [13]: paste0('Week 1 \\n(n = ', n.stage.count[n.stage.count$stage == 'weekly.1.progress_t1', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'weekly.1.progress_t1', 'n.any'], ')')
                  [14]: paste0('Week 2 \\n(n = ', n.stage.count[n.stage.count$stage == 'weekly.2.progress_t1', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'weekly.2.progress_t1', 'n.any'], ')')
                  [15]: paste0('Week 3 \\n(n = ', n.stage.count[n.stage.count$stage == 'weekly.3.progress_t1', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'weekly.3.progress_t1', 'n.any'],')')
                  [16]: paste0('Week 4 \\n(n = ', n.stage.count[n.stage.count$stage == 'weekly.4.progress_t1', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'weekly.4.progress_t1', 'n.any'], ')')
                  [17]: paste0('Sample \\n(n = ', n.stage.count[n.stage.count$stage == 'sample.status_xt1', 'n.any'], ')')
                  [18]: paste0('1-year follow-up')
                  [19]: paste0('Primary \\n(n = ', n.stage.count[n.stage.count$stage == 'primary.progress_t4', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'primary.progress_t4', 'n.any'],')')
                  [20]: paste0('Secondary \\n(n = ', n.stage.count[n.stage.count$stage == 'secondary.progress_t4', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'secondary.progress_t4', 'n.any'], ')')
                  [21]: paste0('Weekly \\n(n = ', n.stage.count[n.stage.count$stage == 'weekly.progress_t4', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'weekly.progress_t4', 'n.any'], ')')
                  [22]: paste0('Sample \\n(n = ', n.stage.count[n.stage.count$stage == 'sample.status_xt4', 'n.any'], ')')
                  [23]: paste0('Weekly \\n(n = ', n.stage.count[n.stage.count$stage == 'x4m.weekly.progress_t2', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'x4m.weekly.progress_t2', 'n.any'], ')')
                  [24]: paste0('Weekly \\n(n = ', n.stage.count[n.stage.count$stage == 'weekly.progress_t4', 'n.100'], ' of ', n.stage.count[n.stage.count$stage == 'weekly.progress_t4', 'n.any'], ')')
                  ")

            
        })
        
        
        
        
        
        
        
        
    }
    )
}