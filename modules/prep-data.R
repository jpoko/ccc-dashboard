# PREP DATA ----

## Preparation needed for data to be used various places in the app


# LOAD DATA ----

# main data, long
dat_subscale <- readRDS("./data/app.data.long.rds")

# main data, wide
dat_mini_subscale <- readRDS("./data/app.mini.data.rds")

# covid cases/deaths data
covid_cases_deaths_dat <- readRDS("./data/app.covid.cases.deaths.rds")

# demographic alluvial flow data
load("./data/app.demographicCategoriesFlow.data.RData")

# stai, pss alluvial flow data
#alluvial_dat <- load("./data/app.alluvial.data.RData")

# participant zipcode data
zipcode_dat <- readRDS("./data/app.zipcode.data.rds")

# variable information 
load("./data/app.variable.info.data.RData")

# study events timeline data
timeline_dat <- readRDS("./data/app.timeline.by.timepoint.data.rds")

# data for participant flowcharts
load("./data/app.participant.flowchart.data.RData")


#to test code:
# input <- list(demo_treemaps_var = "Gender",
#               timepoint_var = "T4",
#               scale_complete = "All",
#               fit = TRUE,
#               demographic_var = "gender",
#               corr_matrix_sig_p_val = 0.05,
#               corr_matrix_type = "meditation variables",
#               scale_type_var_single = "stai",
#               demo_category_explain = "ethnicity",
#               demo_category = "Ethnicity",
#               scores_by_date_type = "avg_score",
#               covid_cases_deaths = "cases",
#               sankey_diagram_stage = "applied",
#               x_var_scatter = "sc",
#               y_var_scatter = "scs",
#               #corr_vars = c("stai", "dts", "meditation.years", "mins.per.sit", "mins.per.week.practice", "days.week.practice"),
#               corr_vars = c("dts", "chronicity.discrimination"),
#               # next 2 options must match - from variables.xlsx sheet
#               scale_main_group_single = "anxiety (STAI)",
#               scale_sub_group_single = "total",
#               corset_plot_tmpts = "T1 vs T4"
# )



# GENERAL ----


## VARIABLES ----

# pass 'score' or 'measure' as variable to use in plots (scale scores)
score_option <- "score"
measure_option <- "value"



## COLORS ----

### general colors ----
# circle question mark icon
info_icon_color = "#cc0000"   # red color

# correlation matrices
heat_col <- colorRampPalette(c("#482576", "#287d8e","#FFFFFF", "#95d840","#fde725"))  

# color of difference histograms
histo_color <- '#277f8e'

mean_color <- '#ff9966'


### alluvials ----

pss_alluvial_dat <- dat_mini_subscale |>
    dplyr::select(starts_with("pss.level."))

stai_alluvial_dat <- dat_mini_subscale |>
    dplyr::select(starts_with("stai.level."))


alluvial_colors <- c(
    "#1C1CFF", # blue, low
    "#ea5f94", # pink, moderate
    "#ffd700", # yellow, high
    "gray"
)





# DEMOGRAPHIC PAGE ----

## TREEMAPS ----

# function to create treemaps for all demographic categories

plot_treemap_demo <- function(data, var_visualize) {
    labels <- count(data, !!sym(var_visualize))[[1]]
    values <- count(data, !!sym(var_visualize))[[2]]
    num_rows <- nrow(count(data, !!sym(var_visualize)))
    
    plot_ly(
        type = "treemap",
        labels = labels %>% stringr::str_wrap(width = 15),
        values = values,
        parents = rep("", num_rows),
        marker = list(
            colors = viridis(num_rows),
            line = list(color = "#000000", width = 1)
        ),
        textposition = "inside",
        textinfo = "label",
        hoverinfo = "text",
        text = ~paste("n=", values)
    ) |>
        plotly::config(displayModeBar = FALSE) |>
        plotly::layout(margin = list(t = 0, b = 0, l = 0, r = 0))
    
}



# INDIVIDUAL SCALES PAGE ----

### Variable information ----
## from variables.xlsx sheet

# Used in dropdowns for individual scales page for main dropdown of concept/scale and 2nd dropdown of total or subscale 
## (scale.main.group, scale.sub.group)

individual_scale_variables <- rbind(
    variable_info_scales,
    variable_info_adversity)

# put in alphabetical order according to scale concept name and subscale name
individual_scale_variables <- individual_scale_variables[order(individual_scale_variables$scale.main.group, individual_scale_variables$scale.sub.group),]



### Variables for diff tmpts ----
# create list of variables that have 1 tmpt, 2 tmpts, or 4 tmpts
# Used in individual scales - what time points each variable was given
# computes 'tmpt_pattern': tmpt_t1, tmpt_t4, tmpt_all, tmpt_pre_post

# count number of time points each scale was administered 
individual_scale_variables$count_tmps <- rowSums(!is.na(individual_scale_variables[, c("T1", "T2", "T3", "T4")]))

# get unique count_tmps values
unique_tmpts_counts <- unique(individual_scale_variables$count_tmps)

# initialize an empty list
tmpt_count_vars <- list()

# loop through unique count_tmps values
for (count_value in unique_tmpts_counts) {
    # get variable names for this unique_tmpts_counts value
    vars_for_count <- individual_scale_variables$variable.name[individual_scale_variables$count_tmps == count_value]
    
    # add to the tmpt_count_vars list
    tmpt_count_vars[[as.character(count_value)]] <- vars_for_count
}

# function to get the pattern of x's of the time points for each variable
get_pattern <- function(row) {
    if (sum(!is.na(row[c("T1", "T2", "T3", "T4")])) == 1 && !is.na(row["T1"])) {
        return("tmpt_t1")
    } else if (sum(!is.na(row[c("T1", "T2", "T3", "T4")])) == 1 && !is.na(row["T4"])) {
        return("tmpt_t4")
    } else if (sum(!is.na(row[c("T1", "T2", "T3", "T4")])) == 4) {
        return("tmpt_all")
    } else if (!is.na(row["T1"]) && !is.na(row["T4"]) && sum(!is.na(row[c("T2", "T3")])) == 0) {
        return("tmpt_pre_post")
    } else {
        return("Unknown Pattern")
    }
}

# apply the function to each row and create a new column (tmpt_pattern)
individual_scale_variables$tmpt_pattern <- apply(individual_scale_variables, 1, get_pattern)



## CORSET PLOT DATA ----

# functions to calculate changes between tmpts if have 4 tmpts or 2 tmpts
# use tmpt_pattern info (individual_scale_variables$tmpt_pattern) to decide what function to run


# For scales with 4 timepoints
calculate_changes_4tmpts_subscale <- function(list_scales) {
    
    for (i in seq_along(list_scales)) {
        
        ## Time t1 vs t2
        
        # name for change column 
        col_name <- paste(list_scales[i], "change", "t1", "t2", sep = ".")
        
        dat_mini_subscale <- dat_mini_subscale |>
            mutate({{col_name}} := !!sym(paste0(list_scales[i], "_total_t2")) - !!sym(paste0(list_scales[i], "_total_t1")))
        
        # name for direction column
        direction_name <- paste(list_scales[i], "direction", "t1", "t2", sep = ".")
        
        dat_mini_subscale[[direction_name]] <- ifelse(
            dat_mini_subscale[[col_name]] < 0, "Decrease",
            ifelse(dat_mini_subscale[[col_name]] > 0, "Increase",
                   "No change"))
        
        colnames(dat_mini_subscale)[which(colnames(dat_mini_subscale) == direction_name)] <-
            direction_name
        
        
        ## Time t2 vs t3
        
        # name for change column 
        col_name <- paste(list_scales[i], "change", "t2", "t3", sep = ".")
        
        dat_mini_subscale <- dat_mini_subscale |>
            mutate({{col_name}} := !!sym(paste0(list_scales[i], "_total_t3")) - !!sym(paste0(list_scales[i], "_total_t2")))
        
        # name for direction column
        direction_name <- paste(list_scales[i], "direction", "t2", "t3", sep = ".")
        
        dat_mini_subscale[[direction_name]] <- ifelse(
            dat_mini_subscale[[col_name]] < 0, "Decrease",
            ifelse(dat_mini_subscale[[col_name]] > 0, "Increase",
                   "No change"))
        
        colnames(dat_mini_subscale)[which(colnames(dat_mini_subscale) == direction_name)] <-
            direction_name
        
        ## Time t3 vs t4
        
        # name for change column 
        col_name <- paste(list_scales[i], "change", "t3", "t4", sep = ".")
        
        dat_mini_subscale <- dat_mini_subscale |>
            mutate({{col_name}} := !!sym(paste0(list_scales[i], "_total_t4")) - !!sym(paste0(list_scales[i], "_total_t3")))
        
        # name for direction column
        direction_name <- paste(list_scales[i], "direction", "t3", "t4", sep = ".")
        
        dat_mini_subscale[[direction_name]] <- ifelse(
            dat_mini_subscale[[col_name]] < 0, "Decrease",
            ifelse(dat_mini_subscale[[col_name]] > 0, "Increase",
                   "No change"))
        
        colnames(dat_mini_subscale)[which(colnames(dat_mini_subscale) == direction_name)] <-
            direction_name
        
        
        ## Time t1 vs t4
        
        # name for change column 
        col_name <- paste(list_scales[i], "change", "t1", "t4", sep = ".")
        
        dat_mini_subscale <- dat_mini_subscale |>
            mutate({{col_name}} := !!sym(paste0(list_scales[i], "_total_t4")) - !!sym(paste0(list_scales[i], "_total_t1")))
        
        # name for direction column
        direction_name <- paste(list_scales[i], "direction", "t1", "t4", sep = ".")
        
        dat_mini_subscale[[direction_name]] <- ifelse(
            dat_mini_subscale[[col_name]] < 0, "Decrease",
            ifelse(dat_mini_subscale[[col_name]] > 0, "Increase",
                   "No change"))
        
        colnames(dat_mini_subscale)[which(colnames(dat_mini_subscale) == direction_name)] <-
            direction_name
    }
    return(dat_mini_subscale)
}



## For scales with 2 time points
calculate_changes_2tmpts_subscale <- function(list_scales) {
    
    for (i in seq_along(list_scales)) {
        
        ## Time t1 vs t4
        
        # name for change column 
        col_name <- paste(list_scales[i], "change", "t1", "t4", sep = ".")
        
        dat_mini_subscale <- dat_mini_subscale |>
            mutate({{col_name}} := !!sym(paste0(list_scales[i], "_total_t4")) - !!sym(paste0(list_scales[i], "_total_t1")))
        
        # name for direction column
        direction_name <- paste(list_scales[i], "direction", "t1", "t4", sep = ".")
        
        dat_mini_subscale[[direction_name]] <- ifelse(
            dat_mini_subscale[[col_name]] < 0, "Decrease",
            ifelse(dat_mini_subscale[[col_name]] > 0, "Increase",
                   "No change")
        )
        
        colnames(dat_mini_subscale)[which(colnames(dat_mini_subscale) == direction_name)] <-
            direction_name
    }
    return(dat_mini_subscale)
}


# Run appropriate function for each scale: 


# get list of variable names that have all 4 time points
list_scales_all <- individual_scale_variables |>
    dplyr::filter(tmpt_pattern == "tmpt_all") |>
    dplyr::pull(variable.name)

# run 4 tmpt function to get list of variables with 4 time points
dat_mini_subscale <- calculate_changes_4tmpts_subscale(list_scales_all)



# get list of variable names that have 2 (pre & post) time points
list_scales_two <- individual_scale_variables |>
    dplyr::filter(tmpt_pattern == "tmpt_pre_post") |>
    dplyr::pull(variable.name)

# run 2 tmpt function to get list of variables with 2 time points
dat_mini_subscale <- calculate_changes_2tmpts_subscale(list_scales_two)



# get list of variable names that have only 1 time point
list_scales_one_tmpt <- individual_scale_variables |>
    dplyr::filter(tmpt_pattern %in% c("tmpt_t1", "tmpt_t4"))




# CORRELATION MATRICES PAGE -----

## Get data ----

# get data for correlation matrices - contains "_total_", meditation variables
# NOTE: select here any of the variables that should be in the matrix

dat_corr <- dat_mini_subscale |>
    dplyr::select(
        contains("_total_"),
        contains("meditation.years"),
        contains("days.week.practice"),
        contains("mins.per.sit"),
        contains("mins.per.week.practice")
    )


# Create correlation matrices ----
## Use corrplot package - pass correlation and p-value matrices, sig level, ordering info 

correlation_heatmap <- function(corr_time, p_mat_time, sig_level, order_method, title_text, ...) {
    corrplot(
        corr_time,
        method = "color",
        col = heat_col(200),
        addCoef.col = "black",
        tl.col = "black",
        tl.srt = 45,
        tl.cex = 1,
        p.mat = p_mat_time,
        sig.level = sig_level,
        insig = "blank",
        diag = FALSE,
        order = order_method,
        title = title_text,
        mar = c(0, 0, 1, 0),
        cl.ratio = .3,
        cl.cex = 1
    )
}




# SCATTER PLOT PAGE ----

## PASS VARIABLES ----

### Time point labels ----
# Time point labels (used in scatter plot tab text updating what time point is selected)

timepoint_label <- c(
    "baseline"          = "T1",
    "4-month follow-up" = "T2",
    "8-month follow-up" = "T3",
    "1-year follow-up"  = "T4") 



### Variable information ----
## from variables.xlsx sheet

# Used in dropdown labels & variables for x and y var inputs & correlation matrices
# page to select variables
## (dropdown.description, variable.name, grouping_var)

# scale_variables <- variable_info_scales |>
#     as.data.frame() |>
#     mutate(grouping_var = "scale")
# 
# adversity_variables <- variable_info_adversity |>
#     as.data.frame() |>
#     mutate(grouping_var = "adversity")




meditation_variables <- variable_info_meditation |>
    as.data.frame() |>
    mutate(grouping_var = "meditation")

# put scale, adversity, and meditation variables together, grouped by scale type
# variable_information_grouped <- rbind(
#     scale_variables,
#     adversity_variables,
#     meditation_variables
# )

# use individual_scale_variables and bind with meditation_variables

scale_variables <- individual_scale_variables |>
    dplyr::select(
        -count_tmps,
        -tmpt_pattern
    ) |>
    mutate(grouping_var = "scale")

# put scale and meditation variables together, grouped by scale type
variable_information_grouped <- rbind(
    meditation_variables,
    scale_variables
)





### Variables at each time point ----
# create list of variables available at each time point
# used in scatter plot page

# the time points
time_points <- c("T1", "T2", "T3", "T4")

# initialize empty list
available_vars <- list()

# loop through each time point
for (time_point in time_points) {
    
    # get variables available at this time point
    vars_available <- variable_information_grouped[!is.na(variable_information_grouped[[time_point]]), "variable.name"]
    
    # add to the available_vars list
    available_vars[[time_point]] <- vars_available
}


















