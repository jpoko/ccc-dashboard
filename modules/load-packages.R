# LOAD PACKAGES ----

library(tidyverse)
library(lubridate)    # to deal with dates
library(markdown)     # to include markdown files
library(readxl)       # read in excel files
library(shinythemes)
library(leaflet)      # for interactive maps

library(sjlabelled)   # add label to variable names
library(ggeasy)       # add label names to ggplot - not sure if still using

library(plotly)       # interactive plots
library(highcharter)  # interactive plots
library(viridis)      # color palette
library(ggforce)      # sina plots

library(psych)        # correlations with p values
library(ggcorrplot)   # correlation matrices
library(corrplot)     # view correlation matrices as heatmap
library(ppcor)        # partial correlations
library(Hmisc)

library(ggcorset)     # corset plots
library(networkD3)    # sankey diagrams
library(DiagrammeR)   # for flowcharts

library(shinyhelper)  # helper pop-ups - shouldn't be using any more, but check
library(fontawesome)  # icons - not sure if actually using these or just using bsicons - check
library(shinyWidgets) # custom input widgets
library(shinyjs)      # extend shiny
library(bslib)        # shiny bootstrap Sass themes
library(bsicons)      # icons

#library(shinycssloaders)  # spinner

# alluvial plots
library(ggalluvial)
library(alluvial)

library(easyalluvial)
library(parcats)

library(vistime)      # for timeline
library(thematic)     # for plots updating with theme

library(cartography)  # for colors

library(conflicted)   # checks if have conflicting function names
