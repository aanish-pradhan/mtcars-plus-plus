# ------------------------------------------------------------------------------
# File: app.R
# Author: Aanish Pradhan
# Version: 2025-08-20
# Description: Entry point for the mtcars++ Shiny dashboard. Loads global 
#     settings, UI layout, and server logic.
# ------------------------------------------------------------------------------

source("src/global.R")
source("src/app_ui.R")     # Defines: app_ui <- function(request) { ... }
source("src/app_server.R") # Defines: app_server <- function(input, output, session) { ... }

shinyApp(ui = app_ui, server = app_server)
