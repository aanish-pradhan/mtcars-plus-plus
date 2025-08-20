# ------------------------------------------------------------------------------
# File: app_server.R
# Author: Aanish Pradhan
# Version: 2025-08-20
# Description: Top-level server logic for mtcars++ dashboard.
#              Connects and initializes all module server components.
# ------------------------------------------------------------------------------

# LOAD MODULES
source("src/Modules/mod_home.R", local = TRUE)
source("src/Modules/mod_car_explorer.R", local = TRUE)
source("src/Modules/mod_eda.R", local = TRUE)
source("src/Modules/mod_faq.R", local = TRUE)

#' Initializes server-side logic for the dashboard application.
#' 
#' This function connects and runs all server modules corresponding to each 
#' dashboard page (e.g., Home, Car Explorer, EDA, FAQ, etc.). It acts as the 
#' central coordinator for all reactive behavior in the app.
#' 
#' @param input Shiny input object passed automatically by `shiny::shinyApp()`
#' @param output Shiny output object passed automatically by `shiny::shinyApp()`
#' @param session Shiny session object passed automatically by `shiny::shinyApp()`
#' 
#' @seealso [app_ui()] for top-level UI definition
app_server <- function(input, output, session) {
	
	mod_home_server("home")
	
	selected_ids <- mod_car_explorer_server("car_explorer")
	
	mod_eda_server("eda", selected_idx = selected_ids)

	mod_faq_server("faq")

	# source(
	# 	"src/Server/03_exploratory_data_analysis.R",
	# 	local = TRUE,         # <- gives the file access to input/output/reactives
	# 	chdir = TRUE
	# )$value
}
