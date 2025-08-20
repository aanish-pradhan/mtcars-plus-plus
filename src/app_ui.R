# ------------------------------------------------------------------------------
# File: app_ui.R
# Author: Aanish Pradhan
# Version: 2025-08-20
# Description: Top-level UI layout function for mtcars++ dashboard.
#              Returns the main bs4Dash page and tab layout.
# ------------------------------------------------------------------------------

# LOAD MODULES
source("src/Modules/mod_home.R", local = TRUE)
source("src/Modules/mod_car_explorer.R", local = TRUE)
source("src/Modules/mod_eda.R", local = TRUE)
source("src/Modules/mod_faq.R", local = TRUE)

#' Constructs top-level UI for the dashboard.
#' 
#' This function defines the full dashboard layout using bs4Dash, including the 
#' header, sidebar, body, and footer. It wraps the UI inside a function to 
#' allow Shiny to pass in the `request` object for dynamic routing, theming, or 
#' language handling if needed.
#' 
#' @param request Internal Shiny parameter for HTTP request handling (Optional)
#' 
#' @return A bs4Dash dashboardPage object representing the UI layout
app_ui <- function(request) {
	bs4Dash::dashboardPage(
		title = "mtcars++",
		header = bs4Dash::dashboardHeader(title = "mtcars++", skin = "dark"),
		sidebar = bs4Dash::dashboardSidebar(
			collapsed = FALSE,
			skin = "dark",
			bs4Dash::sidebarUserPanel(name = "Pages"),
			bs4Dash::sidebarMenu(
				bs4Dash::menuItem("Home", tabName = "home-page", icon = icon("home")),
				bs4Dash::menuItem("Car Explorer", tabName = "car-explorer-page", icon = icon("table")),
				bs4Dash::menuItem("Exploratory Data Analysis", tabName = "eda-page", icon = icon("magnifying-glass")),
				bs4Dash::menuItem("FAQ", tabName = "faq-page", icon = icon("question"))
			)
		),
		controlbar = bs4Dash::dashboardControlbar(skin = "dark"),
		footer = bs4Dash::dashboardFooter(),
		body = bs4Dash::dashboardBody(
			bs4Dash::tabItems(
				mod_home_ui("home"),
				mod_car_explorer_ui("car_explorer"),
				mod_eda_ui("eda"),
				mod_faq_ui("faq")
			)
		)
	)
}
