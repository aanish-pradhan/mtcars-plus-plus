# ------------------------------------------------------------------------------
# File: mod_home.R
# Module: Home Page
# Author: Aanish Pradhan
# Version: 2025-08-20
# Description: UI and (empty) server logic for the Home page of mtcars++
# ------------------------------------------------------------------------------

#' UI function for the Home page module
#' 
#' @param id Unique namespace ID
#' 
#' @return A tabItem containing the home page layout
mod_home_ui <- function (id) {
	ns <- NS(id)
	
	tabItem(
		tabName = "home-page",
		
		# Hero section
		box(
			title = NULL, width = 12, solidHeader = FALSE,
			collapsible = FALSE,
			div(
				style = "width: 100%; display: flex; justify-content: center;",
				tags$img(src = "assets/mtcars++.png", height = "400px")
			),
			HTML("
        <div class='text-center'>
          <h4>Your gateway to interactive car analytics</h4>
          <h5>Developed by: Aanish Pradhan</h5>
        </div>
      ")
		),
		
		# Summary metric value boxes
		fluidRow(
			lapply(seq_along(valuebox_data), function(i) {
				item <- valuebox_data[[i]]
				bg_color <- zissou_colors[i]
				text_color <- "white"
				class_name <- paste0("valuebox-wes-", i)
				
				# Create the value box
				box <- bs4ValueBox(
					value = tags$div(style = paste0("color:", text_color), item$value),
					subtitle = tags$div(style = paste0("color:", text_color), item$subtitle),
					icon = icon(item$icon),
					width = 3,
					color = NULL
				)
				
				# Append class so we can target it via CSS
				styled_box <- tagAppendAttributes(box, class = class_name)
				
				# Add CSS style
				tagList(
					tags$style(HTML(paste0(
						".", class_name, " .small-box { background-color:", bg_color, " !important; }",
						".", class_name, " .inner, .", class_name, " .icon { color:", text_color, " !important; }"
					))),
					styled_box
				)
			})
		),
		
		
		# About box
		box(
			title = HTML("<strong>About</strong>"), width = 12, status = "info", solidHeader = TRUE, collapsible = FALSE,
			HTML("
        <p><strong>mtcars++</strong> is a modern data dashboard inspired by the classic <code>mtcars</code> dataset.</p>
        <p>Built on top of The Car Connection dataset by <a href='https://github.com/nicolas-gervais/predicting-car-price-from-scraped-data' target='_blank'>Nicolas Gervais</a>, it offers insights into over <strong>32,000 cars</strong> and <strong>150+ features</strong> scraped from thecarconnection.com.</p>
        <p>This dashboard is designed to help users, researchers, and employers explore vehicle characteristics, trends, and comparisons across a wide market.</p>")
		),
		
		# Navigation help
		box(
			title = "📚 How to Use This Dashboard", width = 12, status = "secondary", solidHeader = TRUE, collapsible = FALSE,
			HTML("
        <ul>
          <li><strong>Data Explorer:</strong> Browse and search the full dataset of 32,000+ vehicles. Select cars to compare and get similar recommendations.</li>
          <li><strong>Exploratory Data Analysis:</strong> Discover trends and high-level insights from the full dataset, with interactive visualizations and narratives.</li>
          <li><strong>Car Comparison:</strong> (Coming soon) Dive deep into comparisons between selected vehicles and the broader market.</li>
        </ul>
        <p>Use the sidebar to switch between these sections at any time.</p>")
		)
	)	
}

#' mod_home_server
#'
#' Server logic for the Home page module (currently empty).
#'
#' @param id Namespace ID
mod_home_server <- function(id) {
	moduleServer(id, function(input, output, session) {
		# No server logic yet
	})
}
