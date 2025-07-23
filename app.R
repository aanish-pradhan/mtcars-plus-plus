library(bs4Dash)
library(ggplot2)
library(shiny)

shiny::shinyApp(
	ui = bs4Dash::dashboardPage(
		title = "mtcars++",
		header = bs4Dash::dashboardHeader(title = "mtcars++"), # Mandatory
		sidebar = bs4Dash::dashboardSidebar( # Mandatory
			collapsed = FALSE,
			skin = "dark",
			sidebarUserPanel(name = "Pages"),
			sidebarMenu(
				menuItem(
					"Home",
					tabName = "item1",
					icon = icon("home")
				),
				menuItem(
					"Data Explorer",
					tabName = "item2",
					icon = icon("table")
				),
				menuItem(
					"Exploratory Data Analysis",
					tabName = "item3",
					icon = icon("magnifying-glass")
				)
			)
		),
		controlbar = dashboardControlbar(),
		footer = dashboardFooter(),
		body = dashboardBody( # Mandatory
			box(title = "About mtcars++", 
				"mtcars++, inspired by the mtcars dataset from the 1974 Motor 
				Trend U.S. magazine, analyzes over 3,300 cars from The Car 
				Connection Dataset containing cars that are currently, or 
				previously, available for sale on the U.S. market.", 
				collapsible = FALSE, 
				width = ),
			plotOutput("volcano")
		)		
	),
	server = function(input, output) {
		output$volcano <- renderPlot(
			image(volcano)
		)
		
		
	}
)




# shinyApp(
# 	# ui = dashboardPage(
# 	# 	scrollToTop = TRUE,
# 	# 	header = dashboardHeader(),
# 	# 	sidebar = dashboardSidebar(minified = TRUE, collapsed = FALSE),
# 	# 	body = dashboardBody(
# 	# 		"Hello"
# 	# 	),
# 	# 	controlbar = dashboardControlbar(),
# 	# 	title = "Title"
# 	# ),
# 	ui = bs4Dash::dashboardPage(
# 		header = bs4Dash::dashboardHeader(title = "mtcars++"),
# 		sidebar = bs4Dash::dashboardSidebar(
# 			sidebarHeader("sidebar1"),
# 			sidebarHeader("sidebar2"),
# 		),
# 		body = bs4Dash::dashboardBody(
# 			shiny::plotOutput("plot")
# 		)
# 	),
# 	server = function(input, output) {
# 		output$plot <- shiny::renderPlot(
# 			ggplot(mtcars, aes(hp, mpg, color = as.factor(cyl))) + 
# 				geom_point() + 
# 				labs(title = "MPG versus HP", 
# 					 x = "HP", 
# 					 y = "MPG", 
# 					 color = "Cylinders")
# 		)
# 	}
# )
