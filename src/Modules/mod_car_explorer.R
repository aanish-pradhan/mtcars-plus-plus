# ------------------------------------------------------------------------------
# File: mod_car_explorer.R
# Module: Car Explorer
# Author: Aanish Pradhan
# Version: 2025-08-20
# Description: UI and server logic for the Car Explorer page of mtcars++
# ------------------------------------------------------------------------------

#' mod_car_explorer_ui
#'
#' UI for the Car Explorer tab, including full dataset browsing and selected car summary.
#'
#' @param id Unique namespace ID for the module
#' @return A bs4Dash tabItem object
mod_car_explorer_ui <- function(id) {
	ns <- NS(id)
	
	tabItem(
		tabName = "car-explorer-page",
		
		h1("Car Explorer"),
		
		HTML("
      <p>
        Explore the full dataset of over 32,000 cars scraped from 
        <a href=\"https://www.thecarconnection.com\">TheCarConnection</a>.
      </p>
      <p>
        Use the table below to search, sort, and filter the cars you're 
        interested in. You can select the rows for deeper analysis of the cars 
        on the Compare Cars page. mtcars++ also provides recommendations of similar cars to the selected cars.
      </p>
    "),
		
		p(" "),
		
		# Main Dataset Table
		box(
			title = HTML("<strong>The Car Connection Dataset</strong>"),
			width = 12,
			collapsible = FALSE,
			headerBorder = TRUE,
			DT::DTOutput(ns("car_df"))
		),
		
		# Selected Cars Summary
		box(
			title = HTML("<strong>Selected Cars</strong>"),
			width = 12,
			collapsible = TRUE,
			headerBorder = TRUE,
			footer = actionButton(ns("clear_sel"), "Clear Selection"),
			DT::DTOutput(ns("selected_cars"))
		),
		
		# Recommendation Teaser Box
		box(
			title = tagList(icon("lightbulb"), "Learn more about mtcars++'s recommendation system"),
			status = "warning",
			solidHeader = TRUE, 
			width = 12, 
			collapsible = TRUE, 
			collapsed = TRUE
		)
	)
}

#' mod_car_explorer_server
#'
#' Server logic for the Car Explorer module. Handles the car table, selection, and summary.
#'
#' @param id Unique namespace ID for the module
mod_car_explorer_server <- function(id) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		
		# If tcc is global, no need to load it here
		key_col <- if ("car_id" %in% names(tcc)) "car_id" else NULL
		
		# Render main car table
		output$car_df <- DT::renderDT(
			tcc,
			filter = "top",
			rownames = FALSE,
			options = list(scrollX = TRUE)
		)
		
		# Get selected rows
		selected_rows <- reactive({
			idx <- input$car_df_rows_selected
			if (is.null(idx) || length(idx) == 0) return(tcc[0, , drop = FALSE])
			tcc[idx, , drop = FALSE]
		})
		
		# Show summary of selected cars
		output$selected_cars <- DT::renderDT({
			dat <- selected_rows()
			if (nrow(dat) == 0) {
				return(DT::datatable(
					data.frame(Note = "No cars selected yet."),
					rownames = FALSE, options = list(dom = "t")
				))
			}
			keep <- intersect(c(key_col, "year", "make", "model", "style_name", "msrp_usd"), names(dat))
			DT::datatable(
				dat[, if (length(keep)) keep else names(dat), drop = FALSE],
				rownames = FALSE,
				options = list(scrollX = TRUE, pageLength = 10, dom = "tp")
			)
		})
		
		# Clear selection
		observeEvent(input$clear_sel, {
			proxy <- DT::dataTableProxy(ns("car_df"))
			DT::selectRows(proxy, NULL)
		})
		
		# Expose selected IDs (optional, e.g., for future modules)
		selected_ids <- reactive({
			if (is.null(key_col)) return(NULL)
			rows <- input$car_df_rows_selected
			if (is.null(rows) || !length(rows)) return(NULL)
			tcc[[key_col]][rows]
		})
		
		# You can return selected_ids if needed
		# return(list(selected_ids = selected_ids))
	})
}
