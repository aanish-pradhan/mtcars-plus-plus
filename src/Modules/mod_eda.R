# ------------------------------------------------------------------------------
# File: mod_eda.R
# Author: Aanish Pradhan
# Version: 2025-08-20
# Description: Module for the Exploratory Data Analysis (EDA) page of the mtcars++
#              dashboard. This includes KPIs, summary cards, and visualizations 
#              across pricing, efficiency, performance, and safety.
# ------------------------------------------------------------------------------

#' EDA Module UI
#'
#' @param id The module namespace ID
#'
#' @return A tabItem UI element for the EDA page
#' @export
mod_eda_ui <- function(id) {
	ns <- NS(id)
	
	tabItem(
		tabName = "eda-page",
		h1("Exploratory Data Analysis"),
		
		# KPIs
		fluidRow(
			bs4ValueBoxOutput(ns("kpi_selected_n"), width = 3),
			bs4ValueBoxOutput(ns("kpi_avg_price_sel"), width = 3),
			bs4ValueBoxOutput(ns("kpi_avg_city_mpg_sel"), width = 3),
			bs4ValueBoxOutput(ns("kpi_avg_hwy_mpg_sel"), width = 3)
		),
		
		# Price
		fluidRow(
			bs4Card(
				title = "Price & Value",
				width = 12, collapsible = TRUE, maximizable = TRUE,
				uiOutput(ns("msrp_summary")),
				tabsetPanel(
					id = ns("tab_price"), type = "tabs",
					tabPanel("By Body Style", plotOutput(ns("p_price_box"), height = "520px")),
					tabPanel("Market Distribution", plotOutput(ns("p_price_density"), height = "420px"))
				)
			)
		),
		
		# MPG
		fluidRow(
			bs4Card(
				title = "Fuel Efficiency & Range",
				width = 12, collapsible = TRUE, maximizable = TRUE,
				uiOutput(ns("mpg_summary")),
				tabsetPanel(
					id = ns("tab_mpg"), type = "tabs",
					tabPanel("City vs Highway MPG", plotOutput(ns("p_mpg_scatter"), height = "500px")),
					tabPanel("Estimated Range", plotOutput(ns("p_range_scatter"), height = "500px"))
				)
			)
		),
		
		# Engine & Performance
		fluidRow(
			bs4Card(
				title = "Performance & Engine",
				width = 12, collapsible = TRUE, maximizable = TRUE,
				uiOutput(ns("engine_summary")),
				tabsetPanel(
					id = ns("tab_engine"), type = "tabs",
					tabPanel("Displacement by Body Style", plotOutput(ns("p_disp_by_style"), height = "520px")),
					tabPanel("Cylinders by Drivetrain", plotOutput(ns("p_cylinders_by_drivetrain"), height = "460px")),
					tabPanel("Displacement vs Cylinders", plotOutput(ns("p_disp_cyl_scatter"), height = "520px"))
				)
			)
		),
		
		# Safety
		fluidRow(
			bs4Card(
				title = "Safety Features",
				width = 12, collapsible = TRUE, maximizable = TRUE,
				uiOutput(ns("safety_summary")),
				tabsetPanel(
					id = ns("tab_safety"), type = "tabs",
					tabPanel("Who Has What?", plotOutput(ns("p_safety_matrix"), height = "520px")),
					tabPanel("Market Loadout", plotOutput(ns("p_safety_distribution"), height = "420px"))
				)
			)
		)
	)
}

#' EDA Module Server
#'
#' @param id Module namespace ID
#' @param selected_idx A reactive expression returning indices of selected rows
#'
#' @export
mod_eda_server <- function(id, selected_idx) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		
		flagged_tcc <- reactive({
			idx <- selected_idx()
			if (is.null(idx)) idx <- integer(0)
			tcc %>%
				dplyr::mutate(
					.row_index = dplyr::row_number(),
					is_selected = .row_index %in% idx
				)
		})
		
		fmt_d   <- function(x) scales::dollar(x, accuracy = 1)
		fmt_n   <- function(x) scales::number(x, accuracy = 0.1)
		safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
		
		sel_df   <- reactive(flagged_tcc() %>% dplyr::filter(is_selected))
		marketdf <- reactive(flagged_tcc())
		
		output$kpi_selected_n <- renderUI({
			n <- nrow(sel_df())
			bs4Dash::bs4ValueBox(
				value = as.character(n),
				subtitle = "Cars selected",
				color = if (n > 0) "primary" else "info",
				icon = shiny::icon("check-square")
			)
		})
		
		output$kpi_avg_price_sel <- renderUI({
			dat <- sel_df(); v <- if (nrow(dat)) safe_mean(dat$msrp_usd) else NA_real_
			bs4Dash::bs4ValueBox(
				value = ifelse(is.na(v), "—", fmt_d(v)),
				subtitle = "Avg MSRP (selected)",
				color = "success",
				icon = shiny::icon("dollar-sign")
			)
		})
		
		output$kpi_avg_city_mpg_sel <- renderUI({
			dat <- sel_df(); v <- if (nrow(dat)) safe_mean(dat$city_mileage_mpg) else NA_real_
			bs4Dash::bs4ValueBox(
				value = ifelse(is.na(v), "—", fmt_n(v)),
				subtitle = "Avg City MPG (selected)",
				color = "info",
				icon = shiny::icon("city")
			)
		})
		
		output$kpi_avg_hwy_mpg_sel <- renderUI({
			dat <- sel_df(); v <- if (nrow(dat)) safe_mean(dat$hwy_mileage_mpg) else NA_real_
			bs4Dash::bs4ValueBox(
				value = ifelse(is.na(v), "—", fmt_n(v)),
				subtitle = "Avg Highway MPG (selected)",
				color = "info",
				icon = shiny::icon("road")
			)
		})
		
		output$msrp_summary <- renderUI({
			all <- marketdf() %>% dplyr::filter(!is.na(msrp_usd))
			sel <- sel_df() %>% dplyr::filter(!is.na(msrp_usd))
			if (!nrow(all)) return(NULL)
			med_mkt <- stats::median(all$msrp_usd)
			if (!nrow(sel)) {
				HTML(sprintf(
					"<p><b>How to read:</b> The boxplot shows MSRP by body style for the whole market. Market median MSRP: <b>%s</b>.</p>",
					fmt_d(med_mkt)
				))
			} else {
				med_sel <- stats::median(sel$msrp_usd)
				HTML(sprintf(
					"<p><b>Takeaway:</b> Your selected cars’ median MSRP is <b>%s</b> vs market <b>%s</b>.</p>",
					fmt_d(med_sel), fmt_d(med_mkt)
				))
			}
		})
		
		output$p_price_box <- renderPlot({
			df_all <- flagged_tcc() %>%
				dplyr::filter(!is.na(msrp_usd), !is.na(body_style))
			
			ggplot2::ggplot(
				df_all,
				ggplot2::aes(x = forcats::fct_reorder(body_style, msrp_usd, .fun = median),
							 y = msrp_usd)
			) +
				ggplot2::geom_boxplot(outlier.alpha = 0.15, fill = "grey90", colour = "grey40") +
				ggplot2::geom_point(
					data = dplyr::filter(df_all, is_selected),
					ggplot2::aes(y = msrp_usd, color = "Selected"),
					size = 2, alpha = 0.95,
					position = ggplot2::position_jitter(width = 0.2, height = 0)
				) +
				ggplot2::scale_color_manual(name = NULL, values = c(Selected = "#2C7BE5")) +
				ggplot2::labs(
					x = "Body style", y = "MSRP (USD)",
					title = "MSRP by body style (selected cars vs full database)"
				) +
				ggplot2::coord_flip() +
				ggplot2::theme_minimal() +
				ggplot2::theme(legend.position = "top")
		}, res = 96, height = 600)
	})
}
