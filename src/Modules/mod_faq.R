# ------------------------------------------------------------------------------
# File: mod_faq.R
# Author: Aanish Pradhan
# Version: 2025-08-20
# Description: UI module for the FAQ page of the mtcars++ dashboard.
# ------------------------------------------------------------------------------

#' FAQ Module UI
#'
#' @param id The module namespace ID
#'
#' @return A tabItem UI element for the FAQ page
#' @export
mod_faq_ui <- function(id) {
	ns <- NS(id)
	
	tabItem(
		tabName = "faq-page",
		h1("Frequently Asked Questions"),
		bs4Dash::box(
			title = shiny::HTML("<strong>How does mtcars++'s recommendation system work?</strong>"),
			width = 12,
			shiny::HTML("
        <p>
          Traditional recommendation systems, like those found on platforms such 
          as streaming services, suggest new media to users based on the 
          similarity of their previously liked media to other users' liked media.
        </p>
        <p>These systems face two main issues:</p>
        <ol>
          <li><strong>Data Requirement:</strong> Recommendation requires existing 
          data of items that users have liked.</li>
          <li><strong>Cold Starting:</strong> To provide recommendations to new 
          users, we need to have an understanding of what they like or 
          dislike. Since the user is new, this data doesn't exist, making the 
          challenge paradoxical. This is referred to as the \"Cold Starting\" 
          problem.</li>
        </ol>
        <p>
          To circumvent this problem, mtcars++ uses a two-phase cluster and 
          neighbor search recommendation system.
        </p>
        <p>
          When a user first
        </p>
      ")
		)
	)
}

#' FAQ Module Server
#'
#' @param id Module namespace ID
#'
#' @export
mod_faq_server <- function(id) {
	moduleServer(id, function(input, output, session) {
		# No server logic needed for now
	})
}
