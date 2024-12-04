library(shiny)
library(dplyr)
library(rlang)
library(DT)

# Define UI for the application
ui <- fluidPage(
  titlePanel("NBA Lineups and Player Stats Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select team, with an "All Teams" option
      selectInput("team", "Select Team:", 
                  choices = c("All Teams", unique(Lineups$Team))),
      selectInput("sort_by", "Sort By:", choices = names(Lineups)[sapply(Lineups, is.numeric)])
    ),
    
    mainPanel(
      h3("Lineup Data"),
      DTOutput("lineupTable"),  # Interactive table with selectable rows
      
      h3("Player Stats for Selected Lineup"),
      tableOutput("playerTable")  # Table for displaying player stats
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Render the lineup table
  output$lineupTable <- renderDT({
    req(input$team, input$sort_by)  # Ensure inputs are available
    
    # Adjust filtering based on team selection
    if (input$team == "All Teams") {
      Lineups %>%
        arrange(desc(!!sym(input$sort_by))) %>%
        select(Lineups, Team, !!sym(input$sort_by)) %>%
        datatable(
          selection = "single",  # Allow selecting a single row
          options = list(pageLength = 5)  # Display 5 rows per page
        )
    } else {
      Lineups %>%
        filter(Team == input$team) %>%
        arrange(desc(!!sym(input$sort_by))) %>%
        select(Lineups, Team, !!sym(input$sort_by)) %>%
        datatable(
          selection = "single",  # Allow selecting a single row
          options = list(pageLength = 5)  # Display 5 rows per page
        )
    }
  })
  
  # Render the player stats for the selected lineup
  output$playerTable <- renderTable({
    selected <- input$lineupTable_rows_selected  # Get the selected row index
    
    if (!is.null(selected)) {
      # Extract the selected lineup
      selected_lineup <- Lineups %>%
        filter(Team == input$team | input$team == "All Teams") %>%
        arrange(desc(!!sym(input$sort_by))) %>%
        slice(selected) %>%
        pull(Lineups)
      
      # Extract player names from the selected lineup
      players <- unlist(strsplit(selected_lineup, " - "))
      
      # Check if any player has duplicates in the Players dataset
      players_with_duplicates <- Players %>%
        filter(Player %in% players) %>%
        group_by(Player) %>%
        filter(n() > 1)  # Find players with duplicate names
      
      # If there are players with duplicates, filter by the selected team
      if (nrow(players_with_duplicates) > 0) {
        Players %>%
          filter(Player %in% players) %>%
          filter(TEAM == input$team | input$team == "All Teams")
      } else {
        # If no duplicates, return all players
        Players %>%
          filter(Player %in% players)
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
