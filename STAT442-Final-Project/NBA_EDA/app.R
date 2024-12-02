
library(shiny)
library(dplyr)
library(rlang)
library(DT)

# Example data for Lineups_Advanced and Player_Stats (Replace these with your datasets)
# Lineups_Advanced <- ...
# Player_Stats <- ...

# Define UI for the application
ui <- fluidPage(
  titlePanel("NBA Lineups and Player Stats Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select team
      selectInput("team", "Select Team:", choices = unique(Lineups$Team)),
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
    
    Lineups %>%
      filter(Team == input$team) %>%
      arrange(desc(!!sym(input$sort_by))) %>%
      select(Lineups, Team, !!sym(input$sort_by)) %>%
      datatable(
        selection = "single",  # Allow selecting a single row
        options = list(pageLength = 5)  # Display 5 rows per page
      )
  })
  
  # Render the player stats for the selected lineup
  output$playerTable <- renderTable({
    selected <- input$lineupTable_rows_selected  # Get the selected row index
    
    if (!is.null(selected)) {
      # Extract the selected lineup
      selected_lineup <- Lineups_Advanced %>%
        filter(Team == input$team) %>%
        arrange(desc(!!sym(input$sort_by))) %>%
        slice(selected) %>%
        pull(Lineups)
      
      # Extract player names from the selected lineup
      players <- unlist(strsplit(selected_lineup, " - "))
      
      # Filter player stats
      Players %>%
        filter(Player %in% players)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
