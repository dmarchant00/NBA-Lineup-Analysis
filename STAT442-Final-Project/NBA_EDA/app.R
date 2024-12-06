library(shiny)
library(dplyr)
library(rlang)
library(DT)
library(readr)
library(stringi)

# Define UI for the application
ui <- fluidPage(
  theme = shinythemes::shinytheme("journal"), 
  titlePanel("NBA Lineups and Player Stats Analysis"),
  
  mainPanel(
      tabsetPanel(
        tabPanel("Home",
                 h2("Welcome to the NBA Lineup Analysis App"),
                 p("This app allows you to analyze NBA lineups and player stats. You can filter and sort lineups by team and other metrics."),
                 p("Select a team and sorting option in the 'Lineup Analysis' tab to get started."),
                 br(),
                 p("To use the app, follow these steps:"),
                 tags$ul(
                   tags$li("Select a team or choose 'All Teams' to view all teams."),
                   tags$li("Choose a sorting criteria based on numeric values."),
                   tags$li("Click on a lineup in the table to view detailed player stats.")
                 )
        ),
        tabPanel("Lineup Analysis",
                 # Moving the inputs inside the tab
                 selectInput("team", "Select Team:", choices = NULL),
                 selectInput("sort_by", "Sort By:", choices = NULL),
                 
                 h3("Lineup Data"),
                 DTOutput("lineupTable"),  # Interactive table with selectable rows
                 
                 h3("Player Stats for Selected Lineup"),
                 tableOutput("playerTable")  # Table for displaying player stats
        )
      )
    )
  )

# Define server logic
server <- function(input, output, session) {
 
  # Read in Data
  Lineups <- read_excel("Lineups.xlsx")
  Players <- read_excel("Players.xlsx")
  Players_Info <- read_csv("Players.Info.csv")
  
  # Clean Data
  Players <- Players %>%
    mutate(Player = gsub("(^[A-Za-z'\\-])[A-Za-z'\\-]*\\s([A-Za-z]+)", "\\1. \\2", Player)) %>%
    mutate(Player = stri_trans_general(Player, "Latin-ASCII"))
  
  # Update team and sort dropdown choices dynamically
  observe({
    updateSelectInput(session, "team", choices = c("All Teams", unique(Lineups$Team)))
    updateSelectInput(session, "sort_by", choices = names(Lineups)[sapply(Lineups, is.numeric)])
  })
  
  # Render the lineup table
  output$lineupTable <- renderDT({
    req(input$team, input$sort_by)  # Ensure inputs are available
    
    # Filter data based on team selection
    filtered_data <- if (input$team == "All Teams") {
      Lineups
    } else {
      Lineups %>% filter(Team == input$team)
    }
    
    # Arrange and display data
    filtered_data <- filtered_data %>%
      arrange(desc(!!sym(input$sort_by))) %>%
      select(Lineups, Team, !!sym(input$sort_by))
    
    datatable(filtered_data, selection = "single", options = list(pageLength = 5))
  })
  
  # Render the player stats for the selected lineup
  output$playerTable <- renderTable({
    selected <- input$lineupTable_rows_selected  # Get the selected row index
    req(selected)
    
    filtered_data <- if (input$team == "All Teams") {
      Lineups
    } else {
      Lineups %>% filter(Team == input$team)
    }
    
      # Extract the selected lineup
      selected_lineup <- filtered_data %>%
        filter(Team == input$team | input$team == "All Teams") %>%
        arrange(desc(!!sym(input$sort_by))) %>%
        slice(selected) %>%
        pull(Lineups)
      
      # Extract the team of the selected lineup
      selected_team <- filtered_data %>%
        arrange(desc(!!sym(input$sort_by))) %>%
        slice(selected) %>%
        pull(Team)
      
      # Extract player names from the selected lineup
      players <- unlist(strsplit(selected_lineup, " - "))
      
      # Identify duplicate players
      duplicate_players <- Players %>%
        filter(Player %in% players) %>%
        group_by(Player) %>%
        filter(n() > 1) %>%
        pull(Player) %>%
        unique()
      
      # Filter and display player stats
      Players %>%
        filter(Player %in% players) %>%
        filter(
          # For duplicate players, match by selected team; otherwise include all
          (Player %in% duplicate_players & Team == selected_team) |
            !(Player %in% duplicate_players)
        )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
