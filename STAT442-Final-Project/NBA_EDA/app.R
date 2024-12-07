library(shiny)
library(tidyr)
library(dplyr)
library(rlang)
library(readxl)
library(DT) # data table
library(readr) # read in csv data
library(stringi) # remove accent symbols on letters
library(fmsb) # radar chart
library(scales) # percentile calculations
library(plotly)

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
      ),
      tabPanel("Lineup Composition",
               h3("Radar Chart of Player Stats Percentiles"),
               uiOutput("radarChart")  # Radar chart for selected lineup
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Read in Data
  Lineups <- readxl::read_excel("Lineups.xlsx")
  Players <- readxl::read_excel("Players.xlsx")
  Players_Info <- readr::read_csv("Players.Info.csv")
  
  # Clean Data
  Players <- Players %>%
    mutate(Player = gsub("(^[A-Za-z'\\-])[A-Za-z'\\-]*\\s([A-Za-z]+)", "\\1. \\2", Player)) %>%
    mutate(Player = stri_trans_general(Player, "Latin-ASCII"))
  
  # Pre-calculate percentiles for all players and store it
  player_stats_percentiles <- Players %>%
    mutate(
      PTS = percent_rank(PTS) * 100,
      AST = percent_rank(AST) * 100,
      REB = percent_rank(REB) * 100,
      `FG%` = percent_rank(`FG%`) * 100,
      `3P%` = percent_rank(`3P%`) * 100,
      STL = percent_rank(STL) * 100,
      BLK = percent_rank(BLK) * 100
    )
  
  # Reactive to store player stats for the selected lineup
  selected_player_stats <- reactiveVal(NULL)
  
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
    player_stats <- Players %>%
      filter(Player %in% players) %>%
      filter(
        # For duplicate players, match by selected team; otherwise include all
        (Player %in% duplicate_players & Team == selected_team) |
          !(Player %in% duplicate_players)
      )
    
    # Store the player stats for the selected lineup
    selected_player_stats(player_stats)
    
    player_stats
  })
  
  # Render radar charts for each player in the selected lineup
  output$radarChart <- renderUI({
    req(selected_player_stats())  # Ensure data is available
    
    # Get the selected player stats
    player_stats <- selected_player_stats()
    
    # Get the list of players selected in the lineup table
    selected_players <- player_stats$Player
    
    # Filter the pre-calculated percentiles for selected players
    player_percentiles_selected <- player_stats_percentiles %>%
      filter(Player %in% selected_players)
    
    # Create a list to hold the radar charts for each player
    radar_plots <- lapply(1:nrow(player_percentiles_selected), function(i) {
      # Extract stats for the player
      player_data <- player_percentiles_selected[i, c("PTS", "AST", "REB", "FG%", "3P%", "STL", "BLK")]
      
      # Prepare the data for the radar chart
      radar_data <- data.frame(
        stats = c("PTS", "AST", "REB", "FG%", "3P%", "STL", "BLK"),
        value = as.numeric(player_data),
        max = rep(100, 7),
        min = rep(0, 7)
      )
      
      # Prepare the hover text
      hover_text <- paste(radar_data$stats, ": ", selected_player_stats()[i, radar_data$stats], " (Percentile: ",
                          round(player_data, 1), "%)", sep = "")
      
      # Create a radar chart using Plotly
      radar_plot <- plot_ly(
        type = 'scatterpolar',
        r = radar_data$value,
        theta = radar_data$stats,
        fill = 'toself',
        name = player_percentiles_selected$Player[i],
        mode = 'lines+markers',
        hoverinfo = 'text',  # Show custom text on hover
        hovertext = hover_text  # Display the stat and percentile on hover
      ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = TRUE,
              range = c(0, 100)
            )
          ),
          title = player_percentiles_selected$Player[i],
          showlegend = FALSE,
          margin = list(t = 35, r = 40, b = 40, l = 40)
        )
      
      return(radar_plot)
    })
    
    # Group radar charts into rows of 3 charts each
    num_rows <- ceiling(length(radar_plots) / 3)
    
    # Create the UI layout with 3 radar charts per row
    radar_ui <- lapply(1:num_rows, function(row_idx) {
      # Define the starting and ending index for this row's charts
      start_idx <- (row_idx - 1) * 3 + 1
      end_idx <- min(row_idx * 3, length(radar_plots))
      
      # Create a fluidRow with 3 columns containing the radar charts
      row_columns <- list()
      for (i in start_idx:end_idx) {
        row_columns <- append(row_columns, list(column(4, radar_plots[[i]])))
      }
      
      # Ensure that the row has up to 3 columns
      fluidRow(row_columns)
    })
    
    # Render each plot individually in a vertical layout
    do.call(tagList, radar_ui)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
