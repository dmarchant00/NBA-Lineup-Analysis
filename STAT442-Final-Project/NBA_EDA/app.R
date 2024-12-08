library(shiny)
library(tidyr)
library(dplyr)
library(rlang)
library(readxl)
library(DT) # data table
library(readr) # read in csv data
library(stringi) # remove accent symbols on letters
library(scales) # percentile calculations
library(plotly) # radar chart
library(shinythemes)  # Themes

# Define UI for the application
ui <- fluidPage(
  theme = shinythemes::shinytheme("journal"), 
  titlePanel("NBA Lineups Analysis"),
  
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
      tabPanel("Lineup Builder",
               h3("Build Your Custom NBA Lineup"),
               p("Select 5 total players across positions."),
               
               # Layout for positioning the selectors and total metric
               fluidRow(
                 # Left side: Player Selectors
                 column(6,
                        selectInput("Guard", "Select Guard (PG/SG):", choices = NULL, multiple = TRUE),
                        selectInput("Forward", "Select Forward (SF/PF):", choices = NULL, multiple = TRUE),
                        selectInput("Center", "Select Center (C):", choices = NULL, multiple = TRUE),
                        actionButton("build_lineup", "Build Lineup"),
                        tableOutput("customLineup")
                 ),
                 
                 # Right side: Metric Selection and Total
                 column(6,
                        # Metric Selector
                        selectInput("metric", "Select Metric:", choices = c("PER", "OWS", "DWS", "WS", "OBPM", "DBPM", "BPM", "VORP")),
                        
                        # Total Sum of the Selected Metric
                        tableOutput("metricTotal")
                 )
               )
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
  
  # Read and clean data
  Lineups <- read_excel("Lineups.xlsx")
  Players <- read_excel("Players.xlsx") %>% 
    mutate(Player = stri_trans_general(Player, "Latin-ASCII"))
  Advanced <- read_excel("Advanced.xlsx") %>% 
    select(Player, Team, Pos, PER, OWS, DWS, WS, OBPM, DBPM, BPM, VORP, Awards) %>% 
    mutate(Player = stri_trans_general(Player, "Latin-ASCII"))
  Players.custom <- left_join(Players, Advanced, by = c("Player", "Team"))
  
  # Reshape player names to match names in lineups
  Players <- Players %>%
    mutate(Player = gsub("(^[A-Za-z'\\-])[A-Za-z'\\-]*\\s([A-Za-z]+)", "\\1. \\2", Player))
  
  # Reactive Values
  selected_player_stats <- reactiveVal(NULL)
  lineup_source <- reactiveVal(NULL)  # Track source of lineup selection
  
  # Update dropdown choices
  observe({
    updateSelectInput(session, "team", choices = c("All Teams", unique(Lineups$Team)))
    updateSelectInput(session, "sort_by", choices = names(Lineups)[sapply(Lineups, is.numeric)])
    updateSelectInput(session, "Guard", choices = Players.custom$Player[Players.custom$Pos %in% c("PG", "SG")])
    updateSelectInput(session, "Forward", choices = Players.custom$Player[Players.custom$Pos %in% c("SF", "PF")])
    updateSelectInput(session, "Center", choices = Players.custom$Player[Players.custom$Pos == "C"])
  })
  
  # Lineup Analysis
  output$lineupTable <- renderDT({
    req(input$team, input$sort_by)
    filtered_data <- if (input$team == "All Teams") Lineups else Lineups %>% filter(Team == input$team)
    datatable(filtered_data %>% arrange(desc(!!sym(input$sort_by))) %>% select(Lineups, Team, !!sym(input$sort_by)), selection = "single", options = list(pageLength = 5))
  })
  
  
  # Render the player stats for the selected lineup
  output$playerTable <- renderTable({
    req(input$lineupTable_rows_selected)
    selected <- input$lineupTable_rows_selected
    
    # Extract the selected lineup
    selected_lineup <- Lineups %>% 
      arrange(desc(!!sym(input$sort_by))) %>% 
      slice(selected) %>% 
      pull(Lineups)
    
    # Extract the team of the selected lineup
    selected_team <- Lineups %>%
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
        (Player %in% duplicate_players & Team == selected_team) |
          !(Player %in% duplicate_players)
      )
    
    # Store the player stats for the selected lineup
    selected_player_stats(player_stats)
    lineup_source("analysis")  # Set source to lineup analysis
    player_stats
  })
  
  # Lineup Builder - Custom Lineup
  observeEvent(input$build_lineup, {
    req(input$Guard, input$Forward, input$Center)
    selected_players <- c(input$Guard, input$Forward, input$Center)
    custom_lineup <- Players.custom %>% filter(Player %in% selected_players)
    selected_player_stats(custom_lineup)
    lineup_source("builder")  # Set source to lineup builder
    output$customLineup <- renderTable(custom_lineup)
  })
  
  # Show Total Metric for the Selected Metric
  output$metricTotal <- renderTable({
    req(selected_player_stats(), input$metric)
    custom_lineup <- selected_player_stats()
    
    # Calculate the sum of the selected metric
    total_metric <- sum(custom_lineup[[input$metric]], na.rm = TRUE)
    
    # Create a data frame to display the sum
    data.frame(Metric = input$metric, Total = total_metric)
  })
  
  # Render radar charts for each player in the selected lineup
  output$radarChart <- renderUI({
    req(selected_player_stats() )  # Ensure data is available
    
    # Get the selected player stats
    player_stats <- selected_player_stats()
    
    # Get the list of players selected in the lineup table
    selected_players <- player_stats$Player
    selected_teams <- player_stats$Team
    
    # Get the source of the lineup
    source <- lineup_source()
    
    # Select the appropriate dataset based on the source
    dataset <- if (source == "builder") Players.custom else Players
    
    # Grab percentile of players stats
    player_stats_percentiles <- dataset %>%
      mutate(
        PTS = percent_rank(PTS) * 100,
        AST = percent_rank(AST) * 100,
        REB = percent_rank(REB) * 100,
        `FG%` = percent_rank(`FG%`) * 100,
        `3P%` = percent_rank(`3P%`) * 100,
        STL = percent_rank(STL) * 100,
        BLK = percent_rank(BLK) * 100
      )
    
    # Filter the pre-calculated percentiles for selected players, considering teams
    player_percentiles_selected <- player_stats_percentiles %>%
      filter(Player %in% selected_players & Team %in% selected_teams)
    
    # Create a list to hold the radar charts for each player
    radar_plots <- lapply(1:nrow(player_percentiles_selected), function(i) {
      # Extract stats for the player
      player_data <- player_percentiles_selected[i, c("PTS", "AST", "REB", "FG%", "3P%", "STL", "BLK")]
      
      # Prepare the data for the radar chart
      radar_data <- data.frame(
        stats = c("PTS", "AST", "REB", "FG%", "3P%", "STL", "BLK"),
        value = as.numeric(player_data),
        max = 100, min = 0
      )
      
      # Create a radar chart using Plotly
      radar_plot <- plot_ly(
        type = 'scatterpolar',
        r = radar_data$value,
        theta = radar_data$stats,
        fill = 'toself',
        name = player_percentiles_selected$Player[i],
        mode = 'lines+markers',
        hoverinfo = 'text',  # Show custom text on hover
        hovertext = paste(radar_data$stats, ": ", selected_player_stats()[i, radar_data$stats], " (Percentile: ",
                          round(player_data, 1), "%)", sep = "")  # Display the stat and percentile on hover
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