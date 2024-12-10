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
library(shinyWidgets)
library(car)
library(MASS)
library(caret)

ui <- navbarPage(
  title = "NBA Lineups Analysis",
  theme = shinythemes::shinytheme("cosmo"),  # Sleek theme
  id = "navbar",
  
  # Home Page
  tabPanel(
    "Home",
    fluidPage(
      div(
        class = "jumbotron text-center",
        style = "background: linear-gradient(to right, #0052D4, #65C7F7); color: white; padding: 30px;",
        h1("NBA Lineups Analysis"),
        p("Your one-stop platform for exploring and building NBA lineups!")
      ),
      fluidRow(
        column(
          4,
          div(
            class = "card text-center",
            style = "padding: 20px; background-color: #f7f7f7; border-radius: 10px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
            h3("Analyze Lineups"),
            p("Dive into performance metrics for your favorite teams."),
            icon("chart-line", style = "font-size: 40px; color: #0052D4;")
          )
        ),
        column(
          4,
          div(
            class = "card text-center",
            style = "padding: 20px; background-color: #f7f7f7; border-radius: 10px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
            h3("Build Custom Lineups"),
            p("Create dream lineups with real NBA data."),
            icon("basketball-ball", style = "font-size: 40px; color: #65C7F7;")
          )
        ),
        column(
          4,
          div(
            class = "card text-center",
            style = "padding: 20px; background-color: #f7f7f7; border-radius: 10px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
            h3("Explore Insights"),
            p("Analyze trends with advanced EDA tools."),
            icon("chart-bar", style = "font-size: 40px; color: #1C1C1C;")
          )
        )
      )
    )
  ),
  
  # Lineup Analysis
  tabPanel(
    "Lineup Analysis",
    sidebarLayout(
      sidebarPanel(
        h4("Filter Lineups"),
        pickerInput(
          inputId = "team",label = "Select Team:",choices = NULL, choicesOpt = list(content = NULL)
        ),
        pickerInput(
          inputId = "sort_by",label = "Sort By:",choices = NULL, choicesOpt = list(content = NULL)
        ),
        actionButton("analyze", "Analyze")
      ),
      mainPanel(
        h3("Lineup Data"),
        DTOutput("lineupTable"),
        br(),
        h4("Player Stats"),
        DTOutput("playerTable")
      )
    )
  ),
  
  # Lineup Builder UI
  tabPanel(
    "Lineup Builder",
    fluidPage(
      fluidRow(
        column(
          6,
          h3("Select Players"),
          selectInput("Guard", "Select Guards (PG/SG):", choices = NULL, multiple = TRUE),
          selectInput("Forward", "Select Forwards (SF/PF):", choices = NULL, multiple = TRUE),
          selectInput("Center", "Select Centers (C):", choices = NULL, multiple = TRUE),
          actionButton("build_lineup", "Build Lineup"),
          actionButton("reset_lineup", "Reset Lineup", icon = icon("refresh"))
        ),
        column(
          6,
          h3("Custom Lineup Metrics"),
          selectInput("metric", "Select Metric:", choices = c("PER", "OWS", "DWS", "WS", "OBPM", "DBPM", "BPM", "VORP")),
          textOutput("metricDescription"),  # Added textOutput for the metric description
          tableOutput("metricTotal")
        )
      ),
      br(),
      h4("Custom Lineup Data"),
      DTOutput("customLineup")
    )
  ),
  
  tabPanel("Lineup Composition",
           h3("Radar Chart of Player Stats Percentiles"),
           uiOutput("radarChart"),  # This renders the radar chart output
           tags$p(tags$small("Stat Definitions"))
  ),
  
  tabPanel( "EDA",
    fluidPage(
      titlePanel("NBA Lineups EDA"),
      
      # Scatter Plot and Linear Model directly under EDA without extra tabs
      plotlyOutput("scatter_plot"),
      verbatimTextOutput("linear_model_summary")
    )
  ),
  
  
  # Footer
  tags$footer(
    style = "text-align: center; padding: 10px; background: #f1f1f1; border-top: 1px solid #ccc; color: #555;",
    "© 2024 NBA Lineups Analysis | Drew Marchant and Joseph Uttecht"
  )
)


server <- function(input, output, session) {
  
  # Stat descriptions for tooltips
  stat_descriptions <- list(
    PER = "Player Efficiency Rating",
    OWS = "Offensive Win Shares",
    DWS = "Defensive Win Shares",
    WS = "Win Shares",
    OBPM = "Offensive Box Plus-Minus",
    DBPM = "Defensive Box Plus-Minus",
    BPM = "Box Plus-Minus",
    VORP = "Value Over Replacement Player",
    Player = "The name of the player",
    Team = "The team the player is associated with",
    Age = "The player's age",
    GP = "Games Played - Number of games the player has participated in",
    W = "Wins - The number of games won by the player's team",
    L = "Losses - The number of games lost by the player's team",
    Min = "Minutes - The total minutes the player has played",
    PTS = "Points - The total points scored by the player",
    FGM = "Field Goals Made - The number of field goals made by the player",
    FGA = "Field Goals Attempted - The number of field goals attempted by the player",
    `FG%` = "Field Goal Percentage - The percentage of successful field goals made (FGM / FGA)",
    `3PM` = "Three-Point Field Goals Made - The number of three-point shots made",
    `3PA` = "Three-Point Field Goals Attempted - The number of three-point shots attempted by the player",
    `3P%` = "Three-Point Percentage - The percentage of successful three-point shots made (3PM / 3PA)",
    FTM = "Free Throws Made - The number of free throws made by the player",
    FTA = "Free Throws Attempted - The number of free throws attempted by the player",
    `FT%` = "Free Throw Percentage - The percentage of successful free throws made (FTM / FTA)",
    OREB = "Offensive Rebounds - The number of rebounds grabbed on the offensive end",
    DREB = "Defensive Rebounds - The number of rebounds grabbed on the defensive end",
    REB = "Rebounds - The total number of rebounds grabbed by the player (OREB + DREB)",
    AST = "Assists - The total number of assists by the player",
    TOV = "Turnovers - The total number of times the player lost possession of the ball",
    STL = "Steals - The total number of steals by the player",
    BLK = "Blocks - The total number of shots blocked by the player",
    PF = "Personal Fouls - The total number of personal fouls committed by the player",
    FP = "Fantasy Points - A fantasy basketball scoring system based on stats",
    DD2 = "Double-Double (Points and Rebounds or Assists) - The number of times the player has achieved a double-double",
    TD3 = "Triple-Double (Points, Rebounds, Assists) - The number of times the player has achieved a triple-double",
    `+/-` = "Plus/Minus - The point differential when the player is on the court"
  )
  
  # Read and clean data
  Lineups <- read_excel("Lineups.xlsx")
  Players <- read_excel("Players.xlsx") %>% 
    mutate(Player = stri_trans_general(Player, "Latin-ASCII"))
  Advanced <- read_excel("Advanced.xlsx") %>% 
    dplyr::select(Player, Team, Pos, PER, OWS, DWS, WS, OBPM, DBPM, BPM, VORP, Awards) %>% 
    mutate(Player = stri_trans_general(Player, "Latin-ASCII"))
  # Now perform the left join
  Players.custom <- left_join(Players, Advanced, by = c("Player", "Team"))
  
  
  Players <- Players %>%
    mutate(Player = gsub("(^[A-Za-z'\\-])[A-Za-z'\\-]*\\s([A-Za-z]+)", "\\1. \\2", Player))
  
  # Reactive Values
  selected_player_stats <- reactiveVal(NULL)
  lineup_source <- reactiveVal(NULL)  # Track source of lineup selection
  
  # Update dropdown choices for custom lineup and EDA variables
  observe({
    updatePickerInput(session, "team", choices = c("All Teams", unique(Lineups$Team)))
    updatePickerInput(session, "sort_by", choices = names(Lineups)[sapply(Lineups, is.numeric)])
    updateSelectInput(session, "Guard", choices = Players.custom$Player[Players.custom$Pos %in% c("PG", "SG")])
    updateSelectInput(session, "Forward", choices = Players.custom$Player[Players.custom$Pos %in% c("SF", "PF")])
    updateSelectInput(session, "Center", choices = Players.custom$Player[Players.custom$Pos == "C"])
    
    # EDA variable choices
    updateSelectInput(session, "eda_variable", choices = names(Players))
    updateSelectInput(session, "eda_corr_var1", choices = names(Players))
    updateSelectInput(session, "eda_corr_var2", choices = names(Players))
  })
  
  # Lineup Analysis
  output$lineupTable <- renderDT({
    req(input$team, input$sort_by)
    filtered_data <- if (input$team == "All Teams") Lineups else Lineups %>% filter(Team == input$team)
    datatable(filtered_data %>% arrange(desc(!!sym(input$sort_by))) %>% select(Lineups, Team, !!sym(input$sort_by)), selection = "single", options = list(pageLength = 5))
  })
  
  # Render the player stats for the selected lineup
  output$playerTable <- renderDT({
    req(input$lineupTable_rows_selected)
    selected <- input$lineupTable_rows_selected
    
    filtered_data <- if (input$team == "All Teams") Lineups else Lineups %>% filter(Team == input$team)
    
    # Extract the selected lineup
    selected_lineup <- filtered_data %>% 
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
        (Player %in% duplicate_players & Team == selected_team) |
          !(Player %in% duplicate_players)
      )
    
    # Store the player stats for the selected lineup
    selected_player_stats(player_stats)
    lineup_source("analysis")  # Set source to lineup analysis
    
    # Render the data as a DT datatable
    datatable(
      player_stats,
      options = list(
        # Display 5 rows per page
        autoWidth = TRUE,  # Adjust column widths automatically
        dom = 'tip',  # Only show table with pagination and search
        columnDefs = list(list(targets = "_all", className = "dt-center"))  # Center-align columns
      ),
      rownames = FALSE  # Disable row names
    )
  })
  
  # Scatter Plot Output
  output$scatter_plot <- renderPlotly({
    p <- ggplot(Lineups, aes(
      x = `+/-`, 
      y = PIE, 
      text = paste("Lineup:", Lineups, "<br>Team:", Team, "<br>+/-:", `+/-`, "<br>PIE:", PIE)
    )) +
      geom_point() +
      labs(
        title = "Correlation between +/- and PIE", 
        x = "Plus Minus", 
        y = "PIE"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Linear Model Summary Output
  output$linear_model_summary <- renderPrint({
    # Initial Linear Model
    linear_model <- lm(PlusMinus ~ . - Lineups - Team - Min, data = Lineups)
    cat("Initial Linear Model Summary:\n")
    print(summary(linear_model))
    
    # Check for Aliased Coefficients
    alias_info <- alias(linear_model)
    cat("\nAliased coefficients:\n")
    print(alias_info)
    
    # Updated Linear Model
    linear_model_updated <- lm(PlusMinus ~ . - Lineups - Team - REB - FTM - Min, data = Lineups)
    cat("\nUpdated Linear Model Summary:\n")
    print(summary(linear_model_updated))
  })
  

  
  # Lineup Builder - Custom Lineup
  observeEvent(input$build_lineup, {
    req(input$Guard, input$Forward, input$Center)
    selected_players <- c(input$Guard, input$Forward, input$Center)
    custom_lineup <- Players.custom %>% filter(Player %in% selected_players)
    
    selected_player_stats(custom_lineup)
    lineup_source("builder")  # Set source to lineup builder
    
    # Ensure JSON is properly formatted
    stat_descriptions_json <- jsonlite::toJSON(stat_descriptions, auto_unbox = TRUE)
    
    output$customLineup <- renderDT({
      custom_lineup <- selected_player_stats()
      
      # Create the datatable with custom tooltips for column headers
      datatable(
        custom_lineup,
        options = list(
          pageLength = 5,         # Display 5 rows per page
          autoWidth = TRUE,       # Automatically adjust column widths
          dom = 'tip',            # Show table and pagination controls
          initComplete = JS(
            # Adding tooltips to the column headers directly
            paste0(
              "function(settings, json) {",
              "var descriptions = ", stat_descriptions_json, ";",
              "this.api().columns().header().each(function (col, i) {",
              "var colName = $(col).text();",
              "if (descriptions[colName]) {",
              "$(col).attr('title', descriptions[colName]);",
              "}",
              "});",
              "}"
            )
          )
        ),
        selection = "none",       # No row selection
        rownames = FALSE          # Hide row names
      )
    })
  })
  
  # Show Metric Description dynamically
  output$metricDescription <- renderText({
    req(input$metric)  # Ensure input is available
    stat_descriptions[[input$metric]]  # Display description based on the selected metric
  })
  
  # Server logic for resetting lineup builder inputs
  observeEvent(input$reset_lineup, {
    # Reset select inputs to NULL
    updateSelectInput(session, "Guard", selected = character(0))
    updateSelectInput(session, "Forward", selected = character(0))
    updateSelectInput(session, "Center", selected = character(0))
    
    # Clear custom lineup table
    selected_player_stats(NULL)
  })
  
  # Show Total Metric for the Selected Metric
  output$metricTotal <- renderTable({
    req(selected_player_stats(), input$metric)
    custom_lineup <- selected_player_stats()
    
    # Calculate the sum of the selected metric
    total_metric <- sum(custom_lineup[[input$metric]], na.rm = TRUE)
    
    league_average <- mean(Players.custom[[input$metric]], na.rm = TRUE) * 5
    
    # Create a data frame to display the sum
    data.frame(Metric = input$metric, `Lineup-Total` = total_metric, `League-Average` = league_average)
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
      fluidRow(
        lapply(start_idx:end_idx, function(chart_idx) {
          column(width = 4, plotlyOutput(paste0("radarChart_", chart_idx)))
        })
      )
    })
    
    # Render the radar charts in each slot
    for (i in seq_along(radar_plots)) {
      local({
        idx <- i
        output[[paste0("radarChart_", idx)]] <- renderPlotly({ radar_plots[[idx]] })
      })
    }
    
    # Combine the rows into a single UI object
    do.call(tagList, radar_ui)
  })
}
shinyApp(ui, server)
