library(shiny)
library(DT)
library(leaflet)
library(Lahman)
library(dplyr)
library(memoise)
library(dplyr)
library(rsconnect)

data(Fielding)
fielding_summary <- Fielding %>%
  group_by(playerID, POS) %>%
  summarise(total_games = sum(G, na.rm = TRUE)) %>%
  ungroup()
primary_positions <- fielding_summary %>%
  group_by(playerID) %>%
  filter(total_games == max(total_games)) %>%
  slice(1) %>%
  select(playerID, primary_position = POS)

Bplayers <- data.frame(playerID = unique(Batting$playerID[Batting$yearID >= 2000 & Batting$yearID <= 2024]))
Pplayers <- data.frame(playerID = unique(Pitching$playerID[Pitching$yearID >= 2000 & Pitching$yearID <= 2024]))

batting_data <- Batting %>%
  filter(yearID >= 2000 & yearID <= 2024) %>%
  select(playerID, teamID, lgID, G, AB, R, H, X2B, X3B, HR, RBI, SB, CS, BB, SO, IBB, HBP, SH, SF, GIDP)

pitching_data <- Pitching %>%
  filter(yearID >= 2000 & yearID <= 2024) %>%
  select(playerID, teamID, lgID, W, L, G, GS, GS, CG, SHO, SV, IPouts, H, ER, HR, BB, SO, ERA, IBB, R)



Bplayers <- merge(Bplayers, People[, c("playerID", "nameFirst", "nameLast")], by = "playerID", all.x = TRUE)
Pplayers <- merge(Pplayers, People[, c("playerID", "nameFirst", "nameLast")], by = "playerID", all.x = TRUE)

Bplayers <- merge(Bplayers, primary_positions, by = "playerID", all.x = TRUE)
Pplayers <- merge(Pplayers, primary_positions, by = "playerID", all.x = TRUE)

Bplayers$role <- ifelse(Bplayers$primary_position %in% c("P", "C", "1B", "2B", "3B", "SS", "LF", "CF", "RF"), Bplayers$primary_position, "Other")
Pplayers$role <- ifelse(Pplayers$primary_position %in% c("P", "C", "1B", "2B", "3B", "SS", "LF", "CF", "RF"), Pplayers$primary_position, "Other")


players_combined <- rbind(Bplayers, Pplayers)
get_geo_data <- memoise(function() {
  mlb_teams <- data.frame(
    Team = c("ARI", "ATL", "BAL", "BOS", "CHN", "CHA", "CIN", "CLE", "COL", "DET",
             "HOU", "KCA", "LAA", "LAN", "MIA", "MIL", "MIN", "NYN", "NYA", "OAK",
             "PHI", "PIT", "SDN", "SFN", "SEA", "SLN", "TBA", "TEX", "TOR", "WAS"),
    Latitude = c(33.4451, 33.8909, 39.2840, 42.3467, 41.9484, 41.8305, 39.0976, 41.4965, 39.7480,
                 42.3390, 29.7577, 39.0511, 33.8003, 34.0736, 25.7781, 43.0273, 44.9810, 40.7572,
                 40.8296, 37.7518, 39.9060, 40.4469, 32.7084, 37.7786, 47.5910, 38.6225, 27.7685,
                 32.7460, 43.6415, 38.8739),
    Longitude = c(-112.0667, -84.4673, -76.6231, -71.0972, -87.6553, -87.6333, -84.5073, -81.6850,
                  -105.0200, -83.0489, -95.3556, -94.4818, -117.8865, -118.2393, -80.2213, -87.9716,
                  -93.2777, -73.8458, -73.9262, -122.2000, -75.1714, -80.0057, -117.1562, -122.3893,
                  -122.3328, -90.1923, -82.6538, -97.0841, -79.3894, -77.0072)
  )
  
  mlb_logos <- data.frame(
    Team = c("ARI", "ATL", "BAL", "BOS", "CHN", "CHA", "CIN", "CLE", "COL", "DET",
             "HOU", "KCA", "LAA", "LAN", "MIA", "MIL", "MIN", "NYN", "NYA", "OAK",
             "PHI", "PIT", "SDN", "SFN", "SEA", "SLN", "TBA", "TEX", "TOR", "WAS"),
    LogoURL = paste0("https://www.mlbstatic.com/team-logos/", 
                     c(109, 144, 110, 111, 112, 145, 113, 114, 115, 116,
                       117, 118, 108, 119, 146, 158, 142, 121, 147, 133,
                       143, 134, 135, 137, 136, 138, 139, 140, 141, 120),
                     ".svg")
  )
  
  merge(mlb_teams, mlb_logos, by = "Team")
})

ui <- fluidPage(
  titlePanel("MLB Player Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Select Team:", choices = sort(unique(get_geo_data()$Team))),
      selectInput("role", "Filter by Position:",
                  choices = c("P", "C", "1B", "2B", "3B", "SS", 
                              "LF", "CF", "RF", "Others"),),
      textInput("player_name", "Player Name Filter:", placeholder = "First Name or Last Name"),
      actionButton("search", "Search", class = "btn-primary"),
      actionButton("reset", "Reset", class = "btn btn-warning")
    ),
    mainPanel(
      shinycssloaders::withSpinner(DTOutput("player_table")),
      shinycssloaders::withSpinner(leafletOutput("mlb_map", height = "600px"))
    )
  )
)

server <- function(input, output, session) {
  search_results <- eventReactive(input$search, {
    req(input$team)
    
    # Get base player list
    players <- players_combined %>%
      select(playerID, nameFirst, nameLast, role)
    
    # Get team associations
    query <- players %>%
      inner_join(
        bind_rows(
          Batting %>% filter(yearID >= 2000) %>% select(playerID, teamID),
          Pitching %>% filter(yearID >= 2000) %>% select(playerID, teamID)
        ),
        by = "playerID"
      ) %>%
      filter(teamID == input$team)
    
    # Apply position filter
    if (!is.null(input$role)) {
      selected_role <- ifelse(input$role == "Others", "Other", input$role)
      query <- query %>% filter(role == selected_role)
    }
    
    # Apply name filter
    if (nchar(input$player_name) > 0) {
      query <- query %>%
        filter(grepl(tolower(input$player_name), 
                     tolower(paste(nameFirst, nameLast))))
    }
    
    query %>%
      distinct() %>%
      arrange(role, nameLast, nameFirst)
  })
  
  output$player_table <- renderDT({
    datatable(
      search_results() %>% 
        select(nameFirst, nameLast, role),
      options = list(
        pageLength = 5,
        autoWidth = TRUE,
        dom = 'tip'
      )
    )
  })
  
  output$mlb_map <- renderLeaflet({
    geo_data <- get_geo_data()
    
    leaflet(geo_data) %>%
      addTiles() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        icon = icons(
          iconUrl = ~LogoURL,
          iconWidth = 30, 
          iconHeight = 30,
          iconAnchorX = 15,
          iconAnchorY = 15
        ),
        popup = ~paste0(
          "<b>", Team, "</b><br>",
          "<img src='", LogoURL, "' width='100'>"
        )
      )
  })
  
  observeEvent(input$mlb_map_marker_click, {
    click <- input$mlb_map_marker_click
    team <- get_geo_data()$Team[
      get_geo_data()$Latitude == click$lat &
        get_geo_data()$Longitude == click$lng
    ]
    updateSelectInput(session, "team", selected = team)
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "team", selected = "")
    updateTextInput(session, "player_name", value = "")
    updateSelectInput(session, "role", selected = "")
  })
}

shinyApp(ui = ui, server = server)

