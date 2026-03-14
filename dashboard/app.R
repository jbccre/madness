library(shiny)
library(shinyWidgets)
library(readr)
library(dplyr)
library(tidyr)
library(DT)

ui <- fluidPage(
  titlePanel("MARCH MADNESS!!!!!"),
  sidebarLayout(
    sidebarPanel(
      radioGroupButtons(inputId = 'tournament', label = 'Tournament:', choices = c("Men", "Women"), selected = "Men"),
      radioGroupButtons(inputId = 'mode', label = 'Explore the Madness:', direction = 'vertical',
         choices = c("Current Standings", "Madness Over Time", "Current Bracket", "Explore Submitted Brackets", "Simulate Scenario"), 
         selected = "Current Standings"),
      conditionalPanel(
        condition = "input.mode == 'Madness Over Time'",
        radioGroupButtons(inputId = 'outcome', direction = 'vertical', label = "View Outcome Over Time:", selected = "Probability, First Place",
        choices = c("Probability, First Place",
                    "Probability, Second Place",
                    "Probability, Third Place",
                    "Market Value",
                     "Points"))),
      conditionalPanel(
        condition = "input.mode == 'Explore Submitted Brackets'",
        selectInput(inputId = 'player', label = "View Bracket Submitted By:", choices = "", selected = ""),
        downloadButton("download_submitted_bracket", "Download Bracket")),
      conditionalPanel(
        condition = "input.mode == 'Current Bracket'",
        downloadButton("download_current_bracket", "Download Bracket")
      ),
      conditionalPanel(
        condition = "input.mode == 'Simulate Scenario'",
          tags$div(class = "sim-scenario",
          tags$style(".sim-scenario .form-group { margin-bottom: 0 !important;}"),
        tags$label(class = "control-label", "Simulation Assumptions:"),
        fluidRow(
          style = "margin-top:-20px;",
          column(4, style = "padding:0;", selectInput("filter_team1", label = '', choices = '', selected = '')), 
          column(2, style = "padding:0;", selectInput("filter_logic1", label = '', choices = c('is', 'is not'), selected = 'is')), 
          column(6, style = "padding:0;", selectInput("filter_outcome1", label = '', choices = c('', 'the champion', 'in the championship game', 'in the final four', 'in the elite eight', 'in the sweet 16'), selected = '')) 
        ),        
        fluidRow(
          column(4, style = "padding:0;", selectInput("filter_team2", label = '', choices = '',  selected = '')), 
          column(2, style = "padding:0;", selectInput("filter_logic2", label = '', choices = c('is', 'is not'), selected = 'is')), 
          column(6, style = "padding:0;", selectInput("filter_outcome2", label = '', choices = c('', 'the champion', 'in the championship game', 'in the final four', 'in the elite eight', 'in the sweet 16'), selected = '')) 
        ),
        fluidRow(
          column(4, style = "padding:0;", selectInput("filter_team3", label = '', choices = '', selected = '')), 
          column(2, style = "padding:0;", selectInput("filter_logic3", label = '', choices = c('is', 'is not'), selected = 'is')), 
          column(6, style = "padding:0;", selectInput("filter_outcome3", label = '', choices = c('', 'the champion', 'in the championship game', 'in the final four', 'in the elite eight', 'in the sweet 16'), selected = ''))
        ),
        actionButton(inputId = 'reset', label='Reset Assumptions')
      )),
      tags$div(
       style = "margin-top:20px; color:#666; font-size:12px; text-align:left;",
       tags$a(href = "https://www.github.com/jbccre/madness", target = "_blank","Click here"), " for code and data.")
    ),
    mainPanel(
      uiOutput("content")
    ),
  )
)

server <- function(input, output, session) {
  
  current_standings_men <- read_csv("http://raw.githubusercontent.com/jbccre/madness/refs/heads/main/men/current_standings.csv")
  current_standings_women <- read_csv("http://raw.githubusercontent.com/jbccre/madness/refs/heads/main/women/current_standings.csv")
  gamestate_men <- read_csv("http://raw.githubusercontent.com/jbccre/madness/refs/heads/main/men/gamestate.csv")
  gamestate_women <- read_csv("http://raw.githubusercontent.com/jbccre/madness/refs/heads/main/women/gamestate.csv")
  players_men <- unlist(current_standings_men$name)
  players_women <- unlist(current_standings_women$name)
  teams_men <- unlist(sort(unique(gamestate_men$elo_name)))
  teams_women <- unlist(sort(unique(gamestate_women$elo_name)))
  source("madness_over_time.R")
  source("current_bracket.R")
  source("explore_submitted_brackets.R")
  source("simulate_scenario.R")
  source("current_standings.R")

  observeEvent(input$tournament, {
    updateRadioGroupButtons(session, inputId='mode', selected='Current Standings')
    if (input$tournament == "Men") {updateSelectInput(session, inputId = 'filter_team1', choices = c("\u2800", teams_men))}
    if (input$tournament == "Men") {updateSelectInput(session, inputId = 'filter_team2', choices = c("\u2800", teams_men))}
    if (input$tournament == "Men") {updateSelectInput(session, inputId = 'filter_team3', choices = c("\u2800", teams_men))}
    if (input$tournament == "Women") {updateSelectInput(session, inputId = 'filter_team1', choices = c("\u2800", teams_women))}
    if (input$tournament == "Women") {updateSelectInput(session, inputId = 'filter_team2', choices = c("\u2800", teams_women))}
    if (input$tournament == "Women") {updateSelectInput(session, inputId = 'filter_team3', choices = c("\u2800", teams_women))}
  })

  observeEvent(input$reset, {
    updateSelectInput(session, inputId = 'filter_team1', selected = '\u2800')
    updateSelectInput(session, inputId = 'filter_team2', selected = '\u2800')
    updateSelectInput(session, inputId = 'filter_team3', selected = '\u2800')
    updateSelectInput(session, inputId = 'filter_logic1', selected = 'is')
    updateSelectInput(session, inputId = 'filter_logic2', selected = 'is')
    updateSelectInput(session, inputId = 'filter_logic3', selected = 'is')
    updateSelectInput(session, inputId = 'filter_outcome1', selected = '')
    updateSelectInput(session, inputId = 'filter_outcome2', selected = '')
    updateSelectInput(session, inputId = 'filter_outcome3', selected = '')
  })


  output$content <- renderUI({
    mode <- input$mode
    if (mode == "Madness Over Time") {madness_over_time(input, output, session)} else {
      if (mode == "Current Bracket") {current_bracket(input, output, session)} else {
        if (mode == "Explore Submitted Brackets") {explore_submitted_brackets(input, output, session)} else {
          if (mode == "Simulate Scenario") {simulate_scenario(input, output, session, teams_men, teams_women, players_men, players_women)} else {
            current_standings(input, output, session, current_standings_men, current_standings_women)
   }}}}})
  
}

shinyApp(ui, server)