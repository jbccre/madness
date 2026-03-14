library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  titlePanel("MARCH MADNESS!!!!!"),
  sidebarLayout(
    sidebarPanel(
      radioGroupButtons(inputId = 'tournament', label = 'Tournament:', choices = c("Men", "Women"), selected = "Men"),
      radioGroupButtons(inputId = 'mode', label = 'Explore the Madness:', direction = 'vertical',
         choices = c("Current Standings", "Madness Over Time", "Current Bracket", "Explore Submitted Brackets", "Simulate Scenario"), 
         selected = "Current Standings"),
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

  output$content <- renderUI({
    mode <- input$mode
    source("madness_over_time.R")
    source("current_bracket.R")
    source("explore_submitted_brackets.R")
    source("simulate_scenario.R")
    source("current_standings.R")
    if (mode == "Madness Over Time") {madness_over_time(input, output, session)} else {
      if (mode == "Current Bracket") {current_bracket(input, output, session)} else {
        if (mode == "Explore Submitted Brackets") {explore_submitted_brackets(input, output, session)} else {
          if (mode == "Simulate Scenario") {simulate_scenario(input, output, session)} else {
            current_standings(input, output, session)
   }}}}})
  
}

shinyApp(ui, server)
  