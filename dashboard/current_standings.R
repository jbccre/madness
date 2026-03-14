current_standings <- function(input, output, session, current_standings_men, current_standings_women) {

  if (input$tournament == "Women") {my_table <- current_standings_women} else {my_table <- current_standings_men}

  output$current_standings_out <- renderDT(server = FALSE, {my_table |>
    select(!timestamp) |>
    arrange(desc(first)) |>
    select(Player = name, `Probability, First Place` = first, `Probability, Second Place` = second,
           `Probability, Third Place` = third, Points = current_points, `Market Value` = market_value) |>
    datatable(extensions = 'Buttons', rownames = FALSE, 
              options = list(dom = 'Bfrtip',
              buttons = list(list(
                extend='excel', 
                title = NULL,
                filename = "standings",
                exportOptions = list(modifier = list(page = 'all')),
                text = 'Download Current Standings')))) |>
    formatPercentage("Probability, First Place", digits = 2) |>
    formatPercentage("Probability, Second Place", digits = 2) |>
    formatPercentage("Probability, Third Place", digits = 2) |>
    formatCurrency("Points", currency = '', interval = 3, digits = 0, mark = ',') |>
    formatCurrency("Market Value", currency = '$', interval = 3, digits = 2, mark = ',')})

  tagList(
    h3(paste0("Current Standings - ",input$tournament,"'s Tournament")),
    DTOutput("current_standings_out"))
  
}