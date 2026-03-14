simulate_scenario <- function(input, output, session, teams_men, teams_women, players_men, players_women) {

  if (input$tournament == 'Men' & !exists("thousands_men")) {thousands_men <<- read_csv("http://raw.githubusercontent.com/jbccre/madness/refs/heads/main/men/simulation.csv")}
  if (input$tournament == 'Women' & !exists("thousands_women")) {thousands_women <<- read_csv("http://raw.githubusercontent.com/jbccre/madness/refs/heads/main/women/simulation.csv")}
  if (input$tournament == "Men") {filtered_data <- thousands_men} else {filtered_data <- thousands_women}
  if (input$tournament == "Men") {n_players <- length(players_men)} else {n_players <- length(players_women)}
  n_rows <- nrow(filtered_data)

  valid1 <- input$filter_team1 != "\u2800" & input$filter_outcome1 != ''
  valid2 <- input$filter_team2 != "\u2800" & input$filter_outcome2 != ''
  valid3 <- input$filter_team3 != "\u2800" & input$filter_outcome3 != ''
  
  # ASSUMPTION 1

  if (valid1 & input$filter_outcome1 == 'the champion') {
    if (input$filter_logic1 == 'is') {
      filtered_data <- filter(filtered_data, `701` == input$filter_team1)
    } else {filtered_data <- filter_out(filtered_data, `701` == input$filter_team1)
  }}

  if (valid1 & input$filter_outcome1 == 'in the championship game') {
    if (input$filter_logic1 == 'is') {
      filtered_data <- filter(filtered_data, `601` == input$filter_team1 | `602` == input$filter_team1) 
    } else {filtered_data <- filter_out(filtered_data, `601` == input$filter_team1 | `602` == input$filter_team1)
  }}

  if (valid1 & input$filter_outcome1 == 'in the final four') {
    if (input$filter_logic1 == 'is') {
      filtered_data <- filter(filtered_data, `501` == input$filter_team1 | `502` == input$filter_team1 | `503` == input$filter_team1 | `504` == input$filter_team1) 
    } else {filtered_data <- filter_out(filtered_data, `501` == input$filter_team1 | `502` == input$filter_team1 | `503` == input$filter_team1 | `504` == input$filter_team1)
  }}

  if (valid1 & input$filter_outcome1 == 'in the elite eight') {
    if (input$filter_logic1 == 'is') {
      filtered_data <- filter(filtered_data, `401` == input$filter_team1 | `402` == input$filter_team1 | `403` == input$filter_team1 | `404` == input$filter_team1 | `405` == input$filter_team1 | `406` == input$filter_team1 | `407` == input$filter_team1 | `408` == input$filter_team1)
    } else {filtered_data <- filter_out(filtered_data, `401` == input$filter_team1 | `402` == input$filter_team1 | `403` == input$filter_team1 | `404` == input$filter_team1 | `405` == input$filter_team1 | `406` == input$filter_team1 | `407` == input$filter_team1 | `408` == input$filter_team1)
  }}

    if (valid1 & input$filter_outcome1 == 'in the sweet 16') {
    if (input$filter_logic1 == 'is') {
      filtered_data <- filter(filtered_data, `301` == input$filter_team1 | `302` == input$filter_team1 | `303` == input$filter_team1 | `304` == input$filter_team1 | `305` == input$filter_team1 | `306` == input$filter_team1 | `307` == input$filter_team1 | `308` == input$filter_team1 |  `309` == input$filter_team1 | `310` == input$filter_team1 | `311` == input$filter_team1 | `312` == input$filter_team1 | `313` == input$filter_team1 | `314` == input$filter_team1 | `315` == input$filter_team1 | `316` == input$filter_team1)
    } else {filtered_data <- filter_out(filtered_data, `301` == input$filter_team1 | `302` == input$filter_team1 | `303` == input$filter_team1 | `304` == input$filter_team1 | `305` == input$filter_team1 | `306` == input$filter_team1 | `307` == input$filter_team1 | `308` == input$filter_team1 |  `309` == input$filter_team1 | `310` == input$filter_team1 | `311` == input$filter_team1 | `312` == input$filter_team1 | `313` == input$filter_team1 | `314` == input$filter_team1 | `315` == input$filter_team1 | `316` == input$filter_team1)
  }}

  # ASSUMPTION 2

  if (valid2 & input$filter_outcome2 == 'the champion') {
    if (input$filter_logic2 == 'is') {
      filtered_data <- filter(filtered_data, `701` == input$filter_team2)
    } else {filtered_data <- filter_out(filtered_data, `701` == input$filter_team2)
  }}

  if (valid2 & input$filter_outcome2 == 'in the championship game') {
    if (input$filter_logic2 == 'is') {
      filtered_data <- filter(filtered_data, `601` == input$filter_team2 | `602` == input$filter_team2) 
    } else {filtered_data <- filter_out(filtered_data, `601` == input$filter_team2 | `602` == input$filter_team2)
  }}

  if (valid2 & input$filter_outcome2 == 'in the final four') {
    if (input$filter_logic2 == 'is') {
      filtered_data <- filter(filtered_data, `501` == input$filter_team2 | `502` == input$filter_team2 | `503` == input$filter_team2 | `504` == input$filter_team2) 
    } else {filtered_data <- filter_out(filtered_data, `501` == input$filter_team2 | `502` == input$filter_team2 | `503` == input$filter_team2 | `504` == input$filter_team2)
  }}

  if (valid2 & input$filter_outcome2 == 'in the elite eight') {
    if (input$filter_logic2 == 'is') {
      filtered_data <- filter(filtered_data, `401` == input$filter_team2 | `402` == input$filter_team2 | `403` == input$filter_team2 | `404` == input$filter_team2 | `405` == input$filter_team2 | `406` == input$filter_team2 | `407` == input$filter_team2 | `408` == input$filter_team2)
    } else {filtered_data <- filter_out(filtered_data, `401` == input$filter_team2 | `402` == input$filter_team2 | `403` == input$filter_team2 | `404` == input$filter_team2 | `405` == input$filter_team2 | `406` == input$filter_team2 | `407` == input$filter_team2 | `408` == input$filter_team2)
  }}

    if (valid2 & input$filter_outcome2 == 'in the sweet 16') {
    if (input$filter_logic2 == 'is') {valid2
      filtered_data <- filter(filtered_data, `301` == input$filter_team2 | `302` == input$filter_team2 | `303` == input$filter_team2 | `304` == input$filter_team2 | `305` == input$filter_team2 | `306` == input$filter_team2 | `307` == input$filter_team2 | `308` == input$filter_team2 |  `309` == input$filter_team2 | `310` == input$filter_team2 | `311` == input$filter_team2 | `312` == input$filter_team2 | `313` == input$filter_team2 | `314` == input$filter_team2 | `315` == input$filter_team2 | `316` == input$filter_team2)
    } else {filtered_data <- filter_out(filtered_data, `301` == input$filter_team2 | `302` == input$filter_team2 | `303` == input$filter_team2 | `304` == input$filter_team2 | `305` == input$filter_team2 | `306` == input$filter_team2 | `307` == input$filter_team2 | `308` == input$filter_team2 |  `309` == input$filter_team2 | `310` == input$filter_team2 | `311` == input$filter_team2 | `312` == input$filter_team2 | `313` == input$filter_team2 | `314` == input$filter_team2 | `315` == input$filter_team2 | `316` == input$filter_team2)
  }}

  # ASSUMPTION 3

  if (valid3 & input$filter_outcome3 == 'the champion') {
    if (input$filter_logic3 == 'is') {
      filtered_data <- filter(filtered_data, `701` == input$filter_team3)
    } else {filtered_data <- filter_out(filtered_data, `701` == input$filter_team3)
  }}

  if (valid3 & input$filter_outcome3 == 'in the championship game') {
    if (input$filter_logic3 == 'is') {
      filtered_data <- filter(filtered_data, `601` == input$filter_team3 | `602` == input$filter_team3) 
    } else {filtered_data <- filter_out(filtered_data, `601` == input$filter_team3 | `602` == input$filter_team3)
  }}

  if (valid3 & input$filter_outcome3 == 'in the final four') {
    if (input$filter_logic3 == 'is') {
      filtered_data <- filter(filtered_data, `501` == input$filter_team3 | `502` == input$filter_team3 | `503` == input$filter_team3 | `504` == input$filter_team3) 
    } else {filtered_data <- filter_out(filtered_data, `501` == input$filter_team3 | `502` == input$filter_team3 | `503` == input$filter_team3 | `504` == input$filter_team3)
  }}

  if (valid3 & input$filter_outcome3 == 'in the elite eight') {
    if (input$filter_logic3 == 'is') {
      filtered_data <- filter(filtered_data, `401` == input$filter_team3 | `402` == input$filter_team3 | `403` == input$filter_team3 | `404` == input$filter_team3 | `405` == input$filter_team3 | `406` == input$filter_team3 | `407` == input$filter_team3 | `408` == input$filter_team3)
    } else {filtered_data <- filter_out(filtered_data, `401` == input$filter_team3 | `402` == input$filter_team3 | `403` == input$filter_team3 | `404` == input$filter_team3 | `405` == input$filter_team3 | `406` == input$filter_team3 | `407` == input$filter_team3 | `408` == input$filter_team3)
  }}

    if (valid3 & input$filter_outcome3 == 'in the sweet 16') {
    if (input$filter_logic3 == 'is') {
      filtered_data <- filter(filtered_data, `301` == input$filter_team3 | `302` == input$filter_team3 | `303` == input$filter_team3 | `304` == input$filter_team3 | `305` == input$filter_team3 | `306` == input$filter_team3 | `307` == input$filter_team3 | `308` == input$filter_team3 |  `309` == input$filter_team3 | `310` == input$filter_team3 | `311` == input$filter_team3 | `312` == input$filter_team3 | `313` == input$filter_team3 | `314` == input$filter_team3 | `315` == input$filter_team3 | `316` == input$filter_team3)
    } else {filtered_data <- filter_out(filtered_data, `301` == input$filter_team3 | `302` == input$filter_team3 | `303` == input$filter_team3 | `304` == input$filter_team3 | `305` == input$filter_team3 | `306` == input$filter_team3 | `307` == input$filter_team3 | `308` == input$filter_team3 |  `309` == input$filter_team3 | `310` == input$filter_team3 | `311` == input$filter_team3 | `312` == input$filter_team3 | `313` == input$filter_team3 | `314` == input$filter_team3 | `315` == input$filter_team3 | `316` == input$filter_team3)
  }}

if (!valid1 & !valid2 & !valid3) {

  out <- tagList(
    h3("Simulate Scenario"),
    p("Please enter simulation assumptions.")
  )

} else {

  out <- tagList(
    h3("Simulation Results"),

    # Assumptions block
    p(
      if (valid1) tagList("Assumption: ", input$filter_team1, " ", input$filter_logic1, " ", input$filter_outcome1, br()),
      if (valid2) tagList("Assumption: ", input$filter_team2, " ", input$filter_logic2, " ", input$filter_outcome2, br()),
      if (valid3) tagList("Assumption: ", input$filter_team3, " ", input$filter_logic3, " ", input$filter_outcome3, br())
    ),

    # Summary line
    p(
      "In ", n_rows, " iterations of March Madness, this scenario occurs ",
      nrow(filtered_data), " times."
    ),

    # No results
    if (nrow(filtered_data) == 0)
      {p("No results found. Your assumptions are either impossible or highly improbable. Please try again.")},

    # Warning
    if (nrow(filtered_data) > 0 & nrow(filtered_data) < 100) 
      {p("Warning: Simulation unstable; relies on fewer than 100 simulated scenarios.")},

    if (nrow(filtered_data > 0)) {
      output$simulation_table_out <- renderDT(server = FALSE, {
       filtered_data |>
        select_at(68:ncol(filtered_data)) |>
        mutate(row=1:nrow(filtered_data)) |>
        pivot_longer(cols=1:(ncol(filtered_data)-67)) |>
        arrange(desc(value)) |>
        group_by(row) |>
        mutate(first = (value==value[1])) |>
        mutate(second = (value==value[2])) |>
        mutate(third = (value==value[3])) |>
        mutate(first = first/sum(first)) |>
        mutate(second = second/sum(second)) |>
        mutate(third = third/sum(third)) |>
        ungroup() |>
        group_by(name) |>
        summarise(first = sum(first)/nrow(filtered_data),
                  second = sum(second)/nrow(filtered_data),
                  third = sum(third)/nrow(filtered_data)
        ) |>
        mutate(name = gsub("^points_","",name)) |>
        mutate(market_value = n_players*(7*first+2*second+1*third)) |>
        arrange(desc(first), desc(market_value)) |>
         select(Player = name, `Probability, First Place` = first, `Probability, Second Place` = second,
           `Probability, Third Place` = third, `Market Value` = market_value) |>
         datatable(extensions = 'Buttons', rownames = FALSE, 
              options = list(dom = 'Bfrtip',
              buttons = list(list(
                extend='excel', 
                title = "Simulation Results",
                filename = "standings",
                exportOptions = list(modifier = list(page = 'all')),
                text = 'Download Simulation Results')))) |>
    formatPercentage("Probability, First Place", digits = 2) |>
    formatPercentage("Probability, Second Place", digits = 2) |>
    formatPercentage("Probability, Third Place", digits = 2) |>
    formatCurrency("Market Value", currency = '$', interval = 3, digits = 2, mark = ',')
              })
      DTOutput("simulation_table_out")
    }
  )
  out
}
}
  