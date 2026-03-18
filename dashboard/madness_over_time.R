madness_over_time <- function(input, output, session) {

  if (input$tournament == 'Men' & !exists("all_standings_men")) {all_standings_men <<- read_csv("http://raw.githubusercontent.com/jbccre/madness/refs/heads/main/men/all_standings.csv", col_names = FALSE)}
  if (input$tournament == 'Women' & !exists("all_standings_women")) {all_standings_women <<- read_csv("http://raw.githubusercontent.com/jbccre/madness/refs/heads/main/women/all_standings.csv", col_names = FALSE)}
  if (input$tournament == "Men") {all_standings <- all_standings_men} else {all_standings <- all_standings_women}
  colnames(all_standings) <- c("player", "points", "first", "second", "third", "market_value", "timestamp")
  all_standings$timestamp <- with_tz(all_standings$timestamp, "UTC")
  all_standings$t <- match(all_standings$timestamp, sort(unique(all_standings$timestamp)))
  
  ticks <- read_csv("http://raw.githubusercontent.com/jbccre/madness/refs/heads/main/dashboard_ticks.csv") |>
    mutate(time = as.POSIXct(time)) |>
    filter(time<Sys.time()) |>
    mutate(timestamp = NA) |>
    filter(tournament == tolower(input$tournament))

  for (i in 1:nrow(ticks)) {
     ticks$timestamp[i] <- as.character(sort(unique(all_standings$timestamp[all_standings$timestamp < ticks$time[i]]),decreasing=T)[1])
  } 
  ticks$timestamp <- as.POSIXct(ticks$timestamp, tz = "America/Chicago")
  ticks$t <- all_standings$t[match(ticks$timestamp, all_standings$timestamp)]

  yvar <- c("first","second","third","market_value","points")[match(
    input$outcome_overtime,
    c("Probability, First Place", "Probability, Second Place", "Probability, Third Place", "Market Value", "Points")
  )]
  all_standings$madness <- unlist(all_standings[,yvar])
  if (grepl("Probability", input$outcome_overtime)) {all_standings$pretty_madness <- scales::percent(all_standings$madness, accuracy = .01) }
  if (grepl("Market", input$outcome_overtime)) {all_standings$pretty_madness <- scales::dollar(all_standings$madness) }
  if (grepl("Points", input$outcome_overtime)) {all_standings$pretty_madness <- prettyNum(all_standings$madness, big.mark = ',') }

  output$plotly_out <- renderPlotly({
       {all_standings |>
        select(Player=player,madness,t,Madness=pretty_madness) |>
        ggplot(aes(x = t, y = madness, group = Player, color = Player, customdata = Madness)) +
        geom_line() +
        labs(x = '', y = '') +
        scale_x_continuous(breaks = ticks$t, labels = ticks$time_label) +
        (if (grepl("Probability", input$outcome_overtime)) {
          scale_y_continuous(labels=scales::percent)} else 
          {NULL}) +
        (if (grepl("Market", input$outcome_overtime)) {
          scale_y_continuous(labels=scales::dollar)} else 
          {NULL}) +
         (if (grepl("Points", input$outcome_overtime)) {
          scale_y_continuous(labels=scales::comma)} else 
          {NULL}) } |>
        ggplotly(tooltip = c("customdata", "colour")) 
       })
  
  tagList(
    h3(input$outcome_overtime),
    plotlyOutput("plotly_out", height = "80vh")
  )

}