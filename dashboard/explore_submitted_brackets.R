explore_submitted_brackets <- function(input, output, session, gamestate_men, gamestate_women, current_standings_men, current_standings_women) {

  if (!exists("submitted_men")) {submitted_men <<- read_csv("http://raw.githubusercontent.com/jbccre/madness/refs/heads/main/men/submitted_brackets.csv")}
  if (!exists("submitted_women")) {submitted_women <<- read_csv("http://raw.githubusercontent.com/jbccre/madness/refs/heads/main/women/submitted_brackets.csv")}  
  if (input$tournament == "Men") {gamestate <- gamestate_men} else {gamestate <- gamestate_women}
  if (input$tournament == "Men") {submitted <- submitted_men} else {submitted <- submitted_women}
  if (input$tournament == "Men") {cs <- current_standings_men} else {cs <- current_standings_women}
  bracket_to_draw <- filter(submitted, X68 == input$player)
  cs <- filter(cs, name == input$player)

  # draw cols 2:7
  bracket_to_draw2 <- bracket_to_draw |>
    select(1:68) |>
    pivot_longer(cols=1:67) |>
    mutate(bracketPositionId = unique(gamestate$bracketPositionId)) |>
    mutate(victorBracketPositionId = gamestate$victorBracketPositionId[match(bracketPositionId, gamestate$bracketPositionId)]) |>
    left_join(gamestate |> filter(isWinner) |> select(bracketPositionId, elo_name)) |>
    mutate(color = case_when(
      value == elo_name ~ "green",
      is.element(value, value[value!=elo_name & !is.na(elo_name)]) ~ "red",
      .default = 'black'
    )) |>
    mutate(x = as.integer(substr(bracketPositionId, 1,1))) |>
    mutate(x = case_when(x == 1 ~ NA, .default = x)) |>
    mutate(y = as.integer(substr(bracketPositionId, 2,3))) |>
    mutate(y = y*2^(x-1) - 2^(x-2)) |>
    mutate(x_strike1 = case_when(color=='red' ~ x - .2, .default = NA)) |>
    mutate(x_strike2 = case_when(color=='red' ~ x + .2, .default = NA)) |>
    mutate(fontface = case_when(
      color %in% c("blue", "green") ~ "bold", .default = 'plain'
    )) |>
    mutate(value = ifelse(
      fontface == 'bold', paste0("**",value,"**"),
      value
    ))

  bracket_to_draw1 <- gamestate |> 
    mutate(bracketPosition = ifelse(isTop, "Top", "Bottom")) |>
    mutate(play_in = is.element(paste(bracketPositionId, bracketPosition), paste(victorBracketPositionId, victorGamePosition)[bracketPositionId<200])) |>
    filter(substr(bracketPositionId,1,1)=='2') |>
    left_join({gamestate |> 
      filter(bracketPositionId < 200) |>
      select(precedingBracketId = bracketPositionId, bracketPositionId = victorBracketPositionId) |>
      distinct()
    }) |>
    mutate(precedingBracketId = case_when(play_in ~ precedingBracketId, .default = NA)) |>
    left_join({
      bracket_to_draw |>
        select(1:4) |>
        rename_with(~as.character(101:104)) |>
        pivot_longer(cols=1:4) |>
        rename(precedingBracketId = name, selectedPreviousBracket = value) |>
        mutate(precedingBracketId = as.integer(precedingBracketId))
    }) |>
    mutate(display_name = case_when(play_in ~ selectedPreviousBracket, .default = elo_name)) |>
    mutate(x = 1, y = (1:64)-.5, color = case_when(
       !play_in ~ gray(.4),
       is.na(elo_name) ~ "blue",
       elo_name == selectedPreviousBracket ~ "green",
       .default = 'red')) |>
    mutate(x_strike1 = case_when(color=='red' ~ x - .2, .default = NA)) |>
    mutate(x_strike2 = case_when(color=='red' ~ x + .2, .default = NA)) |>
    mutate(fontface = case_when(
      color %in% c("blue", "green") ~ "bold", .default = 'plain'
    )) |>
    mutate(display_name = ifelse(
      fontface == 'bold', paste0("**",display_name,"**"),
      display_name
    ))

  line_segments <- rbind(
    data.frame(x=2.35,xend=2.5,y=seq(1,63,2),yend=seq(1,63,2)),
    data.frame(x=2.5,xend=2.5,y=seq(1,63,4),yend=seq(3,64,4)),
    data.frame(x=3.35,xend=3.5,y=seq(2,64,4),yend=seq(2,64,4)),
    data.frame(x=3.5,xend=3.5,y=seq(2,60,8),yend=seq(6,64,8)),
    data.frame(x=3.5,xend=3.6,y=seq(4,64,8),yend=seq(4,64,8)),
    data.frame(x=4.35,xend=4.5,y=seq(4,64,8),yend=seq(4,64,8)),
    data.frame(x=4.5,xend=4.5,y=seq(4,56,16),yend=seq(12,64,16)),
    data.frame(x=4.5,xend=4.6,y=seq(8,64,16),yend=seq(8,64,16)),
    data.frame(x=5.35,xend=5.5,y=seq(8,64,16),yend=seq(8,64,16)),
    data.frame(x=5.5,xend=5.5,y=seq(8,50,32),yend=seq(24,64,32)),
    data.frame(x=5.5,xend=5.6,y=seq(16,64,32),yend=seq(16,64,32)),
    data.frame(x=6.35,xend=6.5,y=c(16,48),yend=c(16,48)),
    data.frame(x=6.5,xend=6.5,y=16,yend=48),
    data.frame(x=6.5,xend=6.6,y=32,yend=32),
    data.frame(x=1.35,xend=1.5,y=(1:64)-.5,yend=(1:64)-.5),
    data.frame(x=1.5,xend=1.5,y=seq(.5,63.5,2),yend=seq(1.5,64.5,2)),
    data.frame(x=1.5,xend=1.6,y=seq(1,64,2),yend=seq(1,64,2)),
    data.frame(x=2.5,xend=2.6,y=seq(2,64,4),yend=seq(2,64,4))) |>
    mutate(color='black') |>
    bind_rows({  bracket_to_draw1 |> 
      filter(!is.na(x_strike1)) |>
      select(x = x_strike1, xend = x_strike2, y = y, yend = y) |>
      mutate(color = 'red')}) |>
    bind_rows({  bracket_to_draw2 |> 
      filter(!is.na(x_strike1)) |>
      select(x = x_strike1, xend = x_strike2, y = y, yend = y) |>
      mutate(color = 'red')}) 
  
  drawn_bracket <-   
    ggplot() +
    scale_x_continuous(limits = c(0.8,7.2)) +
    geom_richtext(data = bracket_to_draw1, aes(x = x, y = y, label = display_name, color = color), fill = NA, label.color = NA) +
    geom_richtext(data = bracket_to_draw2, aes(x = x, y = y, label = value, color = color), fill = NA, label.color = NA) +
    scale_color_manual(values = c("red"="red","black"="black","green"="darkgreen",'blue'='blue')) +
    geom_segment(data = line_segments, aes(x=x,xend=xend,y=y,yend=yend,color=color)) + 
    labs(title = bracket_to_draw$X68[1],
         subtitle = paste0("Probability of First: ", scales::percent(cs$first[1], accuracy = .01), 
                           "\nCurrent Points: ", scales::comma(cs$current_points[1]), 
                          "\nMarket Value: ",scales::dollar(cs$market_value[1]))) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = 'none',
      plot.title = element_text(size=20, face='bold')
    ) 

  output$download_submitted_bracket <- downloadHandler(
  filename = function() {
    paste0(input$player, "_bracket.png")
  },
  content = function(file) {
    png(file, width=1500,height=2000, res=150)
    print(drawn_bracket)
    dev.off()
  }
  )
  
  output$drawn_bracket <- renderPlot({drawn_bracket})
  
  tagList(
    plotOutput("drawn_bracket", height = "80vh")
  )

}
