explore_submitted_brackets <- function(input, output, session, gamestate_men, gamestate_women) {

  if (!exists("submitted_men")) {submitted_men <<- read_csv("http://raw.githubusercontent.com/jbccre/madness/refs/heads/main/men/submitted_brackets.csv")}
  if (!exists("submitted_women")) {submitted_women <<- read_csv("http://raw.githubusercontent.com/jbccre/madness/refs/heads/main/women/submitted_brackets.csv")}  
  if (input$tournament == "Men") {gamestate <- gamestate_men} else {gamestate <- gamestate_women}
  if (input$tournament == "Men") {submitted <- submitted_men} else {submitted <- submitted_women}

  bracket_to_draw <- filter(submitted, X68 == input$player)

  bracket_to_draw <- bracket_to_draw |>
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
    mutate(x_strike2 = case_when(color=='red' ~ x + .2, .default = NA))|>
    mutate(y = case_when(is.na(y) ~ 2*as.integer(substr(victorBracketPositionId,2,3)), .default=y))

  drawn_bracket <- 
    bracket_to_draw |>
    ggplot() +
    scale_x_continuous(limits = c(0.8,7.2)) +
          geom_segment(aes(x = x_strike1, xend = x_strike2, y = y), color='red') +
    geom_text(aes(x = x, y = y, label = value, color = color)) +

    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = 'none',
      plot.title = element_text(size=20, face='bold')
    ) +

    scale_color_manual(values = c("red"="red","black"="black","green"="darkgreen",'blue'='blue')) +
    labs(title = bracket_to_draw$X68[1]) +
    geom_segment(
      data = data.frame(x=2.3,xend=2.5,y=seq(1,63,2),yend=seq(1,63,2)),
      aes(x=x,xend=xend,y=y,yend=yend)
    ) +
    geom_segment(
      data = data.frame(x=2.5,xend=2.5,y=seq(1,63,4),yend=seq(3,64,4)),
      aes(x=x,xend=xend,y=y,yend=yend)
    ) +
    geom_segment(
      data = data.frame(x=2.5,xend=2.7,y=seq(2,64,4),yend=seq(2,64,4)),
      aes(x=x,xend=xend,y=y,yend=yend)
    ) +
    geom_segment(
      data = data.frame(x=3.3,xend=3.5,y=seq(2,64,4),yend=seq(2,64,4)),
      aes(x=x,xend=xend,y=y,yend=yend)
    ) +
    geom_segment(
      data = data.frame(x=3.5,xend=3.5,y=seq(2,60,8),yend=seq(6,64,8)),
      aes(x=x,xend=xend,y=y,yend=yend)
    ) +
    geom_segment(
      data = data.frame(x=3.5,xend=3.7,y=seq(4,64,8),yend=seq(4,64,8)),
      aes(x=x,xend=xend,y=y,yend=yend)
    ) +
    geom_segment(
      data = data.frame(x=4.3,xend=4.5,y=seq(4,64,8),yend=seq(4,64,8)),
      aes(x=x,xend=xend,y=y,yend=yend)
    ) +
    geom_segment(
      data = data.frame(x=4.5,xend=4.5,y=seq(4,56,16),yend=seq(12,64,16)),
      aes(x=x,xend=xend,y=y,yend=yend)
    ) +
    geom_segment(
      data = data.frame(x=4.5,xend=4.7,y=seq(8,64,16),yend=seq(8,64,16)),
      aes(x=x,xend=xend,y=y,yend=yend)
    ) +
    geom_segment(
      data = data.frame(x=5.3,xend=5.5,y=seq(8,64,16),yend=seq(8,64,16)),
      aes(x=x,xend=xend,y=y,yend=yend)
    ) +
    geom_segment(
      data = data.frame(x=5.5,xend=5.5,y=seq(8,50,32),yend=seq(24,64,32)),
      aes(x=x,xend=xend,y=y,yend=yend)
    ) +
    geom_segment(
      data = data.frame(x=5.5,xend=5.7,y=seq(16,64,32),yend=seq(16,64,32)),
      aes(x=x,xend=xend,y=y,yend=yend)
    ) +
    geom_segment(
      data = data.frame(x=6.3,xend=6.5,y=c(16,48),yend=c(16,48)),
      aes(x=x,xend=xend,y=y,yend=yend)
    ) +
    annotate("segment",x=6.5,y=16,yend=48) +
    annotate("segment",x=6.5,xend=6.7,y=32) +
    geom_segment(
      data = data.frame(x=1.3,xend=1.5,y=(1:64)-.5,yend=(1:64)-.5),
      aes(x=x,xend=xend,y=y,yend=yend)
    ) +
    geom_segment(
      data = data.frame(x=1.5,xend=1.5,y=seq(.5,63.5,2),yend=seq(1.5,64.5,2)),
      aes(x=x,xend=xend,y=y,yend=yend)
    ) +
    geom_segment(
      data = data.frame(x=1.5,xend=1.7,y=seq(1,64,2),yend=seq(1,64,2)),
      aes(x=x,xend=xend,y=y,yend=yend)
    ) 

  
  output$download_submitted_bracket <- downloadHandler(
  filename = function() {
    paste0(input$player, "_bracket.png")
  },
  content = function(file) {
    png(file, width=750,height=750, res=100)
    print(drawn_bracket)
    dev.off()
  }
)
  
  output$drawn_bracket <- renderPlot({drawn_bracket})
  
tagList(
    plotOutput("drawn_bracket", height = "80vh")
  )

}
