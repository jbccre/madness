current_bracket <- function(input, output, session, gamestate_men, gamestate_women) {

  if (input$tournament == "Men") {gamestate <- gamestate_men} else {gamestate <- gamestate_women}

  # all teams in first round
  all_teams <- gamestate |>
    filter(substr(bracketPositionId,1,1)=="2") |>
    select(bracketPositionId, elo_name, isTop) |>
    mutate(x = 2, y = 2*bracketPositionId - 400 - isTop) |>
    mutate(color = gray(.4)) |>
    select(elo_name, x, y, color)

  # all winners
  all_winners <- gamestate |>
    filter(isWinner) |>
    mutate(x = as.integer(substr(victorBracketPositionId,1,1))) |>
    mutate(y = as.integer(substr(victorBracketPositionId,2,3))) |>
    mutate(top = victorGamePosition=='Top') |>
    mutate(y = 2^(x-1)*y - 2^(x-2)*top) |>
    mutate(y = y + .5 - 2^(x-3)) |>
    select(elo_name,x,y) |>
    mutate(y = case_when(is.na(y) ~ 32.5, .default = y)) |>
    mutate(x = case_when(is.na(x) ~ 8, .default = x)) |>
    mutate(color = 'green')
 
  teams_to_plot <- bind_rows(all_teams, all_winners) |>
    arrange(x, elo_name, desc(color)) |>
    group_by(x, elo_name) |>
    filter(row_number()==1) |>
    ungroup() |>
    arrange(x,y) |>
    mutate(elo_name = ifelse(color=='green', paste0("**",elo_name,"**"), elo_name))

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
    mutate(x=x+1,xend=xend+1,y=y+.5,yend=yend+.5)
  
  drawn_bracket <- ggplot() +
    geom_richtext(data = teams_to_plot, aes(x = x, y = y, color = color, label = elo_name), fill = NA, label.color = NA) +
    geom_segment(data = line_segments, aes(x=x,xend=xend,y=y,yend=yend)) +
    scale_x_continuous(limits = c(1.8,8.2)) +
    scale_color_manual(values = c("#666666"="#666666", "green"="darkgreen")) +
    labs(title = "Current Bracket") +
    theme(
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = 'none',
        plot.title = element_text(size=20, face='bold'))

  output$download_current_bracket <- downloadHandler(
  filename = function() {"current_bracket.png"},
  content = function(file) {
    png(file, width=1500,height=2000, res=150)
    print(drawn_bracket)
    dev.off()
  }
  )
  
  output$current_bracket <- renderPlot({drawn_bracket})
  
  tagList(
    plotOutput("current_bracket", height = "80vh")
  )

}

