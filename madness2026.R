
################################
# clear R, load packages
################################
# requires men to be set to TRUE or FALSE in prior script to specify men's or women's tournament.
# requires test_envir to be set to TRUE or FALSE (set to FALSE on previous year data; set to TRUE on current year data)
# requires qualtrics_simulate to be set to TRUE or FALSE (set to TRUE only to make fake qualtrics data once in testing)

if (men) {setwd("men/")} else {setwd("women/")}
if (men) {ncaa_url <- "https://ncaa-api.henrygd.me/brackets/basketball-men/d1/2025"} else {ncaa_url <- "https://ncaa-api.henrygd.me/brackets/basketball-women/d1/2025"}
library(httr)
library(jsonlite)
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(readr) 
library(stringdist)
elo <- read_csv("elo.csv")

################################
# initialize for first time
################################
if (!is.element("games_completed.txt", list.files())) {
  writeLines(as.character(0), con = "games_completed.txt")
}

################################
# get the current gamestate
################################
the_data <- fromJSON(content(GET(ncaa_url), "text", encoding = "UTF-8"))
the_games <- the_data$championships$games[[1]]
the_bracket <- the_games$teams
for (i in 1:length(the_bracket)) {the_bracket[[i]]$contestId <- the_games$contestId[i]}
for (i in 1:length(the_bracket)) {the_bracket[[i]] <- as.data.frame(the_bracket[[i]])}
the_bracket <- do.call(rbind,the_bracket)

################################
# do not update dashboard if no new information
################################
games_completed <- readLines("games_completed.txt")
if (sum(the_bracket$isWinner) == as.numeric(games_completed) & !test_envir) {quit(status=0)}

the_bracket <- 
  left_join(the_games, the_bracket, by = 'contestId') |> 
  select(
    contestId,
    bracketPositionId,
    victorBracketPositionId,
    victorGamePosition,
    startDate,
    isWinner,
    isTop,
    seed,
    nameShort
  ) |>
  mutate(startDate = as.Date(startDate, format = '%m/%d/%Y'))

################################
# get the victory probabilities for each pair of teams by fetching elo ratings and doing math
# - first, match ncaa names with elo names
# - second, find probability of victory for each pair of teams (elo names)
################################
the_bracket$elo_name <- 
  elo$Team[sapply(
    the_bracket$nameShort, 
    function(z){which.min(stringdist(z, elo$Team, method='jw'))})]

if (men) {the_bracket$elo_name[the_bracket$nameShort=="SIU Edwardsville"] <- "SIUE"} # manual fix - update each year
if (!men) {the_bracket$elo_name[the_bracket$nameShort=="NC State"] <- "North Carolina State"} # manual fix - update each year
if (!men) {the_bracket$elo_name[the_bracket$nameShort=="Fla. Gulf Coast"] <- "FGCU"} # manual fix - update each year
if (!men) {the_bracket$elo_name[the_bracket$nameShort=="FDU"] <- "Fairleigh Dickinson"} # manual fix - update each year

the_bracket <- the_bracket |> left_join({elo |> select(elo_name = Team)}) |> as_tibble()
rm(list = setdiff(ls(), c("the_bracket", "elo", "test_envir", "men", "qualtrics_simulate")))

################################
# simulate 10,000 matches
################################

############### first, simulate every round that does not require knowledge of a prior round

rounds_to_simulate <- the_bracket |>
  filter(bracketPositionId < 300) |>
  filter(!(bracketPositionId %in% victorBracketPositionId)) |>
  distinct(bracketPositionId) |>
  unlist()

simulate_slot <- function(id) {
  teams <- the_bracket$elo_name[the_bracket$bracketPositionId == id]
  is_winner <- the_bracket$isWinner[the_bracket$bracketPositionId == id]
  elos <- elo$ELO[match(teams, elo$Team)]
  probs <- c(1/(1 + 10^((elos[2]-elos[1])/400)),1/(1 + 10^((elos[1]-elos[2])/400)))
  if (!test_envir & sum(is_winner)>0) {probs <- as.numeric(is_winner)}
  sample(teams, 10000, TRUE, probs)
}

sim <- map_dfc(as.character(rounds_to_simulate), simulate_slot) |> set_names(as.character(rounds_to_simulate))

############### next, simulate first round games that require result from first four

first4_mapping <- the_bracket |>
  filter(bracketPositionId < 200) |>
  select(bracketPositionId, victorBracketPositionId) |>
  distinct()

for (i in 1:nrow(first4_mapping)) { 
  id <- first4_mapping$victorBracketPositionId[i]
  winner <- the_bracket$elo_name[the_bracket$bracketPositionId == id & the_bracket$isWinner] 
  if (length(winner)>0 & !test_envir) {out <- rep(winner, times = 10000)} else {out <- {
    team1 <- unlist(sim[,as.character(first4_mapping$bracketPositionId[i])])
    team2 <- rep(
      {the_bracket |> filter(bracketPositionId==id) |> select(elo_name) |> unlist() |> setdiff(team1)},
      times = 10000)
    elo1 <- elo$ELO[match(team1, elo$Team)]
    elo2 <- elo$ELO[match(team2, elo$Team)]
    prob1 <- 1/(1 + 10^((elo2-elo1)/400))
    prob2 <- 1/(1 + 10^((elo1-elo2)/400))
    as_tibble(bind_cols(team1=team1, team2=team2, prob1=prob1, prob2=prob2)) |>
      rowwise() |>
      mutate(winner = sample(c(team1,team2),size=1,replace=TRUE,prob=c(prob1,prob2))) |>
      ungroup() |>
      select(winner) |>
      unlist()
  }}
  sim$temp <- out
  colnames(sim) <- gsub("temp", id, colnames(sim))
}

sim <- sim[,sort(colnames(sim))]
rm(list = setdiff(ls(), c("elo", "sim", "the_bracket", "test_envir", "men", "qualtrics_simulate")))

############### now, simulate every remaining round

mapping <- the_bracket |>
  filter(victorBracketPositionId >= 300) |>
  select(bracketPositionId, victorBracketPositionId) |>
  distinct()

for (i in seq(1,nrow(mapping),by=2)) { 
  id <- mapping$victorBracketPositionId[i]
  winner <- the_bracket$elo_name[the_bracket$bracketPositionId == id & the_bracket$isWinner] 
  if (length(winner)>0 & !test_envir) {out <- rep(winner, times = 10000)} else {out <- {
    team1 <- unlist(sim[,as.character(mapping$bracketPositionId[i])])
    team2 <- unlist(sim[,as.character(mapping$bracketPositionId[i+1])])
    elo1 <- elo$ELO[match(team1, elo$Team)]
    elo2 <- elo$ELO[match(team2, elo$Team)]
    prob1 <- 1/(1 + 10^((elo2-elo1)/400))
    prob2 <- 1/(1 + 10^((elo1-elo2)/400))
    as_tibble(bind_cols(team1=team1, team2=team2, prob1=prob1, prob2=prob2)) |>
      rowwise() |>
      mutate(winner = sample(c(team1,team2),size=1,replace=TRUE,prob=c(prob1,prob2))) |>
      ungroup() |>
      select(winner) |>
      unlist()
  }}
  sim$temp <- out
  colnames(sim) <- gsub("temp", id, colnames(sim))
}

rm(list = setdiff(ls(), c("elo", "sim", "the_bracket", "test_envir", "men", "qualtrics_simulate")))

############ find out who won each match - load qualtrics data

if (qualtrics_simulate) {
qualtrics <- sim[sample(1:nrow(sim), size=40),] |>
  setNames(paste0("X",1:67)) |>
  mutate(X68 = paste0(sample(letters,size=40,replace=T),sample(letters,size=40,replace=T),sample(letters,size=40,replace=T))) |>
  filter(!duplicated(X68))
} else {
  qualtrics <- read_csv("submitted_brackets.csv")
}
  
for (i in 1:nrow(qualtrics)) {
  mat <- as.matrix(sim[,1:67])              # 10000 x 67
  row_vec <- as.matrix(qualtrics[i,1:67])        # 1 x 67
  seed_mat <- t(matrix(rep(the_bracket$seed[match(unlist(qualtrics[i,1:67]),the_bracket$elo_name)],times=10000),ncol=10000))
  points_mat <- seed_mat
  points_mat[,1:4] <- 20
  points_mat[,5:36] <- 20 + seed_mat[,5:36]
  points_mat[,37:52] <- 40 + 2*seed_mat[,37:52]
  points_mat[,53:60] <- 75 + 3*seed_mat[,53:60]
  points_mat[,61:64] <- 100 + 5*seed_mat[,61:64]
  points_mat[,65:66] <- 140 + 7*seed_mat[,65:66]
  points_mat[,67] <- 200 + 10*seed_mat[,67]
  result <- mat == matrix(row_vec, nrow = nrow(mat), ncol = ncol(mat), byrow = TRUE)  # logicals 10000x67
  sim$temp <- apply(result * points_mat,1,sum)
  colnames(sim)[colnames(sim)=='temp'] <- paste0("points_",qualtrics$X68[i])
  rm(mat,row_vec,seed_mat,points_mat,result)
}

################################
# get probabilities of victory across each simulation
################################

standings <- sim |>
  select_at(68:ncol(sim)) |>
  mutate(row=1:nrow(sim)) |>
  pivot_longer(cols=1:(ncol(sim)-67)) |>
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
  summarise(first = sum(first)/10000,
            second = sum(second)/10000,
            third = sum(third)/10000
) |>
  mutate(name = gsub("^points_","",name)) 

################################
# get current points into standings
################################

qualtrics$current_points <- NA

winners <- the_bracket |>
  filter(isWinner) |>
  select(bracketPositionId, elo_name)
winners_vec <- t(matrix(unlist(winners$elo_name)))
colnames(winners_vec) <- unlist(winners$bracketPositionId)
rm(winners)

for (i in 1:nrow(qualtrics)) {
  mat <- as.matrix(winners_vec)              # 1 x 67
  row_vec <- as.matrix(qualtrics[i,1:67])        # 1 x 67
  seed_mat <- t(matrix(rep(the_bracket$seed[match(unlist(qualtrics[i,1:67]),the_bracket$elo_name)],times=1),ncol=1))
  points_mat <- seed_mat
  points_mat[,1:4] <- 20
  points_mat[,5:36] <- 20 + seed_mat[,5:36]
  points_mat[,37:52] <- 40 + 2*seed_mat[,37:52]
  points_mat[,53:60] <- 75 + 3*seed_mat[,53:60]
  points_mat[,61:64] <- 100 + 5*seed_mat[,61:64]
  points_mat[,65:66] <- 140 + 7*seed_mat[,65:66]
  points_mat[,67] <- 200 + 10*seed_mat[,67]
  result <- mat == matrix(row_vec, nrow = nrow(mat), ncol = ncol(mat), byrow = TRUE)  # logicals 10000x67
  qualtrics$current_points[i] <- apply(result * points_mat,1,sum)
  rm(mat,row_vec,seed_mat,points_mat,result)
}

standings <- qualtrics |>
  select(name = X68, current_points) |>
  left_join(standings, by = join_by(name)) |>
  mutate(market_value = nrow(qualtrics)*(7*first+2*second+1*third)) |>
  mutate(timestamp = Sys.time()) |>
  arrange(name)

write_csv(standings, file = 'current_standings.csv')
write_csv(standings, file = 'all_standings.csv', append = TRUE)
write_csv(sim, file = 'simulation.csv')
write_csv(the_bracket, file = 'gamestate.csv')

qualtrics$current_points <- NULL
if (qualtrics_simulate) {write_csv(qualtrics, file = 'submitted_brackets.csv')}
writeLines(as.character(sum(the_bracket$isWinner)), con = "games_completed.txt")