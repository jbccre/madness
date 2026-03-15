# download ELO ratings for each tournament
library(rvest)
library(readr)
write_csv({read_html("https://www.warrennolan.com/basketball/2026/elo") |> html_element("table") |> html_table()}, file = "men/elo.csv")
write_csv({read_html("https://www.warrennolan.com/basketballw/2026/elo") |> html_element("table") |> html_table()}, file = "women/elo.csv")