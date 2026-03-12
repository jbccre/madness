# download ELO ratings for each tournament
rm(list = ls())
setwd("/Users/jrbnn/OneDrive/Desktop/madness2026")
library(rvest)
library(readr)
write_csv({read_html("https://www.warrennolan.com/basketball/2025/elo") |> html_element("table") |> html_table()}, file = "men/elo.csv")
write_csv({read_html("https://www.warrennolan.com/basketballw/2025/elo") |> html_element("table") |> html_table()}, file = "women/elo.csv")