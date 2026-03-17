rm(list = ls())
library(tidyverse)
qualtrics <- read_csv("/Users/jrbnn/OneDrive/Desktop/mens.csv")
qualtrics <- qualtrics |>
  select(Q1:Q68) |>
  mutate(across(1:67,
                ~ gsub(pattern = "^[0-9]* - ", replacement = "", x = .))) |>
  slice(-(1:2)) |>
  mutate(across(1:67,
                ~ gsub(pattern = "${q://QID", replacement = "Q", x = ., fixed = TRUE))) |>
  mutate(across(1:67,
                ~ gsub(pattern = "/ChoiceGroup/SelectedChoices}", replacement = "", x = ., fixed = TRUE))) 

for (i in 1:67)  { for (j in 1:nrow(qualtrics)) {if (grepl("^Q[0-9]*$",qualtrics[j,i])) {
  qualtrics[j,i] <- qualtrics[j,as.character(qualtrics[j,i])]}}}

colnames(qualtrics) <- gsub("Q","X",colnames(qualtrics))
qualtrics <- mutate(qualtrics, X68 = gsub("DrOJ", "theJuice", X68))
qualtrics <- arrange(qualtrics, tolower(X68))

unique(unlist(qualtrics[,1:67])) %in% read.csv("men/elo.csv")$Team # check to ensure all TRUE

write_csv(qualtrics, file = 'men/submitted_brackets.csv')
