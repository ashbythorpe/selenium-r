## code to prepare `commands` dataset goes here
library(rvest)
library(dplyr)

session <- read_html("https://w3c.github.io/webdriver")

table <- session |>
  html_element("#endpoints") |>
  html_element("table.simple") |>
  html_table()

commands <- table$`URI Template`
names(commands) <- table$Method

usethis::use_data(commands, overwrite = TRUE, internal = TRUE)
