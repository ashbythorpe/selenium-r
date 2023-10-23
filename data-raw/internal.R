## code to prepare `commands` dataset goes here
library(rvest)
library(dplyr)

session <- read_html("https://w3c.github.io/webdriver")

table <- session |>
  html_element("#endpoints") |>
  html_element("table.simple") |>
  html_table()

commands <- mapply(function(x, y) list(method = x, url = y), table$Method, table$`URI Template`, SIMPLIFY = FALSE)
names(commands) <- table$Command

commands[["Element Displayed"]] <- list(
  method = "GET",
  url = "/session/{session id}/element/{element id}/displayed"
)

usethis::use_data(commands, overwrite = TRUE, internal = TRUE)
