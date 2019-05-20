library(shiny)
library(dplyr)
library(tibble)
library(purrr)
library(ggplot2)
library(ggiraph)

# game.R needs to be loaded first
source("R/game.R")
source("R/ai.R")
source("R/ui.R")
source("R/server.R")

shinyApp(ui, server)
