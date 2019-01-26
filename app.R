library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(magrittr)
library(scales)
library(shiny)
library(ggiraph)

source("R/game.R")
source("R/ai.R")
source("R/ui.R")
source("R/server.R")

params <- create_params()
state <- create_state(params)

shinyApp(ui, server)
