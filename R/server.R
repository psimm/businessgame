server <- function(input, output, session) {
  values <- reactiveValues()

  # New game button
  observeEvent(input$new_game, {
    values$params <- create_params(players = c("Player A", "Player B"))
    values$state <- create_state(values$params)
    values$player_id <- ifelse(runif(1) >= 0.5, "Player A", "Player B")
    },
    ignoreInit = FALSE,
    ignoreNULL = FALSE
  )

  output$show_player_id <- renderText({
    paste("You are", values$player_id)
  })

  observe({
    x <- values$params$moves$cost
    updateRadioButtons(
      session = session,
      inputId = "move",
      label = "Choose a move",
      choiceNames = list(
        paste("Do nothing. Cost :", x[1]),
        paste("Adjust marketing right. Cost :", x[2]),
        paste("Adjust marketing left. Cost :", x[3]),
        paste("Improve technology. Cost :", x[4]),
        paste("Imitate competitor. Cost :", x[5]),
        paste("Imitate & Adjust marketing right. Cost :", x[6]),
        paste("Imitate & Adjust marketing left. Cost :", x[7]),
        paste("Imitate & Improve technology. Cost :", x[8])
      ),
      choiceValues = list("c", "r", "l", "u", "i", "ir", "il", "iu")
    )
  })

  observe({
    button_label <- ifelse(values$state$nextplayer == values$player_id, "Do move", "Let AI move")
    updateActionButton(session = session,
                       inputId = "do_move",
                       label = button_label)
  })

  # Do move button
  observeEvent(input$do_move, {
    if (values$state$nextplayer == values$player_id & values$state$game_over == FALSE) {
      values$state %<>% do_move(input$move)
    } else if (!values$state$game_over) {
      move <- move_optimizer(state)
      values$state %<>% do_move(move)
    }
  })

  output$stateplot <- renderPlot({
    plot_state(values$state)
  })

  output$nextplayer <- renderText({
    paste(values$state$nextplayer, "moves next")
  })

  output$game_over <- renderText({
    ifelse(values$state$game_over,
           paste("Game over."),
           paste("Game isn't over."))
  })

  output$move_history <- renderTable({
    values$state$history
  })

  output$balance <- renderText({
    balance <- ifelse(
        values$player_id == "Player A",
        values$state$producers$balance[1],
        values$state$producers$balance[2]
      )
    paste("You have", balance, "money")
  })
}
