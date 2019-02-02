server <- function(input, output, session) {
  params <- create_params()
  state <- create_state(params)

  values <- reactiveValues()
  values$params <- params
  values$state <- state

  # New game button
  observeEvent(input$new_game, {
    values$params <- create_params(players = c("You", "Computer"))
    values$state <- create_state(values$params)
    values$player_id <- "You"
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
    button_label <- ifelse(values$state$nextplayer == values$player_id, "Do move", "Let computer move")
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

  output$stateplot <- renderGirafe({
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
    if (!is.null(values$state$history)) {

      values$state$history %>%
        mutate(move = map_chr(move, move_short_to_fullname)) %>%
        rename(`New balance` = new_balance) %>%
        set_colnames(str_to_title(colnames(.)))
    }
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
