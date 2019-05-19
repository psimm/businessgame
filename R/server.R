server <- function(input, output, session) {
  # Create initial game
  values <- reactiveValues()
  values$params <- params
  values$state <- create_state(params)

  # New game button
  observeEvent(input$new_game, {
    players <- c("You", "Computer")
    if (input$order == "Go second") players <- rev(players)

    # Reset the game
    values$params <- create_params(players)
    values$state <- create_state(values$params)

    # Switch to game tab
    updateTabsetPanel(
      session = session,
      inputId = "tabs",
      selected = "Game"
    )

    # Announce new game to player
    showNotification(
      ui = "New game started.",
      type = "message",
      duration = 5
    )
  })

  output$move_choices <- renderUI({
    if (values$state$nextplayer == "You") {

      moves <- values$state$legal_moves %>%
        mutate(
          move_name = map_chr(move, move_short_to_fullname),
          move_name = paste0(move_name, ". Cost: ", cost)
        ) %>%
        left_join(move_order, by = "move") %>%
        arrange(order)

      tagList(
        radioButtons(
          inputId = "move",
          label = "Choose a move",
          choiceNames = moves$move_name,
          choiceValues = moves$move
        )
      )
    }
  })

  # Select move after click on new product position in ggiraph plot
  observeEvent(input$stateplot_selected, {
    if (input$stateplot_selected %in% values$state$legal_moves$move) {
      updateRadioButtons(
        session = session,
        inputId = "move",
        selected = input$stateplot_selected
      )
    }
  })

  # Do move button
  observeEvent(input$do_move, {
    if (values$state$game_over) {
      player_balance <- balance <- values$state$producers %>%
        filter(name == "You") %>%
        pull(balance)
      ai_balance <- balance <- values$state$producers %>%
        filter(name == "Computer") %>%
        pull(balance)
      if (player_balance > ai_balance) {
        msg <- "You win! Congratulations."
      } else if (player_balance == ai_balance) {
        msg <- "It's a tie."
      } else {
        msg <- "You lost."
      }
      msg <- paste("Game over.", msg)
      showNotification(
        ui = msg,
        type = "message",
        duration = NULL
      )
    } else {
      if (values$state$nextplayer == "You") {
        if (input$move %in% values$state$legal_moves$move) {
          # Do player's turn
          values$state <- do_move(values$state, input$move)
          showNotification(
            ui = "Computer's turn",
            type = "message",
            duration = 3
          )
        } else {
          showNotification(
            ui = "Error: Illegal move",
            type = "error",
            duration = 3
          )
        }
      } else {
        # Do computers turn
        move <- move_optimizer(values$state)
        values$state <- do_move(values$state, move)
        showNotification(
          ui = "Your turn",
          type = "message",
          duration = 3
        )
      }
    }
  })

  observeEvent(values$state$nextplayer, {
    btn_label <- ifelse(
      values$state$nextplayer == "You",
      "Do your move",
      "Let computer move"
    )
    updateActionButton(
        session = session,
        inputId = "do_move",
        label = btn_label
    )
  })

  output$stateplot <- renderGirafe({
    plot_state(values$state)
  })

  output$move_history <- renderTable({
    if (!is.null(values$state$history)) {
      values$state$history %>%
        mutate(
          `No.` = 1:n(),
          move = map_chr(move, move_short_to_fullname)
        ) %>%
        select(`No.`, everything()) %>%
        rename(`New balance` = new_balance)
    }
  })

  output$balance <- renderText({
    balance <- values$state$producers %>%
      filter(name == "You") %>%
      pull(balance)
    paste("You have", balance, "money.")
  })
}
