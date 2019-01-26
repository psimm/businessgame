# Move optimizer
# Try every possible move and target combination
# Choose the one that yields highest profit

test_move <- function(move, state) {
    player <- state$nextplayer
    balance_original <- state$producers[state$producers$name == player,]$balance
    state %<>% do_move(move)
    new_balance <- state$producers[state$producers$name == player,]$balance
    new_balance - balance_original
}

move_optimizer <- function(state) {
  best_move <- legal_moves(state) %>%
    mutate(profit = map_dbl(move, test_move, state)) %>%
    top_n(1, profit) %>%
    pull(move)

  if (length(best_move > 1)) {
    best_move %<>% sample(1)
  }

  best_move
}
