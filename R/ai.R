# Move optimizer
# Try every possible move and target combination
# Choose the one that yields highest profit
# The actual study used a more sophisticated AI similar to DeepMind's Alpha Zero
test_move <- function(move, state) {
    player <- state$nextplayer
    balance_original <- state$producers[state$producers$name == player,]$balance
    state <- do_move(state, move)
    new_balance <- state$producers[state$producers$name == player,]$balance
    new_balance - balance_original
}

move_optimizer <- function(state) {
  move <- legal_moves(state) %>%
    mutate(profit = map_dbl(move, test_move, state)) %>%
    top_n(1, profit) %>%
    pull(move)
  if (length(move > 1)) {
    sample(move, 1)
  } else {
    move
  }
}
