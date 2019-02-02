add_product <- function(state, move, target) {

  move_i <- move #to disambiguate column names
  target_i <- target

  move_df <- legal_moves(state) %>%
    filter(move == move_i, target == target_i)

  stopifnot(nrow(move_df) == 1)

  product_counter <- nrow(state$products %>% filter(owner == state$nextplayer)) + 1

  # Update products
  state$products %<>% add_row(
    xcor = move_df$x,
    ycor = move_df$y,
    owner = state$nextplayer,
    counter = product_counter
  )

  # Update producers
  state$producers[state$producers$name == state$nextplayer,]$xcor <- move_df$x
  state$producers[state$producers$name == state$nextplayer,]$ycor <- move_df$y
  state$producers[state$producers$name == state$nextplayer,]$balance %<>% subtract(move_df$cost)

  state
}

buy <- function(state) {
  # Consumers use their previously bought product as reference and to set color
  state$consumers$last_bought <- "None"

  # Make a shopping list for all consumers
  shopping <- map(
    1:nrow(state$consumers),
    find_best_product,
    consumers = state$consumers,
    products = state$products
  )

  # Pay for the products
  for (consumer in 1:nrow(state$consumers)) {
    haul <- shopping[[consumer]]
    distance_improvement <- state$consumers[consumer,]$distance_bought - haul$distance_consumer
    if (distance_improvement > 0) {
      state$producers[state$producers$name == haul$owner,]$balance <-
        state$producers[state$producers$name == haul$owner,]$balance +
        state$consumers[consumer,]$count * distance_improvement
      state$consumers[consumer,]$distance_bought <- haul$distance_consumer
      state$consumers[consumer,]$last_bought <- haul$owner
    }
  }
  state
}

create_consumers <- function(params, products) {
  consumers <- tibble(
    xcor = 1:params$max_xcor,
    ycor = params$max_ycor,
    count = params$consumer_distribution,
    distance_bought = params$max_ycor + 1
  )

  shopping <- map(
    1:nrow(consumers),
    find_best_product,
    consumers = consumers,
    products = products
  )
  consumers$distance_bought <- map_dbl(shopping, `$`, distance_consumer)
  consumers$last_bought <- map_chr(shopping, `$`, owner)
  consumers
}

create_params <- function(players = NULL) {
  if (!is.null(players)) {
    stopifnot(is.character(players))
  }

  if (is.null(players)) {
    producer_names <- c("Player A", "Player B")
  } else {
    producer_names <- players
  }

  params <- list(
    y_cost = 40,
    x_cost = 20,
    imi_cost = 15,
    producer_names = producer_names,
    start_xcor =  find_start_xcor(length(producer_names)),
    start_ycor = 1,
    start_money = 100,
    max_xcor = 10,
    max_ycor = 10,

    consumer_distribution = c(5, 8, 10, 12, 15, 15, 12, 10, 8, 5)
  )

  moves  <- tibble(
    move = c("c","r", "l", "u", "i", "ir", "il", "iu"),
    is_check = c(TRUE, rep(FALSE, 7)),
    is_imitation = c(rep(FALSE, 4), rep(TRUE, 4)),
    offset_x = c(0, 1, -1, 0, 0, 1, -1, 0),
    offset_y = c(0, 0, 0, 1, 0, 0, 0, 1),
    cost = c(
      0,
      params$x_cost,
      params$x_cost,
      params$y_cost,
      params$imi_cost,
      params$imi_cost + params$x_cost,
      params$imi_cost + params$x_cost,
      params$imi_cost + params$y_cost
    )
  )
  params$moves <- moves
  params
}

create_producers <- function(params) {
  tibble(
    name = params$producer_names,
    balance = params$start_money,
    xcor = params$start_xcor,
    ycor = params$start_ycor
  )
}

create_products <- function(params) {
  tibble(
    xcor = params$start_xcor,
    ycor = params$start_ycor,
    owner = params$producer_names,
    counter = c(rep(1, length(params$producer_names)))
  )
}

create_state <- function(params) {
  products <- create_products(params)
  consumers <- create_consumers(params, products)
  state <- list(producers = create_producers(params),
                consumers = consumers,
                products = products,
                nextplayer = params$producer_names[1],
                game_over = FALSE,
                no_moves = FALSE,
                no_moves_counter = 0,
                check_counter = 0,
                last_move = "u",
                params = params
  )
  state$legal_moves <- legal_moves(state)
  state
}

distance <- function(xcor1, ycor1, xcor2, ycor2) {
  w <- abs(xcor2 - xcor1)
  h <- abs(ycor2 - ycor1)
  h + w # Manhattan distance
}

do_move <- function(state, move) {
  stopifnot(move %in% state$params$moves$move)
  stopifnot(!state$game_over)

  player_names <- state$producers %>% pull(name)

  if (move %in% c("u", "r", "l", "c")) {
    target <- state$nextplayer
  } else {
    target <- player_names[player_names != state$nextplayer]
  }

  # Save information from previous state vector
  state_previous <- state

  # The checking move only passes priority
  if (!state$params$moves[state$params$moves$move == move,]$is_check) {
    state %<>% add_product(move, target) %>% buy()
  }

  state$nextplayer <- find_nextplayer(state)
  state$last_move <- move

  new_hist_entry <- tibble(
    player = state_previous$nextplayer,
    move = state$last_move,
    cost = state$params$moves[state$params$moves$move == move,]$cost,
    profit = state$producers[state$producers$name == player,]$balance -
      state_previous$producers[state$producers$name == player,]$balance,
    new_balance = state$producers[state$producers$name == player,]$balance
  ) %>%
    mutate(revenue = profit + cost) %>%
    select(player, move, revenue, cost, profit, new_balance)

  state$history %<>% bind_rows(new_hist_entry)
  state$legal_moves <- legal_moves(state)

  # Note if there are no legal moves except checking
  state$no_moves <- all(state$params$moves[state$params$moves$move %in% state$legal_moves$move,]$is_check)
  state$no_moves_counter <- ifelse(state$no_moves, state$no_moves_counter + 1, 0)
  state$check_counter <- ifelse(state$params$moves[state$params$moves$move == move,]$is_check,
                                state$check_counter + 1,
                                0)

  # Check game ending conditions
  no_moves <- state$no_moves_counter >= length(state$params$producer_names)
  all_players_checked <- state$check_counter >= length(state$params$producer_names)
  max_ycor_reached <- max(state$products$ycor) == state$params$max_ycor

  if (no_moves | all_players_checked | max_ycor_reached) {
    state$game_over <- TRUE
  }

  state
}

find_best_product <- function(consumer, consumers, products) {
  consumer_xcor <- consumers[consumer,]$xcor
  distance_bought <- consumers[consumer_xcor,]$distance_bought
  relevant_products <- products %>%
    mutate(distance_consumer = distance(xcor, ycor, consumer_xcor, params$max_ycor)) %>%
    arrange(distance_consumer)
  relevant_products[1,]
}

find_nextplayer <- function(state) {
  current <- state$nextplayer
  players <- state$params$producer_names
  next_idx <- which(players == current) + 1
  next_idx <- replace(next_idx, next_idx > length(players), 1)
  players[next_idx]
}

find_start_xcor <- function(n) {
  if (n <= 10) {
    distribution <- switch(n,
                           1,
                           c(3,8),
                           c(2,5,8),
                           c(2,4,6,8),
                           c(2,4,5,7,9),
                           c(1,3,5,7,9,10),
                           c(1,2,4,5,6,8,9),
                           c(1:3,5,6,8:10),
                           c(1:5,7:10),
                           c(1:10)
    )
  } else {
    distribution <- runif(n, min = 2, max = 10) %>% round()
  }
  distribution
}

legal_moves <- function(state) {
  self <- state$nextplayer
  others <- state$params$producer_names[state$params$producer_names != state$nextplayer]

  state$params$moves %>%
    pmap_dfr(
      function(move, is_check, is_imitation, offset_x, offset_y, cost)
        if (is_imitation) {
          tibble(move, target = others, offset_x, offset_y, cost)
        } else {
          tibble(move, target = self, offset_x, offset_y, cost)
        }
    ) %>%
    left_join(state$producers, by = c("target" = "name")) %>%
    mutate(
      balance = state$producers[state$producers$name == self,]$balance,
      x = xcor + offset_x,
      y = ycor + offset_y,
    ) %>%
    filter(
      cost <= balance,
      x <= state$params$max_xcor,
      x >= 1,
      y <= state$params$max_ycor,
    ) %>%
    group_by(x, y) %>%  # Filter out dominated moves (target can be reached at lower cost)
    summarise(
      move = move[cost == min(cost)][1],
      target = target[cost == min(cost)][1],
      cost = min(cost)
    ) %>%
    ungroup()
}

plot_state <- function(
  state,
  showlegalmoves = TRUE,
  showmovecost = FALSE,
  showmoveprofit = FALSE,
  showmovenumber = FALSE,
  consumerpath = NULL
) {
  colors <- hue_pal()(state$params$producer_names %>% length() + 1)
  names(colors) <- c("None", state$params$producer_names)

  consumers <- state$consumers %>%
    mutate(
      tip = paste0(count, " consumers with preference: ", LETTERS[xcor], "\n",
                   "Distance to closest product: ", distance_bought, "\n",
                   "Last bought from: ", last_bought
      )
    )

  mark_moves <- state$legal_moves %>%
    mutate(
      tip = paste0("Cost: ", cost)
    )

  g <- state$products %>%
    ggplot(aes(x = LETTERS[xcor], y = ycor)) +
    geom_point(aes(color = owner), size = 10) +
    geom_point_interactive(data = consumers, aes(color = last_bought, tooltip = tip), size = 8.5, shape = 15) +
    geom_point_interactive(
      data = mark_moves,
      aes(x = x, y = y, tooltip = tip, data_id = move),
      color = colors[which(names(colors) == state$nextplayer)],
      size = 3, stroke = 3, pch = 1
    ) +
    geom_text(data = consumers, aes(label = count)) +
    geom_text(data = state$legal_moves, aes(x = x, y = y, label = cost), nudge_y = 0.25, color = "black") +
    coord_equal() +
    scale_y_continuous(breaks = 1:state$params$max_ycor) +
    scale_color_manual(values = colors) +
    labs(x = "Preference fit", y = "Technology level", color = NULL) +
    guides(color = guide_legend(override.aes = list(shape = 15))) +
    theme_grey((base_size = 14)) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(size = 1),
      legend.position = "bottom",
      plot.margin = margin(5, -50, -50, -50, "pt")
    )

  if (!is.null(consumerpath)) {
    g <- g + geom_step(data = consumerpath, aes(x = LETTERS[x], y = y), direction = "vh")
  }

  x <- ggiraph(ggobj = g, width = 1, selection_type = "single")
}

state_to_vec <- function(state) {
  c(
    map_dbl(
      1:state$params$max_xcor,
      function(x)
        state$consumers[state$consumers$xcor == x,]$distance_bought
    ),
    map(
      state$params$producer_names,
      function(x)
        c(
          state$producers[state$producers$name == x,]$xcor,
          state$producers[state$producers$name == x,]$ycor,
          state$producers[state$producers$name == x,]$balance
        )
    ) %>%
      flatten_dbl(),
    ifelse(state$nextplayer == state$params$producer_names[1], 1, 0),
    map_dbl(
      state$params$possible_moves$move,
      function(x)
        ifelse(x %in% state$legal_moves, 1, 0)
    ),
    map_dbl(
      state$params$possible_moves$move,
      function(x)
        ifelse(state$last_move == x, 1, 0)
    )
  )
}

move_short_to_fullname <- function(shortname) {
  shortname %>% when(
    . == "c" ~ "Check (do nothing)",
    . == "r" ~ "Adjust marketing right",
    . == "l" ~ "Adjust marketing left",
    . == "u" ~ "Improve technology",
    . == "i" ~ "Imitate competitor",
    . == "ir" ~ "Imitate & adjust marketing right",
    . == "il" ~ "Imitate & adjust marketing left",
    . == "iu" ~ "Imitate & improve technology"
  )
}
