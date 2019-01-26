ui <- fluidPage(
  titlePanel("Research & Development Business Game"),
  helpText("Play the business game from Paul Simmering's master thesis."),
  sidebarPanel(
    radioButtons("move", "Choose a move:",
                 list(`Do nothing.` = "c",
                      `Adjust marketing right.` = "r",
                      `Adjust marketing left.` = "l",
                      `Improve technology.` = "u",
                      `Imitate competitor.` = "i",
                      `Imitate competitor & adjust marketing right.` = "ir",
                      `Imitate competitor & adjust marketing left.` = "il",
                      `Imitate competitor & improve technology.` = "iu"
                 )
    ),
    actionButton("do_move", "Do Move"),
    br(),
    br(),
    actionButton("new_game", "New game")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Game",
               fluidRow(
                 column(8, align="left",
                        plotOutput("stateplot")
                 )
               ),
               strong("Information"),
               textOutput("game_over"),
               textOutput("show_player_id"),
               textOutput("balance"),
               textOutput("nextplayer"),
               br(),
               strong("Move history"),
               tableOutput("move_history")
      ),
      tabPanel("Instructions",
               strong("Introduction"),
               p("You are the CEO of a company that researches and sells consumer technology goods.
                 A new product has been invented. You, along with one competitor, are the first to market."),
               strong("Consumers"),
               p("You earn money by creating products that consumers like. Consumers are the
                 blocks at the top row. The number on each block tells you how many consumers are in it. A consumer block's
                 position indicates its dream product. All consumers like products with a high technology level.
                 But there are also differences in taste, shown by their different positions on the preference fit axis.
                 "),
               strong("Products"),
               p("Circles represent products. You can count the distance from a product to a consumer block. The closer a product, the more a consumer block
                 likes it. If a new product is closer to a consumer block than any previous product, the consumer block buys the product
                 and the product maker receives money equal to the number of consumers in the block."),
               strong("Moves"),
               p("New products can be developed by improving the technology of the product
                 that you already have, or by changing its marketing. You can also imitate your competitor's latest product. Player A begins.
                 Products have numbers of them that show the order in which they were made. For example, the third product made by a player will
                 have a 3 on it."),
               strong("End of the game"),
               p("The game ends when one producer makes a product that has the highest technology level (10). It
                 also ends if both producers decide they don't want to do anything one after another.")
               ),
      tabPanel("Settings",
               sliderInput("y_cost", strong("Cost of improving technology"),
                           min = 0, max = 100, value = 45
               ),
               sliderInput("x_cost", strong("Cost of adjusting marketing"),
                           min = 0, max = 100, value = 15
               ),
               sliderInput("imi_cost", strong("Cost of imitation"),
                           min = 0, max = 100, value = 15
               ),
               helpText("Settings take effect when a new game is started.")
      ),
      tabPanel("Research",
               p("This business game is part of a study at Aalborg University on human and AI business decision making."),
               br(),
               p("Contact: paul.simmering@gmail.com")
      )
               )
               )

)
