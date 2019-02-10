ui <- fluidPage(
  titlePanel("Research & Development Business Game"),
  helpText("Take the role of a product manager and navigate a market of innovation and imitation."),
  sidebarPanel(
    textOutput("balance"),
    actionButton("do_move", "Do Move"),
    uiOutput("move_choices"),
    br(),
    actionButton("new_game", "New game"),
    width = 3
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
        title = "Game",
        column(
          12,
          girafeOutput("stateplot", width = "100%", height = "500px"),
          tableOutput("move_history")
        )
      ),
      tabPanel(
        title = "Instructions",
        strong("Introduction"),
        p("You are the CEO of a company that researches and sells consumer technology goods.
          A new product has been invented. You, along with one competitor, are the first to market."),
        strong("Consumers"),
        p("You earn money by creating products that consumers like. Consumers are the
          blocks at the top row. The number on each block tells you how many consumers are in it. A consumer block's
          position indicates its dream product. All consumers like products with a high technology level.
          But there are also differences in taste, shown by their different positions on the preference fit axis."),
        strong("Products"),
        p("Filled circles represent products. Your first product (filled green circle) is already on the market. You can count the distance from a product to a consumer block. The closer a product, the more a consumer block
          likes it. If a new product is closer to a consumer block than any previous product, the consumer block buys the product
          and the product maker receives money equal to the number of consumers in the block."),
        strong("Moves"),
        p("New products can be developed by improving the technology of the product
          that you already have, or by changing its marketing. You can also imitate your competitor's latest product.
          Possible moves are indicated by empty circles. The number atop indicates the cost."),
        strong("End of the game"),
        p("The game ends when one producer makes a product that has the highest technology level (10). It
          also ends if both producers decide they don't want to do anything one after another.")
      ),
      tabPanel(
        title = "Settings",
        radioButtons("order", strong("Move order"),
                     choices = c("Go first", "Go second"), inline = TRUE
        ),
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
      tabPanel(
        title = "Research",
        p("This business game was part of a study at Aalborg University on human and AI business decision making.
          Read more about the study at ", tags$a(href = "https://projekter.aau.dk/projekter/da/studentthesis/human-and-ai-decision-making-in-a-game-of-innovation-and-imitation(9121a1ed-d5d7-4cf0-b725-41f822533544).html",
               "https://projekter.aau.dk/projekter/da/studentthesis/human-and-ai-decision-making-in-a-game-of-innovation-and-imitation(9121a1ed-d5d7-4cf0-b725-41f822533544).html")),
        br(),
        p("Source code is available at ", tags$a(href = "https://github.com/psimm/rd_game", "https://github.com/psimm/rd_game")),
        br(),
        p("Contact: Paul Simmering (paul.simmering@gmail.com)")
      )
    )
  )
)
