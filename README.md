A business game in which the player takes the role of a CEO. They navigate the market for a new product, using innovation and imitation tactics.

![Game screenshot](https://raw.githubusercontent.com/psimm/businessgame/master/www/howtoplay.png)

Play it online:
[psim.shinyapps.io/business_game](psim.shinyapps.io/business_game)

This business game was part of a study at Aalborg University on human and AI business decision making. Read more about the study at https://projekter.aau.dk/projekter/da/studentthesis/human-and-ai-decision-making-in-a-game-of-innovation-and-imitation(9121a1ed-d5d7-4cf0-b725-41f822533544).html

Technical details

* The games uses ggplot2 with coord_equal() for its game board in a grid layout.
* Interactivity is realized with ggiraph::geom_point_interactive()
* The radioButtons() and ggiraph element are linked through an observeEvent() of the input$plot_selected element.
* The game primarily uses dplyr. Its state is tracked in a list of dataframes. The coding style could be described as pseudo-object oriented, as the state object is modified by various functions such as do_move() or buy().
