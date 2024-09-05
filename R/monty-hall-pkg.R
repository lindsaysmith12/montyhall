#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   Select one of the three doors.
#' @description
#'   `select_door()` randomly selects one of the three doors in the Monty Hall
#'   game.
#' @details
#'   This function simulates a contestant randomly choosing one of the three 
#'   doors (numbered 1, 2, or 3) in the Monty Hall problem.
#' @param ... no arguments are used by the function.
#' @return 
#'   A single integer is returned that represents the chosen door: 1, 2, or 3.
#' @examples
#'   select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Open a door revealing a goat.
#' @description
#'   `open_goat_door()` opens a door that reveals a goat in the Monty Hall game 
#'   that is different from the chosen door in select_door.
#' @details
#'   This function simulates the host opening one of the remaining doors that 
#'   reveals a goat after the contestant has made their initial choice. 
#'   If the contestant picks the car, a random door with a goat is selected. 
#'   If the contestant picks a goat, the other door with a goat is opened.
#' @param game A vector of length 3 is passed into the function that represents 
#'   the placement of the car and goats behind the 3 doors.
#' @param a.pick An integer (1-3) is passed into the function that represents 
#'   the number of the door that the contestant initially chose.
#' @return 
#'   An integer representing the door number that is opened to reveal a goat 
#'   (1, 2, or 3).
#' @examples
#'   game <- c("goat", "goat", "car")
#'   a.pick <- 1
#'   open_goat_door(game, a.pick)
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   Change the door selection.
#' @description
#'   `change_door()` allows the contestant to either stay with their original 
#'   door or switch to the remaining unopened door in the Monty Hall game.
#' @details
#'   This function determines the final door choice of the contestant based on 
#'   whether they choose to stay with their initial selection or switch to the 
#'   other unopened door. If the contestant chooses to stay, the function 
#'   returns their original pick. If they choose to switch, the function returns 
#'   the other available door.
#' @param stay A logical value indicating whether the contestant chooses to stay 
#'   with their initial door (`T`) or switch to the other door (`F`).
#' @param opened.door An integer that represents the door number that was opened 
#'   to reveal a goat.
#' @param a.pick An integer that represents the initial door pick by the player.
#' @return 
#'   An integer that represents the contestant's final door choice (1, 2, or 3).
#' @examples
#'   opened.door <- 2
#'   a.pick <- 1
#'   change_door(stay = T, opened.door, a.pick)
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determine winner of the Monty Hall Game.
#' @description
#'   `determine_winner()` checks if the contestant's final pick results in a win 
#'   (car) or a loss (goat) in the Monty Hall game.
#' @details
#'   This function takes the final door choice of the contestant and checks 
#'   whether the prize behind that door is a car or a goat. If the contestant's 
#'   final pick is a car, they win; otherwise, they lose.
#' @param final.pick An integer that represents the contestant's final door 
#'    choice (1, 2, or 3).
#' @param game A vector of length 3 is passed into the function that represents 
#'   the placement of the car and goats behind the 3 doors.
#' @return 
#'   A string indicating the result: `"WIN"` if the contestant picks the car, 
#'   or `"LOSE"` if they pick a goat.
#' @examples
#'   game <- c("car", "goat", "goat")
#'   final.pick <- 1
#'   determine_winner(final.pick, game)
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   Play a round of the Monty Hall Game.
#' @description
#'   `play_game()` simulates a round of the Monty Hall game, including making 
#'   a door selection, opening a door with a goat, and determining the outcome 
#'   for both strategies (staying or switching).
#' @details
#'   This function simlulates a complete round by:
#'   1. Creating a new game with one car and two goats.
#'   2. Allowing the contestant to select an initial door.
#'   3. Opening a door with a goat.
#'   4. Determining the final pick if the contestant stays with the initial door 
#'   or switches to the remaining unopened door.
#'   5. Returning the outcomes of both strategies ("stay" and "switch") in 
#'   a data frame.
#' @param ... no arguments are used by the function.
#' @return 
#'   A data frame with two rows and two columns:
#'   `strategy`: The strategies used ("stay" or "switch").
#'   `outcome`: The outcome of each strategy ("WIN" or "LOSE").
#' @examples
#'   play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   Play multiple rounds of the Monty Hall Game.
#' @description
#'   `play_n_games()` simulates multiple rounds of the Monty Hall game and calculates 
#'   the proportion of wins for each strategy ("stay" or "switch") across all 
#'   rounds.
#' @details
#'   This function performs the following steps:
#'   1. Runs the `play_game()` function `n` times, where `n` is the number of 
#'   games to simulate.
#'   2. Collects the results of each game into a list.
#'   3. Combines the results from all games into a single data frame.
#'   4. Calculates and prints the proportion of wins for each strategy based on 
#'   the collected data.
#' @param n An integer specifying the number of games to simulate.
#' @return 
#'   A data frame containing the results of all simulated games, with columns 
#'   for `strategy` and `outcome`. Additionally, the function prints the 
#'   proportions of wins for each strategy.
#' @examples
#'   play_n_games(n = 100)
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
