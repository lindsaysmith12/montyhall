% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-pkg.R
\name{open_goat_door}
\alias{open_goat_door}
\title{Open a door revealing a goat.}
\usage{
open_goat_door(game, a.pick)
}
\arguments{
\item{game}{A vector of length 3 is passed into the function that represents
the placement of the car and goats behind the 3 doors.}

\item{a.pick}{An integer (1-3) is passed into the function that represents
the number of the door that the contestant initially chose.}
}
\value{
An integer representing the door number that is opened to reveal a goat
(1, 2, or 3).
}
\description{
\code{open_goat_door()} opens a door that reveals a goat in the Monty Hall game
that is different from the chosen door in select_door.
}
\details{
This function simulates the host opening one of the remaining doors that
reveals a goat after the contestant has made their initial choice.
If the contestant picks the car, a random door with a goat is selected.
If the contestant picks a goat, the other door with a goat is opened.
}
\examples{
  game <- c("goat", "goat", "car")
  a.pick <- 1
  open_goat_door(game, a.pick)
}
