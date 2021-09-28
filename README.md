Poke API Vignette
================
Evan Patton
9/27/2021

## Introduction

This space reserved for introduction, talk about the project, my
thoughts, and then an overview of my work.

## Packages Needed

In this Vignette we will need the following packages:

``` r
# You only need to run the install.packages() functions if you have not already downloaded the packages. 
# As I already have them downloaded, I am not doing that here. If you need to, copy and paste the line needed, without the #, into your console and run.

# install.packages("tidyverse")
# install.packages("httr")
# install.packages("jsonlite")

library(httr)
library(jsonlite)
library(tidyverse)
```

## Encounter Function

Pokemon fans around the world, myself included, know the catch phrase
from the TV show by heart. **“Gotta Catch ’em All”**. That speaks to a
major part of both the Pokemon TV shows and the video games, which is
collecting all the different Pokemon from around the fictional world and
filling up your PokeDex with their information.

However, before you can catch ’em, you gotta find ’em. That is the
purpose of this function is to find what games and locations you can
find Pokemon in the wild. Of note, this does not work with Pokemon that
cannot be found in the wild, notable ones are the starting trio from
each game and their evolutions. A list of starters can be found
[here](https://bulbapedia.bulbagarden.net/wiki/Starter_Pok%C3%A9mon).

The function `encounter`, take two arguments. The first one is not
optional, and that is the Pokemon you want to find. The second argument
is optional, and that is what game from the series you want to return
data on, if no game is selected then all games will be returned in the
final tibble. **Both arguments require quoted strings**

``` r
# Define function and inputs, both inputs need to be in a quoted string
encounter <- function(poke, game = "all") {

# Changing inputs to all lower case to allow for some differential in how they are inputed.  
poke <- tolower(poke)
game <- tolower(game)
  
# Getting the requested data from the API
raw_data <- fromJSON(paste0("https://pokeapi.co/api/v2/pokemon/",poke,"/encounters"), flatten = TRUE)


# Using a for loop to get all of the locations, and games that the location is in
for (i in 1:dim(raw_data)[1]) {
  
  # Grabs the location from raw_data
  loc <- raw_data[i,2]
  
  # For loop to get the games, as the location can be in more than one game
  for (b in 1:dim(raw_data[[1]][[i]])[1]){
    
    # Getting the game data
    gm <- raw_data[[1]][[i]][b,3]
    
    # Combine location and game into a line for building a tibble
    line <- c(loc,gm)
    
    # If first iteration of of the game loop, create a tibble. 
    # If not first iteration of loop, add line to the tibble.
    if(b == 1)
      {dfg <- tibble(loc, gm)
    }else{
    dfg <- rbind(dfg,line)
  }
  }

  # After all games for the location are finished, we build a tibble for all of the locations.
  
  # If first iteration of location loop, the new tibble is tibble created in the game loop.
  # If not first iteration, we add what is already in the tibble with the new data from the game loop.
  if(i==1){
    df <- dfg
  }else{
    df <- rbind(df,dfg)
  }
  }


  # Allow user to only return locations for certain games.
  # If the user does not select an input for game, it defaults to all and will return the whole data set.
  # If the user does select an input, the function will return only locations for that game.
if(game == "all"){
   return(df)
}else{
    return(df %>%
             filter(gm == game))
  }

}
```
