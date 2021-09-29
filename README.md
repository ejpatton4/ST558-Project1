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
purpose of this function is to find what locations in games you can find
Pokemon in the wild. It will also return the chance of encountering the
Pokemon there, the minimum level, the maximum level, and how you would
encounter the animal (walking in tall grass vs being given as a gift as
an example.)

The function `encounter`, takes two arguments. The first one is not
optional, and that is the Pokemon you want to find. The second argument
is optional, and that is what game from the series you want to return
data on, if no game is selected then all games will be returned in the
final tibble. **Both arguments require quoted strings**

``` r
encounter <- function(poke, game = "all") {

poke <- tolower(poke)
game <- tolower(game)
  
raw_data <- fromJSON(paste0("https://pokeapi.co/api/v2/pokemon/",poke,"/encounters"), flatten = TRUE)


# this function is designed for getting where to find specific Pokemon
for (i in 1:dim(raw_data)[1]) {
  
  loc <- raw_data[i,2]
  


  for (b in 1:dim(raw_data[[1]][[i]])[1]){
    
    gm <- raw_data[[1]][[i]][b,3]
    
    
    for (a in 1:dim(raw_data[[1]][[i]][[1]][[b]])[1]){
      
      chance <- raw_data[[1]][[i]][[1]][[b]][a,1]
      max    <- raw_data[[1]][[i]][[1]][[b]][a,3]
      min    <- raw_data[[1]][[i]][[1]][[b]][a,4]
      meth   <- raw_data[[1]][[i]][[1]][[b]][a,5]
      line   <- c(loc,gm,chance,max,min,meth)

      if(a == 1){
        dfa <- tibble(loc,gm,chance,max,min,meth)
      } else{
        #if(a<=(dim(raw_data[[1]][[i]][[1]][[b]])[1])){
          dfa <- rbind(dfa,line)
        #}else{
         # dfa <- tibble(dfa,line)
          #dfa <- distinct(dfa)
        #}
        }
      }
  
    if(b == 1)
    {dfg <- dfa
    }else{
      dfg <- rbind(dfg,dfa)
    }
    
    
    }
    

  

  if(i==1){
    df <- dfg
  }else{
    df <- rbind(df,dfg)
  }
  }

names(df) <- c("Location","Game","Chance","Max_Level","Min_Level","Method")

df <- distinct(df)

if(game == "all"){
   return(df)
}else{
    return(df %>%
             filter(Game == game))
  } #else close

} #func close
```
