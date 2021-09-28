library(httr)
library(jsonlite)
library(tidyverse)


encounter <- function(poke, game = "all") {

poke <- tolower(poke)
game <- tolower(game)
  
raw_data <- fromJSON(paste0("https://pokeapi.co/api/v2/pokemon/",poke,"/encounters"), flatten = TRUE)


# this function is designed for getting where to find specific Pokemon
for (i in 1:dim(raw_data)[1]) {
  
  loc <- raw_data[i,2]
  


  for (b in 1:dim(raw_data[[1]][[i]])[1]){
    
    gm <- raw_data[[1]][[i]][b,3]
    line <- c(loc,gm)
    if(b == 1)
      {dfg <- tibble(loc, gm)
    }else{
    dfg <- rbind(dfg,line)
  }
  }

  if(i==1){
    df <- dfg
  }else{
    df <- rbind(df,dfg)
  }
  }

if(game == "all"){
   return(df)
}else{
    return(df %>%
             filter(gm == game))
  }

}


ditto <- encounter("ditto")
ditto2 <- encounter("ditto","red")





    