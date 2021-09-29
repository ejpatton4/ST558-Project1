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





ditto <- encounter("ditto")
ditto2 <- encounter("ditto","RED")

char <- encounter("charmander")


mons <- list("charmander","squirtle","bulbasaur")

hope <- lapply(mons, FUN = encounter, game ="all")

hope2 <- tibble(unlist(hope[1]))

hope2 <- unlist(hope)

rattata <- GET("https://pokeapi.co/api/v2/pokemon/ditto/encounters")

rattata_data <- rattata$content

rattata_data <- rawToChar(rattata_data)

rattata_data <- fromJSON(rattata_data,  flatten = TRUE)

rattata_data[[1]][[1]][[1]][[1]][[1]]


    