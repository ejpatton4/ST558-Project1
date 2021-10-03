library(httr)
library(jsonlite)
library(tidyverse)



encounter <- function(poke, game = "all") {
  
  # Converting inputs to lower case for flexibility
  poke <- tolower(poke)
  game <- tolower(game)
  
  for(c in 1:length(poke)){
  
  # Get the requested data   
  raw_data <- fromJSON(paste0("https://pokeapi.co/api/v2/pokemon/",poke[c],"/encounters"), flatten = TRUE)
  
  
  # Beginning of location loop.
  for (i in 1:dim(raw_data)[1]) {
    
    # Get the location from raw.
    loc <- raw_data[i,2]
    
    # Beginning of game loop.
    for (b in 1:dim(raw_data[[1]][[i]])[1]){
      
      # Get the specific game from raw.
      gm <- raw_data[[1]][[i]][b,3]
      
      # Beginning of details loop.
      for (a in 1:dim(raw_data[[1]][[i]][[1]][[b]])[1]){
        
        # Get the name, chance, max level, min level, and method.
        Name   <- poke[c]
        chance <- raw_data[[1]][[i]][[1]][[b]][a,1]
        max    <- raw_data[[1]][[i]][[1]][[b]][a,3]
        min    <- raw_data[[1]][[i]][[1]][[b]][a,4]
        meth   <- raw_data[[1]][[i]][[1]][[b]][a,5]
        
        # Combine all vars into a line for tibble creation
        line   <- c(Name,loc,gm,chance,max,min,meth)
        
        # Build dfd tibble.
        # If this is the first iteration of the details loop
        # then dfd is created with all of the vars.
        # If not first iteration, bind dfd with line to add another line to dfd.
        if(a == 1){
          dfd <- tibble(Name,loc,gm,chance,max,min,meth)
        } else{
          dfd <- rbind(dfd,line)
        } # Close if else statement.
      } # Close details loop.
      
      # Build dfg tibble.
      # If first iteration of game loop, dfg is dfd.
      # If not first iteration, dfg is binded with dfd to add more lines.
      if(b == 1)
      {dfg <- dfd
      }else{
        dfg <- rbind(dfg,dfd)
      } # Close if else statement
    } # Close game loop
    
    # Create final tibble
    # If first iteration of location loop, dfl is the final dfg from the game loop.
    # If not first iteration, dfl is binded with the most recent dfg to add more lines.
    if(i==1){
      dfl <- dfg
    }else{
      dfl <- rbind(dfl,dfg)
    } # End if else statement
  } # End location loop
  # End of all loops
  
  # Rename variables in final dfl.
  names(dfl) <- c("Name","Location","Game","Chance","Max_Level","Min_Level","Method")
  
  # Keep only distinct rows, as there are some that repeat due to variables 
  # not important for this function.
  dfl <- distinct(dfl)
  

  # If first iteration of main loop, final tibble is dfl tibble.
  # If not first iteration, final tibble is binded with df1 tibble
  if(c == 1){
    final <- dfl
  }else{
    final <- rbind(final,dfl)
  } # End if else statement
  
  } # End main loop
  
  # If user wants all games returned, no change to final tibble.
  # If user wants specific games, final is filtered for these games.
  if("all" %in% game){
    final <- final
  }else{
    final <- final %>%
      filter(Game %in% game)
  } # End if else statement.
  
  # Return final tibble.
  return(final)
  } # End function.



mons <- c("charmander","pidgey","abra","beedrill") 
 
str(mons)

encounter(poke = mons, game = "all")

mons[1]





test <- egg_group()



test <- test %>%
  select(Egg_Group_1,Egg_Group_2)

table(test)





kanto_mons <- poke_api("dex", reg = "kanto")

kanto_mons <- as_vector(kanto_mons$Name)


kanto_stats <- poke_api("stats", poke = kanto_mons)

col_names <- names(kanto_stats)
col_names <- col_names[2:7]


hp_sum <- summary(kanto_stats$hp)
  
         