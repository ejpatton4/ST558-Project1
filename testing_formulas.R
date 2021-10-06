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
hp_sum <- unname(hp_sum)  
hp_sum

attack_sum <- summary(kanto_stats$attack)
attack_sum <- unname(attack_sum)
attack_sum

test <- tibble(hp_sum,attack_sum)

tib <- tibble(hp_sum)

val <- col_names[1]

blue <- summary(kanto_stats$paste0(col_names[1]))

blue <- unname(blue)

for (i in 1:length(col_names)){
  
     test <- kanto_stats[2:7]
     
     names(test)[i] <- "need"
  
    summry <- summary(test$need)
    
    summry <- unname(summry,force = TRUE)
  
    sm <- tibble(summry)
  
  names(sm) <- c(paste0(col_names[i],"_summary"))
  
  if(i == 1){
    summ_table <- sm
  }else{
      summ_table <- tibble(summ_table,sm)
    }
}

row.names(summ_table) <- c("Min","1Q","Med","Mean","3Q","Max")

unname(summ_table)
str(summ_table)





summ_table$defense_summary[6]

kanto_stats$avg <- apply(kanto_stats[2:7],MARGIN = 1,FUN = mean)




kanto_stats <- kanto_stats %>%
  mutate(avg_base = mean(kanto_stats$hp,kanto_stats$attack,kanto_stats$defense,kanto_stats$`special-attack`,kanto_stats$`special-defense`,kanto_stats$speed))


size_2 <- function(poke) {
  
  # change poke to lower case for flexibility.
  poke <- tolower(poke)
  
  # Looping for entrance of a vector
  for(i in 1:length(poke)){
    
    # Get requested data from API
    raw_data <- fromJSON(paste0("https://pokeapi.co/api/v2/pokemon/",poke[i]), flatten = TRUE)
    
    # Select only the information about the base stats
    df <- as_tibble(raw_data[["stats"]])
    
    # Add a column for the name of the Pokemon 
    df$Name <- str_to_sentence(raw_data[["name"]])
    
    # Keep only needed columns and reorder
    df <- df[,c(5,1,3)]
    
    # Pivot from long form to a single row
    df <- df %>%
      pivot_wider(names_from = stat.name, values_from = base_stat)
    
    
    # If first iteration of loop, the final tibble  is created
    # If not the first iteration bind the final tibble from the previous loops 
    # with the new information
    if( i == 1){
      final <- df
    }else{
      final <- rbind(final,df)
    } # End if else statement
  
    message(paste0("Completed ",i," out of ",length(poke)," Rows."))
    }# End loop
  
  # Return the final tibble.
  return(final)
}

test_df <- size_2(poke = natl_dex$Name)


raw_data <- fromJSON("https://pokeapi.co/api/v2/berry/1")



berries <- function(berry){
  
  berry <- tolower(berry)

  if("all" %in% berry){
    berry <- c(1:64)
  }else{
    berry <- berry
  }

  for(i in 1:length(berry)){
  
  raw_data <- fromJSON(paste0("https://pokeapi.co/api/v2/berry/",berry[i]))

  name <- raw_data[["name"]]
  
  id <- raw_data[["id"]]
  
  size <- raw_data[["size"]]
  
  smoothness <- raw_data[["smoothness"]]
  
  nat_gift_power <- raw_data[["natural_gift_power"]]
  
  nat_gift_type <- raw_data[["natural_gift_type"]][["name"]]

  row_info <- tibble_row(name,id,size,smoothness,nat_gift_type,nat_gift_power)
  
  if(i == 1){
    df <- row_info
  }else{
    df <- rbind(df,row_info)
  } # End if else statement
  
} # End Berry loop

  
  return(df)
} #End Function


berries("cheri")


















