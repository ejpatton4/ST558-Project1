library(httr)
library(jsonlite)
library(tidyverse)


90.5/(1.7^2)

raw_data <- fromJSON("https://pokeapi.co/api/v2/pokemon/charizard", flatten = TRUE)


df <- raw_data[["stats"]]

df$name <- str_to_sentence("charizard")

df <- df[,c(5,1,3)]

df <- df %>%
  pivot_wider(names_from = stat.name, values_from = base_stat)


# This function retrieves the base stats for a given pokemon, or hopefully vector of pokemon.

stats <- function(poke) {
  
  raw_data <- fromJSON(paste0("https://pokeapi.co/api/v2/pokemon/",poke), flatten = TRUE)
  
  df <- raw_data[["stats"]]
  
  df$name <- str_to_sentence(poke)
  
  df <- df[,c(5,1,3)]
  
  df <- df %>%
    pivot_wider(names_from = stat.name, values_from = base_stat)
  
  return(df)
  
  
}