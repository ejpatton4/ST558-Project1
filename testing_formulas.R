library(httr)
library(jsonlite)
library(tidyverse)



raw_data <- fromJSON("https://pokeapi.co/api/v2/pokedex/1")



get_all_ids <- function(x){
  
  for(i in 1:length(x)){
    
  raw_data <- fromJSON(paste0("https://pokeapi.co/api/v2/pokedex/",x[i]))
  
  region_id <- raw_data[["id"]]
  
  region_name <- raw_data[["name"]]
  
  df <- tibble(region_id,region_name)
  
  if(i == 1){
    final <- df
  }else{
    final <- rbind(final,df)
  }
  
  }
  return(final)
}


get_all_ids(c(1:9,11:29))









dex <- function(reg = 1,poke = "all"){
  
  if( "all" %in% tolower(reg)){
    
    reg <- c(1:9,11:29)
  }else{
    reg <- reg
  }
  
  
  for(a in 1:length(reg)){
    
    raw_data <- fromJSON(paste0("https://pokeapi.co/api/v2/pokedex/",reg[a]), flatten = TRUE)
    
    dex_name <- str_to_sentence(raw_data[["name"]])
    
    for(i in 1:dim(raw_data[["pokemon_entries"]])[1]){
      
      
      dex_num <- raw_data[["pokemon_entries"]][i,1]
      nm <- str_to_sentence(raw_data[["pokemon_entries"]][i,2])
      
      
      row_info <- tibble(nm,dex_num)
      
      if(i == 1){
        df <- row_info
      }else{
        df <- rbind(df,row_info)
      }
    }
    
    
    
    names(df) <- c("Name",paste0(dex_name,"_Dex_Number"))
    
    
    
    if(a == 1){
      
      final <- df
    }else{
      final <- full_join(final,df,by = c("Name"="Name"))
    }
  }
  
  if("all" %in% tolower(poke)){
    final <- final
  }else{
    final <- final %>%
      filter(tolower(Name) %in% poke)
  }
  return(final)
}

yell <- c(5:1)

test <- dex(c(1,2),c("charizard","pidgey"))


reg <- c(1:10)

if("all" %in% reg){
  
  reg <- c(1:21)
}else{
  reg <- reg
}

length(reg)


