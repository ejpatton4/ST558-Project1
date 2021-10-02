library(httr)
library(jsonlite)
library(tidyverse)








groups <- fromJSON("https://pokeapi.co/api/v2/egg-group/")


# FiNISH THIS TABLE ASAP
  

get_group_ids <- function(x){
  
  raw_data <- fromJSON("https://pokeapi.co/api/v2/egg-group/")
  
  for(i in 1:length(x)){
    
    group_id <- x[i]
    
    group_name <- raw_data$results[["name"]][i]
    
    group_row_info <- tibble(group_id,group_name)
    
    if(i == 1){
      final_tibble <- group_row_info
    }else{
      final_tibble <- rbind(final_tibble,group_row_info)
    }
}

  return(final_tibble)
}

get_group_ids(c(1:15))







