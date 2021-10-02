library(httr)
library(jsonlite)
library(tidyverse)




exp_func <- function(rate = "all",poke = "all",level = "all"){

rate <- tolower(rate)
poke <- tolower(poke)
level <- tolower(level) 

level_vec <- as.character()

if("all" %in% rate){
  rate <- c(1:6)
}else{
  rate <- rate
}


for(a in 1:100){
  
  
  level_vec[a] <- paste0("Level_",a)
  
}

for(i in 1:length(rate)){

raw_data <- fromJSON(paste0("https://pokeapi.co/api/v2/growth-rate/",rate[i]))


Description <- raw_data[["descriptions"]][["description"]][3]


all_exp <- raw_data[["levels"]][["experience"]]

for(b in 1:length(raw_data[["pokemon_species"]][["name"]])){
  
  Name <- raw_data[["pokemon_species"]][["name"]][b]
  
  row_info <- tibble(Name,Description,all_exp,level_vec)
 
  
  if(b == 1){
    
    df <- row_info
  }else{
    df <- rbind(df,row_info)
  }
}

df2 <- pivot_wider(df, id_cols = c(Name,Description), values_from = all_exp, names_from = level_vec)


if(i == 1){
  
  final <- df2
}else{
    final <- rbind(final,df2)
  }

}



if("all" %in% poke){
  final <- final
}else{
  final <- final %>%
          filter(Name %in% poke)
}

if("all" %in% level){
  final <- final
}else{
  lvl_vec <- as.character()
  
  for(c in 1:length(level)){
    lvl_vec[c] <- paste0("Level_",level[c])
  }
  
  final <- final %>%
    select(Name,Description,lvl_vec)
}


final$Name <- str_to_sentence(final$Name)

return(final)

} # End of function



exp_func("all","tentacool",c(1:10))


raw_exp_data <- fromJSON("https://pokeapi.co/api/v2/growth-rate/")

for(i in 1:6){
  
  id_num <- i
  
  exp_name <- raw_data[["results"]][["name"]][i]
  
  
  exp_df1 <- tibble(id_num,exp_name)
  
  if(i == 1){
    exp_df2 <- exp_df1
  } else{
    exp_df2 <- rbind(exp_df2,exp_df1)
  }
}

print(exp_df2)

