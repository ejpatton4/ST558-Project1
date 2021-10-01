library(httr)
library(jsonlite)
library(tidyverse)

blue <- c(1:5)

blue <- tolower(blue)

dim(raw_data$pokemon_species)[1]


group <- c(2,1,5)

length(group)


egg_group <- function(group = "all", poke = "all"){

  
  group <- tolower(group)
  poke  <- tolower(poke)
  
  if(group == "all"){
    group <- c(1:15)
  }else{
    group <- group
  }
  
  
for(i in 1:length(group)) {
  
raw_data <- fromJSON(paste0("https://pokeapi.co/api/v2/egg-group/",group[i]))

grp <- raw_data$name

   for(a in 1:dim(raw_data$pokemon_species)[1]){
     
     name <- raw_data$pokemon_species[a,1]
     
     
     row_info <- tibble(name,grp)
     
     
     if(a == 1){
       df <- row_info
     } else{
       df <- rbind(df,row_info)
     }
     
   }

    names(df) <- c("name",paste0(i,"_iteration"))

 if(i == 1){
  df2 <- df
 }else{
  df2 <- full_join(df2,df, by = c("name"= "name")) 
 }
}
    for(b in 1:dim(df2)[1]){
      
      row_na_info <- as.numeric(!is.na(df2[b,]))
      
      not_na <- which(row_na_info %in% 1)
      
      nm <- df2[b,not_na[1]]
      
      group1 <- df2[b,not_na[2]]
      
      if(length(not_na) == 3){
        group2 <- df2[b,not_na[3]]
      }else{
        group2 <- NA
      }
      
      new_row <- tibble(nm,group1,group2)
      
      names(new_row) <- c("Name","Egg_Group_1","Egg_Group_2")
      
      if(b == 1){
      df3 <- new_row
      }else{
      df3 <- rbind(df3,new_row)
    }
}

  
  if("all" %in% poke){
    final <- df3
  }else{
    final <- df3 %>%
      filter(Name %in% poke)
  }
  
  
  final$Name <- str_to_sentence(final$Name)
  
  return(final)
}





groups <- fromJSON("https://pokeapi.co/api/v2/egg-group/")


# FiNISH THIS TABLE ASAP
  
raw_data <- fromJSON("https://pokeapi.co/api/v2/egg-group/")

for(i in 1:length(raw_data$results[["name"]])){
  
  group_ids <- c(1:15)
  
  group_id <- group_ids[i]
  
  group_name <-  raw_data$results[["name"]][i]
  
  
  group_row_info <- tibble(group_id,group_name)
  
  if(i == 1){
    group_tibble <- group_row_info
  }else{
    group_tibble <- rbind(group_tibble,group_name)
  }
  
}

#kable(group_tibble)







test <- egg_group()




dim(final)[1]

row_na_info <- !is.na(final[1,])

row_na_info <- as.numeric(row_na_info)

not_na <- which(row_na_info %in% 1)

name <- final[1,(not_na[1])]

group_1 <- final[1,(not_na[2])]

group_2 <- final[1,(not_na[3])]


new_row <- tibble(name,group_1,group_2)


str(row_na_info)
typeof(row_na_info)

final <- final %>%
  mutate(Egg_Group_1 = )

