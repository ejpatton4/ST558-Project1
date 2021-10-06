
-   [Introduction](#introduction)
    -   [Packages Needed](#packages-needed)
-   [Building the Functions](#building-the-functions)
    -   [Encounter Function](#encounter-function)
    -   [Stats Function](#stats-function)
    -   [Size Function](#size-function)
    -   [Dex Function](#dex-function)
    -   [Egg Group Function](#egg-group-function)
    -   [Experience Function](#experience-function)
    -   [Berry Function](#berry-function)
    -   [Wrapper Function](#wrapper-function)
-   [Basic Functionality](#basic-functionality)
    -   [Encounter Example](#encounter-example)
    -   [Stats Example](#stats-example)
    -   [Size Example](#size-example)
    -   [Dex Example](#dex-example)
    -   [Egg Group Example](#egg-group-example)
    -   [Experience Function Example](#experience-function-example)
    -   [Berries Function Example](#berries-function-example)
    -   [Poke API Function Example](#poke-api-function-example)
-   [Exploratory Analysis](#exploratory-analysis)
    -   [Contingency Tables](#contingency-tables)
    -   [Numerical Summaries](#numerical-summaries)
    -   [BMI Scatter Plots](#bmi-scatter-plots)
    -   [Berry Bar Graph](#berry-bar-graph)
    -   [Base Stats Histograms](#base-stats-histograms)
    -   [BMI Box Plot](#bmi-box-plot)

# Introduction

Below you will find my Poke API Vignette. There are eight functions
available for use, one of which is a wrapper function to house the other
seven. You can use the functions for querying data from the [Poke API
website](https://pokeapi.co/). After showing how all the functions are
built I do a simple example of each. Finally I do some data analysis
using my created functions to show the type of analysis that can be
conducted with these functions and the data they provide access to.
Throughout this process,
[Bulbapedia](https://bulbapedia.bulbagarden.net/wiki/Main_Page) has been
an invaluable resource and will no doubt help someone trying to use
these functions. Thank you for reading and I hope you find this vignette
and these functions useful for your analysis!

## Packages Needed

In this Vignette we will need the following packages:

``` r
# You only need to run the install.packages() functions if you have not already downloaded the packages. 
# As I already have them downloaded, I am not doing that here. 
# If you need to, copy and paste the line needed, without the #, into your console and run.

# install.packages("tidyverse")
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("knitr")

library(httr)
library(jsonlite)
library(tidyverse)
library(knitr)
```

# Building the Functions

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
encounter the Pokemon (walking in tall grass vs being given as a gift as
an example.)

The function `encounter` is built below and an example will be shown
later. `encounter` takes two arguments, the first one is `poke` and is
not optional. That is for the Pokemon you want to find, it can be for a
single character string or a vector of them. The second argument,
`game`, is optional, and that is what game from the series you want to
return data on, if no game is selected then all games will be returned
in the final tibble.

Of note, this function will error if you request the data for a Pokemon
not able to be acquired. For instance Charizard can’t just be found in a
game you have to evolve a Charmander to a Charizard in order to have
one.

``` r
# Define function
encounter <- function(poke, game = "all") {
  
  # Converting inputs to lower case for flexibility
  poke <- tolower(poke)
  game <- tolower(game)
  
  # Start all loops.
  # Start loop for specific pokemon.
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
  
  # Capitalize name of the Pokemon.
  final$Name <- str_to_sentence(final$Name)
  
  # Return final tibble.
  return(final)
} # End function.
```

## Stats Function

Another major portion of the Pokemon video games is battling. During a
battle you want to make sure you have the strongest Pokemon, the way
their strength is determined is through their “base stats”.

Their base stats are:  
\* [HP](https://bulbapedia.bulbagarden.net/wiki/Stat#HP)  
\* [Attack](https://bulbapedia.bulbagarden.net/wiki/Stat#Attack)  
\* [Defense](https://bulbapedia.bulbagarden.net/wiki/Stat#Defense)  
\* [Special
Attack](https://bulbapedia.bulbagarden.net/wiki/Stat#Special_Attack)  
\* [Special
Defense](https://bulbapedia.bulbagarden.net/wiki/Stat#Special_Defense)  
\* [Speed](https://bulbapedia.bulbagarden.net/wiki/Stat#Speed)

You can read about what each one means by clicking on their names as
they are links to the Bulbapedia website which is a treasure trove of
Pokemon knowledge.

The `stats` function is built below and it will return this information
about the Pokemon the user chooses. The function takes a single argument
which is `poke` this can be a either a single character string or a
vector of them if you want to return the details about more Pokemon.

``` r
# Create the function
stats <- function(poke) {
  
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
  }# End loop
  
  # Return the final tibble.
  return(final)
} # End function
```

## Size Function

If you were interested in the heights and weights of Pokemon you can use
the `size` function. The `size` function takes a single argument, `poke`
that can be either a single character string of a Pokemon name, or a
vector of names if you wanted the information on more than one Pokemon.

The output of the `size` function is a tibble with the Pokemons name,
Height in meters, and Weight in kilograms.

``` r
# Create function.
size <- function(poke) {
  
  # Change poke to lower case for flexibility.
  poke <- tolower(poke)
  
  # Start loop if there is more than one Pokemon inputted.
  for(i in 1:length(poke)){
    
    # Get requested data from API.
    raw_data <- fromJSON(paste0("https://pokeapi.co/api/v2/pokemon/",poke[i]), flatten = TRUE)
    
    # Create objects to hold the height weight and name. 
    ht <- raw_data[["height"]]
    wt <- raw_data[["weight"]]
    Name <- str_to_sentence(raw_data[["name"]])
    
    # Turn the objects into a tibble. 
    df <- tibble(Name,ht,wt)
    
    # If first iteration of loop, the final tibble  is created
    # If not the first iteration bind the final tibble from the previous loops 
    # with the new information
    if( i == 1){
      final <- df
    }else{
      final <- rbind(final,df)
    } # End if else statement
  } # End loop
  
  # Edit the tibble before returning.
  # Height is stored decimeters, and weight as hectograms. 
  # We change them to meters and kilograms.
  # Finally select only the needed columns.
  final <- final %>%
    mutate(Height_Meters = ht / 10,
           Weight_Kilograms = wt / 10) %>%
    select(Name,Height_Meters,Weight_Kilograms)
  
  # Return the tibble.
  return(final)
} # End of function
```

## Dex Function

Throughout the Pokemon world there are various regions. Each region has
their own Pokedex as each region has their own Pokemon, sometimes
Pokemon can be found in multiple regions and other times they are only
found in one region. There is also a “National Dex” that gives a unique
number to every Pokemon in existence. The `dex` function is designed to
look up the Pokedex entries for a given region or in all regions.

The `dex` function takes two arguments, the first is `reg` which is the
specific region the user is looking up. This will default to the
National Dex if no region is chosen. The `reg` argument can accept
either the character string version of the region name or the id number
for the region (The relationship between the id number and name will be
shown after the function is built). This can also be a single input or a
vector.

The second argument is `poke` which is optional, this is to be used if
you want to filter the results down to specific Pokemon. This can be a
single character string or a vector of character strings. If no argument
is provided, the function defaults to all Pokemon that are in the
requested Pokedex(es).

**OF NOTE THE API DOES NOT HAVE A REGION ID \#10 IF YOU INCLUDE 10 IN
THE `reg` ARGUMENT, THE FUNCTION WILL NOT WORK.**

``` r
# Define function
dex <- function(reg = 1,poke = "all"){
  
  # Change both inputs to lower case for flexibility.
  reg <- tolower(reg)
  poke <- tolower(poke)
  
  
  # If "all" is selected for the reg argument, reg is defined as all available ID numbers.
  # If not reg remains what the user inputs.
  if( "all" %in% reg){
    reg <- c(1:9,11:29)
  }else{
    reg <- reg
  }  # End if else statement.
  
  # Begin Dex loop, for all Pokedexes requested.
  for(a in 1:length(reg)){
    
    # Get the data on the specific dex.
    raw_data <- fromJSON(paste0("https://pokeapi.co/api/v2/pokedex/",reg[a]), flatten = TRUE)
    
    # Save name of region
    dex_name <- str_to_sentence(raw_data[["name"]])
    
    # Begin poke loop for every Pokemon inside of specific dex.
    for(i in 1:dim(raw_data[["pokemon_entries"]])[1]){
      
      # Save the Pokedex entry number for the Pokemon.
      dex_num <- raw_data[["pokemon_entries"]][i,1]
      
      # Save the name of the Pokemon.
      nm <- str_to_sentence(raw_data[["pokemon_entries"]][i,2])
      
      # Combine the entry number and the name into a tibble.
      row_info <- tibble(nm,dex_num)
      
      # If first iteration of Poke loop, df is defined as the row_info tibble.
      # If not first iteration, df is combined with the latest row_info tibble to add new data.
      if(i == 1){
        df <- row_info
      }else{
        df <- rbind(df,row_info)
      } # End if else statement
    } # End Poke loop
    
    
    # Rename columns of df tibble, as Name for the Pokemons name and
    # (dex_name)_Dex_Number, so if the National Dex is the one being put together,
    # the column will be titled "National_Dex_Number".
    names(df) <- c("Name",paste0(dex_name,"_Dex_Number"))
    
    
    # If first iteration of Dex loop the final tibble is df tibble.
    # If not first iteration join final tibble and df tibble.
    if(a == 1){
      final <- df
    }else{
      final <- full_join(final,df,by = c("Name"="Name"))
    } # End of if else statement
  } # End of Dex loop
  
  
  # If user specifies Pokemon, filter the final tibble by defined inputs.
  if("all" %in% poke){
    final <- final
  }else{
    final <- final %>%
      filter(tolower(Name) %in% poke)
  } # End of if else statement.
  
  # Return final tibble.
  return(final)
  
} # End of function
```

Here is the relationship between the region names and their ID numbers.

| region\_id | region\_name      |
|-----------:|:------------------|
|          1 | national          |
|          2 | kanto             |
|          3 | original-johto    |
|          4 | hoenn             |
|          5 | original-sinnoh   |
|          6 | extended-sinnoh   |
|          7 | updated-johto     |
|          8 | original-unova    |
|          9 | updated-unova     |
|         11 | conquest-gallery  |
|         12 | kalos-central     |
|         13 | kalos-coastal     |
|         14 | kalos-mountain    |
|         15 | updated-hoenn     |
|         16 | original-alola    |
|         17 | original-melemele |
|         18 | original-akala    |
|         19 | original-ulaula   |
|         20 | original-poni     |
|         21 | updated-alola     |
|         22 | updated-melemele  |
|         23 | updated-akala     |
|         24 | updated-ulaula    |
|         25 | updated-poni      |
|         26 | updated-kanto     |
|         27 | galar             |
|         28 | isle-of-armor     |
|         29 | crown-tundra      |

## Egg Group Function

Another way to get new Pokemon is to breed the ones you already have.
Pokemon can only breed with other Pokemon in their specific egg groups.
For more information on egg groups click
[here](https://bulbapedia.bulbagarden.net/wiki/Egg_Group). If you have a
Pokemon and you want to know what egg group it is in for breeding
purposes you can use the `egg_group` function!

This function is designed to return a tibble with the Pokemons name and
two columns of egg group data with it. The function takes two arguments
and both are optional. The first is `group` which is for if the user
wants to specify which egg group(s) data is returned on. If no argument
is given then all groups will be returned. The inputs can be the
character string for the name of the group or the id number for the
group. This can be a single argument or a vector of them. The
relationship between the names and ids will be shown after the function
is built.

The second argument is `poke` which is if the user wants the data for
only specific Pokemon. The function defaults to all Pokemon available in
the selects group(s). The input can be either a single character string
or a vector of them.

Every Pokemon belongs to at least one egg group even if that group is
“no eggs”. Some Pokemon will belong to two different egg groups, so the
function is designed to always return two columns of egg groups, however
if the specific Pokemon only belongs to one the second column will be
`NA`. This is also true if the second egg group for the Pokemon is never
searched.

``` r
# Build function
egg_group <- function(group = "all", poke = "all"){
  
  # Change inputs to lower to case to allow for some flexibility.
  group <- tolower(group)
  poke  <- tolower(poke)
  
  # If group is all, we change what group is defined as to the numbers for every egg group id.
  # If group is not all, group remains unchanged
  if(group == "all"){
    group <- c(1:15)
  }else{
    group <- group
  } # End if else statement
  
  # Begin loop for specific egg group.
  for(i in 1:length(group)) {
    
    # Get the data we need from API.
    raw_data <- fromJSON(paste0("https://pokeapi.co/api/v2/egg-group/",group[i]))
    
    # Save the name of the egg group as grp.
    grp <- raw_data$name
    
    # Begin loop for Pokemon inside of egg group.
    for(a in 1:dim(raw_data$pokemon_species)[1]){
      
      # Save the name of the Pokemon as name.
      name <- raw_data$pokemon_species[a,1]
      
      # Create tibble with the name of the Pokemon and egg group.
      row_info <- tibble(name,grp)
      
      # If first iteration of Pokemon loop, the df tibble is defined as the row_info tibble.
      # If not first iteration, the df binded with row_info to add more inforamtion.
      if(a == 1){
        df <- row_info
      } else{
        df <- rbind(df,row_info)
      } # End of if else statement
    } # End of Pokemon for loop
    
    # Changing the names of the columns for easy joining.
    # The new column of egg groups is called _iteration with the iteration of the
    # egg group loop included before the _, this is to not overwrite previous data collected.
    names(df) <- c("name",paste0(i,"_iteration"))
    
    # If first iteration of egg group loop, the df2 tibble is defined as the most recent df tibble.
    # If not first iteration, the df2 tibble is full joined with df to add new data.
    if(i == 1){
      df2 <- df
    }else{
      df2 <- full_join(df2,df, by = c("name"= "name")) 
    } # End of if else statement.
  } # End of egg group loop.
  
  # After the egg group loop is completed we are left with a tibble that is not
  # very descriptive and contains a lot of NAs. This section below helps to clean 
  # data for return.
  
  # Begin cleaning loop.
  for(b in 1:dim(df2)[1]){
    
    # Find columns for row b that are not NA and return a vector of 1s and 0s.
    row_na_info <- as.numeric(!is.na(df2[b,]))
    
    # This finds what position in the df2 tibble was not NA.
    not_na <- which(row_na_info %in% 1)
    
    # The first non NA is always the name, this is save as nm.
    nm <- df2[b,not_na[1]]
    
    # The second non NA is the first egg group for a Pokemon.
    # This is saved as group1.
    group1 <- df2[b,not_na[2]]
    
    # Not all Pokemon have two egg groups this if else statement accounts for that.
    # If there are three columns of non NA, group2 is defined as the second egg group.
    # If there are not three NA free columns, group2 is defined as NA.
    if(length(not_na) == 3){
      group2 <- df2[b,not_na[3]]
    }else{
      group2 <- NA
    } # End of if else statement.
    
    # Create new_row tibble, with cleaned name and egg group(s).
    new_row <- tibble(nm,group1,group2)
    
    # Rename columns
    names(new_row) <- c("Name","Egg_Group_1","Egg_Group_2")
    
    # If first iteration of cleaning loop, df3 is defined as the new_row tibble.
    # If not first iteration df3 is binded with new_row to add new data.
    if(b == 1){
      df3 <- new_row
    }else{
      df3 <- rbind(df3,new_row)
    } # End of if else statement.
  } # End of cleaning loop
  
  # If user didn't specify Pokemon to return, the final tibble is defined as the most recent df3.
  # If user did specify df3 is filtered for the Pokemon and saved as the final tibble.
  if("all" %in% poke){
    final <- df3
  }else{
    final <- df3 %>%
      filter(Name %in% poke)
  } # End of if else statement.
  
  # Capitalize first letter of Pokemon name to match rest of functions.
  final$Name <- str_to_sentence(final$Name)
  
  # Return final tibble to user. 
  return(final)
} # End of function
```

Here is a table showing the relationship between the egg group id
numbers and their associated names.

| group\_id | group\_name   |
|----------:|:--------------|
|         1 | monster       |
|         2 | water1        |
|         3 | bug           |
|         4 | flying        |
|         5 | ground        |
|         6 | fairy         |
|         7 | plant         |
|         8 | humanshape    |
|         9 | water3        |
|        10 | mineral       |
|        11 | indeterminate |
|        12 | water2        |
|        13 | ditto         |
|        14 | dragon        |
|        15 | no-eggs       |

## Experience Function

When Pokemon battle they gain experience. This experience can make the
Pokemon grow to a new level, when Pokemon reach a new level they upgrade
their base stats that we looked at earlier and they can even evolve into
new Pokemon.

Every Pokemon has a certain formula that defines how much experience it
takes to get to the next level. These formulas can be read about
[here](https://bulbapedia.bulbagarden.net/wiki/Experience). If you were
interested in a question along the lines of “How much experience does it
take for my Pokemon to reach level 50?” you can use the `exp_func`!

`exp_func` is deigned to return a tibble with the Pokemons name, the
name of the level formula used, and the experience needed to reach
levels 1 through 100 (the maximum for every Pokemon). The function takes
three arguments `rate`, `poke`, and `level`, all arguments are optional
as they default to “all”. They also all can take a single input or a
vector of them.

`rate` is for the specific experience formulas you want to return data
on, if you were wondering what Pokemon were in a certain formula group.
You can define this as the name of the rate or the id number, the
relationship between these will be shown after.

`poke` is for if you only want to return the data on specific Pokemon.

`level` is for what levels you would like to return data on, if not all.
So if you wanted levels 10, 20, 30….level = c(10,20,30), would return
the data for those levels.

``` r
# Define function.
exp_func <- function(rate = "all",poke = "all",level = "all"){
  
  # Change inputs to lower case for variability in inputs.
  rate <- tolower(rate)
  poke <- tolower(poke)
  level <- tolower(level) 
  
  # Create empty object to be used later as the col names for the tibble.
  level_vec <- as.character()
  
  # If all rate are requested, rate is 1 through 6, if not rate is what was inputted.
  if("all" %in% rate){
    rate <- c(1:6)
  }else{
    rate <- rate
  } # End if else statement
  
  # Begin level for loop
  # Create vector for future column names.
  # Paste together "Level_" with the level number.
  for(a in 1:100){
    level_vec[a] <- paste0("Level_",a)
  } # End level for loop
  
  # Begin rate loop.
  # Returns all below data for requested rates.
  for(i in 1:length(rate)){
    
    # Get the data about the specific rate from API.
    raw_data <- fromJSON(paste0("https://pokeapi.co/api/v2/growth-rate/",rate[i]))
    
    # Save description of rate 
    Description <- raw_data[["descriptions"]][["description"]][3]
    
    # Save experience required to reach ever level.
    all_exp <- raw_data[["levels"]][["experience"]]
    
    # Begin pokemon loop.
    # Gets every Pokemon in the group.
    for(b in 1:length(raw_data[["pokemon_species"]][["name"]])){
      
      # Save name of Pokemon.
      Name <- raw_data[["pokemon_species"]][["name"]][b]
      
      # Create tibble with name, description, required experience, and the future column names.
      row_info <- tibble(Name,Description,all_exp,level_vec)
      
      # if first iteration of pokemon loop, the df tibble is the row_info tibble.
      # if not first iteration of pokemon loop, the df tibble is binded with the the new row_info to add more data.
      if(b == 1){
        df <- row_info
      }else{
        df <- rbind(df,row_info)
      } # End if else statement.
    } # End Pokemon loop
    
    # The tibble at the end of the Pokemon loop is in long form.
    # This changes it to wide form (one row per Pokemon) and saves as the df2 tibble.
    df2 <- pivot_wider(df, id_cols = c(Name,Description), values_from = all_exp, names_from = level_vec)
    
    # If first iteration of rate loop, the final tibble is the df2 tibble.
    # If not first iteration, final tibble is binded with the newest df2 tibble.
    if(i == 1){
      final <- df2
    }else{
      final <- rbind(final,df2)
    } # End if else statement.
  } # End of rate loop.
  
  
  # If all Pokemon are requested to return, no change to the final tibble.
  # If certain Pokemon are requested, the final tibble is filtered for them.
  if("all" %in% poke){
    final <- final
  }else{
    final <- final %>%
      filter(Name %in% poke)
  } # End if else statement.
  
  # If all levels are requested to return, no change to final tibble.
  # If certain levels are requested we build the names of the columns to return,
  # and select only those columns to return.
  if("all" %in% level){
    final <- final
  }else{
    lvl_vec <- as.character()
    
    for(c in 1:length(level)){
      lvl_vec[c] <- paste0("Level_",level[c])
    } 
    
    final <- final %>%
      select(Name,Description,lvl_vec)
  }# End if else statement.
  
  # Capitalize Names of Pokemon to match rest of functions.
  final$Name <- str_to_sentence(final$Name)
  
  # Return the final tibble.
  return(final)
  
} # End of function
```

Here is the relationship between experience rate formula names and id
numbers.

| id\_num | exp\_name           |
|--------:|:--------------------|
|       1 | slow                |
|       2 | medium              |
|       3 | fast                |
|       4 | medium-slow         |
|       5 | slow-then-very-fast |
|       6 | fast-then-very-slow |

## Berry Function

Berrries are items in the Pokemon world that can be given to a Pokemon
to either eat or hold on to. Different berries have different properties
that they effect. One of these properties is, when a Pokemon is taught
the move “Natural Gift” if they are holding a berry the “type” of attack
(think water, fire, electric etc.) can change. For more information
about this I recommend
[this](https://bulbapedia.bulbagarden.net/wiki/Natural_Gift_(move)) page
on Bulbapedia.

So if you were interested in what berry does what type of attack with
the “Natural Gift” move you could use the `berries` function! This
function is designed to return the name, id number, size, smoothness,
natural gift type, and natural gift power. This function takes one
argument `berry`, this is defaulted to “all” since that is the most
likely request, but you can give a vector of ID numbers or names if you
only wanted specific information.

``` r
# Define Function
berries <- function(berry){
  
  # Change to lower to allow for flexibility.
  berry <- tolower(berry)
  
  # If all in berry, then berry is changed to 1-64.
  # If all isn't requested, berry remains unchanged.
  if("all" %in% berry){
    berry <- c(1:64)
  }else{
    berry <- berry
  } # End if else statement
  
  # Start Berry Loop
  for(i in 1:length(berry)){
    
    # Get the data for specific berry.
    raw_data <- fromJSON(paste0("https://pokeapi.co/api/v2/berry/",berry[i]))
    
    # Save name of berry.
    name <- raw_data[["name"]]
    
    # Save ID number of berry.
    id <- raw_data[["id"]]
    
    # Save size of berry
    size <- raw_data[["size"]]
    
    # Save Smootheness of berry
    smoothness <- raw_data[["smoothness"]]
    
    # Save power of natural gift.
    nat_gift_power <- raw_data[["natural_gift_power"]]
    
    # Save type of natural gift.
    nat_gift_type <- raw_data[["natural_gift_type"]][["name"]]
    
    # Save as a row.
    row_info <- tibble_row(name,id,size,smoothness,nat_gift_type,nat_gift_power)
    
    # If first iteration of loop, the df tibble is row_info.
    # If not first iteration of loop, bind df and row_info together.
    if(i == 1){
      df <- row_info
    }else{
      df <- rbind(df,row_info)
    } # End if else statement
    
  } # End Berry loop
  
  # Capitalize name of berry and the type.
  df$name <- str_to_sentence(df$name)
  df$nat_gift_type <- str_to_sentence(df$nat_gift_type)
  
  # Return df tibble
  return(df)
} #End Function
```

## Wrapper Function

The final function to show is just a wrapper function called `poke_api`.
The purpose of this function is to house all the other functions that we
have shown here. Essentially you can just use `poke_api` to get any of
the data you would like.

`poke_api` takes one required argument, `func`, that is the name of the
function you would like to use as a quoted string and then you also pass
the arguments required for the requested function as well.

If this is confusing an example will be shown later.

``` r
# Build function
poke_api <- function(func,...){
  
  # Choose which function to use
  switch(func,
         "encounter" = encounter(...),
         "stats"     = stats(...),
         "size"      = size(...),
         "dex"       = dex(...),
         "egg_group" = egg_group(...),
         "exp_func"  = exp_func(...),
         "berries"   = berries(...)
  )
} #End function
```

# Basic Functionality

Below we show a basic example of each function.

## Encounter Example

Here we test the encounter function with a vector we create called mons
that we will use throughout the examples. Also, we are only requesting
information of the red and fire red versions of the games.

``` r
# Create a vector of four Pokemon
mons <- c("charmander","pidgey","abra","beedrill") 

# Use the function and save results in an object
enc_example <- encounter(poke = mons, game = c("red","firered"))

# Print the object
print(enc_example)
```

    ## # A tibble: 91 x 7
    ##    Name       Location            Game    Chance Max_Level Min_Level Method
    ##    <chr>      <chr>               <chr>   <chr>  <chr>     <chr>     <chr> 
    ##  1 Charmander pallet-town-area    red     100    5         5         gift  
    ##  2 Charmander pallet-town-area    firered 100    5         5         gift  
    ##  3 Pidgey     kanto-route-12-area red     20     25        25        walk  
    ##  4 Pidgey     kanto-route-12-area red     15     23        23        walk  
    ##  5 Pidgey     kanto-route-12-area red     5      27        27        walk  
    ##  6 Pidgey     kanto-route-12-area firered 10     23        23        walk  
    ##  7 Pidgey     kanto-route-12-area firered 10     25        25        walk  
    ##  8 Pidgey     kanto-route-12-area firered 5      27        27        walk  
    ##  9 Pidgey     kanto-route-12-area firered 4      23        23        walk  
    ## 10 Pidgey     kanto-route-12-area firered 1      23        23        walk  
    ## # ... with 81 more rows

## Stats Example

Here is an example of the `stats` function, with the mons vector.

``` r
# Use the function on the vector
stat_example <- stats(mons)

# Print results
print(stat_example)
```

    ## # A tibble: 4 x 7
    ##   Name          hp attack defense `special-attack` `special-defense` speed
    ##   <chr>      <int>  <int>   <int>            <int>             <int> <int>
    ## 1 Charmander    39     52      43               60                50    65
    ## 2 Pidgey        40     45      40               35                35    56
    ## 3 Abra          25     20      15              105                55    90
    ## 4 Beedrill      65     90      40               45                80    75

## Size Example

Here we test the `size` function, with the same vector of four Pokemon.

``` r
# Use the function on the vector
size_example <- size(mons)

# Print results
print(size_example)
```

    ## # A tibble: 4 x 3
    ##   Name       Height_Meters Weight_Kilograms
    ##   <chr>              <dbl>            <dbl>
    ## 1 Charmander           0.6              8.5
    ## 2 Pidgey               0.3              1.8
    ## 3 Abra                 0.9             19.5
    ## 4 Beedrill             1               29.5

## Dex Example

Here we test the `dex` function. We are using regions with dex ids 1
through 5, and only returning results on our same four Pokemon. Any `NA`
means that the Pokemon is not in that Pokedex.

``` r
# Use function on vectors
dex_example <- dex(reg = c(1:5), poke = mons)

# Print results
print(dex_example)
```

    ## # A tibble: 4 x 6
    ##   Name       National_Dex_Number Kanto_Dex_Number `Original-johto_Dex_Number` Hoenn_Dex_Number `Original-sinnoh_Dex_Number`
    ##   <chr>                    <int>            <int>                       <int>            <int>                        <int>
    ## 1 Charmander                   4                4                         229               NA                           NA
    ## 2 Beedrill                    15               15                          29               NA                           NA
    ## 3 Pidgey                      16               16                          10               NA                           NA
    ## 4 Abra                        63               63                          89               39                           20

## Egg Group Example

Here we test the `egg_group` function. We are using all egg groups, and
returning the information for the Pokemon in our `mons` vector that we
have been using. Any `NA` means the Pokemon doesn’t have a second egg
group.

``` r
# Use function on mons vector.
egg_example <- egg_group(group = "all", poke = mons)

# Return the tibble.
print(egg_example)
```

    ## # A tibble: 4 x 3
    ##   Name       Egg_Group_1 Egg_Group_2
    ##   <chr>      <chr>       <chr>      
    ## 1 Charmander monster     dragon     
    ## 2 Beedrill   bug         <NA>       
    ## 3 Pidgey     flying      <NA>       
    ## 4 Abra       humanshape  <NA>

## Experience Function Example

Here we test the `exp_func` function. We are using all rate groups, the
same `mons` vector we have been using, and levels 10, 25, 50, 75, and
100.

``` r
# Use function on mons vector and for the specific levels we want.
exp_example <- exp_func(rate = "all", poke = mons, level = c(10,25,50,75,100))

# Return the tibble.
print(exp_example)
```

    ## # A tibble: 4 x 7
    ##   Name       Description Level_10 Level_25 Level_50 Level_75 Level_100
    ##   <chr>      <chr>          <int>    <int>    <int>    <int>     <int>
    ## 1 Beedrill   medium          1000    15625   125000   421875   1000000
    ## 2 Charmander medium slow      560    11735   117360   429235   1059860
    ## 3 Pidgey     medium slow      560    11735   117360   429235   1059860
    ## 4 Abra       medium slow      560    11735   117360   429235   1059860

## Berries Function Example

Below we show how to use the `berries` function. We simply return the
information on all berries and print the head of the list.

``` r
# Use function to get data
berry_example <- berries("all")

# Print the head of the tibble.
kable(head(berry_example))
```

| name   |  id | size | smoothness | nat\_gift\_type | nat\_gift\_power |
|:-------|----:|-----:|-----------:|:----------------|-----------------:|
| Cheri  |   1 |   20 |         25 | Fire            |               60 |
| Chesto |   2 |   80 |         25 | Water           |               60 |
| Pecha  |   3 |   40 |         25 | Electric        |               60 |
| Rawst  |   4 |   32 |         25 | Grass           |               60 |
| Aspear |   5 |   50 |         25 | Ice             |               60 |
| Leppa  |   6 |   28 |         20 | Fighting        |               60 |

## Poke API Function Example

Here we test the final function! `poke_api` if you remember this is a
wrapper function that has all the other functions included inside of it.
We are testing the `encounter` function with our `mons` vector and for
only the games red and fire red. This is the exact same result as the
`encounter` example from earlier.

``` r
# Use function
poke_api(func = "encounter", poke = mons, game = c("red","firered"))
```

    ## # A tibble: 91 x 7
    ##    Name       Location            Game    Chance Max_Level Min_Level Method
    ##    <chr>      <chr>               <chr>   <chr>  <chr>     <chr>     <chr> 
    ##  1 Charmander pallet-town-area    red     100    5         5         gift  
    ##  2 Charmander pallet-town-area    firered 100    5         5         gift  
    ##  3 Pidgey     kanto-route-12-area red     20     25        25        walk  
    ##  4 Pidgey     kanto-route-12-area red     15     23        23        walk  
    ##  5 Pidgey     kanto-route-12-area red     5      27        27        walk  
    ##  6 Pidgey     kanto-route-12-area firered 10     23        23        walk  
    ##  7 Pidgey     kanto-route-12-area firered 10     25        25        walk  
    ##  8 Pidgey     kanto-route-12-area firered 5      27        27        walk  
    ##  9 Pidgey     kanto-route-12-area firered 4      23        23        walk  
    ## 10 Pidgey     kanto-route-12-area firered 1      23        23        walk  
    ## # ... with 81 more rows

# Exploratory Analysis

Now we will begin to use our functions to do some data analysis and
create some graphs!

## Contingency Tables

The first thing we are going to do is make some contingency tables!

Our first one is a table on the relationship of egg groups to one
another. Personally I have always been curious about which egg groups
have the most overlap so lets find out.

``` r
# Use poke_api to get the data we need and save as object egg_table.
# We want all Pokemon and egg groups so no need for other arguments.
egg_table <- poke_api("egg_group")

# Keep only rows needed for table.
egg_table <- egg_table %>%
  select(Egg_Group_1,Egg_Group_2)

# Create the contingency table for egg groups and use kable to make it render well.
kable(table(egg_table))
```

|               | bug | dragon | fairy | flying | ground | humanshape | indeterminate | mineral | plant | water1 | water2 | water3 |
|:--------------|----:|-------:|------:|-------:|-------:|-----------:|--------------:|--------:|------:|-------:|-------:|-------:|
| bug           |   0 |      0 |     2 |      0 |      0 |          2 |             0 |       2 |     2 |      0 |      0 |      4 |
| ditto         |   0 |      0 |     0 |      0 |      0 |          0 |             0 |       0 |     0 |      0 |      0 |      0 |
| dragon        |   0 |      0 |     0 |      0 |      0 |          0 |             0 |       0 |     0 |      0 |      0 |      0 |
| fairy         |   0 |      0 |     0 |      0 |      0 |          0 |             1 |       4 |    11 |      0 |      0 |      0 |
| flying        |   0 |      2 |     2 |      0 |      3 |          0 |             0 |       0 |     0 |      0 |      0 |      2 |
| ground        |   0 |      5 |    10 |      0 |      0 |         11 |             0 |       0 |     6 |      0 |      2 |      0 |
| humanshape    |   0 |      0 |     0 |      0 |      0 |          0 |             0 |       0 |     0 |      0 |      0 |      0 |
| indeterminate |   0 |      0 |     0 |      0 |      0 |          0 |             0 |       0 |     0 |      0 |      0 |      0 |
| mineral       |   0 |      0 |     0 |      0 |      0 |          0 |             2 |       0 |     0 |      0 |      0 |      0 |
| monster       |   0 |     21 |     0 |      0 |     13 |          0 |             0 |       0 |    12 |     13 |      0 |      0 |
| no-eggs       |   0 |      0 |     0 |      0 |      0 |          0 |             0 |       0 |     0 |      0 |      0 |      0 |
| plant         |   0 |      0 |     0 |      0 |      0 |          2 |             2 |       2 |     0 |      0 |      0 |      0 |
| water1        |   4 |     10 |     4 |      4 |     20 |          0 |             3 |       0 |     3 |      0 |      6 |     11 |
| water2        |   0 |      2 |     0 |      0 |      0 |          0 |             0 |       0 |     0 |      0 |      0 |      0 |
| water3        |   0 |      0 |     0 |      0 |      0 |          0 |             0 |       0 |     0 |      0 |      0 |      0 |

You can read this as the number being how many Pokemon have both of
these egg groups. Of note, this is only for Pokemon with two egg groups,
if they only had one they were excluded.

It looks like the monster and dragon egg groups have the most overlap,
with 21 Pokemon having both followed closely by 20 from water1 and
ground. So if you are looking for a Pokemon to breed those two
combinations are likely a good place to start.

For this next table we are looking at where to find the Pokemon Rattata
and Pidgey in the Pokemon Red game. Both of these Pokemon can usually be
found early in the game and are good starting members of your team so
finding their location could be quite helpful.

``` r
# Use poke_api to get the data we need and save as pidg_rat_table.
# We want to see where all you can find these Pokemon in Pokemon Red.
pidg_rat_table <- poke_api("encounter",poke = c("pidgey","rattata"),game ="red")

# Keep only the columns we need.
pidg_rat_table <- pidg_rat_table %>%
  select(Name,Location)

# Create table and use kable to print nicely.
kable(table(pidg_rat_table))
```

|         | kanto-route-1-area | kanto-route-12-area | kanto-route-13-area | kanto-route-14-area | kanto-route-15-area | kanto-route-16-area | kanto-route-2-south-towards-viridian-city | kanto-route-22-area | kanto-route-24-area | kanto-route-25-area | kanto-route-3-area | kanto-route-4-area | kanto-route-5-area | kanto-route-6-area | kanto-route-7-area | kanto-route-8-area | kanto-route-9-area | kanto-sea-route-21-area |
|:--------|-------------------:|--------------------:|--------------------:|--------------------:|--------------------:|--------------------:|------------------------------------------:|--------------------:|--------------------:|--------------------:|-------------------:|-------------------:|-------------------:|-------------------:|-------------------:|-------------------:|-------------------:|------------------------:|
| Pidgey  |                  6 |                   3 |                   2 |                   1 |                   1 |                   0 |                                         3 |                   0 |                   2 |                   1 |                  3 |                  0 |                  3 |                  3 |                  2 |                  2 |                  0 |                       2 |
| Rattata |                  4 |                   0 |                   0 |                   0 |                   0 |                   3 |                                         4 |                   3 |                   0 |                   0 |                  0 |                  3 |                  0 |                  0 |                  0 |                  0 |                  3 |                       2 |

As you can see from the table, if you are looking to catch both of these
Pokemon, the Kanto Route 1 Area looks like a great place to start.

For this next contingency table we want to look at what games to find
some Pokemon in. For this I have chosen Charmander, Lapras, MewTwo,
Hitmonchan, and Pansear as they are some of my favorites.

``` r
# Use poke_api to get the data, save as object game_table
game_table <- poke_api("encounter",poke = c("Charmander","lapras","mewtwo","hitmonchan","pansear"))

# Only keep needed columns.
game_table <- game_table %>%
  select(Name, Game)

# Create table and use kable.
kable(table(game_table))
```

|            | black | black-2 | blue | diamond | firered | heartgold | leafgreen | pearl | platinum | red | soulsilver | white | white-2 |   x |   y | yellow |
|:-----------|------:|--------:|-----:|--------:|--------:|----------:|----------:|------:|---------:|----:|-----------:|------:|--------:|----:|----:|-------:|
| Charmander |     0 |       0 |    1 |       0 |       1 |         1 |         1 |     0 |        0 |   1 |          1 |     0 |       0 |   1 |   1 |      1 |
| Hitmonchan |     0 |       0 |    1 |       0 |       1 |         0 |         1 |     0 |        0 |   1 |          0 |     0 |       0 |   0 |   0 |      1 |
| Lapras     |     2 |       2 |    1 |       3 |       2 |         0 |         2 |     3 |        3 |   1 |          0 |     2 |       2 |   3 |   3 |      1 |
| Mewtwo     |     0 |       0 |    1 |       0 |       1 |         1 |         1 |     0 |        0 |   1 |          1 |     0 |       0 |   0 |   0 |      1 |
| Pansear    |     3 |       2 |    0 |       0 |       0 |         0 |         0 |     0 |        0 |   0 |          0 |     3 |       2 |   1 |   1 |      0 |

From the table it appears the Fire Red and Leaf Green games would be a
great place to start. Except for Pansear, if I wanted to find that
Pokemon, the Black and White versions of the game look like the winner.

## Numerical Summaries

Next I want to look into the base stats from each Pokemon in the Kanto
region. First we have to get a list of all Pokemon in the Kanto region
if only there was someway to do that, OH WAIT, the `dex` function will
do that for us. After we get this list we can use it in the `stats`
function to retrieve all of their base stats.

After getting all of their base stats we can create six number summaries
for each stat.

``` r
# First get a list of all Pokemon in the Kanto region.
kanto_mons <- poke_api("dex", reg = "kanto")

# Get the stats for the Kanto region Pokemon
kanto_stats <- poke_api("stats", poke = kanto_mons$Name)

# Create six number summary for each of the base stats.

# Create vector of the names of the base stats.
col_names <- names(kanto_stats)[2:7]

# for loop to get the information for each needed column.
for (i in 1:length(col_names)){
  
  # Save the kanto_stats tibble without the Name column
  kanto_stats_2 <- kanto_stats[2:7]
  
  # rename the wanted column in the second tibble.
  names(kanto_stats_2)[i] <- "need"
  
  # Use the summary function on the wanted column.
  summry <- summary(kanto_stats_2$need)
  
  # Remove the names from the six number summary.
  summry <- unname(summry,force = TRUE)
  
  # create a tibble with the six summary data. 
  sm <- tibble(summry)
  
  # rename the new summary information with the correct column name
  names(sm) <- c(paste0(col_names[i],"_summary"))
  
  # If first iteration of loop, summ_table is the sm tibble.
  # If not first iteration, combine the summ_table and sm.
  if(i == 1){
    summ_table <- sm
  }else{
    summ_table <- tibble(summ_table,sm)
  } # End if else statement. 
} # End for loop.

# Names of the summary rows.
values <- c("Min","1Q","Med","Mean","3Q","Max")

# Add names of the rows to the tibble for better looking print.
summ_table <- tibble(values,summ_table)

# Kable function for good printing.
kable(summ_table)
```

| values | hp\_summary | attack\_summary | defense\_summary | special-attack\_summary | special-defense\_summary | speed\_summary |
|:-------|------------:|----------------:|-----------------:|------------------------:|-------------------------:|---------------:|
| Min    |       10.00 |            5.00 |             5.00 |                   15.00 |                    20.00 |          15.00 |
| 1Q     |       45.00 |           51.00 |            50.00 |                   45.00 |                    49.00 |          46.50 |
| Med    |       60.00 |           70.00 |            65.00 |                   65.00 |                    65.00 |          70.00 |
| Mean   |       64.21 |           72.91 |            68.23 |                   67.14 |                    66.09 |          69.07 |
| 3Q     |       80.00 |           92.00 |            84.00 |                   87.50 |                    80.00 |          90.00 |
| Max    |      250.00 |          134.00 |           180.00 |                  154.00 |                   125.00 |         150.00 |

Personally I was the most surprised about the big difference between the
maximum of attack vs the maximum of defense. I thought these would be
closer instead of the 46 point difference that we see.

While preforming this analysis, I became interested in the mean base
stat for each Pokemon and the summary of those, so lets find that out.
Here I calculate the average stats value for each Pokemon, I print the
top six values in descending order of their average base stat. Then I
create a six number summary of the average base stat.

``` r
# Use apply to calculate the average for each row.
kanto_stats$avg_base <- apply(kanto_stats[2:7],MARGIN = 1,FUN = mean)

# Print the top six values in descending order of high average base stat.
kable(head(kanto_stats %>%
             arrange(-avg_base)))
```

| Name      |  hp | attack | defense | special-attack | special-defense | speed | avg\_base |
|:----------|----:|-------:|--------:|---------------:|----------------:|------:|----------:|
| Mewtwo    | 106 |    110 |      90 |            154 |              90 |   130 | 113.33333 |
| Dragonite |  91 |    134 |      95 |            100 |             100 |    80 | 100.00000 |
| Mew       | 100 |    100 |     100 |            100 |             100 |   100 | 100.00000 |
| Articuno  |  90 |     85 |     100 |             95 |             125 |    85 |  96.66667 |
| Zapdos    |  90 |     90 |      85 |            125 |              90 |   100 |  96.66667 |
| Moltres   |  90 |    100 |      90 |            125 |              85 |    90 |  96.66667 |

``` r
# Create six number summary.
avg_sum <- summary(kanto_stats$avg_base)

# Print the summary
kable(as.array(avg_sum))
```

| Var1    |      Freq |
|:--------|----------:|
| Min.    |  32.50000 |
| 1st Qu. |  53.33333 |
| Median  |  67.50000 |
| Mean    |  67.94040 |
| 3rd Qu. |  81.66667 |
| Max.    | 113.33333 |

My biggest take away from this analysis here is that I need to try and
get a Dragonite in my next play thorough of a game in the Kanto region.
Dragonite is the only one in the top six who is not a legendary Pokemon
so Dragonites inclusion, not only on the list in general but at the
second spot on the list, is impressive.

## BMI Scatter Plots

Now lets make some plots to investigate the data further.

I have always been curious if a Pokemons size has any impact on their
base stats. So lets find out!

First we get the national dex number for each Pokemon. Then we get the
size and stats for all of them. In order to quantify the “size” I use
the human [BMI
formula](https://www.cdc.gov/nccdphp/dnpao/growthcharts/training/bmiage/page5_1.html#:~:text=With%20the%20metric%20system%2C%20the,by%2010%2C000%2C%20can%20be%20used.)
on the Pokemon to calculate their BMI. I then create a scatter plot for
each base stat with a smooth line added.

Of note we removed the Pokemon Cosmoem from the data as they are a huge
outlier with a BMI of 99990, the next closest was 444.

``` r
# Get all Pokemons national dex number
natl_dex <- poke_api("dex", reg = 1, poke = "all")

# Use their national dex number to get the size and stats for each Pokemon.
all_stats <- poke_api("stats", poke = natl_dex$National_Dex_Number)
all_size  <- poke_api("size", poke = natl_dex$National_Dex_Number)

# Print head of size
kable(head(all_size))
```

| Name       | Height\_Meters | Weight\_Kilograms |
|:-----------|---------------:|------------------:|
| Bulbasaur  |            0.7 |               6.9 |
| Ivysaur    |            1.0 |              13.0 |
| Venusaur   |            2.0 |             100.0 |
| Charmander |            0.6 |               8.5 |
| Charmeleon |            1.1 |              19.0 |
| Charizard  |            1.7 |              90.5 |

``` r
# We calcualte the BMI for each and add to the table.
all_size <- all_size %>%
  mutate(BMI = (Weight_Kilograms / (Height_Meters^2)))

# Print top of BMI.
kable(head(all_size %>%
             arrange(-BMI)))
```

| Name              | Height\_Meters | Weight\_Kilograms |        BMI |
|:------------------|---------------:|------------------:|-----------:|
| Cosmoem           |            0.1 |             999.9 | 99990.0000 |
| Minior-red-meteor |            0.3 |              40.0 |   444.4444 |
| Aron              |            0.4 |              60.0 |   375.0000 |
| Durant            |            0.3 |              33.0 |   366.6667 |
| Clamperl          |            0.4 |              52.5 |   328.1250 |
| Torkoal           |            0.5 |              80.4 |   321.6000 |

``` r
# Join stats and size
all_size_stats <- left_join(all_stats,all_size, by = c("Name" = "Name"))

# Remove a major outlier.
all_size_stats <- all_size_stats %>%
  filter(Name != "Cosmoem")

# Create plot with HP.
all_size_stats %>%
  ggplot(aes(x = BMI, y = hp))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "BMI", y = "HP Stat", title = "Does a Pokemons BMI Effect Their HP?")
```

![](README_files/figure-gfm/BMI%20plots-1.png)<!-- -->

``` r
# Create plot with Attack.
all_size_stats %>%
  ggplot(aes(x = BMI, y = attack))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "BMI", y = "Attack Stat", title = "Does a Pokemons BMI Effect Their Attack?")
```

![](README_files/figure-gfm/BMI%20plots-2.png)<!-- -->

``` r
# Create plot with Defense.
all_size_stats %>%
  ggplot(aes(x = BMI, y = defense))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "BMI", y = "Defense Stat", title = "Does a Pokemons BMI Effect Their Defense?")
```

![](README_files/figure-gfm/BMI%20plots-3.png)<!-- -->

``` r
# Create plot with Special - Attack.
all_size_stats %>%
  ggplot(aes(x = BMI, y = `special-attack`))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "BMI", y = "Special-Attack Stat", title = "Does a Pokemons BMI Effect Their Special-Attack?")
```

![](README_files/figure-gfm/BMI%20plots-4.png)<!-- -->

``` r
# Create plot with Special-Defense.
all_size_stats %>%
  ggplot(aes(x = BMI, y = `special-defense`))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "BMI", y = "Special-Defense Stat", title = "Does a Pokemons BMI Effect Their Special-Defense?")
```

![](README_files/figure-gfm/BMI%20plots-5.png)<!-- -->

``` r
# Create plot with Speed.
all_size_stats %>%
  ggplot(aes(x = BMI, y = speed))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "BMI", y = "Speed Stat", title = "Does a Pokemons BMI Effect Their Speed?")
```

![](README_files/figure-gfm/BMI%20plots-6.png)<!-- -->

Looking at these scatter plots it does appear that there is some
relationship between BMI and Speed, Special Attack,and Defense. While
HP, Attack, and Special Defense do not appear to have much of an effect
from BMI. Maybe this would change if we removed a few more of the
outliers.

## Berry Bar Graph

I was curious how many berries are related to each natural gift type. So
lets make a bar plot to check it out. Earlier we created `berry_example`
which is a list of all berries and their attributes, so no need to call
the function again here we will just use the one already created. For
fun, in the graphs we also use the official Pokemon yellow and blue!

``` r
# Create bar plot
berry_example %>%
  ggplot(aes(y = nat_gift_type))+
  geom_bar(position = "dodge", fill = "#FFDE00", color = "#3B4CCA")+ # Use official Pokemon colors for graph.
  labs(
    x = "Count of Berries",
    y = "Natural Gift Type",
    title = "How Many Berries Give Each Type?"
  )
```

![](README_files/figure-gfm/berry%20bar%20graph-1.png)<!-- -->

I was surprised to see that this was so consistent. I thought that there
would be more spread between how many berries cause each natural gift
type, instead almost all of them have four berries that cause the
specific type. The exceptions are Steel which has three berries that
could be used, and Normal which only has the two berries.

## Base Stats Histograms

Earlier we looked at six number summary for the base stats of Pokemon in
the Kanto region. Here we are going to continue some analysis along the
same path. Except now we will create histograms for it!

From earlier we already have an object called `kanto_stats` that has all
the base stats for the Pokemon in the Kanto region, as well as the
`avg_base` variable that we created earlier. Since this is already
created we will use it here.

``` r
# Create Histogram for HP
kanto_stats %>%
  ggplot(aes(x = hp))+
  geom_histogram(fill = "#FFDE00", color = "#3B4CCA", binwidth = 10)+
  labs(
    x = "HP Base Stat",
    y = "Count of Bin",
    title = "Histogram of the HP Base Stat")
```

![](README_files/figure-gfm/base%20stats%20histograms-1.png)<!-- -->

``` r
# Create Histogram for Attack
kanto_stats %>%
  ggplot(aes(x = attack))+
  geom_histogram(fill = "#FFDE00", color = "#3B4CCA", binwidth = 10)+
  labs(
    x = "Attack Base Stat",
    y = "Count of Bin",
    title = "Histogram of the Attack Base Stat")
```

![](README_files/figure-gfm/base%20stats%20histograms-2.png)<!-- -->

``` r
# Create Histogram for Defense
kanto_stats %>%
  ggplot(aes(x = defense))+
  geom_histogram(fill = "#FFDE00", color = "#3B4CCA", binwidth = 10)+
  labs(
    x = "Defense Base Stat",
    y = "Count of Bin",
    title = "Histogram of the Defense Base Stat")
```

![](README_files/figure-gfm/base%20stats%20histograms-3.png)<!-- -->

``` r
# Create Histogram for Special Attack
kanto_stats %>%
  ggplot(aes(x = `special-attack`))+
  geom_histogram(fill = "#FFDE00", color = "#3B4CCA", binwidth = 10)+
  labs(
    x = "Special-Attack Base Stat",
    y = "Count of Bin",
    title = "Histogram of the Special-Attack Base Stat")
```

![](README_files/figure-gfm/base%20stats%20histograms-4.png)<!-- -->

``` r
# Create Histogram for Special Defense
kanto_stats %>%
  ggplot(aes(x = `special-defense`))+
  geom_histogram(fill = "#FFDE00", color = "#3B4CCA", binwidth = 10)+
  labs(
    x = "Special-Defense Base Stat",
    y = "Count of Bin",
    title = "Histogram of the Special-Defense Base Stat")
```

![](README_files/figure-gfm/base%20stats%20histograms-5.png)<!-- -->

``` r
# Create Histogram for Speed
kanto_stats %>%
  ggplot(aes(x = speed))+
  geom_histogram(fill = "#FFDE00", color = "#3B4CCA", binwidth = 10)+
  labs(
    x = "Speed Base Stat",
    y = "Count of Bin",
    title = "Histogram of the Speed Base Stat")
```

![](README_files/figure-gfm/base%20stats%20histograms-6.png)<!-- -->

``` r
# Create Histogram for The average of base stats.
kanto_stats %>%
  ggplot(aes(x = avg_base))+
  geom_histogram(fill = "#FFDE00", color = "#3B4CCA", binwidth = 10)+
  labs(
    x = "Average of Base Stats",
    y = "Count of Bin",
    title = "Histogram of the Avegrage of Base Stats")
```

![](README_files/figure-gfm/base%20stats%20histograms-7.png)<!-- -->

All of these provide some interesting insight but the thing that sticks
out the most to me might be just how much of an outlier the 250 HP base
stat is. Upon further investigation this Pokemon is Chansey, which I
suppose makes some sense as HP is health, and in the Pokemon TV series,
Chansey always worked in the PokeCenter (A hospital for Pokemon). You
can read more about Chansey
[here](https://bulbapedia.bulbagarden.net/wiki/Chansey_(Pok%C3%A9mon)).

## BMI Box Plot

For our last analysis I was curious what the spread of BMIs looked like
in every egg group. So lets create some box plots!

First we use the `poke_api` function to use the `egg_group` function.
Then we take out the second egg group column as not every Pokemon has
two. Fianlly we make the boxplot.

``` r
# Get needed data
egg_group_data <- poke_api("egg_group",poke="all",group = "all")

# Remove Egg group 2
egg_group_data <- egg_group_data %>%
  select(Name,Egg_Group_1)

# Join with BMI data
egg_group_bmi <- left_join(egg_group_data,all_size_stats, by = c("Name" = "Name"))

# Keep only name, egg group, and BMI.
egg_group_bmi <- egg_group_bmi %>%
  select(Name,Egg_Group_1,BMI)

# Create boxplot
egg_group_bmi %>%
  ggplot(aes(x = Egg_Group_1, y = BMI))+
  geom_boxplot(fill = "#FFDE00", color = "#3B4CCA")+
  labs(
    x = "Egg Group",
    y = "BMI",
    title = "BMI Boxplot By Egg Group")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](README_files/figure-gfm/bmi%20box%20plot-1.png)<!-- -->

Looking at the Boxplots, the Egg Group with the most variability looks
to me like the Mineral group. Whereas the smallest is Ditto, as that is
only one Pokemon. Ignoring the Ditto group, Humanshape seems to be the
group with the least variability in BMI. I suppose this makes some sense
because if the Pokemon are all roughly human shaped and sized they would
all have similar BMIs.
