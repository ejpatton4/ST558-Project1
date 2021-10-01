Poke API Vignette
================
Evan Patton
9/27/2021

## Introduction

This space reserved for introduction, talk about the project, my
thoughts, and then an overview of my work.

## Packages Needed

In this Vignette we will need the following packages:

``` r
# You only need to run the install.packages() functions if you have not already downloaded the packages. 
# As I already have them downloaded, I am not doing that here. If you need to, copy and paste the line needed, without the #, into your console and run.

# install.packages("tidyverse")
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("knitr")

library(httr)
library(jsonlite)
library(tidyverse)
library(knitr)
```

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
later. `encounter` takes two arguments, the first one is not optional,
and that is the Pokemon you want to find. The second argument is
optional, and that is what game from the series you want to return data
on, if no game is selected then all games will be returned in the final
tibble. **Both arguments require quoted strings**

``` r
# Starting the function
encounter <- function(poke, game = "all") {

# Converting inputs to lower case for flexability
poke <- tolower(poke)
game <- tolower(game)

# Get the requested data   
raw_data <- fromJSON(paste0("https://pokeapi.co/api/v2/pokemon/",poke,"/encounters"), flatten = TRUE)


# Start loops.
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
      
      # Get the chance, max level, min level, and method.
      chance <- raw_data[[1]][[i]][[1]][[b]][a,1]
      max    <- raw_data[[1]][[i]][[1]][[b]][a,3]
      min    <- raw_data[[1]][[i]][[1]][[b]][a,4]
      meth   <- raw_data[[1]][[i]][[1]][[b]][a,5]
      
      # Combine all vars into a line for tibble creation
      line   <- c(loc,gm,chance,max,min,meth)
         
      # Build dfd tibble.
      # If this is the first iteration of the details loop
      # then dfd is created with all of the vars.
      # If not first iteration, bind dfd with line to add another line to dfd.
      if(a == 1){
        dfd <- tibble(loc,gm,chance,max,min,meth)
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
names(dfl) <- c("Location","Game","Chance","Max_Level","Min_Level","Method")

# Keep only distinct rows, as there are some that repeat due to variables 
# not important for this function.
dfl <- distinct(dfl)

# If user doesn't input an option for game or wants all games, dfl is returned.
# If user does input an option for game, then dfl is filtered for that game and returned.
if(game == "all"){
   return(dfl)
}else{
    return(dfl %>%
             filter(Game == game))
  } # End of if else statement.

} # Close of function.
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
  
  # Looping for entrance of a vector
  for(i in 1:length(poke)){
  
    # Get requested data from API
  raw_data <- fromJSON(paste0("https://pokeapi.co/api/v2/pokemon/",poke[i]), flatten = TRUE)

  # Select only the information about the base stats
  df <- as_tibble(raw_data[["stats"]])
  
  # Add a column for the name of the Pokemon 
  df$name <- str_to_sentence(poke[i])
  
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

The output of the `stats` function is a tibble with the Pokemons name,
Height in meters, and Weight in kilograms.

``` r
# Create function.
size <- function(poke) {
  
  # Start loop if there is more than one Pokemon inputted.
  for(i in 1:length(poke)){
  
  # Get requested data from API.
  raw_data <- fromJSON(paste0("https://pokeapi.co/api/v2/pokemon/",poke[i]), flatten = TRUE)
  
  # Create objects to hold the height weight and name. 
  ht <- raw_data[["height"]]
  wt <- raw_data[["weight"]]
  Name <- str_to_sentence(poke[i])
  
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
  
  # If "all" is selected for the reg argument, reg is defined as all available ID numbers.
  # If not reg remains what the user inputs.
  if( "all" %in% tolower(reg)){
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
  if("all" %in% tolower(poke)){
    final <- final
  }else{
    final <- final %>%
      filter(tolower(Name) %in% tolower(poke))
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

## Basic Functionality

Here we test the encounter function with my favorite starter Pokemon
**CHARMANDER**.

``` r
# Use the function and save results in an object
charmander <- encounter("charmander")

# Print the object
print(charmander)
```

    ## # A tibble: 9 x 6
    ##   Location            Game       Chance Max_Level Min_Level Method
    ##   <chr>               <chr>       <int>     <int>     <int> <chr> 
    ## 1 pallet-town-area    red           100         5         5 gift  
    ## 2 pallet-town-area    blue          100         5         5 gift  
    ## 3 pallet-town-area    firered       100         5         5 gift  
    ## 4 pallet-town-area    leafgreen     100         5         5 gift  
    ## 5 pallet-town-area    heartgold     100         5         5 gift  
    ## 6 pallet-town-area    soulsilver    100         5         5 gift  
    ## 7 kanto-route-24-area yellow        100        10        10 gift  
    ## 8 lumiose-city-area   x             100        10        10 gift  
    ## 9 lumiose-city-area   y             100        10        10 gift

Here is an example of the `stats` function, with a vector of four
Pokemon.

``` r
# Create a vector of four Pokemon
mons <- c("charizard","pidgey","abra","beedrill") 

# Use the function on the vector
stat_example <- stats(mons)

# Print results
print(stat_example)
```

    ## # A tibble: 4 x 7
    ##   name         hp attack defense `special-attack` `special-defense` speed
    ##   <chr>     <int>  <int>   <int>            <int>             <int> <int>
    ## 1 Charizard    78     84      78              109                85   100
    ## 2 Pidgey       40     45      40               35                35    56
    ## 3 Abra         25     20      15              105                55    90
    ## 4 Beedrill     65     90      40               45                80    75

Here we test the `size` function, with the same vector of four Pokemon
from the `stats` example.

``` r
# Use the function on the vector
size_example <- size(mons)

# Print results
print(size_example)
```

    ## # A tibble: 4 x 3
    ##   Name      Height_Meters Weight_Kilograms
    ##   <chr>             <dbl>            <dbl>
    ## 1 Charizard           1.7             90.5
    ## 2 Pidgey              0.3              1.8
    ## 3 Abra                0.9             19.5
    ## 4 Beedrill            1               29.5

Here we test the `dex` function. We are using regions with dex ids 1
through 5, and only returning results on our same four Pokemon from the
previous two examples. Any `N/A` means that the Pokemon is not in that
Pokedex.

``` r
# Use function on vectors
dex_example <- dex(reg = c(1:5), poke = mons)

# Print results
print(dex_example)
```

    ## # A tibble: 4 x 6
    ##   Name      National_Dex_Number Kanto_Dex_Number `Original-johto_Dex_Number` Hoenn_Dex_Number `Original-sinnoh_Dex_Number`
    ##   <chr>                   <int>            <int>                       <int>            <int>                        <int>
    ## 1 Charizard                   6                6                         231               NA                           NA
    ## 2 Beedrill                   15               15                          29               NA                           NA
    ## 3 Pidgey                     16               16                          10               NA                           NA
    ## 4 Abra                       63               63                          89               39                           20
