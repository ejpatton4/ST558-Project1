library(httr)
library(jsonlite)


ditto <- GET("https://pokeapi.co/api/v2/pokemon/ditto/encounters")

ditto_data <- ditto$content

ditto_data <- rawToChar(ditto_data)

ditto_data <- fromJSON(ditto_data,  flatten = TRUE)

rattatt



rattata <- GET("https://pokeapi.co/api/v2/pokemon/rattata/encounters")

rattata_data <- rattata$content

rattata_data <- rawToChar(rattata_data)

rattata_data <- fromJSON(rattata_data,  flatten = TRUE)
