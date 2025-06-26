player_data = readRDS("lista_giocatori.rds")
# player_data2 = readRDS("lista_giocatori.rds")
# rm(player_data2)

# install.packages("httr")
library(httr)

url <- "https://api.clashroyale.com/v1/cards"
headers <- c("Authorization" = "Bearer __key__"
)

response <- GET(url, add_headers(headers))
data <- content(response, "parsed")

all_cards <- character()
for (i in 1:length(data$items)) {
  all_cards <- c(all_cards, data$items[[i]]$name)
}
head(all_cards)

all_cards_support <- character()
for (i in 1:length(data$supportItems)) {
  all_cards_support <- c(all_cards_support, data$supportItems[[i]]$name)
}
head(all_cards_support)


for(card_name in all_cards) {
  player_data <- lapply(player_data,
                        function(player) {
                          player[[card_name]] <- any(sapply(player$currentDeck, function(card){ card$name == card_name }));
                          return(player)
                        })
}

player_data <- lapply(player_data,
                      function(player) {
                        player[["meanCostDeck"]] <- mean(sapply(player$currentDeck, function(card){ if(card$name == "Mirror") return(NA) else return(card$elixirCost) }), na.rm = TRUE);
                        return(player)
                      })

player_data <- lapply(player_data, function(player)
                      { player[!names(player) %in% "currentDeck"]})

for(card_name in all_cards_support) {
  player_data <- lapply(player_data,
                        function(player) {
                          player[[card_name]] <- any(sapply(player$currentDeckSupportCards, function(card){ card$name == card_name }));
                          return(player)
                        })
}

player_data <- lapply(player_data, function(player)
                      { player[!names(player) %in% "currentDeckSupportCards"]})

# lista_temp = lapply(player_data,
#        function(player) {
#          if(player[["Mirror"]] == T)
#          return(player)
#       })
# rm(lista_temp)

player_data <- lapply(player_data,
                      function(player) { player[!names(player) %in% "clan"]})

# giocatore_temp = player_data[[1]][!names(player_data[[1]]) %in% "clan"]
# rm(giocatore_temp)


player_data <- lapply(player_data,
                      function(player) { player[!names(player) %in% "arena"]})


player_data <- lapply(player_data,
                      function(player) { player[!names(player) %in% "leagueStatistics"]})


player_data <- lapply(player_data, function(player) {
  # Inizializza la nuova variabile con 180 (circa mediana di 365)
  # Questo è un valore di default sensato nel caso l'utente giocasse da meno di
  # un anno e non potessimo risalire ai giorni di gioco effettivi
  player[["daysSinceRegistration"]] <- 180
  player[["yearsSinceRegistration"]] <- 0
  # sta proprio ad indicare che gioca da meno di un anno e per questo non
  # sappiamo il numero di giorni di gioco
  player[["is_new_player"]] <- TRUE
  
  # Controlla se 'badges' esiste e non è vuota
  if (!is.null(player[["badges"]]) && length(player[["badges"]]) > 0) {
    indice <- sapply(player[["badges"]], function(x) x$name == "YearsPlayed")
    # Controlla se la prima medaglia esiste
    first_badge <- player[["badges"]][indice]
    if ( length(first_badge) == 1) {
      first_badge <- first_badge[[1]]
      # Controlla se 'progress' esiste nella prima medaglia
      if (!is.null(first_badge) && "progress" %in% names(first_badge)) {
        player[["daysSinceRegistration"]] <- first_badge$progress
        player[["yearsSinceRegistration"]] <- first_badge$level
        player[["is_new_player"]] <- FALSE
      }
    }
  }
  return(player)
})

player_data[[1]]$daysSinceRegistration

player_data <- lapply(player_data, function(player)
                      { player[!names(player) %in% "badges"]})
player_data <- lapply(player_data, function(player)
                      { player[!names(player) %in% "achievements"]})

player_data <- lapply(player_data, function(player) {
  player[["cardsOwned"]] <- length(player$cards)
  return(player)
})

player_data <- lapply(player_data, function(player) {
  player$cards <- lapply(player$cards, function(card) {
    card$realLevel <- 14 - card$maxLevel + card$level
    return(card)})
  return(player)
})

player_data <- lapply(player_data, function(player) {
  # Inizializza la nuova variabile con NA (Not Available)
  # Questo è utile se non c'è un valore di default sensato
  player[["meanLevelCards"]] <- NA
  player[["CardsLevel15"]] <- 0
  player[["CardsLevel14"]] <- 0
  player[["CardsLevel13"]] <- 0
  player[["CardsLevel12"]] <- 0
  player[["CardsLevel11"]] <- 0
  player[["CardsLevel10"]] <- 0
  player[["CardsEvo"]] <- 0
  
  # Controlla se 'badges' esiste e non è vuota
  if (!is.null(player[["cards"]]) && length(player[["cards"]]) > 0) {
    listLevel <- lapply(player[["cards"]], function(card) {card$realLevel})
    vectorLevel <- unlist(listLevel)
    
    # Controlla se 'progress' esiste nella prima medaglia
    if (!is.null(listLevel) && length(listLevel) > 0) {
      player[["meanLevelCards"]] <- mean(vectorLevel)
      
      occorrenze_totali <- table(vectorLevel)
      if(!is.na(occorrenze_totali["15"]))
        player[["CardsLevel15"]] <- occorrenze_totali["15"]
      if(!is.na(occorrenze_totali["14"]))
        player[["CardsLevel14"]] <- occorrenze_totali["14"]
      if(!is.na(occorrenze_totali["13"]))
        player[["CardsLevel13"]] <- occorrenze_totali["13"]
      if(!is.na(occorrenze_totali["12"]))
        player[["CardsLevel12"]] <- occorrenze_totali["12"]
      if(!is.na(occorrenze_totali["11"]))
        player[["CardsLevel11"]] <- occorrenze_totali["11"]
      if(!is.na(occorrenze_totali["10"]))
        player[["CardsLevel10"]] <- occorrenze_totali["10"]
    }
    
    listEvo <- lapply(player$cards, function(card) {
      if(!is.null(card$evolutionLevel))
        return(1)
      else
        return(0)})
    player[["CardsEvo"]] <- sum(unlist(listEvo))
  }
  return(player)
})

# carteDonMod <- lapply(Donato$cards, function(card) {
#   card$realLevel <- 14 - card$maxLevel + card$level
#   return(card)})
# carteDonMod <- lapply(Donato$cards, function(card) {
#   if(!is.null(card$evolutionLevel))
#     return(1)
#   else
#     return(0)})
# rm(carteDonMod)

player_data <- lapply(player_data, function(player)
                      { player[!names(player) %in% "cards"]})

player_data <- lapply(player_data, function(player) {
  player$supportCards <- lapply(player$supportCards, function(card) {
    card$realLevel <- 14 - card$maxLevel + card$level
    return(card)})
  return(player)
})

player_data <- lapply(player_data, function(player) {
  # Inizializza la nuova variabile con NA (Not Available)
  # Questo è utile se non c'è un valore di default sensato
  player[["meanLevelSupportCards"]] <- NA
  player[["SupportCardsLevel15"]] <- 0
  player[["SupportCardsLevel14"]] <- 0
  player[["SupportCardsLevel13"]] <- 0
  player[["NumberSupportCards"]] <- 0
  
  # Controlla se 'badges' esiste e non è vuota
  if (!is.null(player[["supportCards"]]) && length(player[["supportCards"]]) > 0) {
    listLevel <- lapply(player[["supportCards"]], function(card) {card$realLevel})
    vectorLevel <- unlist(listLevel)
    player[["NumberSupportCards"]] <- length(listLevel)
    
    # Controlla se 'progress' esiste nella prima medaglia
    if (!is.null(listLevel) && length(listLevel) > 0) {
      player[["meanLevelSupportCards"]] <- mean(vectorLevel)
      
      occorrenze_totali <- table(vectorLevel)
      if(!is.na(occorrenze_totali["15"]))
        player[["SupportCardsLevel15"]] <- occorrenze_totali["15"]
      if(!is.na(occorrenze_totali["14"]))
        player[["SupportCardsLevel14"]] <- occorrenze_totali["14"]
      if(!is.na(occorrenze_totali["13"]))
        player[["SupportCardsLevel13"]] <- occorrenze_totali["13"]
    }
  }
  return(player)
})

player_data <- lapply(player_data, function(player){ 
  player[!names(player) %in% "supportCards"]})

player_data <- lapply(player_data, function(player) {
  # Applica la logica di trasformazione per ogni singolo 'player'
  if (!is.null(player$currentFavouriteCard) && "name" %in% names(player$currentFavouriteCard)) {
    player$currentFavouriteCard <- player$currentFavouriteCard$name
  } else {
    player$currentFavouriteCard <- NA
  }
  return(player) # Restituisce il giocatore modificato
})

player_data <- lapply(player_data, function(player) {
  # Inizializza la nuova variabile con NA (Not Available)
  # Questo è utile se non c'è un valore di default sensato
  
  # numero della lega nell'attuale stagione
  player[["currentLeagueNumber"]] <- 1
  # trofei nel percorso della lega nell'attuale stagione
  player[["currentLeagueTrophies"]] <- 0
  # rango (top mondiale) nel percorso della lega nell'attuale stagione
  player[["currentLeagueRank"]] <- 0
  # gli stessi valori di sopra ma nella stagione appena terminata
  player[["lastLeagueNumber"]] <- 1
  player[["lastLeagueTrophies"]] <- 0
  player[["lastLeagueRank"]] <- 0
  # gli stessi valori di sopra ma nella stagione in cui si è raggiunto massimo valore di trofei
  player[["bestLeagueNumber"]] <- 1
  player[["bestLeagueTrophies"]] <- 0
  player[["bestLeagueRank"]] <- 0
  
  # Controlla se 'currentPathOfLegendSeasonResult' esiste e non è vuota
  if (!is.null(player[["currentPathOfLegendSeasonResult"]]) && length(player[["currentPathOfLegendSeasonResult"]]) > 0) {
    player[["currentLeagueNumber"]] <- player$currentPathOfLegendSeasonResult$leagueNumber
    player[["currentLeagueTrophies"]] <- player$currentPathOfLegendSeasonResult$trophies
    if(!is.null(player$currentPathOfLegendSeasonResult$rank))
      player[["currentLeagueRank"]] <- player$currentPathOfLegendSeasonResult$rank
  }
  
  # Controlla se 'lastPathOfLegendSeasonResult' esiste e non è vuota
  if (!is.null(player[["lastPathOfLegendSeasonResult"]]) && length(player[["lastPathOfLegendSeasonResult"]]) > 0) {
    player[["lastLeagueNumber"]] <- player$lastPathOfLegendSeasonResult$leagueNumber
    player[["lastLeagueTrophies"]] <- player$lastPathOfLegendSeasonResult$trophies
    if(!is.null(player$lastPathOfLegendSeasonResult$rank))
      player[["lastLeagueRank"]] <- player$lastPathOfLegendSeasonResult$rank
  }
  
  # Controlla se 'bestPathOfLegendSeasonResult' esiste e non è vuota
  if (!is.null(player[["bestPathOfLegendSeasonResult"]]) && length(player[["bestPathOfLegendSeasonResult"]]) > 0) {
    player[["bestLeagueNumber"]] <- player$bestPathOfLegendSeasonResult$leagueNumber
    player[["bestLeagueTrophies"]] <- player$bestPathOfLegendSeasonResult$trophies
    if(!is.null(player$bestPathOfLegendSeasonResult$rank))
      player[["bestLeagueRank"]] <- player$bestPathOfLegendSeasonResult$rank
  }
  return(player)
})


player_data <- lapply(player_data, function(player){ 
  player[!names(player) %in% "currentPathOfLegendSeasonResult"]})
player_data <- lapply(player_data, function(player){ 
  player[!names(player) %in% "lastPathOfLegendSeasonResult"]})
player_data <- lapply(player_data, function(player){ 
  player[!names(player) %in% "bestPathOfLegendSeasonResult"]})
player_data <- lapply(player_data, function(player){ 
  player[!names(player) %in% "progress"]})

sum(unlist(lapply(player_data,
           function(player) {
             if(length(player) != 174)
               return(1)
             else
               return(0)
           })))
# ottengo 27 perchè c'è un giocatore senza clan (e quindi senza ruolo) e ci sono
# 26 giocatori senza starPoints

player_data <- lapply(player_data, function(player) {
  if (is.null(player$starPoints) || is.na(player$starPoints)) {
    player$starPoints <- NA
  }
  return(player)
})

sum(unlist(lapply(player_data,
                  function(player) {
                    if(length(player) != 174)
                      return(1)
                    else
                      return(0)
                  })))

# ottengo 1 perchè c'è un giocatore senza clan (e quindi senza ruolo)

player_data <- lapply(player_data, function(player) {
  if (is.null(player$role) || is.na(player$role)) {
    player$role <- NA
  }
  return(player)
})

sum(unlist(lapply(player_data,
                  function(player) {
                    if(length(player) != 174)
                      return(1)
                    else
                      return(0)
                  })))
# ottengo 0

# install.packages("dplyr") # Se non lo hai già installato
library(dplyr)

# Converti la lista di liste direttamente in un tibble (data frame migliorato)
player_data_tibble <- bind_rows(player_data)

print(head(player_data_tibble))
print(dim(player_data_tibble))

player_data_tibble$role <- factor(player_data_tibble$role,
                                  levels = c("member", "elder", "coLeader", "leader"),
                                  labels = c("member", "elder", "coLeader", "leader"))
player_data_tibble$currentFavouriteCard <- as.factor(player_data_tibble$currentFavouriteCard)
player_data_tibble$is_new_player <- as.factor(player_data_tibble$is_new_player)
for(card in all_cards) {
  player_data_tibble[[card]] <- as.factor(player_data_tibble[[card]])
}
for(card in all_cards_support) {
  player_data_tibble[[card]] <- as.factor(player_data_tibble[[card]])
}

# install.packages("arrow") # Se non lo hai già installato
library(arrow)

# Salva come Parquet
write_parquet(player_data_tibble, "player_data.parquet")

# Per ricaricarli in futuro:
player_data_tibble <- read_parquet("player_data.parquet")

summary(player_data_tibble$expLevel)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00   24.00   33.00   32.37   41.00   70.00 

summary(player_data_tibble$meanCostDeck)
#    Min. 1st Qu.  Median    Mean  3rd Qu.    Max.    NA's 
#   1.286   3.250   3.625   3.603   3.875   7.250       4 

plot(player_data_tibble[1:500,3:11])

plot(player_data_tibble[1:500,]$meanCostDeck, player_data_tibble[1:500,]$trophies)

rm(data)
rm(response)
rm(headers)
rm(url)
rm(first_badge)
rm(listEvo)
rm(listLevel)
rm(card)
rm(card_name)
rm(i)
rm(occorrenze_totali)
rm(vectorLevel)
