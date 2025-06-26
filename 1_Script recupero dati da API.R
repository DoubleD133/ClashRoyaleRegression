# Caricamento dei pacchetti necessari
library(httr)

# --- Configurazione API ---
# URL base dell'API di Clash Royale
base_url <- "https://api.clashroyale.com/v1"

# Chiave API (Bearer Token)
api_key <- "_"
headers <- c("Authorization" = paste("Bearer", api_key))

# Funzione per trasformare i tag (es. #QRGQJ88Q in %23QRGQJ88Q)
# Necessaria per l'uso nei link URL dell'API
trasforma_tag_per_api <- function(tag_originale) {
  return(gsub("#", "%23", tag_originale))
}

# --- Fase 1: Recupero dei Tag dei Clan ---
# Vengono cercati clan con un numero di membri compreso tra 10 e 50
cat("Recupero dei tag dei clan...\n")
url_clans <- paste0(base_url, "/clans?minMembers=10&maxMembers=50")
response_clans <- GET(url_clans, add_headers(headers))

clans_tags <- c()
if (httr::status_code(response_clans) == 200) {
  data_clans <- httr::content(response_clans, "parsed")
  for (clan in data_clans$items) {
    clans_tags <- c(clans_tags, clan$tag)
  }
  cat(paste("Recuperati", length(clans_tags), "clan tags.\n"))
} else {
  cat(paste("Errore nel recupero dei clan:", httr::status_code(response_clans), "\n"))
}
# Visualizza i primi tag recuperati per verifica (commentato per relazione finale)
# head(clans_tags)

# --- Fase 2: Recupero dei Tag dei Membri per ogni Clan ---
# Iterazione su ogni clan per ottenere i tag dei suoi membri
cat("Recupero dei tag dei membri dei clan...\n")
all_member_tags <- c()
for (clan_tag in clans_tags) {
  url_members <- paste0(base_url, "/clans/", trasforma_tag_per_api(clan_tag), "/members")
  response_members <- GET(url_members, add_headers(headers))
  
  if (httr::status_code(response_members) == 200) {
    data_members <- httr::content(response_members, "parsed")
    # Estrae tutti i tag dei membri del clan
    tags_members <- sapply(data_members$items, function(x) x$tag)
    all_member_tags <- c(all_member_tags, tags_members)
  } else {
    cat(paste("Errore nel recupero membri per clan", clan_tag, ":", httr::status_code(response_members), "\n"))
  }
}
# Rimuovi duplicati nel caso un giocatore sia apparso in più clan
all_member_tags <- unique(all_member_tags)
cat(paste("Recuperati", length(all_member_tags), "tag di giocatori unici.\n"))
# Visualizza i primi tag dei membri (commentato per relazione finale)
# head(all_member_tags)

# --- Fase 3: Recupero dei Dati Dettagliati di ogni Giocatore ---
# Iterazione su tutti i tag dei membri per ottenere i dati completi di ogni giocatore
cat("Recupero dei dati dettagliati dei giocatori...\n")
all_player_data <- list()
player_count <- 0
for (player_tag in all_member_tags) {
  url_player <- paste0(base_url, "/players/", trasforma_tag_per_api(player_tag))
  response_player <- GET(url_player, add_headers(headers))
  
  if (httr::status_code(response_player) == 200) {
    data_player <- httr::content(response_player, "parsed")
    player_count <- player_count + 1
    all_player_data[[player_count]] <- data_player
  } else {
    cat(paste("Errore nel recupero dati per giocatore", player_tag, ":", httr::status_code(response_player), "\n"))
  }
  # Aggiungi un ritardo se stai facendo molte richieste per non superare i limiti dell'API
  # Sys.sleep(0.1) # Ritardo di 0.1 secondi
}
cat(paste("Recuperati i dati per", length(all_player_data), "giocatori.\n"))

# --- Salvataggio dei Dati Recuperati ---
# Salva la lista completa dei dati dei giocatori in un file .rds per riutilizzo
saveRDS(all_player_data, file = "lista_giocatori_raw.rds")

# Visualizza la struttura del primo giocatore recuperato (commentato per relazione finale)
# str(head(all_player_data, 1))