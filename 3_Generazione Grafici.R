# --- Installazione dei pacchetti (se non già installati) ---
# install.packages("ggplot2")
# install.packages("dplyr")

# --- Caricamento dei pacchetti ---
library(ggplot2)
library(dplyr)
library(httr)
library(arrow) # Per caricare il file .parquet
library(patchwork)  # Per combinare i grafici

# --- Caricamento del dataset ---
# Il file 'player_data.parquet' è nella stessa directory
# dove sto eseguendo questo codice R.
player_data_tibble <- read_parquet("player_data.parquet")

# --- Controllo e conversione delle variabili fattoriali (se non già fatto) ---
# Convertire le variabili 'role' e 'currentFavouriteCard' in fattori
player_data_tibble$role <- factor(player_data_tibble$role,
                                  levels = c("member", "elder", "coLeader", "leader"),
                                  labels = c("member", "elder", "coLeader", "leader"))
player_data_tibble$currentFavouriteCard <- as.factor(player_data_tibble$currentFavouriteCard)
player_data_tibble$is_new_player <- as.factor(player_data_tibble$is_new_player)
# Convertiamo in fattoriali anche le variabili booleane
# Per farlo leggiamo dall'API il nome di tutte le carte
url <- "https://api.clashroyale.com/v1/cards"
headers <- c("Authorization" = "Bearer __key__")

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
for(card in all_cards) {
  player_data_tibble[[card]] <- as.factor(player_data_tibble[[card]])
}
for(card in all_cards_support) {
  player_data_tibble[[card]] <- as.factor(player_data_tibble[[card]])
}
# Salva come Parquet queste modifiche
write_parquet(player_data_tibble, "player_data.parquet")

# --- Plot 1: Distribuzione dei Trofei (`trophies`) ---
# Istogramma per mostrare la frequenza dei trofei
ggplot(player_data_tibble, aes(x = trophies)) +
  geom_histogram(binwidth = 250, fill = "skyblue", color = "black") + # binwidth controlla la larghezza delle barre
  labs(title = "Distribuzione dei Trofei dei Giocatori",
       x = "Numero di Trofei",
       y = "Frequenza") +
  theme_minimal()

# Density plot per una visione più liscia della distribuzione dei trofei
ggplot(player_data_tibble, aes(x = trophies)) +
  geom_density(fill = "lightgreen", alpha = 0.7, color = "black") +
  labs(title = "Densità della Distribuzione dei Trofei",
       x = "Numero di Trofei",
       y = "Densità") +
  theme_minimal()

# --- Plot 2: Distribuzione del Livello di Esperienza (`expLevel`) ---
# Istogramma per il livello di esperienza
ggplot(player_data_tibble, aes(x = expLevel)) +
  geom_histogram(binwidth = 1, fill = "salmon", color = "black") +
  labs(title = "Distribuzione del Livello di Esperienza (ExpLevel)",
       x = "Livello di Esperienza",
       y = "Frequenza") +
  theme_minimal()

# --- Plot 3: Distribuzione dei Ruoli nel Clan (`role`) ---
# Bar plot per le variabili categoriche
ggplot(player_data_tibble, aes(x = role)) +
  geom_bar(fill = "darkblue", color = "black") +
  labs(title = "Distribuzione dei Ruoli dei Giocatori nei Clan",
       x = "Ruolo nel Clan",
       y = "Conteggio Giocatori") +
  theme_minimal()

# --- Plot 4: Distribuzione del Costo Medio del Deck (`meanCostDeck`) ---
ggplot(player_data_tibble, aes(x = meanCostDeck)) +
  geom_density(fill = "purple", alpha = 0.2, color = "black") +
  labs(title = "Distribuzione del Costo Medio del Deck",
       x = "Costo Medio del Deck",
       y = "Densità") +
  theme_minimal()


# --- Plot 5: Top 10 Carte Preferite (`currentFavouriteCard`) ---
# Prima, contiamo le occorrenze di ciascuna carta e selezioniamo le top 10
top_cards <- player_data_tibble %>%
  count(currentFavouriteCard, sort = TRUE) %>%
  top_n(10, n) # Seleziona le prime 10 basate sulla frequenza 'n'

ggplot(top_cards, aes(x = reorder(currentFavouriteCard, n), y = n)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  coord_flip() + # Rende le etichette delle carte più leggibili
  labs(title = "Top 10 Carte Preferite",
       x = "Carta",
       y = "Numero di Giocatori") +
  theme_minimal()

# --- Plot 6: Distribuzione del Livello di Lega della Stagione Scorsa (`lastLeagueNumber`) - ESCLUDENDO 1 ---
# Filtriamo i dati per escludere i valori di lastLeagueNumber pari a 0 e 1
filtered_league_data <- player_data_tibble %>%
  filter(lastLeagueNumber > 1) # Ora filtriamo per valori maggiori di 1 (sono oltre 10000 ad avere 1)

# Ora plottiamo la distribuzione dei livelli di lega
# Ordiniamo i livelli
unique_filtered_leagues <- sort(unique(filtered_league_data$lastLeagueNumber))

ggplot(filtered_league_data, aes(x = factor(lastLeagueNumber, levels = unique_filtered_leagues))) +
  geom_bar(fill = "#5DADE2", color = "black") + # Un bel blu chiaro
  labs(title = "Distribuzione dei Giocatori per Livello di Lega (Stagione Scorsa)",
       x = "Livello di Lega (Stagione Scorsa)",
       y = "Numero di Giocatori") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Per inclinare le etichette se sono molte


# --- Plot 7: Distribuzione dei Migliori Trofei nel Percorso delle Leggende (`bestLeagueTrophies`)
p_hist <- ggplot(filter(player_data_tibble, bestLeagueTrophies > 0), aes(x = bestLeagueTrophies)) +
  geom_histogram(binwidth = 100, fill = "#F1C40F", color = "black") + # Un giallo/oro per i trofei
  labs(title = "Distribuzione Trofei migliori nel Percorso delle Leggende (filtro chi ha trofei)",
       x = "Migliori Trofei (Percorso delle Leggende)",
       y = "Frequenza") +
  theme_minimal()
p_density <- ggplot(filter(player_data_tibble, bestLeagueTrophies > 0), aes(x = bestLeagueTrophies)) +
  geom_density(fill = "#F1C40F", alpha = 0.7, color = "black") + # Un giallo/oro per i trofei
  labs(title = "Distribuzione Trofei migliori nel Percorso delle Leggende (filtro chi ha trofei)",
       x = "Migliori Trofei (Percorso delle Leggende)",
       y = "Densità") +
  theme_minimal()
combined_plot <- p_hist / p_density
print(combined_plot)
ggsave("Combined_BestLeagueTrophies.png", plot = combined_plot, width = 8, height = 10)

# --- Plot 8: Distribuzione dei days ---
# Istogramma per mostrare la frequenza dei trofei
ggplot(filter(player_data_tibble, daysSinceRegistration > 364), aes(x = daysSinceRegistration)) +
  geom_histogram(binwidth = 250, fill = "skyblue", color = "black") + # binwidth controlla la larghezza delle barre
  labs(title = "Distribuzione dei Trofei dei Giocatori",
       x = "Numero di Trofei",
       y = "Frequenza") +
  theme_minimal()

rm(data)
rm(response)
rm(headers)
rm(url)
rm(top_cards)
rm(card)
rm(filtered_league_data)
rm(p_density)
rm(p_hist)
rm(combined_plot)
rm(unique_filtered_leagues)
rm(unique_last_leagues)
