# Regressione multivariata
# cerchiamo di inferire sul numero di trofei di un giocatore

# --- Caricamento dei pacchetti ---
library(ggplot2) # Per i grafici di dispersione
library(dplyr)
library(httr)
library(arrow) # Per caricare il file .parquet
library(patchwork)  # Per combinare i grafici
library(caTools)  # Per la funzione sample.split
library(GGally)  # Per ggpairs (utile per matrici di correlazione e scatter plot)
library(car)     # Per la funzione vif()
library(lmtest)  # per bptest
library(nortest) # per ad.test di normalità

player_data_tibble <- read_parquet("player_data.parquet")

# Contare i valori assenti per ogni colonna (utile per capire se ci problemi)
righe_con_NA <- colSums(is.na(player_data_tibble))
# Numero di valori mancanti per colonna:
print(righe_con_NA[righe_con_NA != 0])
# role      currentFavouriteCard                starPoints legacyTrophyRoadHighScore              meanCostDeck 
# 1                        90                        26                      4791                         4 
# meanLevelSupportCards 
# 193
print(dim(filter(player_data_tibble, is_new_player == TRUE))[1])
# 2643 giocatori hanno un numero fittizio per la variabile daysSinceRegistration

# risolviamo questi problemi:

# vediamo come fa ad essere NA il role
(player_data_tibble %>%
  filter(is.na(role)) )$tag
# otteniamo UGL08VJLU
# questo utente ha ruolo NA perchè ha abbandonato il clan
# ometteremo questo player

length(which(is.na(player_data_tibble$currentFavouriteCard)))
# otteniamo 90 player senza carta favorita, probabilmente perchè non hanno fatto
# un numero sufficiente di partite da avere una carta giocata abbastanza volte
# da essere la preferita del giocatore
# possiamo scartare questi giocatori/ queste righe

length(which(is.na(player_data_tibble$starPoints)))
# otteniamo 26 player senza questa moneta cosmetica, qui possiamo omettere i
# giocatori che non possiedono questa moneta, impostare che possiedono 0
# starPoints oppure eliminare colpetamente la variabile
# la scelta migliore sarà cancellare le righe

length(which(is.na(player_data_tibble$legacyTrophyRoadHighScore)))
# come già osservato legacyTrophyRoadHighScore riguarda una modalità di gioco
# non più presente nel gioco
# la scelta migliore è eliminare l'intera colonna

length(which(is.na(player_data_tibble$meanCostDeck )))
# otteniamo 4 player per i quali l'api non ci ha fornito il current deck, quindi
# convene eliminare le righe corrispondenti

length(which(is.na(player_data_tibble$meanLevelSupportCards )))
# otteniamo 193 player che hanno smesso di giocare prima dell'introduzione delle
# carte di supporto. Per quanto questa variabile non sia parecchio rilevante
# conviene eliminare le righe

# Gestione NA per
# 'role', 'currentFavouriteCard', 'starPoints', 'meanCostDeck', 'meanLevelSupportCards' ---
# Identifichiamo le righe da rimuovere
rows_to_remove <- player_data_tibble %>%
  filter(is.na(role) | is.na(currentFavouriteCard) | is.na(starPoints)
         | is.na(meanCostDeck) | is.na(meanLevelSupportCards))
nrow(rows_to_remove)
# un totale di 286 righe da rimuovere

# Rimuovi le righe con NA in queste colonne
player_data_tibble_cleaned <- player_data_tibble %>%
  filter(!is.na(role), !is.na(currentFavouriteCard), !is.na(starPoints),
         !is.na(meanCostDeck), !is.na(meanLevelSupportCards))
# otteniamo 12812-286=12526

# Rimuovi l'intera colonna 'legacyTrophyRoadHighScore'
player_data_tibble_cleaned <- player_data_tibble_cleaned %>%
  select(-legacyTrophyRoadHighScore)

sum(is.na(player_data_tibble_cleaned))
# restituisce 0 perchè in player_data_tibble_cleaned ci sono 0 valori NA



# Assicuriamoci che le variabili numeriche del dataframe 'player_data_tibble_cleaned'
# non contengano valori negativi (dovrebbero essere >= 0):

# Seleziona solo le colonne numeriche dal dataframe
numerical_cols <- player_data_tibble_cleaned %>%
  select(where(is.numeric)) %>%
  colnames()

# Inizializza un flag per sapere se abbiamo trovato valori negativi
negative_values_found <- FALSE
problematic_cols <- c()

# Itera attraverso ogni colonna numerica
for (col_name in numerical_cols) {
  # Controlla se ci sono valori minori di zero nella colonna corrente
  if (any(player_data_tibble_cleaned[[col_name]] < 0)) {
    num_negative <- sum(player_data_tibble_cleaned[[col_name]] < 0)
    cat(paste0("   - Colonna '", col_name, "': Trovati ", num_negative, " valori negativi.\n"))
    negative_values_found <- TRUE
    problematic_cols <- c(problematic_cols, col_name)
  }
}
# - Colonna 'starPoints': Trovati 1 valori negativi.

player_data_tibble_cleaned <- player_data_tibble_cleaned %>%
  filter(starPoints >= 0)
# per definizione starPoints (come le altre variabili numeriche del dataset)
# deve essere >= di 0, ma per un bug dell'API o di clash Royale stesso è
# negativo per un player

# Salva come Parquet
write_parquet(player_data_tibble_cleaned, "player_data_tibble_cleaned.parquet")
rm(righe_con_NA)
rm(rows_to_remove)
rm(numerical_cols)
rm(negative_values_found)
rm(problematic_cols)
rm(col_name)
rm(num_negative)

# Per ricaricarli in futuro:
player_data_tibble_cleaned <- read_parquet("player_data_tibble_cleaned.parquet")


# creiamo e blindiamo un insieme di test che useremo solo nell'ultima fasere per
# giudicare la bontà del metodo scelto alla fine

# --- Impostazione del seed per la riproducibilità ---
# Impostare un seed ti permette di ottenere la stessa divisione del dataset ogni
# volta che esegui il codice, cruciale per la riproducibilità dei risultati
set.seed(123)

# --- Creazione dell'indice di split ---
# sample.split crea un vettore logico TRUE/FALSE.
# TRUE per le righe che andranno nel training set, FALSE per il test set.
# 'SplitRatio' definisce la proporzione del training set.
split_index <- sample.split(player_data_tibble_cleaned$tag, SplitRatio = 0.80)

# Usiamo 'tag' solo come riferimento per il campionamento
# sample.split è robusto

# --- Creazione dei dataset di Training e Test ---
train_data <- player_data_tibble_cleaned %>%
  filter(split_index == TRUE)

test_data <- player_data_tibble_cleaned %>%
  filter(split_index == FALSE)

# Dimensione totale del dataset: 12526 righe
# Dimensione del set di Training (80%): 10020 righe
# Dimensione del set di Test (20%): 2506 righe
# Verifica che la somma delle righe sia uguale al totale

# --- Salva i dataset di training e test (raccomandato) ---
write_parquet(train_data, "player_data_train.parquet")
write_parquet(test_data, "player_data_test.parquet")

rm(split_index)
rm(test_data)

# ------------------------------------------------------------------------------
## PRELIMINARI REGRESSIONE LINEARE

# train_data <- read_parquet("player_data_train.parquet")

# Definizione della variabile dipendente
dependent_var <- "trophies"

# Seleziona le variabili indipendenti.
# Escludiamo 'tag', 'name'che sono solo degli identificatori
# escludiamo 'trophies' che è la variabile dipendente
# escludiamo 'bestTrophies' perchè è approssimabile una ripetizione di 'trophies'
# escludiamo le variabili legate al rango nel percorso delle legende perchè
# è gestito male dall'API ed è 0 per quasi tutti i player

independent_vars_candidates <- train_data %>%
  select(-tag, -name, -bestTrophies, -currentLeagueRank,
         -lastLeagueRank, -bestLeagueRank) %>%
  select(-!!dependent_var)

# la gestione dei valori NA ed la conversione delle variabili categoriali come
# in variabili fattoriali sono già state fatte

# Seleziona solo le colonne numeriche per la matrice di correlazione e VIF
numeric_independent_vars <- independent_vars_candidates %>%
  select(where(is.numeric))

#-----------------------------------------
# Assunzioni
#-----------------------------------------
## La regressione lineare fa diverse assunzioni sui dati, tra cui:
# - Linearità dei dati: si assume che la relazione tra predittori (X) e risultato (Y) sia
# lineare.
# - Normalità dei residui: si assume che gli errori residui siano distribuiti normalmente.
# - Omogeneità della varianza dei residui: si assume che i residui abbiano una varianza
# costante (omoschedasticità).
# - Indipendenza degli errori residui.
#-----------------------------------------
## Verificare se queste assunzioni sono soddisfatte. Possibili problemi includono:
# - Non-linearità nella relazione tra risultato e predittori.
# - Eteroscedasticità: varianza non costante degli errori.
# Presenza di valori limite nei dati che possono essere:
# - Outliers: valori estremi nella variabile risultato (Y).
# - High-leverage points: valori estremi nelle variabili predittive (X).
#----------------------------------------

#-----------------------------------------
# Scatter plot
#-----------------------------------------
# Si possono fare scatter plot individuali o usare ggpairs per un overview
# plot individuale (per daysSinceRegistration vs trophies)
ggplot(train_data, aes(x = daysSinceRegistration, y = trophies)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Aggiunge una linea di regressione lineare
  labs(title = "Relazione tra giorni di registrazione e trofei",
       x = "Giorni dalla Registrazione", y = "Trofei") +
  theme_minimal()
# si nota una certa correlazione lineare positiva.

for(colonna in colnames(numeric_independent_vars)) {
  # Calcola il coefficiente di correlazione Pearson tra le due variabili
  cor_val <- cor(train_data[[colonna]], train_data[[dependent_var]], use = "pairwise.complete.obs")
  cor_label <- paste0("Correlazione (r): ", round(cor_val, 2)) # Crea un'etichetta per il grafico
  
  plottt <- ggplot(train_data, aes_string(x = colonna, y = dependent_var)) +
    geom_point(alpha = 0.4, color = "steelblue") +
    geom_smooth(method = "lm", se = FALSE, color = "darkblue", linetype = "dashed") +
    annotate("text",
             x = Inf, y = Inf,
             label = cor_label,
             hjust = 1.1, vjust = 1.1,
             size = 12, color = "darkred", fontface = "bold") +
    labs(title = paste0("Relazione tra ", colonna, " e ", dependent_var),
         x = colonna,
         y = "Trofei") +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA),
      
      plot.title = element_text(size = 32, face = "bold"),   # Titolo del grafico più grande
      axis.title = element_text(size = 30, face = "bold"),   # Titoli degli assi più grandi
      axis.text = element_text(size = 28),                   # Etichette numeriche/testuali sugli assi più grandi
      legend.text = element_text(size = 28),                 # Testo della legenda più grande (se presente)
      legend.title = element_text(size = 30, face = "bold")
    )
  
  ggsave(file.path("plotLineari",paste0("plot_trofei_vs_", colonna, ".png")),
         plot = plottt, width = 15, height = 15, dpi = 300)
}

# # --- Esecuzione di ggpairs ---
# # Questo comando genererà la matrice di scatter plot, densità e correlazioni.
# # 'lower = list(continuous = wrap("points", alpha = 0.3))' mostra scatter plot con trasparenza
# # 'diag = list(continuous = wrap("densityDiag"))' mostra distribuzioni di densità sulle diagonali
# # 'upper = list(continuous = wrap("cor", size = 3))' mostra i coefficienti di correlazione di Pearson
# variables_for_ggpairs <- train_data %>%
#   select(-tag, -name, -bestTrophies, -currentLeagueRank,
#          -lastLeagueRank, -bestLeagueRank) %>%
#   select(where(is.numeric))
# 
# # --- CAMPIONAMENTO PER VELOCIZZARE GGPAIRS ---
# sample_size <- 2000
# # Imposta un seed per la riproducibilità del campionamento
# set.seed(456)
# # Esegui il campionamento casuale
# ggpairs_plot <- ggpairs(variables_for_ggpairs %>%
#                           sample_n(sample_size),
#                         lower = list(continuous = wrap("points", alpha = 0.3)),
#                         diag = list(continuous = wrap("densityDiag")),
#                         upper = list(continuous = wrap("cor", size = 2))) +
#   theme(
#     strip.text = element_text(size = 6),  # Riduci la dimensione del testo delle etichette delle variabili
#     axis.text = element_text(size = 4),   # Riduci la dimensione dei numeri sugli assi
#     plot.title = element_text(size = 10)  # Puoi regolare il titolo generale se ne hai uno
#     # Puoi aggiungere altre modifiche qui, ad esempio:
#     # axis.title = element_text(size = 7) # Per i titoli degli assi se ggpairs li mostra
#   )
# 
# # Stampa il plot per visualizzarlo
# print(ggpairs_plot)
# ggsave("ggpairs2000_overview.png", plot = ggpairs_plot, width = 20, height = 20, dpi = 200)
# rm(sample_size)

rm(ggpairs_plot)
rm(variables_for_ggpairs)
rm(independent_vars_candidates)
rm(numerical_cols)
rm(numeric_independent_vars)
rm(plottt)
rm(colonna)
rm(cor_label)
rm(cor_val)
rm(clean_col_name)

# --- 1. Rimozione delle Variabili Inutili ---
# queste variabili sono piatte (nel senso che hanno lo stesso valore per quasi tutto
# il dataset) e sembrano slegate da Y
variables_to_remove <- c(
  "bestLeagueTrophies",
  "lastLeagueTrophies",
  "currentLeagueTrophies",
  "tournamentCardsWon",
  # già rimosse anche prima:
  "currentLeagueRank",
  "lastLeagueRank",
  "bestLeagueRank"
)

train_data_processed <- train_data %>%
  select(-all_of(variables_to_remove))

# --- 2. Applicazione delle Trasformazioni ---

# Logaritmiche (log1p)
# applichiamo la funzione log a delle variabili che crescono molto velocemnete
# (esponenzialmente) rispetto ad Y

train_data_processed <- train_data_processed %>%
  mutate(
    log_battleCount = log1p(battleCount),
    log_CardsLevel14 = log1p(CardsLevel14),
    log_CardsLevel15 = log1p(CardsLevel15),
    log_challengeCardsWon = log1p(challengeCardsWon),
    log_clanCardsCollected = log1p(clanCardsCollected),
    log_totalDonations = log1p(totalDonations),
    log_donations = log1p(donations),
    log_donationsReceived = log1p(donationsReceived),
    log_expPoints = log1p(expPoints),
    log_losses = log1p(losses),
    log_threeCrownWins = log1p(threeCrownWins),
    log_starPoints = log1p(starPoints),
    log_totalExpPoints = log1p(totalExpPoints),
    log_tournamentBattleCount = log1p(tournamentBattleCount),
    log_warDayWins = log1p(warDayWins)
  )

# Radice Quadrata (sqrt)
train_data_processed <- train_data_processed %>%
  mutate(
    sqrt_bestLeagueNumber = sqrt(bestLeagueNumber),
    sqrt_CardsEvo = sqrt(CardsEvo),
    sqrt_currentLeagueNumber = sqrt(currentLeagueNumber),
    sqrt_lastLeagueNumber = sqrt(lastLeagueNumber),
    sqrt_SupportCardsLevel13 = sqrt(SupportCardsLevel13),
    sqrt_SupportCardsLevel14 = sqrt(SupportCardsLevel14),
    sqrt_SupportCardsLevel15 = sqrt(SupportCardsLevel15),
    sqrt_wins = sqrt(wins),
    sqrt_yearsSinceRegistration = sqrt(yearsSinceRegistration)
  )

# Quadrato (x^2)
train_data_processed <- train_data_processed %>%
  mutate(
    sq_meanLevelCards = meanLevelCards^2,
    sq_meanLevelSupportCards = meanLevelSupportCards^2
  )

# --- 3. Rimozione delle variabili originali che sono state trasformate  ---
variables_to_replace_with_transformed <- c(
  "battleCount", "CardsLevel14", "CardsLevel15", "challengeCardsWon",
  "clanCardsCollected", "totalDonations", "donations", "donationsReceived",
  "expPoints", "losses", "threeCrownWins", "starPoints", "totalExpPoints",
  "tournamentBattleCount", "warDayWins", "bestLeagueNumber", "CardsEvo",
  "currentLeagueNumber", "lastLeagueNumber", "SupportCardsLevel13",
  "SupportCardsLevel14", "SupportCardsLevel15", "wins",
  "yearsSinceRegistration", "meanLevelCards", "meanLevelSupportCards"
)

train_data_processed <- train_data_processed %>%
  select(-any_of(variables_to_replace_with_transformed))
# dalle 173 variabili originali 7 sono rimosse e 26 sostituite
# così train_data_processed ha 166 variabili
# andiamo a plottare solo le 26 modificate e salviamo i plot in una nuova cartella

# i nuovi nomi delle 26 variabili trasformate
variables_transformed <- c(
  "log_battleCount", "log_CardsLevel14", "log_CardsLevel15",
  "log_challengeCardsWon", "log_clanCardsCollected", "log_totalDonations",
  "log_donations", "log_donationsReceived", "log_expPoints", "log_losses",
  "log_threeCrownWins", "log_starPoints", "log_totalExpPoints",
  "log_tournamentBattleCount", "log_warDayWins", "sqrt_bestLeagueNumber",
  "sqrt_CardsEvo", "sqrt_currentLeagueNumber", "sqrt_lastLeagueNumber",
  "sqrt_SupportCardsLevel13", "sqrt_SupportCardsLevel14",
  "sqrt_SupportCardsLevel15", "sqrt_wins", "sqrt_yearsSinceRegistration",
  "sq_meanLevelCards", "sq_meanLevelSupportCards"
)

for(colonna in variables_transformed) {
  
  # Calcola il coefficiente di correlazione Pearson
  cor_val <- cor(train_data_processed[[colonna]], train_data_processed[[dependent_var]], use = "pairwise.complete.obs")
  cor_label <- paste0("Correlazione (r): ", round(cor_val, 2))
  
  # Prepara l'etichetta pulita per il titolo del grafico e l'asse X
  clean_col_name <- colonna
  if (startsWith(colonna, "log_")) {
    clean_col_name <- paste0("Log(", sub("log_", "", colonna), ")")
  } else if (startsWith(colonna, "sqrt_")) {
    clean_col_name <- paste0("Radice Quadrata(", sub("sqrt_", "", colonna), ")")
  } else if (startsWith(colonna, "sq_")) {
    clean_col_name <- paste0("Quadrato(", sub("sq_", "", colonna), ")")
  }
  
  plottt <- ggplot(train_data_processed, aes_string(x = colonna, y = dependent_var)) +
    geom_point(alpha = 0.4, color = "steelblue") +
    geom_smooth(method = "lm", se = FALSE, color = "darkblue", linetype = "dashed") +
    annotate("text",
             x = Inf, y = Inf, # Posiziona il testo nell'angolo in alto a destra
             label = cor_label,
             hjust = 1.1, vjust = 1.1, # Regola la giustificazione per distanziare dal bordo
             size = 12, color = "darkred", fontface = "bold") +
    labs(title = paste0("Relazione tra ", clean_col_name, " e ", dependent_var),
         x = clean_col_name,
         y = "Trofei") +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA),
      
      plot.title = element_text(size = 32, face = "bold"),   # Titolo del grafico più grande
      axis.title = element_text(size = 30, face = "bold"),   # Titoli degli assi più grandi
      axis.text = element_text(size = 28),                   # Etichette numeriche/testuali sugli assi più grandi
      legend.text = element_text(size = 28),                 # Testo della legenda più grande (se presente)
      legend.title = element_text(size = 30, face = "bold")
    )
  
  # Salva il plot nella nuova cartella specificata
  ggsave(file.path("plotColonneTrasformate", paste0("plot_trofei_vs_", colonna, ".png")),
         plot = plottt, width = 15, height = 15, dpi = 300)
}

# Da questi grafici si nota che ci sono altre due variabili piatte e che non
# sembrano influenzare Y, rimuoviamo anche loro
# Includiamo le nuove esclusioni: donationsReceived e warDayWins
variables_to_remove<- c(
  "currentLeagueRank",
  "lastLeagueRank",
  "bestLeagueRank",
  "bestLeagueTrophies",
  "lastLeagueTrophies",
  "currentLeagueTrophies",
  "tournamentCardsWon",
  # rimosse in più di prima:
  "donationsReceived",
  "warDayWins"
)

# Applichiamo queste rimozioni al dataset di training
train_data_processed <- train_data %>%
  select(-all_of(variables_to_remove))

# --- 2. Applicazione di TUTTE le trasformazioni ---
# Creiamo le nuove variabili trasformate.

train_data_processed <- train_data_processed %>%
  mutate(
    # --- Trasformazioni Logaritmiche (log1p)
    log_CardsLevel14 = log1p(CardsLevel14),
    log_CardsLevel15 = log1p(CardsLevel15),
    log_challengeCardsWon = log1p(challengeCardsWon),
    log_clanCardsCollected = log1p(clanCardsCollected),
    log_totalDonations = log1p(totalDonations),
    log_donations = log1p(donations),
    log_threeCrownWins = log1p(threeCrownWins),
    log_starPoints = log1p(starPoints),
    log_totalExpPoints = log1p(totalExpPoints),
    log_tournamentBattleCount = log1p(tournamentBattleCount),
    log_wins = log1p(wins),
    
    # --- Trasformazione Quadrato del Logaritmo (per battleCount) ---
    sq_log_battleCount = (log1p(battleCount))^2,
    
    # --- Trasformazioni Radice Quarta (per expPoints, losses)
    fourth_root_expPoints = expPoints^(1/4),
    fourth_root_losses = losses^(1/4),
    
    # --- Trasformazioni Radice Quadrata (confermate dal piano precedente) ---
    sqrt_bestLeagueNumber = sqrt(bestLeagueNumber),
    sqrt_CardsEvo = sqrt(CardsEvo),
    sqrt_currentLeagueNumber = sqrt(currentLeagueNumber),
    sqrt_lastLeagueNumber = sqrt(lastLeagueNumber),
    sqrt_SupportCardsLevel13 = sqrt(SupportCardsLevel13),
    sqrt_SupportCardsLevel14 = sqrt(SupportCardsLevel14),
    sqrt_SupportCardsLevel15 = sqrt(SupportCardsLevel15),
    sqrt_yearsSinceRegistration = sqrt(yearsSinceRegistration),
    
    # --- Trasformazioni al Quadrato (confermate dal piano precedente) ---
    sq_meanLevelCards = meanLevelCards^2,
    sq_meanLevelSupportCards = meanLevelSupportCards^2
  )

# dopo aver rimosso 9 variabili ed aggiunto le 24 modificate il dataset ha
# 173 - 9 + 24 = 188 variabili

# --- 3. Rimozione delle variabili originali che sono state trasformate  ---
variables_to_replace_with_transformed <- c(
  "battleCount", "CardsLevel14", "CardsLevel15", "challengeCardsWon",
  "clanCardsCollected", "totalDonations", "donations", "expPoints", "losses",
  "threeCrownWins", "starPoints", "totalExpPoints", "tournamentBattleCount",
  "bestLeagueNumber", "CardsEvo", "currentLeagueNumber", "lastLeagueNumber",
  "SupportCardsLevel13", "SupportCardsLevel14", "SupportCardsLevel15", "wins",
  "yearsSinceRegistration", "meanLevelCards", "meanLevelSupportCards"
)

train_data_processed <- train_data_processed %>%
  select(-any_of(variables_to_replace_with_transformed))
# dalle 173 variabili originali 9 sono rimosse e 24 sostituite
# così train_data_processed ha 164 variabili
# andiamo a plottare solo le 4 modificate e salviamo i plot in una nuova cartella
# i nuovi nomi delle 4 variabili trasformate rispetto a prima

variables_transformed <- c(
  "sq_log_battleCount", "fourth_root_expPoints", "fourth_root_losses",
  "log_wins")

for(colonna in variables_transformed) {
  
  # Calcola il coefficiente di correlazione Pearson
  cor_val <- cor(train_data_processed[[colonna]], train_data_processed[[dependent_var]], use = "pairwise.complete.obs")
  cor_label <- paste0("Correlazione (r): ", round(cor_val, 2))
  
  # Prepara l'etichetta pulita per il titolo del grafico e l'asse X
  clean_col_name <- colonna
  if (startsWith(colonna, "log_")) {
    clean_col_name <- paste0("Log(", sub("log_", "", colonna), ")")
  } else if (startsWith(colonna, "sqrt_")) {
    clean_col_name <- paste0("Radice Quadrata(", sub("sqrt_", "", colonna), ")")
  } else if (startsWith(colonna, "sq_")) {
    clean_col_name <- paste0("Quadrato(", sub("sq_", "", colonna), ")")
  } else if (startsWith(colonna, "sq_log_")) {
    clean_col_name <- paste0("Quadrato(Log(", sub("sq_log", "", colonna), "))")
  }
  
  plottt <- ggplot(train_data_processed, aes_string(x = colonna, y = dependent_var)) +
    geom_point(alpha = 0.4, color = "steelblue") +
    geom_smooth(method = "lm", se = FALSE, color = "darkblue", linetype = "dashed") +
    annotate("text",
             x = Inf, y = Inf, # Posiziona il testo nell'angolo in alto a destra
             label = cor_label,
             hjust = 1.1, vjust = 1.1, # Regola la giustificazione per distanziare dal bordo
             size = 12, color = "darkred", fontface = "bold") +
    labs(title = paste0("Relazione tra ", clean_col_name, " e ", dependent_var),
         x = clean_col_name,
         y = "Trofei") +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA),
      
      plot.title = element_text(size = 32, face = "bold"),   # Titolo del grafico più grande
      axis.title = element_text(size = 30, face = "bold"),   # Titoli degli assi più grandi
      axis.text = element_text(size = 28),                   # Etichette numeriche/testuali sugli assi più grandi
      legend.text = element_text(size = 28),                 # Testo della legenda più grande (se presente)
      legend.title = element_text(size = 30, face = "bold")
    )
  
  # Salva il plot nella nuova cartella specificata
  ggsave(file.path("plotColonneTrasformate2", paste0("plot_trofei_vs_", colonna, ".png")),
         plot = plottt, width = 15, height = 15, dpi = 300)
}

write_parquet(train_data_processed, "player_data_train_processed.parquet")

# ----------------------------------------------------------------------------------------------------------------
## nuovo codice con variabili trasformate in fattoriali

# --- Definizione delle variabili da rimuovere inizialmente ---
variables_to_remove<- c(
  "currentLeagueRank",
  "lastLeagueRank",
  "bestLeagueRank",
  "bestLeagueTrophies",
  "lastLeagueTrophies",
  "currentLeagueTrophies",
  "tournamentCardsWon",
  # rimosse in più di prima:
  "donationsReceived",
  "warDayWins"
)

# Applichiamo queste rimozioni al dataset di training
train_data_processed <- train_data %>%
  select(-all_of(variables_to_remove))

# --- Conversione di Variabili Numeriche in Fattori Ordinati ---
# Definiamo le variabili da convertire e i loro livelli (range di valori)

# per scoprire i livelli:
# levels(as.factor(train_data$bestLeagueNumber))
# levels(as.factor(train_data$currentLeagueNumber))
# levels(as.factor(train_data$lastLeagueNumber))
# levels(as.factor(train_data$NumberSupportCards))
# levels(as.factor(train_data$SupportCardsLevel13))
# levels(as.factor(train_data$SupportCardsLevel14))
# levels(as.factor(train_data$SupportCardsLevel15))

factor_vars_and_levels <- list(
  bestLeagueNumber = 1:10,
  currentLeagueNumber = 1:10,
  lastLeagueNumber = 1:10,
  NumberSupportCards = 1:4,
  SupportCardsLevel13 = 0:4,
  SupportCardsLevel14 = 0:4,
  SupportCardsLevel15 = 0:4
)

for (var_name in names(factor_vars_and_levels)) {
  levels_vec <- factor_vars_and_levels[[var_name]]
  if (var_name %in% colnames(train_data_processed)) {
    train_data_processed[[var_name]] <- factor(train_data_processed[[var_name]],
                                               levels = levels_vec,
                                               ordered = TRUE)
  } else {
    warning(paste0("Variabile '", var_name, "' non trovata in train_data_processed per la conversione a fattore."))
  }
}
# levels(train_data_processed$bestLeagueNumber)
# levels(train_data_processed$currentLeagueNumber)
# levels(train_data_processed$lastLeagueNumber)
# levels(train_data_processed$NumberSupportCards)
# levels(train_data_processed$SupportCardsLevel13)
# levels(train_data_processed$SupportCardsLevel14)
# levels(train_data_processed$SupportCardsLevel15)

# --- Applicazione di TUTTE le altre trasformazioni (log, radice, quadrato) ---
# Modificato per escludere le variabili ora fattoriali dalle trasformazioni numeriche.
train_data_processed <- train_data_processed %>%
  mutate(
    # --- Trasformazioni Logaritmiche (log1p)
    log_CardsLevel14 = log1p(CardsLevel14),
    log_CardsLevel15 = log1p(CardsLevel15),
    log_challengeCardsWon = log1p(challengeCardsWon),
    log_clanCardsCollected = log1p(clanCardsCollected),
    log_totalDonations = log1p(totalDonations),
    log_donations = log1p(donations),
    log_threeCrownWins = log1p(threeCrownWins),
    log_starPoints = log1p(starPoints),
    log_totalExpPoints = log1p(totalExpPoints),
    log_tournamentBattleCount = log1p(tournamentBattleCount),
    log_wins = log1p(wins),
    
    # --- Trasformazione Quadrato del Logaritmo (per battleCount) ---
    sq_log_battleCount = (log1p(battleCount))^2,
    
    # --- Trasformazioni Radice Quarta (per expPoints, losses)
    fourth_root_expPoints = expPoints^(1/4),
    fourth_root_losses = losses^(1/4),
    
    # --- Trasformazioni Radice Quadrata (confermate dal piano precedente) ---
    sqrt_CardsEvo = sqrt(CardsEvo),
    sqrt_yearsSinceRegistration = sqrt(yearsSinceRegistration),
    
    # --- Trasformazioni al Quadrato (confermate dal piano precedente) ---
    sq_meanLevelCards = meanLevelCards^2,
    sq_meanLevelSupportCards = meanLevelSupportCards^2
  )
# dopo aver rimosso 9 variabili ed aggiunto le 18 (24 precedenti - 6 perchè
# NumberSupportCards non la modificavamo neanche prima) modificate il dataset ha
# 173 - 9 + 18 = 182 variabili

# --- 3. Rimozione delle variabili originali che sono state trasformate  ---
variables_to_replace_with_transformed <- c(
  "battleCount", "CardsLevel14", "CardsLevel15", "challengeCardsWon",
  "clanCardsCollected", "totalDonations", "donations", "expPoints", "losses",
  "threeCrownWins", "starPoints", "totalExpPoints", "tournamentBattleCount",
  "CardsEvo", "wins", "yearsSinceRegistration", "meanLevelCards",
  "meanLevelSupportCards"
)

train_data_processed <- train_data_processed %>%
  select(-any_of(variables_to_replace_with_transformed))
# dalle 173 variabili originali 9 sono rimosse e 18 sostituite
# così train_data_processed ha 164 variabili

rm(factor_vars_and_levels)
rm(levels_vec, var_name)

length(colnames(train_data_processed %>%
  select(where(is.numeric))))

write_parquet(train_data_processed, "player_data_train_processed.parquet")

# --------------------------------------------------------------------------------
# Regressione con lm ed analisi delle correlazioni con VIF

# --- 0. Caricamento del dataset train_data_processed ---
train_data_processed <- read_parquet("player_data_train_processed.parquet")

# --- 1. Ulteriore Rimozione di Variabili Specifiche ---
train_data_processed <- train_data_processed %>%
  select(-tag, -name, -bestTrophies)

# --- 2. Fitta il Modello di Regressione Lineare COMPLETO ---
model_lm_completo <- lm(trophies ~ ., data = train_data_processed)

message("\n--- Modello lineare completo fittato con successo. ---")

# --- 3. Calcolo dei VIF (Variance Inflation Factor) ---
message("\n--- Calcolo dei VIF ---")
vif_results <- vif(model_lm_completo)

# --- 4. Stampa e Analizza i Risultati dei VIF ---
!is.null(vif_results)
# Trasforma la matrice VIF in un tibble, prendendo i nomi delle righe come 'Variable'
# e la colonna 'GVIF^(1/(2*Df))' come il valore VIF da analizzare.
vif_df <- as_tibble(vif_results, rownames = "Variable") %>%
  rename(VIF_Adjusted = `GVIF^(1/(2*Df))`) # Rinomina la colonna per chiarezza

# Filtra e ordina i VIF molto alti (alta multicollinearità)
# Una soglia di 2.5 o 3 per GVIF^(1/(2*Df)) è spesso usata per indicare problemi.
high_vif <- vif_df %>%
  filter(VIF_Adjusted > 2.5) %>% # Soglia più conservativa per GVIF^(1/(2*Df))
  arrange(desc(VIF_Adjusted))

message("\n--- Variabili con VIF Elevato (Multicollinearità Rilevante, VIF_Adjusted > 2.5) ---")
print(high_vif)
# Ci sono 72 variabili con VIF_Adjusted > 2.5. Queste indicano una multicollinearità
# significativa. Considera la loro analisi per migliorare l'interpretabilità.

# Stampa una panoramica dei VIF (i primi 20 più alti, ordinati per VIF_Adjusted)
message("\n--- Panoramica dei VIF (i primi 20 più alti per VIF_Adjusted) ---")
print(vif_df %>%
        arrange(desc(VIF_Adjusted)) %>%
        head(20))
#     Variable                    GVIF     Df  VIF_Adjusted
#  1 meanCostDeck                 787.      1        28.1 
#  2 sq_log_battleCount           413.      1        20.3 
#  3 expLevel                     216.      1        14.7 
#  4 log_totalExpPoints           157.      1        12.5 
#  5 log_wins                     156.      1        12.5 
#  6 fourth_root_losses           137.      1        11.7 
#  7 sqrt_yearsSinceRegistration  107.      1        10.4 
#  8 `Mega Knight`                103.      1        10.1 
#  9 P.E.K.K.A                    61.7      1        7.85
# 10 daysSinceRegistration        60.1      1        7.75
# 11 Skeletons                    59.5      1        7.71
# 12 `Ice Spirit`                 54.9      1        7.41
# 13 `The Log`                    44.0      1        6.63
# 14 Zap                          26.6      1        5.16
# 15 cardsOwned                   24.8      1        4.98
# 16 Witch                        24.0      1        4.89
# 17 Bats                         21.9      1        4.68
# 18 log_threeCrownWins           21.3      1        4.61
# 19 Golem                        19.3      1        4.39
# 20 `Electro Spirit`             17.9      1        4.23


rm(vif_df, vif_results)
rm(model_lm_completo)

# --- Definizione delle variabili da rimuovere basate su vif e logica ---
variables_to_remove_final_vif <- c(
  "meanCostDeck",
  "sq_log_battleCount",
  "expLevel",
  "sqrt_yearsSinceRegistration"
)

message("--- Rimozione delle variabili selezionate per multicollinearità ---")

# Applica la rimozione delle variabili al dataset
train_data_processed_final <- train_data_processed %>%
  select(-any_of(variables_to_remove_final_vif))

message("Variabili rimosse in questo passaggio: ", paste(variables_to_remove_final_vif, collapse = ", "))
message("Nuovo numero di colonne nel dataset: ", ncol(train_data_processed_final))


# --- Rifitting del Modello di Regressione Lineare con il dataset pulito ---
model_lm_final <- lm(trophies ~ ., data = train_data_processed_final)

vif_results_final_cleaned <- vif(model_lm_final)

# --- Stampa e Analizza i Risultati dei VIF ---
!is.null(vif_results_final_cleaned)
# Converti la matrice VIF in un tibble
vif_df_final_cleaned <- as_tibble(vif_results_final_cleaned, rownames = "Variable") %>%
  rename(VIF_Adjusted = `GVIF^(1/(2*Df))`)

# Filtra e ordina i VIF_Adjusted molto alti (> 2.5)
high_vif_final_cleaned <- vif_df_final_cleaned %>%
  filter(VIF_Adjusted > 2.5) %>%
  arrange(desc(VIF_Adjusted))

# --- Variabili con VIF Elevato (DOPO LA PRIMA PULIZIA) VIF_Adjusted > 2.5
print(high_vif_final_cleaned)
# Ci sono ancora 34 variabili con VIF_Adjusted > 2.5, è necessaria un'ulteriore rimozione.

# Panoramica dei VIF (i primi 20 più alti DOPO LA PRIMA PULIZIA)
print(vif_df_final_cleaned %>%
        arrange(desc(VIF_Adjusted)) %>%
        head(20))
#     Variable                 GVIF    Df  VIF_Adjusted
#  1 log_wins                  66.5     1         8.15
#  2 log_totalExpPoints        47.7     1         6.90
#  3 fourth_root_losses        25.8     1         5.08
#  4 cardsOwned                21.4     1         4.62
#  5 log_threeCrownWins        21.1     1         4.59
#  6 `The Log`                 13.1     1         3.62
#  7 sq_meanLevelCards         13.1     1         3.62
#  8 Valkyrie                  12.0     1         3.47
#  9 `Mini P.E.K.K.A`          12.0     1         3.46
# 10 Fireball                  12.0     1         3.46
# 11 `Tower Princess`          11.9     1         3.45
# 12 Arrows                    11.9     1         3.45
# 13 `Goblin Barrel`           11.1     1         3.34
# 14 Knight                    10.8     1         3.28
# 15 `Hog Rider`               10.5     1         3.23
# 16 `Mega Knight`             10.4     1         3.23
# 17 Witch                     9.75     1         3.12
# 18 Firecracker               9.71     1         3.12
# 19 `Skeleton Army`           9.61     1         3.10
# 20 sq_meanLevelSupportCards  9.01     1         3.00

rm(high_vif, high_vif_final_cleaned, model_lm_final, train_data_processed_final)
rm(vif_df_final_cleaned, vif_results_final_cleaned)
rm(variables_to_remove_final_vif)



# --- 0. Caricamento del dataset train_data_processed ---
train_data_processed <- read_parquet("player_data_train_processed.parquet")

# --- 1. Ulteriore Rimozione di Variabili Specifiche ---
train_data_processed <- train_data_processed %>%
  select(-tag, -name, -bestTrophies)

# --- 2. Variabili rimosse nella prima passata VIF ---
# Queste sono le variabili rimosse nella penultima interazione.
first_pass_removed_vars <- c(
  "meanCostDeck",
  "sq_log_battleCount",
  "expLevel",
  "sqrt_yearsSinceRegistration"
)

# --- Rimozione delle variabili selezionate per multicollinearità (PRIMA PASSATA) ---
train_data_processed_first_cleaned <- train_data_processed %>%
  select(-any_of(first_pass_removed_vars))
rm(first_pass_removed_vars)

# --- 3. Definizione delle variabili da rimuovere in questa NUOVA passata (seconda passata VIF) ---
variables_to_remove_second_vif_pass <- c(
  "fourth_root_losses",
  "log_threeCrownWins",
  "sq_meanLevelCards",
  "sq_meanLevelSupportCards"
)

# --- Rimozione delle variabili selezionate per multicollinearità (SECONDA PASSATA)
train_data_processed_final <- train_data_processed_first_cleaned %>%
  select(-any_of(variables_to_remove_second_vif_pass))
rm(train_data_processed_first_cleaned)
rm(variables_to_remove_second_vif_pass)

# Nuovo numero di colonne nel dataset: 153


# --- 4. Rifitta il Modello di Regressione Lineare con il dataset ulteriormente pulito ---
model_lm_final_vif_cleaned <- lm(trophies ~ ., data = train_data_processed_final, na.action = na.omit)

# --- Calcolo dei VIF sul modello ulteriormente pulito ---
vif_results_final_cleaned <- vif(model_lm_final_vif_cleaned)

# --- 5. Stampa e Analizza i Risultati dei VIF ---
!is.null(vif_results_final_cleaned)
vif_df_final_cleaned <- as_tibble(vif_results_final_cleaned, rownames = "Variable") %>%
  rename(VIF_Adjusted = `GVIF^(1/(2*Df))`) %>%
  select(Variable, VIF_Adjusted, Df, GVIF)

# Filtra e ordina i VIF_Adjusted molto alti (> 2.5)
high_vif_final_cleaned <- vif_df_final_cleaned %>%
  filter(VIF_Adjusted > 2.5)

# --- Variabili con VIF Elevato (DOPO TUTTE LE PULIZIE, VIF_Adjusted > 2.5) ---
print(high_vif_final_cleaned)
# Ci sono ancora 29 variabili con VIF_Adjusted > 2.5.

# --- Panoramica dei VIF (i primi 20 più alti DOPO TUTTE LE PULIZIE) ---
print(vif_df_final_cleaned %>%
        arrange(desc(VIF_Adjusted)) %>%
        head(20))
# Variable              VIF_Adjusted    Df  GVIF
#  1 log_totalExpPoints         5.50     1  30.2 
#  2 cardsOwned                 4.20     1  17.6 
#  3 log_wins                   4.10     1  16.8 
#  4 `The Log`                  3.62     1  13.1 
#  5 Valkyrie                   3.46     1  12.0 
#  6 Fireball                   3.46     1  11.9 
#  7 `Mini P.E.K.K.A`           3.45     1  11.9 
#  8 `Tower Princess`           3.45     1  11.9 
#  9 Arrows                     3.44     1  11.9 
# 10 `Goblin Barrel`            3.33     1  11.1 
# 11 Knight                     3.28     1  10.8 
# 12 `Hog Rider`                3.23     1  10.4 
# 13 `Mega Knight`              3.22     1  10.4 
# 14 Witch                      3.11     1  9.70
# 15 Firecracker                3.11     1  9.69
# 16 `Skeleton Army`            3.10     1  9.59
# 17 Skeletons                  2.96     1  8.78
# 18 Cannon                     2.91     1  8.47
# 19 `Ice Spirit`               2.87     1  8.25
# 20 Musketeer                  2.86     1  8.19

rm(high_vif_final_cleaned, train_data_processed_final)
rm(vif_df_final_cleaned, vif_results_final_cleaned)


# --- Genera e Salva i Grafici Diagnostici ---
output_dir_diagnostics <- "diagnostic_plots"
if (!dir.exists(output_dir_diagnostics)) {
  dir.create(output_dir_diagnostics)
}

# --- Generazione e Salvataggio dei Grafici Diagnostici Finali ---"

# Grafico 1: Residui vs Valori Fittati (Residuals vs Fitted)
png(file.path(output_dir_diagnostics, "residuals_vs_fitted.png"), width = 800, height = 600, res = 100)
plot(model_lm_final_vif_cleaned, which = 1)
dev.off()
message("Grafico 'Residui vs Valori Fittati' salvato.")

# Grafico 2: Q-Q Plot Normale (Normal Q-Q)
png(file.path(output_dir_diagnostics, "qq_plot.png"), width = 800, height = 600, res = 100)
plot(model_lm_final_vif_cleaned, which = 2)
dev.off()
message("Grafico 'Q-Q Plot Normale' salvato.")

# Grafico 3: Scala-Posizione (Scale-Location)
png(file.path(output_dir_diagnostics, "scale_location.png"), width = 800, height = 600, res = 100)
plot(model_lm_final_vif_cleaned, which = 3)
dev.off()
message("Grafico 'Scala-Posizione' salvato.")

# Grafico 4: Residui vs Leverage (Residuals vs Leverage)
png(file.path(output_dir_diagnostics, "leverage.png"), width = 800, height = 600, res = 100)
plot(model_lm_final_vif_cleaned, which = 5)
dev.off()
message("Grafico 'Residui vs Leverage' salvato.")

# Tutti i grafici diagnostici finali sono stati salvati nella cartella 'diagnostic_plots'

# --- Visualizza il Riepilogo del Modello (Summary) ---
summary(model_lm_final_vif_cleaned)

#-----------------------------------------
# Specificazione del modello
#-----------------------------------------
# a) t-test sulla media dei residui
t.test(residuals(model_lm_final_vif_cleaned))
# --- T-test sulla media dei residui (Modello Originale) ---
#          One Sample t-test
# t = 1.7549e-15, df = 10019, p-value = 1
# 95 percent confidence interval:
#   -9.135382  9.135382
# sample estimates:
#   mean of x 
# 8.178651e-15
# così possiamo assumere nulla la media dei residui

# b) Anderson-Darling test per la normalità
# poichè il dataset è troppo grande non è computazionalmente possibile eseguire
# lo shapiro, si è eseguito l'ad.test di nortestcon il seguente risultato:
ad.test(residuals(model_lm_final_vif_cleaned))
#   Anderson-Darling normality test
# 
# data:  residuals(model_lm_final_vif_cleaned)
# A = 75.622, p-value < 2.2e-16

# c) Studentized Breusch-Pagan test per l'eteroschedasticità
# Si usa 'bptest' dal pacchetto 'lmtest'
fit <- formula(model_lm_final_vif_cleaned)
bptest(fit, data = train_data_processed_final)
rm(fit)
# --- Studentized Breusch-Pagan test ---
# BP = 1874, df = 302, p-value < 2.2e-16 (indica eteroschedasticità)

# d) Durbin-Watson test per l'autocorrelazione
# Si usa 'dwtest' dal pacchetto 'lmtest'
fit <- formula(model_lm_final_vif_cleaned)
dwtest(fit, data = train_data_processed_final)
rm(fit)
# --- Durbin-Watson test ---
# Valore Durbin-Watson vicino a 2 indica assenza di autocorrelazione.
# p-value basso (< 0.05) indica autocorrelazione.
# Durbin-Watson test
# DW = 1.2359, p-value < 2.2e-16
# alternative hypothesis: true autocorrelation is greater than 0

# e) Istogramma dei residui
png(file.path("diagnostic_plots", "residuals_histogram.png"), width = 800, height = 600, res = 100)
hist(residuals(model_lm_final_vif_cleaned),col="#CCCCFF", main = "Istogramma dei Residui",
     xlab = "Residui", prob=TRUE, ylab="Frequenza", ylim = c(0, 1.2*10^(-3)))
lines(density(residuals(model_lm_final_vif_cleaned)), col="darkblue", lwd=2)
dev.off()
message("Istogramma dei residui salvato.")


rm(model_lm_final_vif_cleaned, train_data_processed_final)
rm(train_data_final_for_model, output_dir_diagnostics)
rm(variables_to_remove, variables_to_replace_with_transformed, variables_transformed)

# ------------------------------------------------------------------------------------------
# trasformazione trophies -> log_trophies come Y

# --- 0. Caricamento del dataset train_data_processed ---
train_data_processed <- read_parquet("player_data_train_processed.parquet")

# --- 1. Creazione della nuova variabile risposta trasformata: log_trophies ---
train_data_transformed_response <- train_data_processed %>%
  mutate(log_trophies = log(trophies + 1)) %>%
  select(-trophies)


# --- 2. Rimozione di Variabili Specifiche Iniziali (identificatori) ---
initial_removed_vars <- c("tag", "name", "bestTrophies")

# --- 3. Definizione del set COMPLETO di variabili da rimuovere (incluse quelle VIF) ---
# Questa lista include tutte le variabili che abbiamo deciso di rimuovere in tutte le fasi.
all_vars_to_remove <- c(
  initial_removed_vars,
  "meanCostDeck",
  "sq_log_battleCount",
  "expLevel",
  "sqrt_yearsSinceRegistration",
  "fourth_root_losses",
  "log_threeCrownWins",
  "sq_meanLevelCards",
  "sq_meanLevelSupportCards"
)
rm(initial_removed_vars)

# Applica la rimozione di TUTTE le variabili concordate
train_data_final_for_model <- train_data_transformed_response %>%
  select(-any_of(all_vars_to_remove))
rm(all_vars_to_remove)
rm(train_data_transformed_response)

# Variabili totali rimosse: tag, name, bestTrophies, meanCostDeck, sq_log_battleCount,
#                           expLevel, sqrt_yearsSinceRegistration, fourth_root_losses,
#                           log_threeCrownWins, sq_meanLevelCards, sq_meanLevelSupportCards
# Nuovo numero di colonne nel dataset per il modello: 153

# --- 4. Rifitta il Modello di Regressione Lineare con log_trophies come variabile risposta ---
model_lm_log_trophies <- lm(log_trophies ~ ., data = train_data_final_for_model)

# --- Calcolo dei VIF sul nuovo modello con 'log_trophies' ---
# Ricordiamo che la multicollinearità è tra i predittori, quindi i VIF dovrebbero essere simili,
# ma li ricalcoliamo per sicurezza.
vif_results_log_trophies <- vif(model_lm_log_trophies)

# --- 5. Stampa e Analizza i Risultati dei VIF (nuovo modello) ---
!is.null(vif_results_log_trophies)
vif_df_log_trophies <- as_tibble(vif_results_log_trophies, rownames = "Variable") %>%
  rename(VIF_Adjusted = `GVIF^(1/(2*Df))`) %>%
  select(Variable, VIF_Adjusted, Df, GVIF)
rm(vif_results_log_trophies)

high_vif_log_trophies <- vif_df_log_trophies %>%
  filter(VIF_Adjusted > 2.5) %>%
  arrange(desc(VIF_Adjusted))

nrow(high_vif_log_trophies)
# --- Variabili con VIF Elevato (MODELLO CON LOG_TROPHIES, VIF_Adjusted > 5) ---
print(high_vif_log_trophies)
# Ci sono ancora 29 variabili con VIF_Adjusted > 2.5

message("\n--- Panoramica dei VIF (i primi 20 più alti nel MODELLO CON LOG_TROPHIES) ---")
print(vif_df_log_trophies %>%
        arrange(desc(VIF_Adjusted)) %>%
        head(20))
#    Variable           VIF_Adjusted    Df  GVIF
#  1 log_totalExpPoints         5.50     1  30.2 
#  2 cardsOwned                 4.20     1  17.6 
#  3 log_wins                   4.10     1  16.8 
#  4 `The Log`                  3.62     1  13.1 
#  5 Valkyrie                   3.46     1  12.0 
#  6 Fireball                   3.46     1  11.9 
#  7 `Mini P.E.K.K.A`           3.45     1  11.9 
#  8 `Tower Princess`           3.45     1  11.9 
#  9 Arrows                     3.44     1  11.9 
# 10 `Goblin Barrel`            3.33     1  11.1 
# 11 Knight                     3.28     1  10.8 
# 12 `Hog Rider`                3.23     1  10.4 
# 13 `Mega Knight`              3.22     1  10.4 
# 14 Witch                      3.11     1  9.70
# 15 Firecracker                3.11     1  9.69
# 16 `Skeleton Army`            3.10     1  9.59
# 17 Skeletons                  2.96     1  8.78
# 18 Cannon                     2.91     1  8.47
# 19 `Ice Spirit`               2.87     1  8.25
# 20 Musketeer                  2.86     1  8.19

rm(vif_df_log_trophies, high_vif_log_trophies)


# --- 6. Genera e Salva i Grafici Diagnostici (nuovo modello) ---
output_dir_diagnostics_log <- "diagnostic_plots_log_trophies"
if (!dir.exists(output_dir_diagnostics_log)) {
  dir.create(output_dir_diagnostics_log)
}

message("\n--- Generazione e Salvataggio dei Grafici Diagnostici per il modello con 'log_trophies' ---")

# Grafico 1: Residui vs Valori Fittati
png(file.path(output_dir_diagnostics_log, "residuals_vs_fitted_log_trophies.png"), width = 800, height = 600, res = 100)
plot(model_lm_log_trophies, which = 1)
dev.off()
message("Grafico 'Residui vs Valori Fittati (log_trophies)' salvato.")

# Grafico 2: Q-Q Plot Normale
png(file.path(output_dir_diagnostics_log, "qq_plot_log_trophies.png"), width = 800, height = 600, res = 100)
plot(model_lm_log_trophies, which = 2)
dev.off()
message("Grafico 'Q-Q Plot Normale (log_trophies)' salvato.")

# Grafico 3: Scala-Posizione
png(file.path(output_dir_diagnostics_log, "scale_location_log_trophies.png"), width = 800, height = 600, res = 100)
plot(model_lm_log_trophies, which = 3)
dev.off()
message("Grafico 'Scala-Posizione (log_trophies)' salvato.")

# Grafico 4: Residui vs Leverage
png(file.path(output_dir_diagnostics_log, "leverage_log_trophies.png"), width = 800, height = 600, res = 100)
plot(model_lm_log_trophies, which = 5)
dev.off()
message("Grafico 'Residui vs Leverage (log_trophies)' salvato.")

# Tutti i grafici diagnostici per il modello con 'log_trophies' sono stati salvati

#-----------------------------------------
# Specificazione del modello
#-----------------------------------------
# a) t-test sulla media dei residui
t.test(residuals(model_lm_log_trophies))
# --- T-test sulla media dei residui ---
# t = 3.2476e-15, df = 10019, p-value = 1
# 95 percent confidence interval:
#   -0.00505513  0.00505513
# sample estimates:
#   mean of x 
# 8.375093e-18 
# così possiamo assumere nulla la media dei residui

# b) Anderson-Darling test per la normalità
# poichè il dataset è troppo grande non è computazionalmente possibile eseguire
# lo shapiro, si è eseguito l'ad.test di nortestcon il seguente risultato:
ad.test(residuals(model_lm_log_trophies))
#   Anderson-Darling normality test
# 
# data:  residuals(model_lm_log_trophies)
# A = 419.07, p-value < 2.2e-16

# c) Studentized Breusch-Pagan test per l'eteroschedasticità
# Si usa 'bptest' dal pacchetto 'lmtest'
fit <- formula(model_lm_log_trophies)
bptest(fit, data = train_data_final_for_model)
rm(fit)
# --- Studentized Breusch-Pagan test ---
# BP = 815.9, df = 302, p-value < 2.2e-16 (indica eteroschedasticità)

# d) Durbin-Watson test per l'autocorrelazione
# Si usa 'dwtest' dal pacchetto 'lmtest'
fit <- formula(model_lm_log_trophies)
dwtest(fit, data = train_data_final_for_model)
rm(fit)
# --- Durbin-Watson test ---
# Valore Durbin-Watson vicino a 2 indica assenza di autocorrelazione.
# p-value basso (< 0.05) indica autocorrelazione.
# Durbin-Watson test
# DW = 1.6881, p-value < 2.2e-16
# alternative hypothesis: true autocorrelation is greater than 0

# e) Istogramma dei residui
png(file.path("diagnostic_plots_log_trophies", "residuals_histogram_log_trophies.png"), width = 800, height = 1000, res = 100)
hist(residuals(model_lm_log_trophies),col="#CCCCFF", main = "Istogramma dei Residui nel caso Y = log_trophies",
     xlab = "Residui", prob=TRUE, ylab="Frequenza", ylim = c(0, 2.5))
lines(density(residuals(model_lm_log_trophies)), col="darkblue", lwd=2)
dev.off()
message("Istogramma dei residui salvato.")


# --- 7. Visualizza il Riepilogo del Modello (Summary) ---
message("\n--- Riepilogo del Modello Lineare Finale con 'log_trophies' ---")
options(max.print = 5000)
summary(model_lm_log_trophies)
options(max.print = 1000)

# Call:
#   lm(formula = log_trophies ~ ., data = train_data_final_for_model)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.1837 -0.0921  0.0123  0.1174  1.6359 
# 
# Coefficients:
#                                         Estimate Std. Error t value Pr(>|t|)
# (Intercept)                            3.316e+00  1.883e-01  17.614  < 2e-16 ***
# challengeMaxWins                      -4.800e-03  1.716e-03  -2.797 0.005170 **
# roleelder                              6.933e-03  8.811e-03   0.787 0.431373
# rolecoLeader                          -1.010e-03  8.288e-03  -0.122 0.903014
# roleleader                             9.429e-04  1.049e-02   0.090 0.928349
# currentFavouriteCardArchers           -2.402e-01  6.950e-02  -3.456 0.000550 ***
# currentFavouriteCardArrows            -2.049e-01  7.355e-02  -2.786 0.005354 **
# currentFavouriteCardBaby Dragon       -1.053e-03  5.973e-02  -0.018 0.985937
# currentFavouriteCardBalloon           -2.186e-02  6.073e-02  -0.360 0.718911
# currentFavouriteCardBandit            -5.788e-02  8.002e-02  -0.723 0.469483
# currentFavouriteCardBarbarian Barrel   1.071e-02  1.305e-01   0.082 0.934629
# currentFavouriteCardBarbarian Hut      4.280e-02  1.477e-01   0.290 0.771928
# currentFavouriteCardBarbarians         7.587e-03  5.887e-02   0.129 0.897456
# currentFavouriteCardBats               7.850e-03  1.308e-01   0.060 0.952145
# currentFavouriteCardBattle Healer     -2.325e-02  8.755e-02  -0.265 0.790630
# currentFavouriteCardBattle Ram        -4.721e-02  6.295e-02  -0.750 0.453281
# currentFavouriteCardBerserker          6.406e-02  2.690e-01   0.238 0.811810
# currentFavouriteCardBomb Tower        -1.923e-02  7.969e-02  -0.241 0.809359
# currentFavouriteCardBomber            -1.359e-02  1.437e-01  -0.095 0.924700
# currentFavouriteCardBoss Bandit       -1.790e-02  6.098e-02  -0.294 0.769118
# currentFavouriteCardBowler            -2.087e-02  6.479e-02  -0.322 0.747416
# currentFavouriteCardCannon            -4.041e-02  6.837e-02  -0.591 0.554537
# currentFavouriteCardCannon Cart       -2.188e-02  7.708e-02  -0.284 0.776498
# currentFavouriteCardClone             -3.224e-03  1.626e-01  -0.020 0.984182
# currentFavouriteCardDark Prince       -4.175e-02  6.166e-02  -0.677 0.498309
# currentFavouriteCardDart Goblin       -5.445e-02  6.506e-02  -0.837 0.402670
# currentFavouriteCardEarthquake        -6.568e-02  1.436e-01  -0.458 0.647292
# currentFavouriteCardElectro Dragon    -3.154e-02  7.357e-02  -0.429 0.668171
# currentFavouriteCardElectro Giant     -2.249e-03  6.478e-02  -0.035 0.972309
# currentFavouriteCardElectro Wizard    -2.648e-02  6.525e-02  -0.406 0.684829
# currentFavouriteCardElite Barbarians  -2.689e-02  6.040e-02  -0.445 0.656169
# currentFavouriteCardElixir Collector  -1.680e-01  8.892e-02  -1.890 0.058854 .
# currentFavouriteCardElixir Golem      -3.254e-03  1.147e-01  -0.028 0.977357
# currentFavouriteCardExecutioner       -4.642e-02  6.270e-02  -0.740 0.459073
# currentFavouriteCardFireball          -1.598e-01  5.924e-02  -2.697 0.007006 **
# currentFavouriteCardFirecracker       -1.235e-02  6.207e-02  -0.199 0.842322
# currentFavouriteCardFisherman         -1.014e-02  9.196e-02  -0.110 0.912224
# currentFavouriteCardFlying Machine     1.089e-01  8.564e-02   1.272 0.203518
# currentFavouriteCardFreeze            -5.668e-02  8.252e-02  -0.687 0.492132
# currentFavouriteCardFurnace            3.021e-05  8.769e-02   0.000 0.999725
# currentFavouriteCardGiant             -1.209e-01  5.830e-02  -2.074 0.038095 *
# currentFavouriteCardGiant Skeleton    -5.146e-03  5.976e-02  -0.086 0.931385
# currentFavouriteCardGiant Snowball     1.785e-02  2.711e-01   0.066 0.947503
# currentFavouriteCardGoblin Barrel     -4.877e-02  6.044e-02  -0.807 0.419733
# currentFavouriteCardGoblin Cage       -8.355e-02  7.504e-02  -1.113 0.265569
# currentFavouriteCardGoblin Curse      -3.069e-02  1.433e-01  -0.214 0.830432
# currentFavouriteCardGoblin Demolisher  1.227e-01  1.228e-01   0.999 0.317729
# currentFavouriteCardGoblin Drill       9.035e-02  7.135e-02   1.266 0.205411
# currentFavouriteCardGoblin Gang       -6.237e-02  6.420e-02  -0.972 0.331305
# currentFavouriteCardGoblin Giant      -1.740e-02  6.668e-02  -0.261 0.794198
# currentFavouriteCardGoblin Hut        -2.832e-03  6.531e-02  -0.043 0.965411
# currentFavouriteCardGoblin Machine    -2.116e-02  9.789e-02  -0.216 0.828849
# currentFavouriteCardGoblins           -6.488e-02  1.306e-01  -0.497 0.619244
# currentFavouriteCardGoblinstein       -2.074e-02  8.685e-02  -0.239 0.811273
# currentFavouriteCardGolden Knight      2.930e-03  8.551e-02   0.034 0.972667
# currentFavouriteCardGolem             -4.066e-02  6.239e-02  -0.652 0.514600
# currentFavouriteCardGraveyard         -5.327e-02  8.373e-02  -0.636 0.524646
# currentFavouriteCardGuards            -1.068e-01  1.142e-01  -0.935 0.350009
# currentFavouriteCardHeal Spirit       -6.391e-02  2.705e-01  -0.236 0.813247
# currentFavouriteCardHog Rider         -1.618e-02  5.649e-02  -0.286 0.774536
# currentFavouriteCardHunter            -5.308e-02  7.351e-02  -0.722 0.470312
# currentFavouriteCardIce Golem          5.838e-02  1.625e-01   0.359 0.719458
# currentFavouriteCardIce Wizard         7.925e-04  9.724e-02   0.008 0.993498
# currentFavouriteCardInferno Dragon    -1.239e-02  6.301e-02  -0.197 0.844180
# currentFavouriteCardInferno Tower     -2.914e-02  6.092e-02  -0.478 0.632399
# currentFavouriteCardKnight            -7.608e-02  5.827e-02  -1.306 0.191717
# currentFavouriteCardLava Hound        -6.997e-02  7.454e-02  -0.939 0.347972
# currentFavouriteCardLightning         -4.719e-02  8.004e-02  -0.590 0.555444
# currentFavouriteCardLittle Prince      7.777e-02  1.304e-01   0.597 0.550781
# currentFavouriteCardLumberjack        -2.545e-02  6.303e-02  -0.404 0.686360
# currentFavouriteCardMagic Archer       1.991e-03  6.528e-02   0.030 0.975670
# currentFavouriteCardMega Knight       -9.760e-03  5.571e-02  -0.175 0.860924
# currentFavouriteCardMega Minion       -1.877e-01  1.623e-01  -1.157 0.247467
# currentFavouriteCardMighty Miner       1.428e-02  9.416e-02   0.152 0.879474
# currentFavouriteCardMiner             -1.200e-02  7.243e-02  -0.166 0.868468
# currentFavouriteCardMini P.E.K.K.A     1.827e-03  5.674e-02   0.032 0.974320
# currentFavouriteCardMinion Horde      -1.324e-01  7.921e-02  -1.671 0.094728 .
# currentFavouriteCardMinions           -1.509e-01  7.779e-02  -1.939 0.052498 .
# currentFavouriteCardMirror            -8.576e-02  1.433e-01  -0.599 0.549490
# currentFavouriteCardMonk               1.700e-02  9.745e-02   0.174 0.861517
# currentFavouriteCardMortar             2.051e-02  6.723e-02   0.305 0.760370
# currentFavouriteCardMother Witch      -4.363e-02  9.433e-02  -0.462 0.643751
# currentFavouriteCardMusketeer          4.058e-02  5.882e-02   0.690 0.490258
# currentFavouriteCardNight Witch       -6.008e-02  8.700e-02  -0.691 0.489796
# currentFavouriteCardP.E.K.K.A         -1.297e-02  5.667e-02  -0.229 0.819020
# currentFavouriteCardPhoenix           -8.064e-02  1.141e-01  -0.707 0.479641
# currentFavouriteCardPoison            -5.543e-02  8.211e-02  -0.675 0.499634
# currentFavouriteCardPrince            -1.502e-02  5.759e-02  -0.261 0.794288
# currentFavouriteCardPrincess          -4.446e-03  6.381e-02  -0.070 0.944460
# currentFavouriteCardRage              -1.731e-01  1.626e-01  -1.065 0.287102
# currentFavouriteCardRam Rider         -2.161e-02  6.223e-02  -0.347 0.728443
# currentFavouriteCardRascals           -5.034e-02  8.338e-02  -0.604 0.546056
# currentFavouriteCardRocket            -4.220e-03  7.405e-02  -0.057 0.954555
# currentFavouriteCardRoyal Delivery     1.366e-02  1.433e-01   0.095 0.924066
# currentFavouriteCardRoyal Ghost       -2.923e-02  1.089e-01  -0.268 0.788456
# currentFavouriteCardRoyal Giant       -3.456e-02  6.056e-02  -0.571 0.568205
# currentFavouriteCardRoyal Hogs        -5.894e-03  6.117e-02  -0.096 0.923244
# currentFavouriteCardRoyal Recruits    -2.027e-02  6.011e-02  -0.337 0.736006
# currentFavouriteCardRune Giant         2.205e-01  2.767e-01   0.797 0.425599
# currentFavouriteCardSkeleton Army     -3.950e-02  6.478e-02  -0.610 0.542039
# currentFavouriteCardSkeleton Barrel   -1.311e-01  1.216e-01  -1.078 0.280975
# currentFavouriteCardSkeleton Dragons  -8.957e-02  7.152e-02  -1.252 0.210453
# currentFavouriteCardSkeleton King     -1.851e-02  8.400e-02  -0.220 0.825569
# currentFavouriteCardSkeletons         -4.654e-02  2.692e-01  -0.173 0.862752
# currentFavouriteCardSparky            -5.086e-03  6.749e-02  -0.075 0.939934
# currentFavouriteCardSpear Goblins      1.020e-01  1.215e-01   0.840 0.401161
# currentFavouriteCardTesla             -1.656e-02  6.296e-02  -0.263 0.792479
# currentFavouriteCardThe Log           -9.677e-02  1.003e-01  -0.965 0.334572
# currentFavouriteCardThree Musketeers   1.562e-02  8.134e-02   0.192 0.847762
# currentFavouriteCardTombstone         -4.542e-02  8.825e-02  -0.515 0.606826
# currentFavouriteCardTornado           -8.261e-02  8.780e-02  -0.941 0.346770
# currentFavouriteCardValkyrie          -2.392e-02  5.647e-02  -0.424 0.671810
# currentFavouriteCardVoid              -9.037e-02  1.436e-01  -0.629 0.529271
# currentFavouriteCardWall Breakers     -1.544e-01  1.442e-01  -1.071 0.284418
# currentFavouriteCardWitch             -7.822e-03  5.639e-02  -0.139 0.889676
# currentFavouriteCardWizard             2.516e-03  5.799e-02   0.043 0.965398
# currentFavouriteCardX-Bow              1.574e-04  6.272e-02   0.003 0.997998
# currentFavouriteCardZap                6.250e-02  1.623e-01   0.385 0.700212
# currentFavouriteCardZappies            7.111e-02  9.448e-02   0.753 0.451718
# KnightTRUE                             2.547e-02  1.955e-02   1.303 0.192549
# ArchersTRUE                           -3.736e-02  2.100e-02  -1.779 0.075277 .
# GoblinsTRUE                            9.847e-02  2.321e-02   4.243 2.23e-05 ***
# GiantTRUE                             -4.466e-02  2.253e-02  -1.982 0.047463 *
# P.E.K.K.ATRUE                          1.719e-02  2.114e-02   0.813 0.416139
# MinionsTRUE                           -3.040e-02  2.103e-02  -1.445 0.148350
# BalloonTRUE                            3.533e-02  2.238e-02   1.578 0.114505
# WitchTRUE                              5.129e-02  1.985e-02   2.583 0.009801 **
# BarbariansTRUE                         7.199e-02  2.206e-02   3.264 0.001102 **
# GolemTRUE                              2.015e-02  2.979e-02   0.676 0.498841
# SkeletonsTRUE                          6.298e-02  1.995e-02   3.156 0.001604 **
# ValkyrieTRUE                           6.722e-02  1.920e-02   3.501 0.000466 ***
# `Skeleton Army`TRUE                    4.716e-02  1.962e-02   2.403 0.016264 *
# BomberTRUE                             7.117e-02  2.048e-02   3.476 0.000511 ***
# MusketeerTRUE                         -1.150e-02  2.019e-02  -0.570 0.568773
# `Baby Dragon`TRUE                      8.969e-03  2.030e-02   0.442 0.658574
# PrinceTRUE                             3.205e-02  2.089e-02   1.534 0.124969
# WizardTRUE                             4.191e-02  2.034e-02   2.060 0.039414 *
# `Mini P.E.K.K.A`TRUE                   4.135e-02  1.961e-02   2.108 0.035024 *
# `Spear Goblins`TRUE                    4.990e-02  2.113e-02   2.361 0.018236 *
# `Giant Skeleton`TRUE                  -7.850e-03  2.628e-02  -0.299 0.765174
# `Hog Rider`TRUE                        4.103e-02  1.964e-02   2.089 0.036729 *
# `Minion Horde`TRUE                    -3.972e-02  2.719e-02  -1.461 0.144105
# `Ice Wizard`TRUE                       2.139e-02  2.505e-02   0.854 0.393159
# `Royal Giant`TRUE                     -1.186e-02  2.562e-02  -0.463 0.643476
# GuardsTRUE                             4.756e-02  2.125e-02   2.238 0.025259 *
# PrincessTRUE                           1.391e-02  2.119e-02   0.656 0.511776
# `Dark Prince`TRUE                      4.396e-02  2.229e-02   1.973 0.048553 *
# `Three Musketeers`TRUE                 5.759e-02  6.403e-02   0.899 0.368441
# `Lava Hound`TRUE                      -6.769e-03  4.839e-02  -0.140 0.888753
# `Ice Spirit`TRUE                       1.999e-02  2.020e-02   0.990 0.322387
# `Fire Spirit`TRUE                      1.318e-02  2.469e-02   0.534 0.593541
# MinerTRUE                              2.933e-02  2.283e-02   1.285 0.198930
# SparkyTRUE                             4.569e-02  2.771e-02   1.649 0.099210 .
# BowlerTRUE                             5.760e-02  3.126e-02   1.843 0.065420 .
# LumberjackTRUE                         3.656e-02  2.269e-02   1.611 0.107120
# `Battle Ram`TRUE                       1.344e-01  2.186e-02   6.145 8.29e-10 ***
# `Inferno Dragon`TRUE                   9.998e-03  2.099e-02   0.476 0.633825
# `Ice Golem`TRUE                        3.673e-03  2.433e-02   0.151 0.879989
# `Mega Minion`TRUE                      8.733e-02  2.315e-02   3.772 0.000163 ***
# `Dart Goblin`TRUE                      6.097e-03  2.033e-02   0.300 0.764229
# `Goblin Gang`TRUE                      2.657e-02  2.032e-02   1.307 0.191087
# `Electro Wizard`TRUE                   1.649e-02  2.094e-02   0.788 0.430969
# `Elite Barbarians`TRUE                 3.049e-02  2.421e-02   1.260 0.207868
# HunterTRUE                             2.239e-02  2.706e-02   0.827 0.407981
# ExecutionerTRUE                        3.458e-02  2.483e-02   1.393 0.163653
# BanditTRUE                             2.748e-02  2.347e-02   1.171 0.241539
# `Royal Recruits`TRUE                   8.672e-03  2.893e-02   0.300 0.764371
# `Night Witch`TRUE                     -3.223e-02  3.214e-02  -1.003 0.316013
# BatsTRUE                               2.614e-02  2.002e-02   1.306 0.191708
# `Royal Ghost`TRUE                      2.246e-02  2.803e-02   0.801 0.422938
# `Ram Rider`TRUE                        3.949e-02  2.452e-02   1.611 0.107282
# ZappiesTRUE                            1.277e-02  4.192e-02   0.305 0.760584
# RascalsTRUE                            3.457e-02  4.449e-02   0.777 0.437225
# `Cannon Cart`TRUE                      1.717e-02  4.230e-02   0.406 0.684876
# `Mega Knight`TRUE                      2.704e-02  2.030e-02   1.332 0.182945
# `Skeleton Barrel`TRUE                  1.438e-02  2.616e-02   0.550 0.582459
# `Flying Machine`TRUE                   4.296e-02  3.422e-02   1.255 0.209362
# `Wall Breakers`TRUE                    7.647e-03  2.320e-02   0.330 0.741708
# `Royal Hogs`TRUE                       9.924e-03  2.727e-02   0.364 0.715928
# `Goblin Giant`TRUE                     4.008e-02  3.556e-02   1.127 0.259693
# FishermanTRUE                          1.183e-01  3.532e-02   3.349 0.000815 ***
# `Magic Archer`TRUE                     1.106e-02  2.281e-02   0.485 0.627850
# `Electro Dragon`TRUE                   5.338e-02  2.898e-02   1.842 0.065526 .
# FirecrackerTRUE                        8.622e-03  1.974e-02   0.437 0.662314
# `Mighty Miner`TRUE                     5.550e-02  3.516e-02   1.579 0.114473
# `Elixir Golem`TRUE                    -3.219e-03  2.920e-02  -0.110 0.912215
# `Battle Healer`TRUE                    7.695e-02  4.056e-02   1.897 0.057801 .
# `Skeleton King`TRUE                    4.521e-02  2.978e-02   1.518 0.129052
# `Archer Queen`TRUE                     6.312e-02  3.152e-02   2.002 0.045260 *
# `Golden Knight`TRUE                    4.661e-03  2.852e-02   0.163 0.870164
# MonkTRUE                               2.355e-02  4.681e-02   0.503 0.614806
# `Skeleton Dragons`TRUE                 4.731e-02  2.557e-02   1.850 0.064323 .
# `Mother Witch`TRUE                    -4.535e-03  3.643e-02  -0.124 0.900937
# `Electro Spirit`TRUE                   5.335e-02  2.275e-02   2.345 0.019041 *
# `Electro Giant`TRUE                    1.826e-02  3.388e-02   0.539 0.589901
# PhoenixTRUE                           -9.227e-04  4.323e-02  -0.021 0.982971
# `Little Prince`TRUE                    1.534e-02  2.735e-02   0.561 0.574834
# `Goblin Demolisher`TRUE                2.679e-02  5.447e-02   0.492 0.622823
# `Goblin Machine`TRUE                   1.755e-02  5.127e-02   0.342 0.732156
# `Suspicious Bush`TRUE                  1.464e-02  4.116e-02   0.356 0.722033
# GoblinsteinTRUE                        3.013e-02  3.262e-02   0.923 0.355775
# `Rune Giant`TRUE                      -7.193e-02  4.679e-02  -1.537 0.124227
# BerserkerTRUE                         -9.263e-03  3.878e-02  -0.239 0.811205
# `Boss Bandit`TRUE                      2.385e-02  2.446e-02   0.975 0.329480
# CannonTRUE                             8.603e-02  2.014e-02   4.271 1.96e-05 ***
# `Goblin Hut`TRUE                       1.212e-01  2.337e-02   5.186 2.19e-07 ***
# MortarTRUE                             1.903e-02  2.926e-02   0.650 0.515518
# `Inferno Tower`TRUE                    7.316e-02  2.137e-02   3.424 0.000619 ***
# `Bomb Tower`TRUE                       7.495e-02  2.610e-02   2.872 0.004092 **
# `Barbarian Hut`TRUE                   -1.218e-01  6.769e-02  -1.800 0.071917 .
# TeslaTRUE                              6.847e-02  2.082e-02   3.288 0.001012 **
# `Elixir Collector`TRUE                -5.503e-02  4.674e-02  -1.177 0.239059
# `X-Bow`TRUE                           -3.039e-02  3.138e-02  -0.969 0.332764
# TombstoneTRUE                          8.215e-02  2.252e-02   3.648 0.000265 ***
# FurnaceTRUE                            2.905e-02  3.456e-02   0.840 0.400718
# `Goblin Cage`TRUE                      9.662e-02  2.370e-02   4.077 4.59e-05 ***
# `Goblin Drill`TRUE                    -1.401e-01  3.335e-02  -4.202 2.67e-05 ***
# FireballTRUE                           1.829e-02  1.946e-02   0.940 0.347302
# ArrowsTRUE                             1.425e-02  1.944e-02   0.733 0.463594
# RageTRUE                               4.188e-03  2.398e-02   0.175 0.861367
# RocketTRUE                             3.599e-02  2.414e-02   1.491 0.136091
# `Goblin Barrel`TRUE                    3.818e-02  1.965e-02   1.943 0.052028 .
# FreezeTRUE                             2.001e-02  2.426e-02   0.825 0.409453
# MirrorTRUE                             1.070e-02  2.550e-02   0.420 0.674845
# LightningTRUE                          9.832e-03  2.688e-02   0.366 0.714566
# ZapTRUE                                5.135e-02  1.989e-02   2.581 0.009866 **
# PoisonTRUE                             7.645e-03  2.451e-02   0.312 0.755099
# GraveyardTRUE                          7.459e-02  3.214e-02   2.321 0.020318 *
# `The Log`TRUE                          2.673e-02  1.967e-02   1.359 0.174228
# TornadoTRUE                            3.437e-02  2.483e-02   1.384 0.166284
# CloneTRUE                              5.302e-02  3.215e-02   1.649 0.099146 .
# EarthquakeTRUE                        -9.609e-03  2.795e-02  -0.344 0.730980
# `Barbarian Barrel`TRUE                 3.266e-02  2.293e-02   1.424 0.154405
# `Heal Spirit`TRUE                     -3.461e-02  4.523e-02  -0.765 0.444179
# `Giant Snowball`TRUE                   4.422e-02  2.348e-02   1.883 0.059686 .
# `Royal Delivery`TRUE                  -9.851e-03  2.907e-02  -0.339 0.734670
# VoidTRUE                               5.278e-02  5.900e-02   0.895 0.371027
# `Goblin Curse`TRUE                     1.642e-02  3.651e-02   0.450 0.652862
# `Tower Princess`TRUE                   6.796e-02  3.084e-02   2.204 0.027547 *
# CannoneerTRUE                          3.517e-02  3.850e-02   0.913 0.361065
# `Dagger Duchess`TRUE                   1.223e-02  3.363e-02   0.364 0.716066
# `Royal Chef`TRUE                       3.629e-02  3.491e-02   1.039 0.298620
# daysSinceRegistration                 -2.670e-05  5.155e-06  -5.180 2.26e-07 ***
# is_new_playerTRUE                      2.983e-02  1.004e-02   2.971 0.002979 **
# cardsOwned                             1.009e-02  3.224e-04  31.300  < 2e-16 ***
# CardsLevel13                          -6.195e-03  7.526e-04  -8.232  < 2e-16 ***
# CardsLevel12                          -2.066e-03  4.478e-04  -4.615 3.99e-06 ***
# CardsLevel11                          -2.291e-03  3.399e-04  -6.741 1.66e-11 ***
# CardsLevel10                          -2.941e-03  2.507e-04 -11.727  < 2e-16 ***
# SupportCardsLevel15.L                 -2.650e-01  5.295e-02  -5.006 5.66e-07 ***
# SupportCardsLevel15.Q                 -9.364e-02  4.183e-02  -2.239 0.025188 *
# SupportCardsLevel15.C                 -1.868e-02  4.351e-02  -0.429 0.667765
# SupportCardsLevel15^4                 -4.341e-03  3.869e-02  -0.112 0.910659
# SupportCardsLevel14.L                  2.736e-03  1.225e-01   0.022 0.982176
# SupportCardsLevel14.Q                  5.015e-02  1.029e-01   0.487 0.626062
# SupportCardsLevel14.C                  2.809e-02  6.447e-02   0.436 0.663039
# SupportCardsLevel14^4                  2.063e-02  3.061e-02   0.674 0.500359
# SupportCardsLevel13.L                 -2.172e-01  1.702e-01  -1.276 0.202024
# SupportCardsLevel13.Q                 -1.771e-01  1.442e-01  -1.228 0.219325
# SupportCardsLevel13.C                 -9.361e-02  9.631e-02  -0.972 0.331109
# SupportCardsLevel13^4                 -1.730e-02  4.954e-02  -0.349 0.726979
# NumberSupportCards.L                  -2.373e-02  9.156e-03  -2.591 0.009577 **
# NumberSupportCards.Q                   3.412e-02  6.543e-03   5.215 1.88e-07 ***
# NumberSupportCards.C                  -2.810e-02  6.837e-03  -4.110 3.99e-05 ***
# currentLeagueNumber.L                  7.564e-02  1.694e-01   0.446 0.655263
# currentLeagueNumber.Q                 -9.830e-02  1.511e-01  -0.650 0.515458
# currentLeagueNumber.C                 -7.817e-02  1.421e-01  -0.550 0.582365
# currentLeagueNumber^4                 -4.511e-02  1.331e-01  -0.339 0.734753
# currentLeagueNumber^5                 -2.568e-02  1.169e-01  -0.220 0.826143
# currentLeagueNumber^6                  1.614e-02  1.090e-01   0.148 0.882287
# currentLeagueNumber^7                  7.013e-02  9.816e-02   0.714 0.474972
# currentLeagueNumber^8                  5.908e-02  7.178e-02   0.823 0.410505
# currentLeagueNumber^9                  7.674e-03  4.506e-02   0.170 0.864758
# lastLeagueNumber.L                    -2.152e-02  5.914e-02  -0.364 0.715915
# lastLeagueNumber.Q                    -3.919e-02  4.297e-02  -0.912 0.361751
# lastLeagueNumber.C                     1.665e-02  3.740e-02   0.445 0.656183
# lastLeagueNumber^4                     1.904e-02  3.634e-02   0.524 0.600254
# lastLeagueNumber^5                     4.120e-02  3.546e-02   1.162 0.245285
# lastLeagueNumber^6                     2.767e-02  3.360e-02   0.824 0.410191
# lastLeagueNumber^7                     3.483e-03  3.158e-02   0.110 0.912188
# lastLeagueNumber^8                     1.244e-03  3.039e-02   0.041 0.967351
# lastLeagueNumber^9                    -1.260e-02  2.844e-02  -0.443 0.657782
# bestLeagueNumber.L                    -2.419e-02  3.790e-02  -0.638 0.523311
# bestLeagueNumber.Q                    -7.095e-02  2.707e-02  -2.621 0.008781 **
# bestLeagueNumber.C                    -1.618e-02  2.295e-02  -0.705 0.480889
# bestLeagueNumber^4                    -2.123e-02  2.354e-02  -0.902 0.367182
# bestLeagueNumber^5                    -1.766e-02  2.457e-02  -0.719 0.472286
# bestLeagueNumber^6                    -6.279e-04  2.319e-02  -0.027 0.978403
# bestLeagueNumber^7                     1.947e-02  2.107e-02   0.924 0.355482
# bestLeagueNumber^8                    -3.438e-04  2.020e-02  -0.017 0.986421
# bestLeagueNumber^9                    -2.144e-04  1.899e-02  -0.011 0.990995
# log_CardsLevel14                      -5.333e-02  5.361e-03  -9.947  < 2e-16 ***
# log_CardsLevel15                      -8.106e-02  9.703e-03  -8.354  < 2e-16 ***
# log_challengeCardsWon                 -8.117e-03  2.323e-03  -3.494 0.000478 ***
# log_clanCardsCollected                -5.964e-03  1.104e-03  -5.403 6.72e-08 ***
# log_totalDonations                    -2.812e-02  2.120e-03 -13.264  < 2e-16 ***
# log_donations                          5.420e-03  1.670e-03   3.245 0.001179 **
# log_starPoints                         4.328e-03  2.392e-03   1.809 0.070481 .
# log_totalExpPoints                     2.491e-01  6.899e-03  36.107  < 2e-16 ***
# log_tournamentBattleCount             -2.951e-02  2.583e-03 -11.424  < 2e-16 ***
# log_wins                               2.707e-01  7.519e-03  35.998  < 2e-16 ***
# fourth_root_expPoints                 -1.427e-02  1.474e-03  -9.679  < 2e-16 ***
# sqrt_CardsEvo                         -5.881e-02  5.202e-03 -11.305  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2621 on 9717 degrees of freedom
# Multiple R-squared:  0.9146,	Adjusted R-squared:  0.9119 
# F-statistic: 344.4 on 302 and 9717 DF,  p-value: < 2.2e-16


rm(train_data_final_for_model, model_lm_log_trophies, output_dir_diagnostics_log)
rm(model_lm_final_vif_cleaned)
rm(train_data_processed_final)
rm(output_dir_diagnostics)
rm(dependent_var)
