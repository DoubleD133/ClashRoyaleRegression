#-------------------------------------------------------------------------------
#           TEST SUL MIGLIOR METODO:
#                  ELASTIC NET
#-------------------------------------------------------------------------------

# --- Caricamento dei pacchetti ---
library(ggplot2) # Per i grafici di dispersione
library(dplyr)
library(httr)
library(arrow) # Per caricare il file .parquet
library(patchwork)  # Per combinare i grafici
library(caTools)  # Per la funzione sample.split
library(GGally)  # Per ggpairs (utile per matrici di correlazione e scatter plot)
library(car)     # Per la funzione vif() e per outlierTest
library(lmtest)  # per bptest
library(nortest) # per ad.test di normalità
library(olsrr)   # per il Cp di Mallows
library(caret)   # per cross-validation
library(Metrics) # Per MAE e RMSE
library(leaps)   # Per regsubsets e leaps basata su Cp
library(faraway) # Per Cpplot
library(glmnet)  # Per l'omonima funzione che serve per ridge, lass ed elastic net

# plot di numero di variabili ed MSE per
# min R2ajs, min Cp, min Bic, ridge, lasso, elastic
plot(c(32, 31, 25, 34, 29, 30),c(222061.3, 222102, 222780.1, 225243.3, 224492.4, 224146.8))
dati_plot <- data.frame(
  numero_predittori = c(32, 31, 25, 34, 29, 30),
  mse_training = c(222061.3, 222102, 222780.1, 225243.3, 224492.4, 224146.8),
  modello = c("Modello min R2ajs", "Modello min Cp", "Modello min BIC", "Ridge", "LASSO", "ELASTIC NET") # Etichette per la legenda
)

# 3. Crea il plot con ggplot2
ggplot(dati_plot, aes(x = numero_predittori, y = mse_training, color = modello)) +
  geom_point(size = 4, alpha = 0.8) + # size per la dimensione dei punti, alpha per la trasparenza
  labs(
    title = "MSE sul Training vs. Numero di Predittori",
    x = "Numero di Predittori",
    y = "MSE sul Training",
    color = "Modello" # Titolo della legenda
  ) +
  theme_minimal() + # Un tema pulito per il plot
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # Centra e grassetta il titolo
    axis.title = element_text(face = "bold"), # Grassetto per i titoli degli assi
    legend.position = "right" # Posizione della legenda (puoi provare "bottom", "top", "left")
  ) +
  scale_color_brewer(palette = "Set1") # Puoi cambiare la palette di colori (es. "Dark2", "Paired", "Spectral")
rm(dati_plot)

# --- Definizione delle variabili da rimuovere inizialmente ---
variables_to_remove<- c(
  "currentLeagueRank",
  "lastLeagueRank",
  "bestLeagueRank",
  "currentLeagueTrophies",
  "tournamentCardsWon",
  "bestTrophies"
)
# rispetto test_data_processed non rimuoviamo a priori le seguenti:
# "lastLeagueTrophies",
# "bestLeagueTrophies",
# "donationsReceived",
# "warDayWins"
# rimuoverle sarà una scelta dei metodi penalizzati se sarà opportuno

# Applichiamo queste rimozioni al dataset di testing
test_data <- read_parquet("player_data_test.parquet")
new_test_data_processed <- test_data %>%
  select(-all_of(variables_to_remove))

# --- Conversione di Variabili Numeriche in Fattori Ordinati ---
# Definiamo le variabili da convertire e i loro livelli (range di valori)

# per scoprire i livelli:
# levels(as.factor(test_data$bestLeagueNumber))
# levels(as.factor(test_data$currentLeagueNumber))
# levels(as.factor(test_data$lastLeagueNumber))
# levels(as.factor(test_data$NumberSupportCards))
# levels(as.factor(test_data$SupportCardsLevel13))
# levels(as.factor(test_data$SupportCardsLevel14))
# levels(as.factor(test_data$SupportCardsLevel15))

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
  if (var_name %in% colnames(new_test_data_processed)) {
    new_test_data_processed[[var_name]] <- factor(new_test_data_processed[[var_name]],
                                                   levels = levels_vec,
                                                   ordered = TRUE)
  } else {
    warning(paste0("Variabile '", var_name, "' non trovata in new_test_data_processed per la conversione a fattore."))
  }
}
# levels(new_test_data_processed$bestLeagueNumber)
# levels(new_test_data_processed$currentLeagueNumber)
# levels(new_test_data_processed$lastLeagueNumber)
# levels(new_test_data_processed$NumberSupportCards)
# levels(new_test_data_processed$SupportCardsLevel13)
# levels(new_test_data_processed$SupportCardsLevel14)
# levels(new_test_data_processed$SupportCardsLevel15)

# --- Applicazione di TUTTE le altre trasformazioni (log, radice, quadrato) ---
# Modificato per escludere le variabili ora fattoriali dalle trasformazioni numeriche.
new_test_data_processed <- new_test_data_processed %>%
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
# dopo aver rimosso 6 variabili ed aggiunto le 18 modificate il dataset ha
# 173 - 6 + 18 = 185 variabili

# --- 3. Rimozione delle variabili originali che sono state trasformate  ---
variables_to_replace_with_transformed <- c(
  "battleCount", "CardsLevel14", "CardsLevel15", "challengeCardsWon",
  "clanCardsCollected", "totalDonations", "donations", "expPoints", "losses",
  "threeCrownWins", "starPoints", "totalExpPoints", "tournamentBattleCount",
  "CardsEvo", "wins", "yearsSinceRegistration", "meanLevelCards",
  "meanLevelSupportCards"
)

new_test_data_processed <- new_test_data_processed %>%
  select(-any_of(variables_to_replace_with_transformed))
# dalle 173 variabili originali, 6 sono rimosse e 18 sostituite
# così new_test_data_processed ha 167 variabili

rm(factor_vars_and_levels)
rm(levels_vec, var_name)

length(colnames(new_test_data_processed %>%
                  select(where(is.numeric))))

# prendiamo ora soltanto le 32 variabili numeriche ed un paio di fattoriali che
# sono risultate parecchio significative
# come fattoriali nelle precedenti analisi è risultata significativa bestLeagueNumber
# gestita a contrasti polinomiali ortogonali.
# in più aggiungerei anche 'Hog Rider', 'Elixir Golem' e 'Mega Knight' come variabili fattoriali.
# Quindi avremo 32 variabili numeriche, 3 variabili fattoriali binarie ed una 
# variabile fattoriale ordinata con 10 livelli.

variables_to_take = c(colnames(new_test_data_processed %>%
                                 select(where(is.numeric))),
                      'bestLeagueNumber',
                      'Hog Rider', 'Elixir Golem', 'Mega Knight')

cat("Il modello contiene ", length(variables_to_take), " variabili\n")
# Il modello contiene  36  variabili

new_test_data_processed <- new_test_data_processed %>%
  select(all_of(variables_to_take))

dim(new_test_data_processed) # 2505   36

write_parquet(new_test_data_processed, "player_data_test_new_processed.parquet")

rm(variables_to_remove, variables_to_replace_with_transformed)

new_test_data_processed <- read_parquet("player_data_test_new_processed.parquet")
new_train_data_processed <- read_parquet("player_data_new_processed.parquet")

new_train_data_processed <- new_train_data_processed %>%
  select(-any_of(c('Hog Rider', 'Elixir Golem', 'Mega Knight')))
new_test_data_processed <- new_test_data_processed %>%
  select(-any_of(c('Hog Rider', 'Elixir Golem', 'Mega Knight')))

dim(new_test_data_processed) # 2505   33
dim(new_train_data_processed) # 10010    33


# esegui "creazione_metodo_per_test.R"
# così avrai la griglia di modelli cv.out.elastic
cv.out.elastic
# Measure: Mean-Squared Error 
#     Lambda Index Measure   SE Nonzero
# min  0.007   100  223267 3507      34
# 1se  3.718    75  226303 3417      30

X_test <- model.matrix(trophies ~ . - bestLeagueNumber + poly(bestLeagueNumber, 3),
                  data = new_test_data_processed)[,-1] # Rimuovo l'intercetta
Y_test <- new_test_data_processed$trophies

elastic.pred <- predict(cv.out.elastic, s = cv.out.elastic$lambda.1se, newx = X_test)

# elastic.pred[ elastic.pred > 9000] = 9000
# Calcolo errore MSE sul test set
MSE_test <- mean((elastic.pred - Y_test)^2)
MSE_test # 277649.5
sqrt(MSE_test) # 526.9246
MAE_test <- mean(abs(elastic.pred - Y_test))
MAE_test # 391.1744
summary(elastic.pred - Y_test)
# Min.   :-1646.833  
# 1st Qu.: -324.602  
# Median :  -76.937  
# Mean   :   -6.638  
# 3rd Qu.:  259.388  
# Max.   : 3317.727 

png("box_plot.png", width = 1200, height = 1500, res = 120)
df_dati <- data.frame(Valore = elastic.pred - Y_test)
ggplot(df_dati, aes(y = s1)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", alpha = 0.7) +
  labs(
    title = "Distribuzione degli errori elastic.pred - Y_test",
    y = "errore"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 14)
  )
rm(df_dati)
dev.off()

Donato_tibble <- read_parquet("Donato_tibble.parquet")
Donato_data <- Donato_tibble %>%
  select(-legacyTrophyRoadHighScore)

sum(is.na(Donato_data))
Donato_data <- Donato_data %>%
  filter(starPoints >= 0)

# --- Definizione delle variabili da rimuovere inizialmente ---
variables_to_remove<- c(
  "currentLeagueRank",
  "lastLeagueRank",
  "bestLeagueRank",
  "currentLeagueTrophies",
  "tournamentCardsWon",
  "bestTrophies"
)

Donato_data_processed <- Donato_data %>%
  select(-all_of(variables_to_remove))

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
  if (var_name %in% colnames(Donato_data_processed)) {
    Donato_data_processed[[var_name]] <- factor(Donato_data_processed[[var_name]],
                                                  levels = levels_vec,
                                                  ordered = TRUE)
  } else {
    warning(paste0("Variabile '", var_name, "' non trovata in Donato_data_processed per la conversione a fattore."))
  }
}

# --- Applicazione di TUTTE le altre trasformazioni
Donato_data_processed <- Donato_data_processed %>%
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
# dopo aver rimosso 6 variabili ed aggiunto le 18 modificate il dataset ha
# 173 - 6 + 18 = 185 variabili

# --- 3. Rimozione delle variabili originali che sono state trasformate  ---
variables_to_replace_with_transformed <- c(
  "battleCount", "CardsLevel14", "CardsLevel15", "challengeCardsWon",
  "clanCardsCollected", "totalDonations", "donations", "expPoints", "losses",
  "threeCrownWins", "starPoints", "totalExpPoints", "tournamentBattleCount",
  "CardsEvo", "wins", "yearsSinceRegistration", "meanLevelCards",
  "meanLevelSupportCards"
)

Donato_data_processed <- Donato_data_processed %>%
  select(-any_of(variables_to_replace_with_transformed))
# dalle 173 variabili originali, 6 sono rimosse e 18 sostituite
# così Donato_data_processed ha 167 variabili

rm(factor_vars_and_levels)
rm(levels_vec, var_name)

variables_to_take = c(colnames(Donato_data_processed %>%
                                 select(where(is.numeric))),
                      'bestLeagueNumber')

cat("Il modello contiene ", length(variables_to_take), " variabili\n")
# Il modello contiene  33  variabili

Donato_data_processed <- Donato_data_processed %>%
  select(all_of(variables_to_take))

dim(Donato_data_processed) # 1   33

rm(variables_to_remove, variables_to_replace_with_transformed)

grande_df_Donato <- new_test_data_processed
grande_df_Donato[1,] <- Donato_data_processed
X_Donato <- model.matrix(trophies ~ . - bestLeagueNumber + poly(bestLeagueNumber, 3),
                       data = grande_df_Donato)[,-1] # Rimuovo l'intercetta
X_Donato <- X_Donato[1,]
rm(grande_df_Donato)
predict(cv.out.elastic, s = cv.out.elastic$lambda.1se, newx = X_Donato)
# 9961.95
Donato_data_processed$trophies
# 9000

rm(new_test_data_processed, new_train_data_processed, X, Y, grid,
   p.factor, cv.out.elastic, elastic.pred, MSE_test, MAE_test)
rm(card, variables_to_take)
