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
library(olsrr)   # per il Cp di Mallows
library(caret)   # per cross-validation
library(Metrics) # Per MAE e RMSE

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


# --- 4.5.1 Metriche di Adattamento e Significatività Complessiva ---

# Ottenere il summary del modello per R^2, R^2 aggiustato, F-statistic
summary_model <- summary(model_lm_log_trophies)

cat("--- Metriche di Adattamento Complessivo ---\n")
print(summary_model) # Stampa l'intero summary per avere tutti i dettagli

# Volendo estrarre i valori specifici per un commento più diretto:
r_squared <- summary_model$r.squared
adj_r_squared <- summary_model$adj.r.squared
f_statistic <- summary_model$fstatistic[1]
f_df1 <- summary_model$fstatistic[2]
f_df2 <- summary_model$fstatistic[3]
f_p_value <- pf(f_statistic, f_df1, f_df2, lower.tail = FALSE) # Calcola il p-value dell'F-statistic

cat("\n--- Valori riassuntivi ---\n")
cat("R-quadro (R^2):", round(r_squared, 4), "\n")
cat("R-quadro aggiustato (Adj. R^2):", round(adj_r_squared, 4), "\n")
cat("F-statistic:", round(f_statistic, 2), "\n")
cat("Gradi di libertà dell'F-statistic:", f_df1, "e", f_df2, "\n")
cat("P-value dell'F-statistic:", formatC(f_p_value, format = "e", digits = 10), "\n")

# --- Valori riassuntivi ---
# R-quadro (R^2): 0.9146 
# R-quadro aggiustato (Adj. R^2): 0.9119 
# F-statistic: 344.43 
# Gradi di libertà dell'F-statistic: 302 e 9717 
# P-value dell'F-statistic: < 2.2e-16

# per la tabella ANOVA delle variabilità
# variabilità totale:
sum((train_data_final_for_model$log_trophies-mean(train_data_final_for_model$log_trophies))^2)
# 7814.746
# chi-quadro con gradi di libertà n-1 = 1019 

# variabilità non spiegata dal modello
sum(model_lm_log_trophies$residuals^2)
# 667.660
# chi-quadro con gradi di libertà n-p-1 = f_df2 = 9717

# variabilità spiegata dal modello
sum((model_lm_log_trophies$fitted.values-mean(train_data_final_for_model$log_trophies))^2)
# 7147.086
# chi-quadro con gradi di libertà p = f_df1 = 302

# Media dei quadrati
sum((model_lm_log_trophies$fitted.values-mean(train_data_final_for_model$log_trophies))^2) / 302
# 23.66585
sum(model_lm_log_trophies$residuals^2) / 9717
# 0.06871054
# F statistic
(sum((model_lm_log_trophies$fitted.values-mean(train_data_final_for_model$log_trophies))^2) / 302)/
  (sum(model_lm_log_trophies$residuals^2) / 9717)
# 344.4282


rm(summary_model, r_squared, adj_r_squared, f_df1, f_df2, f_p_value, f_statistic)

# Tabella ANOVA (Tipo I - Sequenziale)
cat("\n--- Tabella ANOVA (Tipo I - Sequenziale) ---\n")
anova_table_type1 <- anova(model_lm_log_trophies)
print(anova_table_type1 %>%
        arrange(desc(`F value`)) %>%
        head(10))

library(xlsx)
write.xlsx(anova_table_type1 %>%
              arrange(desc(`F value`)) %>%
              head(100), "anova_variabili.xlsx", row.names = TRUE, col.names = TRUE)
detach("package:xlsx", unload = TRUE)

# Analysis of Variance Table
# 
# Response: log_trophies
#                               Df  Sum Sq Mean Sq    F value    Pr(>F)    
#   challengeMaxWins             1 1304.05 1304.05 18978.9074 < 2.2e-16 ***
#   role                         3  172.30   57.43   835.8660 < 2.2e-16 ***
#   currentFavouriteCard       114 2269.36   19.91   289.7174 < 2.2e-16 ***
#   Knight                       1  147.04  147.04  2139.9373 < 2.2e-16 ***
#   Archers                      1  268.38  268.38  3905.8996 < 2.2e-16 ***
#   Goblins                      1   36.31   36.31   528.4485 < 2.2e-16 ***
#   Giant                        1  509.21  509.21  7410.9862 < 2.2e-16 ***
#   P.E.K.K.A                    1    0.80    0.80    11.6644 0.0006397 ***
#   Minions                      1  134.22  134.22  1953.3809 < 2.2e-16 ***
#   Balloon                      1    0.37    0.37     5.3751 0.0204464 *  
#   Witch                        1    0.02    0.02     0.3068 0.5796653    
#   Barbarians                   1    4.57    4.57    66.5761 3.784e-16 ***
#   Golem                        1    0.27    0.27     3.8843 0.0487680 *  
#   Skeletons                    1    0.92    0.92    13.4045 0.0002523 ***
#   Valkyrie                     1    2.89    2.89    42.0236 9.456e-11 ***
#   `Skeleton Army`              1    6.41    6.41    93.2913 < 2.2e-16 ***
#   Bomber                       1    4.69    4.69    68.2887 < 2.2e-16 ***
#   Musketeer                    1  162.13  162.13  2359.5643 < 2.2e-16 ***
#   `Baby Dragon`                1   16.54   16.54   240.7448 < 2.2e-16 ***
#   Prince                       1    1.10    1.10    15.9660 6.497e-05 ***
#   Wizard                       1   13.29   13.29   193.4014 < 2.2e-16 ***
#   `Mini P.E.K.K.A`             1  110.03  110.03  1601.3613 < 2.2e-16 ***
#   `Spear Goblins`              1   40.76   40.76   593.1822 < 2.2e-16 ***
#   `Giant Skeleton`             1    1.03    1.03    15.0249 0.0001068 ***
#   `Hog Rider`                  1    0.16    0.16     2.3127 0.1283538    
#   `Minion Horde`               1    0.41    0.41     6.0116 0.0142300 *  
#   `Ice Wizard`                 1    0.00    0.00     0.0114 0.9149607    
#   `Royal Giant`                1    2.24    2.24    32.5436 1.200e-08 ***
#   Guards                       1    2.28    2.28    33.1400 8.835e-09 ***
#   Princess                     1    0.43    0.43     6.2374 0.0125243 *  
#   `Dark Prince`                1    0.00    0.00     0.0609 0.8050668    
#   `Three Musketeers`           1    0.45    0.45     6.5385 0.0105716 *  
#   `Lava Hound`                 1    0.00    0.00     0.0042 0.9481765    
#   `Ice Spirit`                 1    5.29    5.29    76.9350 < 2.2e-16 ***
#   `Fire Spirit`                1   10.71   10.71   155.9346 < 2.2e-16 ***
#   Miner                        1    0.48    0.48     7.0568 0.0079095 ** 
#   Sparky                       1    0.29    0.29     4.1661 0.0412674 *  
#   Bowler                       1    8.52    8.52   124.0093 < 2.2e-16 ***
#   Lumberjack                   1    2.31    2.31    33.5897 7.017e-09 ***
#   `Battle Ram`                 1    9.68    9.68   140.9116 < 2.2e-16 ***
#   `Inferno Dragon`             1    0.34    0.34     4.9040 0.0268175 *  
#   `Ice Golem`                  1    0.65    0.65     9.4016 0.0021740 ** 
#   `Mega Minion`                1    5.86    5.86    85.2864 < 2.2e-16 ***
#   `Dart Goblin`                1    0.01    0.01     0.1174 0.7318400    
#   `Goblin Gang`                1    0.31    0.31     4.5015 0.0338904 *  
#   `Electro Wizard`             1    2.54    2.54    36.9839 1.236e-09 ***
#   `Elite Barbarians`           1    0.25    0.25     3.6068 0.0575731 .  
#   Hunter                       1    0.02    0.02     0.3050 0.5808008    
#   Executioner                  1    0.94    0.94    13.7107 0.0002144 ***
#   Bandit                       1    2.61    2.61    38.0409 7.204e-10 ***
#   `Royal Recruits`             1    0.05    0.05     0.7151 0.3977657    
#   `Night Witch`                1    0.02    0.02     0.3551 0.5512729    
#   Bats                         1    0.59    0.59     8.5723 0.0034212 ** 
#   `Royal Ghost`                1    0.21    0.21     3.0410 0.0812185 .  
#   `Ram Rider`                  1    0.78    0.78    11.3253 0.0007675 ***
#   Zappies                      1    0.00    0.00     0.0088 0.9252916    
#   Rascals                      1    0.13    0.13     1.8893 0.1693123    
#   `Cannon Cart`                1    0.06    0.06     0.8746 0.3497168    
#   `Mega Knight`                1    7.85    7.85   114.2919 < 2.2e-16 ***
#   `Skeleton Barrel`            1    0.00    0.00     0.0105 0.9182292    
#   `Flying Machine`             1    0.01    0.01     0.0829 0.7733809    
#   `Wall Breakers`              1    0.06    0.06     0.8490 0.3568471    
#   `Royal Hogs`                 1    1.78    1.78    25.9422 3.584e-07 ***
#   `Goblin Giant`               1    0.27    0.27     3.9390 0.0472072 *  
#   Fisherman                    1    0.45    0.45     6.5654 0.0104132 *  
#   `Magic Archer`               1    0.75    0.75    10.8436 0.0009949 ***
#   `Electro Dragon`             1    0.04    0.04     0.6111 0.4343804    
#   Firecracker                  1   28.93   28.93   421.0532 < 2.2e-16 ***
#   `Mighty Miner`               1    1.18    1.18    17.1074 3.562e-05 ***
#   `Elixir Golem`               1    0.17    0.17     2.4139 0.1202980    
#   `Battle Healer`              1    0.35    0.35     5.1602 0.0231321 *  
#   `Skeleton King`              1    3.17    3.17    46.1500 1.160e-11 ***
#   `Archer Queen`               1    6.13    6.13    89.1806 < 2.2e-16 ***
#   `Golden Knight`              1    0.66    0.66     9.6586 0.0018901 ** 
#   Monk                         1    1.39    1.39    20.1586 7.210e-06 ***
#   `Skeleton Dragons`           1    3.29    3.29    47.8916 4.789e-12 ***
#   `Mother Witch`               1    1.12    1.12    16.2366 5.633e-05 ***
#   `Electro Spirit`             1    0.15    0.15     2.1369 0.1438262    
#   `Electro Giant`              1    0.79    0.79    11.4425 0.0007207 ***
#   Phoenix                      1    0.00    0.00     0.0462 0.8297864    
#   `Little Prince`              1    3.49    3.49    50.8488 1.069e-12 ***
#   `Goblin Demolisher`          1    0.04    0.04     0.6408 0.4234439    
#   `Goblin Machine`             1    0.51    0.51     7.4058 0.0065130 ** 
#   `Suspicious Bush`            1    0.03    0.03     0.4863 0.4856184    
#   Goblinstein                  1    0.21    0.21     3.0097 0.0828018 .  
#   `Rune Giant`                 1    0.08    0.08     1.1593 0.2816452    
#   Berserker                    1    0.05    0.05     0.7995 0.3712761    
#   `Boss Bandit`                1    5.37    5.37    78.2174 < 2.2e-16 ***
#   Cannon                       1    0.57    0.57     8.2330 0.0041224 ** 
#   `Goblin Hut`                 1   26.23   26.23   381.7449 < 2.2e-16 ***
#   Mortar                       1    0.45    0.45     6.5121 0.0107293 *  
#   `Inferno Tower`              1    0.97    0.97    14.1597 0.0001689 ***
#   `Bomb Tower`                 1    0.00    0.00     0.0154 0.9013861    
#   `Barbarian Hut`              1    0.98    0.98    14.2473 0.0001612 ***
#   Tesla                        1    7.28    7.28   105.9264 < 2.2e-16 ***
#   `Elixir Collector`           1    0.36    0.36     5.1904 0.0227332 *  
#   `X-Bow`                      1    2.07    2.07    30.1416 4.117e-08 ***
#   Tombstone                    1    6.89    6.89   100.3272 < 2.2e-16 ***
#   Furnace                      1    0.71    0.71    10.4015 0.0012633 ** 
#   `Goblin Cage`                1   19.29   19.29   280.7482 < 2.2e-16 ***
#   `Goblin Drill`               1    2.74    2.74    39.8054 2.928e-10 ***
#   Fireball                     1   31.38   31.38   456.6446 < 2.2e-16 ***
#   Arrows                       1   36.91   36.91   537.1278 < 2.2e-16 ***
#   Rage                         1    1.72    1.72    25.0231 5.764e-07 ***
#   Rocket                       1    0.34    0.34     5.0077 0.0252576 *  
#   `Goblin Barrel`              1    7.07    7.07   102.8502 < 2.2e-16 ***
#   Freeze                       1    1.29    1.29    18.7154 1.533e-05 ***
#   Mirror                       1    3.12    3.12    45.3609 1.732e-11 ***
#   Lightning                    1    0.55    0.55     8.0353 0.0045969 ** 
#   Zap                          1    5.05    5.05    73.5270 < 2.2e-16 ***
#   Poison                       1    2.74    2.74    39.8910 2.803e-10 ***
#   Graveyard                    1    5.81    5.81    84.4880 < 2.2e-16 ***
#   `The Log`                    1   18.86   18.86   274.5448 < 2.2e-16 ***
#   Tornado                      1    0.54    0.54     7.8705 0.0050348 ** 
#   Clone                        1    0.00    0.00     0.0617 0.8038213    
#   Earthquake                   1    3.00    3.00    43.7305 3.967e-11 ***
#   `Barbarian Barrel`           1    0.04    0.04     0.5099 0.4751996    
#   `Heal Spirit`                1    0.03    0.03     0.4048 0.5246434    
#   `Giant Snowball`             1    2.08    2.08    30.3171 3.762e-08 ***
#   `Royal Delivery`             1    3.08    3.08    44.8444 2.251e-11 ***
#   Void                         1    0.10    0.10     1.4704 0.2253172    
#   `Goblin Curse`               1    1.07    1.07    15.5345 8.158e-05 ***
#   `Tower Princess`             1    2.65    2.65    38.6322 5.327e-10 ***
#   Cannoneer                    1    0.02    0.02     0.3572 0.5500741    
#   `Dagger Duchess`             1    3.52    3.52    51.1671 9.096e-13 ***
#   `Royal Chef`                 1    6.56    6.56    95.4740 < 2.2e-16 ***
#   daysSinceRegistration        1   23.12   23.12   336.5396 < 2.2e-16 ***
#   is_new_player                1   39.15   39.15   569.8432 < 2.2e-16 ***
#   cardsOwned                   1 1105.49 1105.49 16089.0250 < 2.2e-16 ***
#   CardsLevel13                 1    0.27    0.27     3.9904 0.0457869 *  
#   CardsLevel12                 1    0.51    0.51     7.4332 0.0064145 ** 
#   CardsLevel11                 1    2.74    2.74    39.9289 2.749e-10 ***
#   CardsLevel10                 1   11.95   11.95   173.9721 < 2.2e-16 ***
#   SupportCardsLevel15          4    2.02    0.51     7.3609 6.477e-06 ***
#   SupportCardsLevel14          4    2.34    0.59     8.5260 7.285e-07 ***
#   SupportCardsLevel13          4    0.28    0.07     1.0063 0.4026609    
#   NumberSupportCards           3    8.07    2.69    39.1323 < 2.2e-16 ***
#   currentLeagueNumber          9    2.15    0.24     3.4821 0.0002629 ***
#   lastLeagueNumber             9    0.76    0.08     1.2367 0.2670198    
#   bestLeagueNumber             9    2.59    0.29     4.1950 1.977e-05 ***
#   log_CardsLevel14             1    0.75    0.75    10.9866 0.0009211 ***
#   log_CardsLevel15             1    0.04    0.04     0.6232 0.4298769    
#   log_challengeCardsWon        1    0.14    0.14     2.0292 0.1543322    
#   log_clanCardsCollected       1    0.04    0.04     0.6494 0.4203344    
#   log_totalDonations           1    0.08    0.08     1.1843 0.2765052    
#   log_donations                1    0.14    0.14     2.1093 0.1464415    
#   log_starPoints               1   46.08   46.08   670.6319 < 2.2e-16 ***
#   log_totalExpPoints           1  244.42  244.42  3557.1972 < 2.2e-16 ***
#   log_tournamentBattleCount    1    0.34    0.34     4.9282 0.0264448 *  
#   log_wins                     1   98.42   98.42  1432.4212 < 2.2e-16 ***
#   fourth_root_expPoints        1    6.69    6.69    97.3196 < 2.2e-16 ***
#   sqrt_CardsEvo                1    8.78    8.78   127.8127 < 2.2e-16 ***
#   Residuals                 9717  667.66    0.07                         
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

rm(anova_table_type1)




# --- Calcolo di Mallows' Cp utilizzando olsrr ---
# La funzione ols_mallows_cp confronta un modello con un "full model".
# Se il tuo 'model_lm_log_trophies' è il modello più completo che hai,
# lo useremo sia come 'model' che come 'fullmodel'.
# In questo caso, il Cp sarà uguale al numero di parametri (p) del modello.

model_lm_full<- lm(log_trophies ~ ., data = train_data_processed %>%
  mutate(log_trophies = log(trophies + 1)) %>%
  select(-trophies) %>%
  select(-any_of(c("tag", "name", "bestTrophies"))))

ols_cp_result <- ols_mallows_cp(model_lm_log_trophies, fullmodel = model_lm_full)

# Stampa il risultato del Cp
print(ols_cp_result) # 14223.6

# --- Calcolo di AIC e BIC (sempre utili) ---
aic_value <- AIC(model_lm_log_trophies)
cat("AIC (Akaike Information Criterion):", round(aic_value, 2), "\n")

bic_value <- BIC(model_lm_log_trophies)
cat("BIC (Bayesian Information Criterion):", round(bic_value, 2), "\n")

# Numero di parametri (p) per confronto con Cp
p_value <- length(coef(model_lm_log_trophies))
cat("Numero di parametri (p) nel modello:", p_value, "\n")

# Mallows' Cp: 303
# AIC (Akaike Information Criterion): 1903.77 
# BIC (Bayesian Information Criterion): 4096.32 
# Numero di parametri (p) nel modello: 303 

# --- Calcolo di AIC e BIC per il Modello Full ---
aic_full <- AIC(model_lm_full)
cat("AIC (Akaike Information Criterion) per model_lm_full:", round(aic_full, 2), "\n")

bic_full <- BIC(model_lm_full)
cat("BIC (Bayesian Information Criterion) per model_lm_full:", round(bic_full, 2), "\n")

# Numero di parametri (p) nel modello full
p_full <- length(coef(model_lm_full))
cat("Numero di parametri (p) nel modello full:", p_full, "\n")
# AIC (Akaike Information Criterion) per model_lm_full: -6995.88 
# BIC (Bayesian Information Criterion) per model_lm_full: -4745.63 
# Numero di parametri (p) nel modello full: 311 

rm(aic_value, aic_full, bic_value, bic_full, ols_cp_result, cp_value, p_value, p_full)
rm(model_lm_full, model_lm_log_trophies)


##-------------------------------------------------------------------------------
# MSE senza log
##-------------------------------------------------------------------------------
# dopo aver eseguito le prima 60 righe per avere il modello
# model_lm_log_trophies <- lm(log_trophies ~ ., data = train_data_final_for_model)

predictions_scale <- exp(model_lm_log_trophies$fitted.values)-1

#    Assicurati che non ci siano previsioni negative dopo la ritrasformazione
#    (anche se improbabile per i trofei, è una buona pratica)
length(predictions_scale[predictions_scale < 0]) # 0

#    Calcola l'Errore Quadratico Medio (MSE) sulla scala originale
#    Y.test.original_trophies deve contenere i valori reali e NON trasformati di 'trophies' per il set di test.
origin_value_scale <- exp(train_data_final_for_model$log_trophies)-1
length(origin_value_scale[origin_value_scale < 0]) # 0

mse_original_scale_ols <- mean((predictions_scale - origin_value_scale)^2)

cat("MSE del modello OLS sulla scala originale dei trofei:", mse_original_scale_ols, "\n")
# MSE del modello OLS sulla scala originale dei trofei: 807877.8 
sqrt(mse_original_scale_ols)
rm(predictions_scale, origin_value_scale, mse_original_scale_ols)


# --- Impostazione del controllo per la cross-validation ---
# --- Cross-Validation con caret::train() ---

# 1. Definisci la formula del modello
# Usiamo la notazione y ~ . per includere tutte le altre variabili come predittori.
# `log_trophies` deve essere la variabile risposta.
formula_model <- log_trophies ~ .

# 2. Imposta il controllo per la cross-validation
set.seed(123) # Per riproducibilità
train_control <- trainControl(
  method = "cv", # Metodo: cross-validation
  number = 10,   # Numero di folds (10-fold CV)
  verboseIter = TRUE # Mostra il progresso durante l'esecuzione
)

# 3. Esegui la cross-validation
# Il metodo "lm" si riferisce ai modelli lineari standard (come la funzione lm() in R).
# La funzione train() addestrerà il modello su diversi folds e raccoglierà le metriche.
message("Inizio della Cross-Validation con caret::train(). Questo potrebbe richiedere tempo...")
model_cv <- train(
  formula_model,
  data = train_data_final_for_model,
  method = "lm", # Specifica che stai addestrando un modello lineare
  trControl = train_control
)

# --- Visualizzazione dei Risultati della Cross-Validation ---
message("\n--- Risultati della Cross-Validation (da caret::train) ---")
print(model_cv)

# Puoi accedere direttamente alle metriche medie calcolate da caret:
cat("\nRMSE medio (su 10 folds):", round(model_cv$results$RMSE, 4), "\n")
cat("R^2 medio (su 10 folds):", round(model_cv$results$Rsquared, 4), "\n")
cat("MAE medio (su 10 folds):", round(model_cv$results$MAE, 4), "\n")

# Per confronto, ricalcoliamo le metriche del modello addestrato su tutto il training set originale
# Darà un'idea del "training error" vs il "test error" stimato dalla CV.
predictions_on_train <- model_lm_log_trophies$fitted.values

# Per RMSE e MAE, useremo le funzioni del pacchetto 'Metrics'
rmse_train <- Metrics::rmse(actual = train_data_final_for_model$log_trophies, predicted = predictions_on_train)
mae_train <- Metrics::mae(actual = train_data_final_for_model$log_trophies, predicted = predictions_on_train)

cat("\n--- Confronto con il Modello addestrato sull'intero Training Set ---\n")
cat("RMSE sul Training Set (non cross-validato):", round(rmse_train, 4), "\n")
cat("MAE sul Training Set (non cross-validato):", round(mae_train, 4), "\n")

# nella cross validatio per stimare l'errore di predizione:
# RMSE medio (su 10 folds): 0.269 
# R^2 medio (su 10 folds): 0.9049 
# MAE medio (su 10 folds): 0.1483 
# Confronto con il Modello addestrato sull'intero Training Set
# RMSE sul Training Set (non cross-validato): 0.2581 
# MAE sul Training Set (non cross-validato): 0.1428 
# in questo caso stiamo considerando lerrore di test che in generale sottostiama
# anche di molto l'errore di validazione


rm(model_cv, model_lm_full, model_lm_log_trophies, train_control)
rm(train_data_final_for_model, formula_model, mae_train, predictions_on_train)
rm(rmse_train)
rm(output_dir_diagnostics, dependent_var)



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

# # Applica la rimozione di TUTTE le variabili concordate
# train_data_final_for_model <- train_data_transformed_response %>%
#   select(-any_of(all_vars_to_remove))
# rm(all_vars_to_remove)
# rm(train_data_transformed_response)
# 
# set.seed(123)
# # Per riproducibilità
# # Creazione dei folds
# folds <- createFolds(train_data_final_for_model$log_trophies, k = 10, list = TRUE, returnTrain = FALSE)
# # Liste per salvare le metriche per ogni fold
# rmse_folds <- numeric(10)
# mae_folds <- numeric(10)
# 
# X <- model.matrix(log_trophies ~ ., data = train_data_final_for_model)[,-1]
# Y <- train_data_final_for_model$log_trophies
# # Ciclo sui folds
# for (i in 1:10) {
#   # Divisione in training e test set per il fold corrente
#   test_indices <- folds[[i]]
#   X_train_fold <- X[-test_indices, ]
#   Y_train_fold <- Y[-test_indices]
#   test_fold <- train_data_final_for_model[test_indices, ]
#   X_test_fold <- X[test_indices, ]
#   Y_test_fold <- Y[test_indices]
#   
#   # Addestramento del modello sul train_fold
#   model_fold <- lm(Y_train_fold ~ X_train_fold)
#   cat("\n", length(coef(model_fold)),"\n")
#   cat(dim(cbind(1, X_test_fold)),"\n")
#   predictions <- cbind(1, X_test_fold) %*% coef(model_fold)
#   cat(length(predictions),"\n")
#   cat(length(Y_test_fold),"\n")
#   # Calcolo RMSE e MAE per il fold corrente
#   rmse_folds[i] <- mean((Y_test_fold - predictions)^2, na.rm=T)
#   mae_folds[i] <- mae(actual = Y_test_fold, predicted = predictions)
# }
# 
# # --- Risultati della Cross-Validation ---
# cat("\n--- Risultati della 10-Fold Cross-Validation ---\n")
# cat("RMSE per ogni fold:", round(rmse_folds, 4), "\n")
# cat("MAE per ogni fold:", round(mae_folds, 4), "\n")
# cat("RMSE medio (su 10 folds):", round(mean(rmse_folds), 4), "\n")
# cat("MAE medio (su 10 folds):", round(mean(mae_folds), 4), "\n")
# 
# # Confronto con il training set (del modello finale su dati OHE)
# model_lm <- lm(log_trophies ~ ., data = train_data_final_for_model)
# predictions_on_train <- predict(model_lm, newdata = train_data_final_for_model)
# rmse_train_ohe <- rmse(actual = train_data_final_for_model$log_trophies, predicted = predictions_on_train)
# mae_train_ohe <- mae(actual = train_data_final_for_model$log_trophies, predicted = predictions_on_train)
# cat("RMSE sul Training Set (non cross-validato, dopo OHE):", round(rmse_train_ohe, 4), "\n")
# cat("MAE sul Training Set (non cross-validato, dopo OHE):", round(mae_train_ohe, 4), "\n")


rm(folds, rmse_folds, mae_folds, X, Y, X_train_fold, Y_train_fold, Y_test_fold,
   model_fold, predictions, model_lm,predictions_on_train, train_control,
   rmse_train_ohe, mae_train_ohe, i, test_indices, X_test_fold, test_fold)
