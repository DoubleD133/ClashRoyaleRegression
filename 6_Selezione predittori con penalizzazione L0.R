# Regressione multivariata penalizzata e selezione automatizzata
# cerchiamo di inferire sul numero di trofei di un giocatore

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

# --- Definizione delle variabili da rimuovere inizialmente ---
variables_to_remove<- c(
  "currentLeagueRank",
  "lastLeagueRank",
  "bestLeagueRank",
  "currentLeagueTrophies",
  "tournamentCardsWon",
  "bestTrophies"
)
# rispetto train_data_processed non rimuoviamo a priori le seguenti:
# "lastLeagueTrophies",
# "bestLeagueTrophies",
# "donationsReceived",
# "warDayWins"
# rimuoverle sarà una scelta dei metodi penalizzati se sarà opportuno

# manualmente andiremo dopo a rimuovere in più la variabile
# "expLevel" perchè sinonimo discretizzato di "totalExpPoints"

# Applichiamo queste rimozioni al dataset di training
train_data <- read_parquet("player_data_train.parquet")
new_train_data_processed <- train_data %>%
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
  if (var_name %in% colnames(new_train_data_processed)) {
    new_train_data_processed[[var_name]] <- factor(new_train_data_processed[[var_name]],
                                                   levels = levels_vec,
                                                   ordered = TRUE)
  } else {
    warning(paste0("Variabile '", var_name, "' non trovata in new_train_data_processed per la conversione a fattore."))
  }
}
# levels(new_train_data_processed$bestLeagueNumber)
# levels(new_train_data_processed$currentLeagueNumber)
# levels(new_train_data_processed$lastLeagueNumber)
# levels(new_train_data_processed$NumberSupportCards)
# levels(new_train_data_processed$SupportCardsLevel13)
# levels(new_train_data_processed$SupportCardsLevel14)
# levels(new_train_data_processed$SupportCardsLevel15)

# --- Applicazione di TUTTE le altre trasformazioni (log, radice, quadrato) ---
# Modificato per escludere le variabili ora fattoriali dalle trasformazioni numeriche.
new_train_data_processed <- new_train_data_processed %>%
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

new_train_data_processed <- new_train_data_processed %>%
  select(-any_of(variables_to_replace_with_transformed))
# dalle 173 variabili originali, 6 sono rimosse e 18 sostituite
# così new_train_data_processed ha 167 variabili

rm(factor_vars_and_levels)
rm(levels_vec, var_name)

length(colnames(new_train_data_processed %>%
                  select(where(is.numeric))))

# prendiamo ora soltanto le 32 variabili numeriche ed un paio di fattoriali che
# sono risultate parecchio significative
# come fattoriali nelle precedenti analisi è risultata significativa bestLeagueNumber
# gestita a contrasti polinomiali ortogonali.
# in più aggiungerei anche 'Hog Rider', 'Elixir Golem' e 'Mega Knight' come variabili fattoriali.
# Quindi avremo 32 variabili numeriche, 3 variabili fattoriali binarie ed una 
# variabile fattoriale ordinata con 10 livelli.

variables_to_take = c(colnames(new_train_data_processed %>%
                                 select(where(is.numeric))),
                      'bestLeagueNumber',
                      'Hog Rider', 'Elixir Golem', 'Mega Knight')

cat("Il modello contiene ", length(variables_to_take), " variabili\n")
# Il modello contiene  36  variabili

new_train_data_processed <- new_train_data_processed %>%
  select(all_of(variables_to_take))

# rimuoviamo outlaier e valori influenti
new_train_data_processed <- new_train_data_processed[-c(146, 1380, 2607, 7659,
                                                        9158, 7411, 3815, 5412,
                                                        8604, 9529),]
dim(new_train_data_processed) # 10010    36

write_parquet(new_train_data_processed, "player_data_new_processed.parquet")

rm(variables_to_remove, variables_to_replace_with_transformed, variables_to_take)


new_train_data_processed <- read_parquet("player_data_new_processed.parquet")
# manualmente andiamo a rimuovere in più di train_data_processed la variabile
# "expLevel" perchè sinonimo discretizzato di "totalExpPoints"
new_train_data_processed <- new_train_data_processed %>%
  select(-"expLevel")
dim(new_train_data_processed) # 10010    35

#-----------------------------------------
# Scatter plots
#-----------------------------------------
#par(mex=0.5)
#pairs(new_train_data_processed, gap=0, cex.labels=0.9)


##-----------------------------------------
## Modello lineare multivariato
#-----------------------------------------
model.m <- lm(trophies ~ ., data = new_train_data_processed)
summary(model.m)

#-----------------------------------------
# Diagnostic plots
#-----------------------------------------
par(mfrow=c(2,2))
plot(model.m)

# Histogram of residuals
par(mfrow=c(1,1))
hist(model.m$residuals,col="#CCCCFF", xlab="residuals", prob=TRUE, ylab="Frequency",
     main = "Histogram", ylim = c(0, 1.2*10^-3))
lines(density(model.m$residuals), col="darkblue", lwd=2)

#-----------------------------------------
# Specificazione del modello
#-----------------------------------------
## Verificare che l'errore medio non sia significativamente diverso da zero
residuals <- residuals(model.m)
t.test(residuals)

## Normalità
ad.test(residuals)

## Eteroscedasticità
fit <- formula(model.m)
testbp <- bptest(fit,data=new_train_data_processed)
testbp

# Autocorrelazione
dw <- dwtest(fit,data=new_train_data_processed)
dw

# Outliers
outlierTest(model.m)

qqPlot(model.m, simulate = TRUE)

new.new_train_data_processed <- new_train_data_processed[-c(2169, 5996),]
model.new <- lm(trophies ~., data = new.new_train_data_processed)
summary(model.new)


rm(residuals, fit, testbp, dw, new.new_train_data_processed, model.new)

#-----------------------------------------
# Subset Selection
#-----------------------------------------
# Le funzioni regsubsets() e step() in R vengono entrambe usate per la selezione
# delle variabili in modelli di regressione, ma lo fanno in modo diverso e
# con criteri diversi.
## step() esegue la selezione automatica delle variabili in un modello lm() o glm() usando
# il criterio dell’AIC (Akaike Information Criterion).
# Modalità:
# direction = "backward" -> parte dal modello completo, rimuove variabili una alla volta
# direction = "forward" -> parte dal modello nullo, aggiunge variabili utili una alla volta
# direction = "both" -> combina forward e backward (stepwise) - aggiunge o rimuove
# variabili in ogni passaggio.
# Obiettivo:
# Minimizzare AIC -> miglior compromesso tra bontà di adattamento e semplicità del modello.
## regsubsets() trova tutti i sottinsiemi di variabili possibili (entro un limite) e
# seleziona il migliore per ogni numero di variabili.
# Metodi supportati:
# "exhaustive": tutte le combinazioni possibili (computazionalmente pesante!)
# "backward": parte dal modello completo e rimuove
# "forward": parte dal modello nullo e aggiunge
# Obiettivo:
# Restituisce il miglior modello con 1, 2, 3, ..., p predittori, in base a criteri come:
# adjr2 (R² aggiustato)
# Cp di Mallows
# BIC
#-----------------------------------------

# La funzione regsubsets() (parte della libreria leaps) esegue la selezione del miglior
# sottoinsieme di variabili identificando il miglior modello che contiene un dato
# numero di predittori, dove il "migliore" è quantificato utilizzando l'RSS (Residual
# Sum of Squares). Il comando summary() restituisce il miglior insieme di variabili per
# ogni dimensione del modello.
model.full <- regsubsets(trophies ~ ., data = new_train_data_processed)
summary(model.full)

# Un asterisco indica che una determinata variabile è inclusa nel modello corrispondente.
# Ad esempio, questo output indica che il miglior modello contiene solo "cardsOwned" e "bestLeagueNumber.L".
# Per impostazione predefinita, regsubsets() riporta solo i risultati fino al miglior modello
# con 8 variabili. Aumentiamo questo numero a 42, cioè includendo tutte le variabili.
model.full <- regsubsets(trophies ~ ., data = new_train_data_processed, nvmax = 42)
model.summary <- summary(model.full)

# Assicurati che model.summary sia già stato creato: model.summary <- summary(model.full)
options(max.print = 5000)
sink("summary_model_full_which.txt")
print(model.summary$which)
sink()

names(model.summary)

model.summary$rsq

# Vediamo che la statistica R² aumenta dal 90%, quando nel modello è inclusa solo una
# variabile, ad oltre il 96%, quando tutte le variabili sono incluse. Come previsto,
# la statistica R² aumenta in modo monotono man mano che vengono incluse più variabili.
model.summary$rss

# Vediamo che la statistica RSS diminuisce da 5.861.066.122, quando nel modello è inclusa solo
# una variabile, a 2.224.204.390, quando tutte le variabili sono incluse.

# Allo stesso tempo, la stessa cosa non accade necessariamente per il coefficiente di R^2
# aggiustato o per altri criteri statistici come BIC o Cp, che sono, quindi, più adatti
# per selezionare il miglior modello.
model.summary$adjr2

model.summary$cp

model.summary$bic

# Possiamo usare coef() per vedere le stime dei coefficienti associate a questo modello.
coef(model.full,10)
#        (Intercept)         cardsOwned       CardsLevel10 
#         460.872752          58.472605           6.585795 
#   log_CardsLevel14   log_CardsLevel15 log_totalDonations 
#         320.623604         347.939144         -55.828422 
# log_threeCrownWins     log_starPoints           log_wins 
#        -386.984182         -81.988862         426.565230 
# bestLeagueNumber.L bestLeagueNumber.Q 
#         624.829411        -442.240555

#-----------------------------------------
# Plot RSS, adjusted R^2, Cp, and BIC
# Tracciare tutti i modelli ci aiuterà a decidere quale modello selezionare.
par(mfrow=c(2,2))

plot(model.summary$rss ,xlab="Number of Variables ",ylab="RSS",type="l")
plot(model.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
k = which.max(model.summary$adjr2)
points(k,model.summary$adjr2[k], col="red",cex=2,pch=20)
cat("Numero di variabili che minimizza R^2 aggiustato: ", k, "\n")
cat("Minimo dell' R^2 aggiustato: ", model.summary$adjr2[k], "\n")
cat("MSE: ", model.summary$rss[k]/dim(X)[1], "\n")

plot(model.summary$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
k = which.min(model.summary$cp)
points(k,model.summary$cp[k],col="red",cex=2,pch=20)
cat("Numero di variabili che minimizza il cp: ", k, "\n")
cat("Minimo di cp: ", model.summary$cp[k], "\n")
cat("MSE: ", model.summary$rss[k]/dim(X)[1], "\n")

plot(model.summary$bic ,xlab="Number of Variables ",ylab="BIC",type='l')
k = which.min(model.summary$bic)
points(k,model.summary$bic [k],col="red",cex=2,pch=20)
cat("Numero di variabili che minimizza bic: ", k, "\n")
cat("Minimo di bic: ", model.summary$bic[k], "\n")
cat("MSE: ", model.summary$rss[k]/dim(X)[1], "\n")

# R2 aggiustato e cp posizionano il minimo a 32 e 31 rispettivamente
# bic penalizza di più mettendo il minimo a 25


# Numero di variabili che minimizza R^2 aggiustato:  32 
# Minimo dell' R^2 aggiustato:  0.9625793 
# MSE:  222061.3 
# Numero di variabili che minimizza il cp:  31 
# Minimo di cp:  26.63481 
# MSE:  222102 
# Numero di variabili che minimizza bic:  25 
# Minimo di bic:  -32648.37 
# MSE:  222780.1 

# Il miglior modello può essere scelto massimizzando R2 aggiustato, oppure
# (e preferibilmente) minimizzando C_p di Mallows o BIC (Bayesian Information Criterion).
# Diversi criteri di selezione potrebbero portare a identificare modelli migliori diversi.
#-----------------------------------------

#-----------------------------------------
# Forward and Backward Stepwise Selection
#-----------------------------------------
model.fwd = regsubsets(trophies ~ ., data = new_train_data_processed,nvmax=43, method ="forward")
model.bwd = regsubsets(trophies ~ ., data = new_train_data_processed,nvmax=43,method ="backward")
summary(model.fwd)
summary(model.bwd)

#-----------------------------------------
# usiamo ora step
# La funzione step() sceglie un modello in base all'AIC utilizzando un algoritmo di
# selezione stepwise.
# La modalità della ricerca stepwise (denotata come direction) può assumere uno dei
# seguenti valori: "both", "backward" o "forward", con "both" come impostazione predefinita
model.backward <- step(model.m, direction = "backward")
model.forward <- step(model.m, direction = "forward")
model.stepwise <- step(model.m, direction = "both")
summary(model.backward) # 39 variabili nel modello greedy migliore
summary(model.forward) # 43 variabili nel modello greedy migliore
summary(model.stepwise) # 39 variabili nel modello greedy migliore

# > length(model.backward$coefficients)-1 (-1 per l'intercetta)
# [1] 36
# > length(model.forward$coefficients)-1
# [1] 42
# > length(model.stepwise$coefficients)-1
# [1] 36

sink("summary_model_backward.txt")
summary(model.backward)
sink()

sink("summary_model_forward.txt")
summary(model.forward)
sink()

sink("summary_model_stepwise.txt")
summary(model.stepwise)
sink()


# Confronto AIC
AIC(model.backward, model.forward, model.stepwise)
#                df      AIC
# model.backward 38 151723.7
# model.forward  44 151731.5
# model.stepwise 38 151723.7
# La funzione step() usa AIC come criterio di selezione:
# un AIC più basso indica un modello migliore (a parità di interpretabilità).
# step() non garantisce il miglior modello assoluto, ma una buona soluzione "greedy".
# Meglio valutare anche su dati di test o usare metodi penalizzati (lasso, ridge) per confronto.


#-----------------------------------------
# usiamo ora una sub set selection basata su Cp: leaps
y <- new_train_data_processed$trophies
x <- model.matrix(model.m)[,-1]
leapcp <- leaps(x,y, method="Cp")
Cpplot(leapcp)
k = which.min(leapcp$cp)
cat("Numero di variabili nel metodo migliore: ", leapcp$size[k], "\n")
points(leapcp$size[k],leapcp$cp[k],col="red",cex=2,pch=20)
# Errore in leaps(x, y, method = "Cp") : 
#   leaps does not allow more than 31 variables; use regsubsets()

# fare vif sul model.m (lm), rimuovere il termine a vif maggiore
# e fare di nuovo lm ottenendo model.mr
vif(model.m)
new.new_train_data_processed = new_train_data_processed  %>%
           select(-any_of(c('sq_log_battleCount')))
model.rm = lm(trophies ~ ., data = new.new_train_data_processed)
summary(model.rm)
vif(model.rm)

# creo un modello più semplice ma con prestazioni migliori/simili a model.rm
model.red = step(model.rm)
anova(model.m,model.red)
#   Res.Df        RSS Df Sum of Sq      F    Pr(>F)    
# 1   9967 2224204390                           
# 2   9974 2226107615 -7  -1903225 1.2184 0.2884
# p-value >= 0.05 => meglio usare il modello semplice model.red

rm(new.new_train_data_processed, model.rm)
rm(model.m, model.fwd, model.bwd, model.backward, model.forward, model.stepwise, k)
rm(model.full, model.summary)
rm(model.red, new.new_train_data_processed)
rm(output_file)
