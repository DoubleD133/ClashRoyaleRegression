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
library(glmnet)  # Per l'omonima funzione che serve per ridge, lass ed elastic net



new_train_data_processed <- read_parquet("player_data_new_processed.parquet")

new_train_data_processed <- new_train_data_processed %>%
  select(-any_of(c('Hog Rider', 'Elixir Golem', 'Mega Knight')))

# Contiene solo le 32 variabili numeriche ed una fattoriale che è risultata
# significativa in train_data_processed: bestLeagueNumber gestita a contrasti
# polinomiali ortogonali.
# Quindi avremo 32 variabili numeriche ed una variabile fattoriale ordinata con
# 10 livelli.
# Inoltre da train_data_processed sono stati rimossi outlaier e valori influenti
# osservazioni: 146, 1380, 2607, 7659, 9158, 7411, 3815, 5412, 8604, 9529
dim(new_train_data_processed) # 10010    33
dim(na.omit(new_train_data_processed)) # 10010    33

#-----------------------------------------
# Costruisco il modello
#-----------------------------------------
# model.matrix() funzione utilizzata per creare la matrice di disegno
# gestiamo la fattoriale bestLeagueNumber: è ordinata quindi andrebbe gestita
# con contrasti polinomiali fino all'ordine 9. Dato che questi modelli penalizzati
# gestiscono con difficoltà le variabili fattoriali e dato che nei modelli già
# analizzati i contrasti di ordine superiore a 3 non sono risultati significativi
# decidiamo di gestire bestLeagueNumber con contrasti fino all'ordine 3
X <- model.matrix(trophies ~ . - bestLeagueNumber + poly(bestLeagueNumber, 3),
                  data = new_train_data_processed)[,-1] # Rimuovo l'intercetta
Y <- new_train_data_processed$trophies
head(X, 3)
# Visualizza i nomi delle colonne di X per capire come model.matrix ha espanso le variabili
# Aiuterà a verificare i nomi da usare per penalty.factor
dim(X) # 10010    34 = 33 - 1 - 1 + 3
head(colnames(X), 34) # Puoi uncommentare per vedere i nomi
var_factor <- c("poly(bestLeagueNumber, 3)1", "poly(bestLeagueNumber, 3)2",
                "poly(bestLeagueNumber, 3)3" )

# Crea la griglia di valori lambda
grid <- exp(seq(-5, 20,length = 100)) # i valori decrescono con l'indice
grid[1:5]
grid[96:100]

# -----------------------------------------
# Gestione di penalty.factor per le variabili fattoriali
# -----------------------------------------

# Inizializza penalty.factor con 1 (tutti i predittori penalizzati di default)
p.factor <- rep(1, ncol(X))

factor_indices <- c()
for(nome in var_factor) {
  factor_indices <- c(factor_indices, which(colnames(X) == nome))
}
factor_indices # 32 33 34
length(factor_indices) # 3
colnames(X)[factor_indices]
# "poly(bestLeagueNumber, 3)1" "poly(bestLeagueNumber, 3)2" "poly(bestLeagueNumber, 3)3"

# Imposta penalty.factor a 0 per queste colonne (non penalizzate)
p.factor[factor_indices] <- 0
p.factor
# 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0

# -----------------------------------------
# Costruisco il modello Ridge con penalty.factor
# -----------------------------------------

ridge.mod <- glmnet(X, Y, alpha=0, lambda = grid, penalty.factor = p.factor)

# Per impostazione predefinita, la funzione glmnet() standardizza le variabili in modo che
# siano sulla stessa scala.
# Per disattivare questa impostazione, è possibile usare l'argomento standardize = FALSE.
# Ad ogni valore di lambda è associato un vettore di coefficienti della regressione ridge,
# memorizzato in una matrice accessibile tramite la funzione coef().
# In questo caso, si tratta di una matrice 35×100, con 35 righe (una per ciascun
# predittore, più un'intercetta) e 100 colonne (una per ciascun valore di lambda).
dim(coef(ridge.mod)) # 35 100

# Plot dei coefficeinti: viene usato per visualizzare l'evoluzione dei coefficienti in un
# modello di regressione ridge (creato con glmnet()), al variare del parametro di
# regolarizzazione lambda.
plot(ridge.mod, xvar="lambda",label=TRUE) # Ad esempio

# Salva come PNG
# width e height sono in pixel, res è la risoluzione (dpi - dots per inch)
# png("ridge_coefficients_plot_zoom3.png", width = 1200, height = 2800, res = 120)
plot(ridge.mod, xvar="lambda", label=TRUE, ylim = c(-70, 160))
# dev.off()

# Si nota un'azione di gruppo in cui variabili tra loro correlate vengono rimpicciolite
# assieme (ad esempio le colonne di indici 15, 24 e 14)
colnames(X)[c(14, 15, 24)]
# "log_CardsLevel14" "log_CardsLevel15" "log_wins"

# Asse x (orizzontale): mostra i valori di log(lambda) (non lambda diretto, ma il suo
# logaritmo), ordinati da sinistra (lambda grande -> più penalizzazione) a destra
# (lambda piccolo -> meno penalizzazione).
# Asse y (verticale): valori dei coefficienti stimati per ciascun predittore.
# Linee colorate: ogni linea rappresenta come cambia il coefficiente di un predittore al
# variare di lambda.
# Etichette (label = TRUE) mostra i nomi delle variabili all’estremità delle curve, per
# identificare visivamente quali linee corrispondono a quali predittori.
# Interpretazione:
# A valori elevati di lambda (a sinistra): i coefficienti sono molto vicini a zero
# -> forte regolarizzazione.
# A valori piccoli di lambda (a destra): i coefficienti diventano più grandi (meno
# penalizzazione).
# I coefficienti si riducono verso 0 -> questo è il comportamento tipico della regressione
# ridge, che "shrinka" i coefficienti ma non li annulla mai completamente (diversamente da
# LASSO). La complessità del modello diminuisce con valori più alti di lambda.

#-----------------------------------------
# Mostrare l'effetto del parametro di regolarizzazione lambda sui coefficienti stimati
# 
# glmnet ordina la griglia che abbiamo fornito in modo decrescente
# infatti ridge.mod$lambda[k] = sort(grid, decreasing = T)[k]
# così se k aumenta allora ridge.mod$lambda[k] diminuisce e quindi la penalizzazione
# diminuisce, di conseguenza la norma dei coefficienti aumenta
# esempio pratico k=30/60

# k piccolo => lambda grande => i coefficienti fanno salire subito la loss function
#           => norma 2 dei coefficienti piccola
ridge.mod$lambda[30] # 320221.3 = sort(grid, decreasing = T)[30]
# Calcola la norma L2 del vettore dei coiefficienti soggetti a penalizzazione
sqrt(sum(coef(ridge.mod)[-c(1, (factor_indices+1)),30]^2))
# norm(as.matrix(coef(ridge.mod)[-c(1, (factor_indices+1)),30]),'2')
# 15.24878

# k grande => lambda piccolo => i coefficienti fanno salire poco la loss function
#          => norma 2 dei coefficienti grande
# Diversamente, visualizza il 60-esimo valore di lambda
ridge.mod$lambda[60] # 164.1877 = sort(grid, decreasing = T)[60]
# Calcola la norma L2 del vettore dei coiefficienti soggetti a penalizzazione
sqrt(sum(coef(ridge.mod)[-c(1, (factor_indices+1)),60]^2))
# 391.196

norme2 <- c()
for(i in 1:100) {
  norme2 <- c(norme2, norm(as.matrix(coef(ridge.mod)[-c(1, (factor_indices+1)),i]),'2'))
}

# Disegna il grafico
df <- data.frame(log_lambda = log(ridge.mod$lambda), norma2_coefficienti = norme2)
#ggsave("Norma2vslambda.png", plot = 
         ggplot(df, aes(x = log_lambda, y = norma2_coefficienti)) +
         geom_line(color = "steelblue", linewidth = 1.2) +
         labs(title = "Norma 2 del vettore dei coefficienti vs \u03BB",
              x = "log(\u03BB)",
              y = "Norma 2 del vettore dei coefficienti") +
         theme(
           panel.background = element_rect(fill = "white", colour = NA),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           plot.background = element_rect(fill = "white", colour = NA),
           
           plot.title = element_text(size = 18, face = "bold"),   # Titolo del grafico più grande
           axis.title = element_text(size = 14, face = "bold"),   # Titoli degli assi più grandi
           axis.text = element_text(size = 9),                   # Etichette numeriche/testuali sugli assi più grandi
         )#, width = 10, height = 8)
rm(df)

#-----------------------------------------
# Usa funzione predict() per ottenere previsioni o coefficienti da un modello già allenato.
# Ad esempio, possiamo ottenere i coefficienti della regressione ridge per un nuovo valore
# di lambda, ad esempio 50.
coef_lambda_50 <- predict(ridge.mod, s = 50, type = "coefficients")
# s = 50: specifica il valore di lambda per cui vogliamo ottenere i coefficienti. Qui usi
# lambda = 50.
# type = "coefficients": indica che vogliamo ottenere i coefficienti stimati (e non le
# predizioni sul target).
# Mostra i coefficienti (inclusa l'intercetta)
print(coef_lambda_50[1:35, ])
norm(as.matrix(coef_lambda_50[-c(1, (factor_indices+1)),]),'2') # 570.0233

# Ad esempio, ogni expLevel in più aumentano i trofei stimati di 7.25 unità, tenendo fisse le
# altre variabili; ogni giorno in più riduce i trofri stimati di 0.0043 unità.
# Con questo comando ottieni i valori dei coefficienti del modello ridge calcolati con
# lambda = 50,
# cioè i pesi assegnati a ciascuna variabile predittiva nel modello regolarizzato con quel
# valore specifico di lambda. Il segno (+ o -) indica la direzione dell’associazione
# tra variabile esplicativa e variabile dipendente (trofei).

#-----------------------------------------
# Training and validation set
#-----------------------------------------
# Dividi in training (50%) e testing (50%)
set.seed(123) # per riproducibilità
train <- sample(1: nrow(X), nrow(X)/2) # prendi metà degli indici
validation <- (- train ) # il resto sono per il test
X.train <- X[train,]
Y.train <- Y[train]
dim(X.train) # 5005   37

X.validation <- X[validation,]
Y.validation <- Y[validation]
dim(X.validation) # 5005   37

#Adattiamo un modello di regressione ridge sul set di training (uso una griglia di lambda)
ridge.mod.train <- glmnet(X.train, Y.train, alpha = 0,
                          lambda = grid, thresh = 1e-12, penalty.factor = p.factor)
# thresh = 1e-12 è la soglia di convergenza molto precisa per l’algoritmo
# Facciamo la previsione sul set di validazione X.validation usando il modello ridge
# con lambda = 4
ridge.pred <- predict(ridge.mod.train, s = 4, newx = X.validation)
# Calcoliamo l’errore quadratico medio (MSE) delle previsioni sul test set.
mean((ridge.pred - Y.validation)^2) # 226041

# Lambda piccolo (es. 4): penalizzazione più leggera -> coefficiente più grandi
#                         -> modello più flessibile, potenzialmente migliore adattamento.
# Lambda grande (es. 1e10): penalizzazione molto forte -> coefficiente quasi zero
#                         -> modello quasi costante, spesso peggiori previsioni (MSE più alto).
# Confrontando i due MSE capiamo quanto la scelta di lambda influisce sulla qualità del
# modello.

#-----------------------------------------
# Cross-validation
#-----------------------------------------
cv.out.ridge <- cv.glmnet(X, Y, alpha=0, lambda = grid, penalty.factor = p.factor)
cv.out.ridge
# Measure: Mean-Squared Error 
#     Lambda Index Measure   SE Nonzero
# min  0.007   100  223119 5062      34
# 1se 16.916    69  227418 5494      34

# significa che il modello ridge da un errore di predizione minimo per lambda = 0.007 (minimo
# della griglia), inoltre i coefficienti sono tutti non nulli a sottolineare che ridge non fa
# davvero selezione delle variabili ma le avvicina solo a 0
# Inoltre c'è un valore di lambda per cui l'errore resta in un raggio di un errore standard
# dall'errore minimo, questo è 16.916 (migliore per interpretazione dei dati ma peggiore per
# predizione)

cv.out.ridge$lambda.min # 0.006737947: lambda che minimizza l'MSE
cv.out.ridge$lambda.1se # 16.91639: lambda più semplice entro 1 deviazione standard
# lambda.min: valore di lambda che fornisce il più piccolo errore a seguito della
#             validazione
# lambda.1se: restituisce il modello più regolarizzato (con meno parametri) ma tale che
#             l’errore non si discosti più di un errore standard dal minimo

# png("cv_out_ridge.png", width = 1200, height = 1000, res = 120)
plot(cv.out.ridge)
# dev.off()
# png("cv_out_ridge_zoom.png", width = 1200, height = 1000, res = 120)
plot(cv.out.ridge, ylim=c(220000,330000), xlim=c(-5,5.1))
# dev.off()
# Il grafico mostra per ciascun lambda: l'errore di cross-validation.
#                                       le barre di errore (± 1 SD).
# Le linee verticali che indicano lambda.min e lambda.1se.

bestlam <- cv.out.ridge$lambda.min # 0.006737947

# Valutazione dell'MSE sul validation set con il miglior lambda (scelto dalla
# cross-validation) Calcoliamo l'MSE (errore quadratico medio) sul set di validazione
# utilizzando il modello ridge e il valore ottimale di lambda (bestlam), ottenuto dalla
# cross-validation.

ridge.pred <- predict(ridge.mod.train, s = bestlam, newx = X.validation)
mean((ridge.pred - Y.validation)^2)
# 225852.7 che è parecchio simile a 223119 che è l'mse stimato con cross-validation
# Inoltre rappresenta un leggero miglioramento rispetto all’MSE ottenuto precedentemente
# usando lambda = 4.
# Significa che la scelta automatica di lambda tramite CV ha prodotto un modello più
# preciso anche se di poco.

# Adattiamo di nuovo il modello di regressione ridge, questa volta usando tutto il dataset
# (X, Y) e il valore di lambda 1se trovato con la cross-validation.
# Poi visualizziamo i primi 20 coefficienti stimati.
predict(ridge.mod, type = "coefficients", s = cv.out.ridge$lambda.1se )[1:34,]
#        (Intercept)                    expLevel            challengeMaxWins           donationsReceived                  warDayWins 
#      -3.626286e+02               -7.165930e+00                7.390836e+00               -7.521628e-01               -1.964814e+00 
#       meanCostDeck       daysSinceRegistration                  cardsOwned                CardsLevel13                CardsLevel12 
#       5.630683e+00                8.161027e-03                5.249809e+01                8.514291e-02                7.045604e+00 
#       CardsLevel11                CardsLevel10          lastLeagueTrophies          bestLeagueTrophies            log_CardsLevel14 
#       4.758582e+00                8.661271e+00               -1.103436e-01               -2.200499e-01                3.128487e+02 
#   log_CardsLevel15       log_challengeCardsWon      log_clanCardsCollected          log_totalDonations               log_donations 
#       4.055612e+02                1.148558e-02               -2.937369e+00               -5.465779e+01                3.998045e+01 
# log_threeCrownWins              log_starPoints          log_totalExpPoints   log_tournamentBattleCount                    log_wins 
#      -2.816615e+02               -6.828572e+01                7.217974e+01                1.997930e+01                3.877588e+02 
# sq_log_battleCount       fourth_root_expPoints          fourth_root_losses               sqrt_CardsEvo sqrt_yearsSinceRegistration 
#       1.156952e+01               -8.118350e+00               -1.123591e+02               -3.740856e+01               -4.724819e+01 
#  sq_meanLevelCards    sq_meanLevelSupportCards  poly(bestLeagueNumber, 3)1  poly(bestLeagueNumber, 3)2 
#      -3.980319e+00                1.145981e+00                2.253233e+04               -3.335427e+03

# Come previsto, nessuno dei coefficienti è esattamente uguale a zero.
# Questo perché la regressione ridge non esegue selezione delle variabili: tende a ridurre
# i coefficienti, ma non li annulla (al contrario della regressione LASSO).

#-----------------------------
# La regressione ridge, con una scelta ben calibrata di lambda, può superare in
# prestazioni sia il modello dei minimi quadrati ordinari (OLS) sia il modello nullo (cioè
# una media costante) sul dataset.
# Ora ci chiediamo se la regressione LASSO possa produrre un modello:
# -  più accurato (cioè con un MSE inferiore),
# -  più interpretabile (cioè con meno variabili, perché il LASSO può portare a coefficienti
#    esattamente pari a zero).
## Differenza chiave:
# Ridge regression: penalizza i coefficienti grandi -> li rimpicciolisce, ma non
#                   li azzera mai.
# Lasso regression: può annullare completamente alcuni coefficienti -> fa anche selezione
#                   di variabili (feature selection).
#-----------------------------

##------------------------------------------------------------------------------
# LASSO
##------------------------------------------------------------------------------
lasso.mod <- glmnet(X, Y, alpha = 1, lambda = grid, penalty.factor = p.factor) # alpha=1 --> LASSO REGRESSION
dim(coef(lasso.mod)) # 35 100

# Qui stiamo semplicemente costruendo la griglia di modelli lasso su tutto il dataset
# per tanti valori di lambda.
# lasso.mod contiene tutte le soluzioni (coefficienti) per tutti i lambda testati.
# Visualizza l'evoluzione dei coefficienti al variare di lambda
plot(lasso.mod, xvar="lambda",label = TRUE)

# Salva come PNG
# width e height sono in pixel, res è la risoluzione (dpi - dots per inch)
# png("lasso_coefficients_plot_zoom3.png", width = 1200, height = 2800, res = 120)
plot(lasso.mod, xvar="lambda", label=TRUE, ylim = c(-155, 120))
# dev.off()

# con lasso si perde l'azione di gruppo ed ogni variabile viene azzerata separatamente,
# anzi se ci sono 2 variabili correlate è probabile che una delle due venga azzerata

# vettore dei coefficienti stimati per lambda = 10 (inclusa l’intercetta).
# Alcuni coefficienti saranno zero: significa che quelle variabili non sono state incluse
#                                   dal modello Lasso.
coef(lasso.mod, s = 10)
# 35 x 1 sparse Matrix of class "dgCMatrix"
#                                        s1
# (Intercept)                 -6.376907e+01
# expLevel                     .           
# challengeMaxWins             4.023145e+00
# donationsReceived           -1.475970e-01
# warDayWins                  -1.877179e+00
# meanCostDeck                 .           
# daysSinceRegistration        .           
# cardsOwned                   5.616168e+01
# CardsLevel13                 .           
# CardsLevel12                 1.661964e+00
# CardsLevel11                 2.999936e+00
# CardsLevel10                 5.390707e+00
# lastLeagueTrophies           .           
# bestLeagueTrophies          -4.240816e-03
# log_CardsLevel14             2.431141e+02
# log_CardsLevel15             3.255850e+02
# log_challengeCardsWon        .           
# log_clanCardsCollected       .           
# log_totalDonations          -4.570029e+01
# log_donations                2.511382e+01
# log_threeCrownWins          -1.516674e+02
# log_starPoints              -6.100591e+01
# log_totalExpPoints           .           
# log_tournamentBattleCount    1.006156e+01
# log_wins                     2.308179e+02
# sq_log_battleCount           .           
# fourth_root_expPoints       -8.434382e-02
# fourth_root_losses           .           
# sqrt_CardsEvo               -9.080030e-01
# sqrt_yearsSinceRegistration -3.611731e+01
# sq_meanLevelCards           -2.485154e-02
# sq_meanLevelSupportCards     .           
# poly(bestLeagueNumber, 3)1   2.551559e+04
# poly(bestLeagueNumber, 3)2  -7.982964e+03
# poly(bestLeagueNumber, 3)3   2.521888e+03

# Nota che, a differenza della regressione ridge, il LASSO azzera esattamente alcuni
# coefficienti. Il numero di coefficienti azzerati aumenta all’aumentare del valore di
# lambda.

norme1 <- c()
for(i in 1:100) {
  norme1 <- c(norme1, norm(as.matrix(coef(lasso.mod)[-c(1, (factor_indices+1)),i]),'1'))
}

# Disegna il grafico
df <- data.frame(log_lambda = log(lasso.mod$lambda), norma1_coefficienti = norme1)
#ggsave("Norma1vslambda.png", plot = 
         ggplot(df, aes(x = log_lambda, y = norma1_coefficienti)) +
         geom_line(color = "steelblue", linewidth = 1.2) +
         labs(title = "Norma 1 del vettore dei coefficienti vs \u03BB",
              x = "log(\u03BB)",
              y = "Norma 1 del vettore dei coefficienti") +
         theme(
           panel.background = element_rect(fill = "white", colour = NA),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           plot.background = element_rect(fill = "white", colour = NA),
           
           plot.title = element_text(size = 18, face = "bold"),   # Titolo del grafico più grande
           axis.title = element_text(size = 14, face = "bold"),   # Titoli degli assi più grandi
           axis.text = element_text(size = 9),                   # Etichette numeriche/testuali sugli assi più grandi
         )#, width = 10, height = 8)
rm(df)

## Calcolo il training MSE per ciascun valore di lambda
# Predizioni per tutti i lambda
pred.train <- predict(lasso.mod, newx = X.train)
# Calcolo del MSE per ciascuna colonna (i.e., per ogni lambda)
train.mse <- colMeans((pred.train - Y.train)^2)
plot(log(grid), train.mse)
# mse sul training con lambda=4:
mean((predict(lasso.mod, s=4, newx = X.train) - Y.train)^2) # 225649.1

## Cross-validation
# Adatta il modello LASSO
cv.out.lasso <- cv.glmnet(X, Y, alpha = 1, lambda = grid, penalty.factor = p.factor)
cv.out.lasso
# Measure: Mean-Squared Error 
#     Lambda Index Measure   SE Nonzero
# min 0.0067   100  223092 3680      34
# 1se 2.2436    77  226368 3634      29

# Grafico: MSE vs log(lambda)
# png("cv_out_lasso.png", width = 1200, height = 1000, res = 120)
plot(cv.out.lasso)
# dev.off()
# png("cv_out_lasso_zoom.png", width = 1200, height = 1000, res = 120)
plot(cv.out.lasso, xlim=c(-5,4.1), ylim=c(220000,321000))
# dev.off()

# Seleziono il miglior lambda
bestlam <- cv.out.lasso$lambda.min # 0.006737947
# Questo restituisce le predizioni del modello lasso (addestrato sul training set) per i
# dati di validazione. Usa il valore lambda scelto dalla cross-validation.
lasso.mod.train  <- glmnet(X.train, Y.train, alpha = 1, lambda = grid, penalty.factor = p.factor)
lasso.pred <- predict(lasso.mod.train, s = bestlam, newx = X.validation)
# Calcolo errore MSE sul test set
mean((lasso.pred - Y.validation)^2)
# 226091.3 che è parecchio simile a 223092 che è l'mse stimato con cross-validation
# Inoltre rappresenta un leggero miglioramento rispetto all’MSE ottenuto precedentemente
# usando lambda = 4.
# Questo valore è molto simile a quello ottenuto con la regressione ridge
# usando il lambda scelto tramite cross-validation.
# Tuttavia, il LASSO ha un grande vantaggio rispetto alla regressione ridge: i
# coefficienti stimati risultano essere sparsi (cioè molti sono esattamente zero).
# Estrai i coefficienti finali con lambda = lambda.1se = 2.243598 (perchè bestlam
# è troppo piccolo e rischia di non fare selezione)
lasso.coef <- coef(cv.out.lasso, s = cv.out.lasso$lambda.1se)
num.nonzero.coef <- sum(lasso.coef[-1] != 0)
num.nonzero.coef
## [1] 29
active.variables <- rownames(lasso.coef)[which(lasso.coef != 0)]
active.variables
# "(Intercept)"                 "expLevel"                    "challengeMaxWins"            "donationsReceived"          
# "warDayWins"                  "meanCostDeck"                "cardsOwned"                  "CardsLevel12"               
# "CardsLevel11"                "CardsLevel10"                "lastLeagueTrophies"          "bestLeagueTrophies"         
# "log_CardsLevel14"            "log_CardsLevel15"            "log_clanCardsCollected"      "log_totalDonations"         
# "log_donations"               "log_threeCrownWins"          "log_starPoints"              "log_tournamentBattleCount"  
# "log_wins"                    "fourth_root_expPoints"       "fourth_root_losses"          "sqrt_CardsEvo"              
# "sqrt_yearsSinceRegistration" "sq_meanLevelCards"           "sq_meanLevelSupportCards"    "poly(bestLeagueNumber, 3)1" 
# "poly(bestLeagueNumber, 3)2"  "poly(bestLeagueNumber, 3)3"

# Estrai i coefficienti per il valore di lambda scelto da cross-validation
lasso.coef <- predict(lasso.mod, type = "coefficients", s = cv.out.lasso$lambda.1se)[1:35,]
## Visualizza solo i coefficienti diversi da zero
lasso.coef[lasso.coef != 0]
#        (Intercept)                    expLevel            challengeMaxWins           donationsReceived                  warDayWins 
#      -1.187014e+02               -1.018692e-01                7.173668e+00               -6.628234e-01               -1.906067e+00 
#       meanCostDeck                  cardsOwned                CardsLevel12                CardsLevel11                CardsLevel10 
#       4.728799e+00                5.695112e+01                5.362599e+00                4.270511e+00                6.918962e+00 
# lastLeagueTrophies          bestLeagueTrophies            log_CardsLevel14            log_CardsLevel15      log_clanCardsCollected 
#      -7.398066e-02               -1.674886e-01                3.119322e+02                3.985977e+02               -6.586897e-01 
# log_totalDonations               log_donations          log_threeCrownWins              log_starPoints   log_tournamentBattleCount 
#      -5.706478e+01                3.627845e+01               -3.133803e+02               -6.664118e+01                1.351844e+01 
#           log_wins       fourth_root_expPoints          fourth_root_losses               sqrt_CardsEvo sqrt_yearsSinceRegistration 
#       4.342973e+02               -7.001204e+00               -3.359528e+01               -4.365439e+01               -4.154771e+01 
#  sq_meanLevelCards    sq_meanLevelSupportCards  poly(bestLeagueNumber, 3)1  poly(bestLeagueNumber, 3)2  poly(bestLeagueNumber, 3)3 
#      -2.491932e+00                4.655259e-01                2.254140e+04               -4.237058e+03                3.768259e+03
lasso.coef[lasso.coef == 0]
# daysSinceRegistration          CardsLevel13 log_challengeCardsWon    log_totalExpPoints    sq_log_battleCount  
#                     0                     0                     0                     0                     0

##------------------------------------------------------------------------------
# ELASTIC NET
##------------------------------------------------------------------------------
elastic.mod <- glmnet(X, Y, alpha = 0.5, lambda = grid, penalty.factor = p.factor)
# alpha=0.5 --> ELASTIC NET REGRESSION
dim(coef(elastic.mod)) # 35 100

# Visualizza l'evoluzione dei coefficienti al variare di lambda
plot(elastic.mod, xvar="lambda", label=TRUE)

# Salva come PNG
# width e height sono in pixel, res è la risoluzione (dpi - dots per inch)
# png("elastic_coefficients_plot_zoom3.png", width = 1200, height = 2800, res = 120)
plot(elastic.mod, xvar="lambda", label=TRUE, ylim = c(-70, 60))
# dev.off()
# torna un'azione di gruppo ma anche la capcità di fare selezione (cioè rendere
# esattamente 0 alcuni coefficienti)

## Cross- validation
cv.out.elastic <- cv.glmnet(X, Y, alpha = 0.5, lambda = grid, penalty.factor = p.factor)
cv.out.elastic
# Measure: Mean-Squared Error 
#     Lambda Index Measure   SE Nonzero
# min  0.007   100  223267 3507      34
# 1se  3.718    75  226303 3417      30

# Grafico: MSE vs log(lambda)
# png("cv_out_elastic.png", width = 1200, height = 1000, res = 120)
plot(cv.out.elastic)
# dev.off()
# png("cv_out_elastic_zoom.png", width = 1200, height = 1000, res = 120)
plot(cv.out.elastic, xlim=c(-5,4.5), ylim=c(220000,310000))
# dev.off()

# Seleziono il miglior lambda
bestlam <- cv.out.elastic$lambda.min # 0.006737947
# Questo restituisce le predizioni del modello elastic net (addestrato sul training set) per i
# dati di validazione. Usa il valore lambda scelto dalla cross-validation.
elastic.mod.train  <- glmnet(X.train, Y.train, alpha = 0.5, lambda = grid, penalty.factor = p.factor)
elastic.pred <- predict(elastic.mod.train, s = bestlam, newx = X.validation)
# Calcolo errore MSE sul test set
mean((elastic.pred - Y.validation)^2)
# 225973.7 che è parecchio simile a 223267 che è l'mse stimato con cross-validation
# Questo valore è molto simile a quello ottenuto con le altre due modalità
# usando il lambda scelto tramite cross-validation.
# Tuttavia, elastic net ha come vantaggio che i coefficienti stimati risultano essere
# sparsi come nella LASSO.

# Estrai i coefficienti finali con lambda = lambda.1se = 3.717797 (perchè bestlam
# è troppo piccolo e rischia di non fare selezione)
elastic.coef <- predict(elastic.mod, type = "coefficients", s = cv.out.elastic$lambda.1se)[1:35,]
## Visualizza solo i coefficienti diversi da zero
elastic.coef[elastic.coef != 0]
#                 (Intercept)                    expLevel            challengeMaxWins           donationsReceived                  warDayWins 
#               -1.495012e+02               -2.357437e+00                7.181783e+00               -6.748163e-01               -1.916151e+00 
#                meanCostDeck                  cardsOwned                CardsLevel13                CardsLevel12                CardsLevel11 
#                5.422999e+00                5.661725e+01               -1.721366e-02                5.641467e+00                4.359706e+00 
#                CardsLevel10          lastLeagueTrophies          bestLeagueTrophies            log_CardsLevel14            log_CardsLevel15 
#                7.118571e+00               -7.888221e-02               -1.791780e-01                3.136427e+02                4.067165e+02 
#      log_clanCardsCollected          log_totalDonations               log_donations          log_threeCrownWins              log_starPoints 
#               -9.295258e-01               -5.564659e+01                3.679877e+01               -3.084909e+02               -6.625223e+01 
#   log_tournamentBattleCount                    log_wins       fourth_root_expPoints          fourth_root_losses               sqrt_CardsEvo 
#                1.482138e+01                4.482848e+02               -7.073419e+00               -4.084692e+01               -4.025094e+01 
# sqrt_yearsSinceRegistration           sq_meanLevelCards    sq_meanLevelSupportCards  poly(bestLeagueNumber, 3)1  poly(bestLeagueNumber, 3)2 
#               -4.022709e+01               -2.528957e+00                6.740370e-01                2.253044e+04               -4.102795e+03 
#  poly(bestLeagueNumber, 3)3 
#                3.870707e+03
elastic.coef[elastic.coef == 0]
# daysSinceRegistration    log_challengeCardsWon    log_totalExpPoints    sq_log_battleCount
#                     0                        0                     0                     0

#-----------------------------
# Osservazione: Calcolo di lambda.min e lambda.1se
#-----------------------------
# La curva della cross-validation (linea rossa tratteggiata) è mostrata insieme alle curve
# della deviazione standard superiore e inferiore lungo la sequenza dei valori di
# lambda (le barre di errore).
# Due valori speciali lungo la sequenza di lambda sono indicati da linee verticali
# tratteggiate:
## lambda.min è il valore di lambda che fornisce l’errore medio di cross-validation più
#             basso.
## lambda.1se è il valore di lambda che fornisce il modello più regolarizzato tale che
#             l’errore di cross-validation sia entro una deviazione standard dal
#             minimo.
# Il vantaggio di identificare il valore di lambda che ha un MSE entro una deviazione
# standard dal minimo diventa più evidente con i modelli Lasso ed Elastic Net.
#-----------------------------

# Confronto
ols.mod <- lm(Y ~ X)
ols.coef <- coef(ols.mod)

ridge.mod <- glmnet(X, Y, alpha = 0, lambda = grid, penalty.factor = p.factor)
ridge.coef <- predict(ridge.mod, type = "coefficients", s = cv.out.ridge$lambda.1se)[1:35,]

lasso.mod <- glmnet(X, Y, alpha = 1, lambda = grid, penalty.factor = p.factor)
lasso.coef <- predict(lasso.mod, type = "coefficients", s = cv.out.lasso$lambda.1se)[1:35,]

elastic.mod <- glmnet(X, Y, alpha = 0.5, lambda = grid, penalty.factor = p.factor)
elastic.coef <- predict(elastic.mod, type = "coefficients", s = cv.out.elastic$lambda.1se)[1:35,]

coef_df <- data.frame(
  Variable = names(ols.coef),
  OLS = ols.coef,
  Ridge = as.vector(ridge.coef),
  Lasso = as.vector(lasso.coef),
  Elastic_net = as.vector(elastic.coef)
)
print(coef_df)
# # Caricare il pacchetto
# library(xlsx)
# 
# # Salvare il dataframe nel file Excel
# write.xlsx(coef_df, "confronto_coef_df.xlsx")

#                                                 Variable           OLS         Ridge         Lasso   Elastic_net
# (Intercept)                                  (Intercept) -8.033658e+02 -3.626286e+02 -1.187014e+02 -1.495012e+02
# expLevel                                        expLevel -5.282724e+01 -7.165930e+00 -1.018692e-01 -2.357437e+00
# challengeMaxWins                        challengeMaxWins  6.193069e+00  7.390836e+00  7.173668e+00  7.181783e+00
# donationsReceived                      donationsReceived -7.122586e-01 -7.521628e-01 -6.628234e-01 -6.748163e-01
# warDayWins                                    warDayWins -1.785186e+00 -1.964814e+00 -1.906067e+00 -1.916151e+00
# meanCostDeck                                meanCostDeck  2.267348e+01  5.630683e+00  4.728799e+00  5.422999e+00
# daysSinceRegistration              daysSinceRegistration  9.345347e-03  8.161027e-03  0.000000e+00  0.000000e+00
# cardsOwned                                    cardsOwned  5.729239e+01  5.249809e+01  5.695112e+01  5.661725e+01
# CardsLevel13                                CardsLevel13  2.791645e+00  8.514291e-02  0.000000e+00 -1.721366e-02
# CardsLevel12                                CardsLevel12  8.243323e+00  7.045604e+00  5.362599e+00  5.641467e+00
# CardsLevel11                                CardsLevel11  5.933755e+00  4.758582e+00  4.270511e+00  4.359706e+00
# CardsLevel10                                CardsLevel10  7.784899e+00  8.661271e+00  6.918962e+00  7.118571e+00
# lastLeagueTrophies                    lastLeagueTrophies -9.943247e-02 -1.103436e-01 -7.398066e-02 -7.888221e-02
# bestLeagueTrophies                    bestLeagueTrophies -2.156901e-01 -2.200499e-01 -1.674886e-01 -1.791780e-01
# log_CardsLevel14                        log_CardsLevel14  3.712179e+02  3.128487e+02  3.119322e+02  3.136427e+02
# log_CardsLevel15                        log_CardsLevel15  5.669474e+02  4.055612e+02  3.985977e+02  4.067165e+02
# log_challengeCardsWon              log_challengeCardsWon  6.575979e-01  1.148558e-02  0.000000e+00  0.000000e+00
# log_clanCardsCollected            log_clanCardsCollected -1.063216e+00 -2.937369e+00 -6.586897e-01 -9.295258e-01
# log_totalDonations                    log_totalDonations -5.457009e+01 -5.465779e+01 -5.706478e+01 -5.564659e+01
# log_donations                              log_donations  3.947214e+01  3.998045e+01  3.627845e+01  3.679877e+01
# log_threeCrownWins                    log_threeCrownWins -3.476249e+02 -2.816615e+02 -3.133803e+02 -3.084909e+02
# log_starPoints                            log_starPoints -7.008271e+01 -6.828572e+01 -6.664118e+01 -6.625223e+01
# log_totalExpPoints                    log_totalExpPoints  2.090949e+02  7.217974e+01  0.000000e+00  0.000000e+00
# log_tournamentBattleCount      log_tournamentBattleCount  1.334688e+01  1.997930e+01  1.351844e+01  1.482138e+01
# log_wins                                        log_wins  3.807633e+02  3.877588e+02  4.342973e+02  4.482848e+02
# sq_log_battleCount                    sq_log_battleCount  2.819827e+01  1.156952e+01  0.000000e+00  0.000000e+00
# fourth_root_expPoints              fourth_root_expPoints -1.470318e+01 -8.118350e+00 -7.001204e+00 -7.073419e+00
# fourth_root_losses                    fourth_root_losses -2.342200e+02 -1.123591e+02 -3.359528e+01 -4.084692e+01
# sqrt_CardsEvo                              sqrt_CardsEvo -4.613656e+01 -3.740856e+01 -4.365439e+01 -4.025094e+01
# sqrt_yearsSinceRegistration  sqrt_yearsSinceRegistration -4.206473e+01 -4.724819e+01 -4.154771e+01 -4.022709e+01
# sq_meanLevelCards                      sq_meanLevelCards -2.208128e+00 -3.980319e+00 -2.491932e+00 -2.528957e+00
# sq_meanLevelSupportCards        sq_meanLevelSupportCards  8.633597e-01  1.145981e+00  4.655259e-01  6.740370e-01
# poly(bestLeagueNumber, 3)1    poly(bestLeagueNumber, 3)1  2.048674e+04  2.253233e+04  2.254140e+04  2.253044e+04
# poly(bestLeagueNumber, 3)2    poly(bestLeagueNumber, 3)2 -3.362578e+03 -3.335427e+03 -4.237058e+03 -4.102795e+03
# poly(bestLeagueNumber, 3)3    poly(bestLeagueNumber, 3)3  3.745664e+03  4.370962e+03  3.768259e+03  3.870707e+03

#-----------------------------
# Grafici riassuntivi di cross-validazione per selezionare un
# valore per alpha.
set.seed(123) # per riproducibilità
foldid <- sample(1:10, size = length(Y), replace = TRUE)
grid <- exp(seq(0, 8,length = 100))
cv1 <- cv.glmnet(X, Y, foldid = foldid, alpha = 1,
                 lambda = grid, penalty.factor = p.factor)
cv1$cvm[cv1$index[1]] # MSE in corrispondenza di lambda.min
# MSE = 224561.8, cv1$index[1] = 100, lambda.min = 1.00

cv.5 <- cv.glmnet(X, Y, foldid = foldid, alpha = 0.5,
                  lambda = grid, penalty.factor = p.factor)
cv.5$cvm[cv.5$index[1]]
# MSE = 223834.5, cv.5$index[1] = 100, lambda.min = 1.00

grid <- exp(seq(0, 12,length = 100))
cv0 <- cv.glmnet(X, Y, foldid = foldid, alpha = 0,
                 lambda = grid, penalty.factor = p.factor)
cv0$cvm[cv0$index[1]]
# MSE = 222823.5, cv0$index[1] = 100, lambda.min = 1.00

# png("cv_out_confronto.png", width = 2000, height = 1500, res = 120)
par(mfrow = c(2,2))
plot(cv1); title("LASSO", line = 2.5);
plot(cv.5); title("ELASTIC", line = 2.5);
plot(cv0); title("RIDGE", line = 2.5);
plot(log(cv1$lambda) , cv1$cvm , pch = 10, col = "red",
     xlab = "Log(\u03BB)", ylab = cv1$name)
points(log(cv.5$lambda), cv.5$cvm, pch = 10, col = "grey")
points(log(cv0$lambda) , cv0$cvm , pch = 10, col = "blue")
legend("topleft", legend = c("alpha= 1", "alpha= .5", "alpha 0"),
       pch = 19, col = c("red","grey","blue"))
# dev.off()

# Vediamo che lasso (alpha = 1) ottiene i risultati migliori in questo caso (con
# la griglia di lambda autonoma, senza probabilmente il migliore è ridge).
# Notiamo anche che l'intervallo di valori di lambda utilizzati varia in base al
# valore di alpha.

rm(new_train_data_processed, X, Y, grid, ridge.mod, p.factor, var_factor, i,
   norme2, coef_lambda_50, train, validation, X.train, Y.train,
   X.validation, Y.validation, ridge.mod.train, ridge.pred, cv.out.ridge,
   bestlam, lasso.mod, norme1, pred.train, train.mse, cv.out.lasso,
   lasso.mod.train, lasso.pred, lasso.coef, num.nonzero.coef, active.variables,
   elastic.mod, cv.out.elastic, elastic.mod.train, elastic.pred, elastic.coef,
   ols.mod, ols.coef, coef_df, foldid, cv1, cv.5, cv0, factor_indices, nome,
   ridge.coef)
