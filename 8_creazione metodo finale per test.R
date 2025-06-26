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

X <- model.matrix(trophies ~ . - bestLeagueNumber + poly(bestLeagueNumber, 3),
                  data = new_train_data_processed)[,-1] # Rimuovo l'intercetta
Y <- new_train_data_processed$trophies
var_factor <- c("poly(bestLeagueNumber, 3)1", "poly(bestLeagueNumber, 3)2",
                "poly(bestLeagueNumber, 3)3" )
grid <- exp(seq(-5, 20,length = 100)) # i valori decrescono con l'indice
p.factor <- rep(1, ncol(X))
factor_indices <- c()
for(nome in var_factor) {
  factor_indices <- c(factor_indices, which(colnames(X) == nome))
}
p.factor[factor_indices] <- 0

set.seed(123) # per riproducibilità
train <- sample(1: nrow(X), nrow(X)/2) # prendi metà degli indici
validation <- (- train ) # il resto sono per il test
X.train <- X[train,]
Y.train <- Y[train]

X.validation <- X[validation,]
Y.validation <- Y[validation]

ridge.mod.train <- glmnet(X.train, Y.train, alpha = 0,
                          lambda = grid, thresh = 1e-12, penalty.factor = p.factor)

cv.out.ridge <- cv.glmnet(X, Y, alpha=0, lambda = grid, penalty.factor = p.factor)

mean((Y-predict(cv.out.ridge, s = cv.out.ridge$lambda.1se, newx = X))^2)
# ridge: p=34, MSE_training=225243.3

bestlam <- cv.out.ridge$lambda.min # 0.006737947

lasso.mod <- glmnet(X, Y, alpha = 1, lambda = grid, penalty.factor = p.factor) # alpha=1 --> LASSO REGRESSION

cv.out.lasso <- cv.glmnet(X, Y, alpha = 1, lambda = grid, penalty.factor = p.factor)

bestlam <- cv.out.lasso$lambda.min # 0.006737947

mean((Y-predict(cv.out.lasso, s = cv.out.lasso$lambda.1se, newx = X))^2)
# ridge: p=29, MSE_training=224492.4

##------------------------------------------------------------------------------
# ELASTIC NET
##------------------------------------------------------------------------------
elastic.mod <- glmnet(X, Y, alpha = 0.5, lambda = grid, penalty.factor = p.factor)
# alpha=0.5 --> ELASTIC NET REGRESSION

## Cross- validation
cv.out.elastic <- cv.glmnet(X, Y, alpha = 0.5, lambda = grid, penalty.factor = p.factor)
cv.out.elastic


mean((Y-predict(cv.out.elastic, s = cv.out.elastic$lambda.1se, newx = X))^2)
# ridge: p=30, MSE_training=224146.8

rm(var_factor, factor_indices, train, validation, X.train, Y.train, X.validation,
   Y.validation, ridge.mod.train, cv.out.ridge, bestlam, lasso.mod, cv.out.lasso,
   nome, elastic.mod)
rm(ridge.mod, var_factor, i,
   norme2, coef_lambda_50, train, validation, X.train, Y.train,
   X.validation, Y.validation, ridge.mod.train, ridge.pred, cv.out.ridge,
   bestlam, lasso.mod, norme1, pred.train, train.mse, cv.out.lasso,
   lasso.mod.train, lasso.pred, lasso.coef, num.nonzero.coef, active.variables,
   elastic.mod)