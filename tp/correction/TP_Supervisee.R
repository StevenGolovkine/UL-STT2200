############################################################
## TP Supervised Learning – R code template
############################################################

## Load required packages
library(dplyr)
library(ggplot2)
library(MASS)         # lda
library(rpart)        # classification trees
library(rpart.plot)
library(randomForest) # random forests
library(caret)        # train/test split, CV, etc.
library(adabag)       # AdaBoost (or ada package)
library(xgboost)      # XGBoost
set.seed(123)

############################################################
## Exercice 1 : Position des joueurs NFL
############################################################

## 1. Import data
nfl <- read.csv("nfl.csv")

## Assume there are columns: PosRec (factor), Ht, Wt
nfl <- nfl %>%
  mutate(PosRec = factor(PosRec))

## 2. Descriptive analysis
summary(nfl)
table(nfl$PosRec)

ggplot(nfl, aes(x = Ht, y = Wt, color = PosRec)) +
  geom_point(alpha = 0.7) +
  theme_minimal()

## 3. LDA: QB vs OL using Ht and Wt
nfl_QB_OL <- nfl |> 
  filter(PosRec %in% c("QB", "OL")) |> 
  droplevels()

lda_QB_OL <- lda(PosRec ~ Ht + Wt, data = nfl_QB_OL)
lda_QB_OL

## 4. Coordinate & class for new player (Ht=76.5, Wt=335.5)
new_player <- data.frame(Ht = 76.5, Wt = 335.5)

## Predict discriminant scores and class
pred_new_QB_OL <- predict(lda_QB_OL, newdata = new_player)
pred_new_QB_OL$x      # coordinate on the discriminant axis
pred_new_QB_OL$class  # predicted position

## 5. Same for OL vs DL, then DL vs QB

## OL vs DL
nfl_OL_DL <- nfl %>%
  filter(PosRec %in% c("OL", "DL")) %>%
  droplevels()

lda_OL_DL <- lda(PosRec ~ Ht + Wt, data = nfl_OL_DL)
lda_OL_DL

## DL vs QB
nfl_DL_QB <- nfl %>%
  filter(PosRec %in% c("DL", "QB")) %>%
  droplevels()

lda_DL_QB <- lda(PosRec ~ Ht + Wt, data = nfl_DL_QB)
lda_DL_QB

## Optionally compute apparent classification error for each:
lda_error <- function(model, data, response = "PosRec") {
  p <- predict(model)$class
  mean(p != data[[response]])
}

err_QB_OL <- lda_error(lda_QB_OL, nfl_QB_OL)
err_OL_DL <- lda_error(lda_OL_DL, nfl_OL_DL)
err_DL_QB <- lda_error(lda_DL_QB, nfl_DL_QB)

err_QB_OL; err_OL_DL; err_DL_QB

## 6. LDA with three positions: QB, OL, DL
nfl_3 <- nfl %>%
  filter(PosRec %in% c("QB", "OL", "DL")) %>%
  droplevels()

lda_3 <- lda(PosRec ~ Ht + Wt, data = nfl_3)
lda_3

pred_lda3 <- predict(lda_3)$class
mean(pred_lda3 != nfl_3$PosRec)  # apparent error

# Il vaut mieux utiliser les modèles précédents. Il est plus difficile de 
# séparer trois groupes que plusieurs fois deux groupes.

############################################################
## Exercice 2 : Prédire les réclamations (classification tree)
############################################################

## 1. Import data
claims <- read.csv("rakuten.csv") |> 
  filter(SELLER_COUNTRY %in% c("FRANCE, METROPOLITAN", "CHINA", "GERMANY")) |> 
  filter(ITEM_PRICE != "1000<5000")
# On selectionne seulement quelques pays pour ne pas avoir de problème lors
# du train/test.


## 2. Descriptive analysis
str(claims)
summary(claims)
table(claims$CLAIM_TYPE)

## 3. Train/validation split (70/30)
set.seed(123)
train_idx <- createDataPartition(claims$CLAIM_TYPE, p = 0.7, list = FALSE)
claims_train <- claims[train_idx, ] |> droplevels()
claims_valid <- claims[-train_idx, ] |> droplevels()

## 4. Classification tree (default parameters)
tree1 <- rpart(CLAIM_TYPE ~ ., data = claims_train, method = "class")
rpart.plot(tree1)

## 5. Predictions on train and validation
pred_train <- predict(tree1, newdata = claims_train, type = "class")
pred_valid <- predict(tree1, newdata = claims_valid, type = "class")

## Confusion matrices
confusionMatrix(pred_train, as.factor(claims_train$CLAIM_TYPE))
confusionMatrix(pred_valid, as.factor(claims_valid$CLAIM_TYPE))

## 6. Question mal posée

## 7. Global accuracy on train & validation
acc_train <- mean(pred_train == claims_train$CLAIM_TYPE)
acc_valid <- mean(pred_valid == claims_valid$CLAIM_TYPE)
acc_train; acc_valid

## 8. Change minsplit and minbucket and rebuild the tree
ctrl2 <- rpart.control(minsplit = 2, minbucket = 1, cp = 0.001)
tree2 <- rpart(CLAIM_TYPE ~ ., data = claims_train, method = "class", control = ctrl2)
rpart.plot(tree2)

pred_train2 <- predict(tree2, newdata = claims_train, type = "class")
pred_valid2 <- predict(tree2, newdata = claims_valid, type = "class")
mean(pred_train2 == claims_train$CLAIM_TYPE)
mean(pred_valid2 == claims_valid$CLAIM_TYPE)

## 9–10. Vary cp and analyse resulting trees
cp_grid <- seq(0.001, 0.05, length.out = 20)
err_train_cp <- numeric(length(cp_grid))
err_valid_cp <- numeric(length(cp_grid))

for (i in seq_along(cp_grid)) {
  ctrl_i <- rpart.control(cp = cp_grid[i], minsplit = 2, minbucket = 1)
  tree_i <- rpart(CLAIM_TYPE ~ ., data = claims_train, method = "class", control = ctrl_i)
  pred_tr_i <- predict(tree_i, newdata = claims_train, type = "class")
  pred_va_i <- predict(tree_i, newdata = claims_valid, type = "class")
  err_train_cp[i] <- mean(pred_tr_i != claims_train$CLAIM_TYPE)
  err_valid_cp[i] <- mean(pred_va_i != claims_valid$CLAIM_TYPE)
}

plot(cp_grid, err_train_cp, type = "l", xlab = "cp", ylab = "Error rate")
lines(cp_grid, err_valid_cp, lty = 2)
legend("topright", legend = c("Train", "Validation"), lty = c(1, 2))

############################################################
## Exercice 3 : Bagging LDA and Trees
############################################################

## 1. Simulate data
mu1  <- c(0, 0)
Sigma1 <- matrix(c(1, 1, 1, 4), nrow = 2)
mu2  <- c(1, 1)
Sigma2 <- matrix(c(4, 4, 4, 16), nrow = 2)

n_train <- 200
n_test  <- 1000

library(MASS)

sim_data <- function(n1, n2) {
  y1 <- mvrnorm(n1, mu = mu1, Sigma = Sigma1)
  y2 <- mvrnorm(n2, mu = mu2, Sigma = Sigma2)
  x  <- rbind(y1, y2)
  class <- factor(c(rep("C1", n1), rep("C2", n2)))
  data.frame(x1 = x[, 1], x2 = x[, 2], class = class)
}

train <- sim_data(n_train / 2, n_train / 2)
test  <- sim_data(n_test / 2,  n_test / 2)

## Helper: majority vote
majority_vote <- function(pred_matrix) {
  apply(pred_matrix, 1, function(row) {
    tab <- table(row)
    names(tab)[which.max(tab)]
  })
}

## 2. Bagging with LDA
## Grid of number of base classifiers
B_grid <- seq(10, 500, length.out = 20)
err_test_lda_bag <- numeric(length(B_grid))

for (b in seq_along(B_grid)) {
  B <- B_grid[b]
  pred_matrix <- matrix(NA, nrow = nrow(test), ncol = B)
  
  for (k in 1:B) {
    idx_boot <- sample(1:nrow(train), replace = TRUE)
    train_boot <- train[idx_boot, ]
    lda_b <- lda(class ~ x1 + x2, data = train_boot)
    pred_test_b <- predict(lda_b, newdata = test)$class
    pred_matrix[, k] <- as.character(pred_test_b)
  }
  
  bag_pred <- factor(majority_vote(pred_matrix), levels = levels(test$class))
  err_test_lda_bag[b] <- mean(bag_pred != test$class)
}

plot(B_grid, err_test_lda_bag, type = "l",
     xlab = "Number of LDA base classifiers (B)", ylab = "Test error (bagging LDA)")

## 3. Bagging with Trees
err_test_tree_bag <- numeric(length(B_grid))

for (b in seq_along(B_grid)) {
  B <- B_grid[b]
  pred_matrix <- matrix(NA, nrow = nrow(test), ncol = B)
  
  for (k in 1:B) {
    idx_boot <- sample(1:nrow(train), replace = TRUE)
    train_boot <- train[idx_boot, ]
    tree_b <- rpart(class ~ x1 + x2, data = train_boot, method = "class",
                    control = rpart.control(minbucket = 5))  # play with minbucket
    pred_test_b <- predict(tree_b, newdata = test, type = "class")
    pred_matrix[, k] <- as.character(pred_test_b)
  }
  
  bag_pred <- factor(majority_vote(pred_matrix), levels = levels(test$class))
  err_test_tree_bag[b] <- mean(bag_pred != test$class)
}

plot(B_grid, err_test_tree_bag, type = "l",
     xlab = "Number of tree base classifiers (B)", ylab = "Test error (bagging trees)")

############################################################
## Exercice 4 : Forêt aléatoire pour le spam
############################################################

## 1. Import data
spam <- read.csv("spam.csv") |> 
  mutate(type = as.factor(type))

## 2. 50/50 train/validation split
set.seed(123)
train_idx_spam <- createDataPartition(spam$type, p = 0.5, list = FALSE)
spam_train <- spam[train_idx_spam, ]
spam_valid <- spam[-train_idx_spam, ]

## 3. Random forest with default parameters
rf_default <- randomForest(type ~ ., data = spam_train)
rf_default

## Predictions & errors
pred_train_rf <- predict(rf_default, spam_train)
pred_valid_rf <- predict(rf_default, spam_valid)

mean(pred_train_rf != spam_train$type)
mean(pred_valid_rf != spam_valid$type)

## 4. Influence of mtry
mtry_grid <- seq(1, ncol(spam) - 1, length.out = 10)
mtry_grid <- unique(round(mtry_grid))

err_valid_mtry <- numeric(length(mtry_grid))

for (i in seq_along(mtry_grid)) {
  rf_i <- randomForest(type ~ ., data = spam_train, mtry = mtry_grid[i])
  pred_valid_i <- predict(rf_i, newdata = spam_valid)
  err_valid_mtry[i] <- mean(pred_valid_i != spam_valid$type)
}

plot(mtry_grid, err_valid_mtry, type = "b",
     xlab = "mtry", ylab = "Validation error")

## 5. Choose mtry by cross-validation (using caret)
ctrl_cv <- trainControl(method = "cv", number = 5)
tune_grid <- expand.grid(mtry = mtry_grid)

rf_cv <- train(
  type ~ .,
  data = spam_train,
  method = "rf",
  trControl = ctrl_cv,
  tuneGrid = tune_grid
)
rf_cv
best_mtry <- rf_cv$bestTune$mtry

## 6. Influence of the number of trees (ntree) for mtry = 1, 7, 35
ntree_grid <- 1:50
mtry_values <- c(1, 7, 35)

err_valid_ntree <- matrix(NA, nrow = length(ntree_grid), ncol = length(mtry_values))
colnames(err_valid_ntree) <- paste0("mtry_", mtry_values)

for (j in seq_along(mtry_values)) {
  mtry_j <- mtry_values[j]
  for (i in seq_along(ntree_grid)) {
    rf_ij <- randomForest(type ~ ., data = spam_train,
                          mtry = mtry_j, ntree = ntree_grid[i])
    pred_valid_ij <- predict(rf_ij, newdata = spam_valid)
    err_valid_ntree[i, j] <- mean(pred_valid_ij != spam_valid$type)
  }
}

matplot(ntree_grid, err_valid_ntree, type = "l",
        xlab = "ntree", ylab = "Validation error")
legend("topright", legend = colnames(err_valid_ntree), lty = 1:ncol(err_valid_ntree))

## 7. OOB error vs test error for mtry = 1 (ntree from 1 to 50)
err_test_ntree <- numeric(length(ntree_grid))
err_oob_ntree  <- numeric(length(ntree_grid))

for (i in seq_along(ntree_grid)) {
  rf_i <- randomForest(type ~ ., data = spam_train,
                       mtry = 1, ntree = ntree_grid[i], keep.inbag = TRUE)
  ## OOB error
  err_oob_ntree[i] <- rf_i$err.rate[ntree_grid[i], "OOB"]
  
  ## Test error (validation set)
  pred_valid_i <- predict(rf_i, newdata = spam_valid)
  err_test_ntree[i] <- mean(pred_valid_i != spam_valid$type)
}

plot(ntree_grid, err_test_ntree, type = "l",
     xlab = "ntree", ylab = "Error rate")
lines(ntree_grid, err_oob_ntree, lty = 2)
legend("topright", legend = c("Validation error", "OOB error"), lty = c(1, 2))

############################################################
## Exercice 5 : Comparaison de classificateurs
############################################################

## 1. Import data
pima <- read.csv("pima.csv") |> 
  mutate(type = as.factor(type))


## 2–3. Evaluate LDA, tree, RF, AdaBoost, XGBoost with repeated splits
n_rep <- 100
err_lda  <- numeric(n_rep)
err_tree <- numeric(n_rep)
err_rf   <- numeric(n_rep)
err_ada  <- numeric(n_rep)
err_xgb  <- numeric(n_rep)

for (r in 1:n_rep) {
  set.seed(100 + r)
  train_idx_cmp <- createDataPartition(pima$type, p = 0.7, list = FALSE)
  cmp_train <- pima[train_idx_cmp, ]
  cmp_test  <- pima[-train_idx_cmp, ]
  
  ## LDA
  lda_r <- lda(type ~ ., data = cmp_train)
  pred_lda <- predict(lda_r, newdata = cmp_test)$class
  err_lda[r] <- mean(pred_lda != cmp_test$type)
  
  ## Tree
  tree_r <- rpart(type ~ ., data = cmp_train, method = "class")
  pred_tree <- predict(tree_r, newdata = cmp_test, type = "class")
  err_tree[r] <- mean(pred_tree != cmp_test$type)
  
  ## Random Forest
  rf_r <- randomForest(type ~ ., data = cmp_train)
  pred_rf <- predict(rf_r, newdata = cmp_test)
  err_rf[r] <- mean(pred_rf != cmp_test$type)
  
  ## AdaBoost (adabag)
  ada_r <- boosting(type ~ ., data = cmp_train)
  pred_ada <- predict(ada_r, newdata = cmp_test)
  err_ada[r] <- mean(pred_ada$class != cmp_test$type)
  
  ## XGBoost (binary or multi-class)
  ## Assume a binary classification {0,1} or multi-class factor
  ## We need numeric labels for xgboost:
  y_train_num <- as.numeric(cmp_train$type) - 1
  y_test_num  <- as.numeric(cmp_test$type) - 1
  X_train <- as.matrix(cmp_train[, setdiff(names(cmp_train), "type")])
  X_test  <- as.matrix(cmp_test[,  setdiff(names(cmp_test),  "type")])
  
  num_class <- length(levels(pima$type))
  objective_xgb <- if (num_class == 2) "binary:logistic" else "multi:softmax"
  
  xgb_r <- xgboost(
    data = X_train,
    label = y_train_num,
    objective = objective_xgb,
    nrounds = 50,
    verbose = 0
  )
  
  pred_xgb <- predict(xgb_r, newdata = X_test)
  if (num_class == 2) {
    pred_label <- ifelse(pred_xgb > 0.5, 1, 0)
  } else {
    pred_label <- pred_xgb
  }
  pred_factor <- factor(pred_label,
                        levels = 0:(num_class - 1),
                        labels = levels(pima$type))
  err_xgb[r] <- mean(pred_factor != cmp_test$type)
}

## Boxplots of error rates
error_df <- data.frame(
  LDA        = err_lda,
  Tree       = err_tree,
  RandomFore = err_rf,
  AdaBoost   = err_ada,
  XGBoost    = err_xgb
)

boxplot(error_df,
        main = "Classification error over 100 repetitions",
        ylab = "Error rate")

