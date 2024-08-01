########## TRECI ALGORITAM - RANDOM FOREST ###########

# Ucitavanje biblioteka

#install.packages("randomForest")
library(randomForest)
library(caret)
source('util.R')

# Ucitavanje train i test seta

test_rf <- readRDS("test.RDS")
train_rf <- readRDS("train.RDS")

# Kreiranje prvog modela sa predefinisanom vrednoscu parametara mtry i ntree

set.seed(1)
rf1 <- randomForest(high_revenue ~ ., data = train_rf)
print(rf1) #OOB error 11.88% 
           #ntree 500
           #mtry 3

rf1_pred <- predict(rf1, newdata = test_rf, type = "class")
cm1_rf <- table(actual = test_rf$high_revenue,
                predicted = rf1_pred)
cm1_rf

eval1_rf <- compute_eval_metrics(cm1_rf)
eval1_rf

# Trazenje optimalne vrednosti za mtry parametar primenom kros-validacije

control <- trainControl(method='cv', number=10)
mtry <- sqrt(ncol(train_rf[,-16]))
mtry_grid <- expand.grid(.mtry=seq(1, mtry + 1, 1))

set.seed(1)
mtry_train <- train(x = train_rf[,-16],
                    y = train_rf$high_revenue,
                    method='rf', 
                    metric='Accuracy', 
                    tuneGrid=mtry_grid, 
                    trControl=control)
print(mtry_train) # mtry 4
best_mtry <- mtry_train$bestTune$mtry

# Kreiranje drugog modela sa optimalnom vrednoscu parametra mtry

set.seed(1)
rf2 <- randomForest(high_revenue ~ ., data = train_rf, mtry = best_mtry)
print(rf2) #OOB error 11.75% 
           #ntree 500
           #mtry 4

rf2_pred <- predict(rf2, newdata = test_rf, type = "class")
cm2_rf <- table(actual = test_rf$high_revenue,
                predicted = rf2_pred)
cm2_rf

eval2_rf <- compute_eval_metrics(cm2_rf)
eval2_rf

data.frame(rbind(eval1_rf, eval2_rf), row.names = paste("RF_", 1:2))

# Provera znacajnosti varijabli

varImp(rf2)

