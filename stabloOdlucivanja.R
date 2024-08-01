### PRVI ALGORITAM - DECISION TREE 

# Ucitavanje potrebnih biblioteka

library(rpart)
#install.packages('rpart.plot')
library(rpart.plot)
library(caret)
source('util.R')

# Ucitavanje sredjenog skupa podataka

movies_data <- readRDS("movies_final.RDS")

# Kreiranje test i train seta

set.seed(1)
train_indices <- createDataPartition(movies_data$high_revenue, p = 0.8, list = FALSE)
train_data <- movies_data[train_indices, ]
test_data <- movies_data[-train_indices, ]

prop.table(table(train_data$high_revenue))
prop.table(table(test_data$high_revenue))

# Cuvamo datasetove da bi ih koristili za naredne modele

saveRDS(train_data, "train.RDS")
saveRDS(test_data, "test.RDS")

# Kreiranje prvog modela sa predefinisanom vrednoscu parametara cp i minsplit

set.seed(1)
tree1 <- rpart(high_revenue ~ ., data = train_data, method = "class")
rpart.plot(tree1, extra = 104)


tree1_pred <- predict(tree1, newdata = test_data, type = "class") #kreiranje predikcija za prvi model
cm1_dt <- table(actual = test_data$high_revenue,
                predicted = tree1_pred) #matrica konfuzije
cm1_dt

eval1_dt <- compute_eval_metrics(cm1_dt) #izracunavanje evaluacionih metrika
eval1_dt

# Kreiranje drugog modela sa nasumicno izabranim vrednostima za parametre cp i minsplit

set.seed(1)
tree2 <- rpart(high_revenue ~ ., data = train_data, 
               control = rpart.control(minsplit = 10, cp = 0.001))

rpart.plot(tree2, extra = 104)

tree2_pred <- predict(tree2, newdata = test_data, type = "class")
cm2_dt <- table(actual = test_data$high_revenue,
                predicted = tree2_pred)
cm2_dt

eval2_dt <- compute_eval_metrics(cm2_dt)
eval2_dt


# Pravljenje predikcija na train setu zbog provere overfitting-a

tree2_pred_train <- predict(tree2, newdata = train_data, type = "class")
cm2_train_dt <- table(actual = train_data$high_revenue,
                      predicted = tree2_pred_train)
cm2_train_dt

eval2_train_dt <- compute_eval_metrics(cm2_train_dt)
eval2_train_dt

#uporedjivanje rezultata metrika
data.frame(rbind(eval2_dt, eval2_train_dt), row.names = paste("Eval ", 1:2)) #dobili smo mnogo bolje vrednosti metrika na train setu sto znaci
                                                                             #da je ovaj model podlegao overfitting-u

# Primena postupka kros-validacija kako bi se pronasla optimalna vrednost za cp

tr_ctrl <- trainControl(method = "cv", number = 10)
cp_grid <- expand.grid( .cp = seq(0.001, 0.02, 0.0005))

set.seed(1)
tree_cv <- train(x = train_data[,-16],
                 y = train_data$high_revenue,
                 method = 'rpart', 
                 trControl = tr_ctrl,
                 tuneGrid = cp_grid, 
                 minsplit = 10)

tree_cv
plot(tree_cv)
best_cp <- tree_cv$bestTune$cp #dobijamo 0.002 kao najbolju vrednost za cp

# Kreiranje treceg modela sa optimalnom vrednoscu parametra cp

set.seed(1)
tree3 <- rpart(high_revenue ~ ., data = train_data, 
               control = rpart.control(minsplit = 10, cp = best_cp))

rpart.plot(tree3, extra = 104)

tree3_pred <- predict(tree3, newdata = test_data, type = "class")
cm3_dt <- table(actual = test_data$high_revenue,
                predicted = tree3_pred)
cm3_dt

eval3_dt <- compute_eval_metrics(cm3_dt)
eval3_dt

# Pravimo predikcije na trening setu i za ovaj model

tree3_pred_train <- predict(tree3, newdata = train_data, type = "class")
cm3_train_dt <- table(actual = train_data$high_revenue,
                      predicted = tree3_pred_train)
cm3_train_dt

eval3_train_dt <- compute_eval_metrics(cm3_train_dt)
eval3_train_dt

data.frame(rbind(eval3_dt, eval3_train_dt), row.names = paste("Eval ", 1:2)) # dobili smo jos jedan overfitovan model

# Uzimamo manji raspon za cp prilikom primene kros-validacije da bi se izbegla pojava overfitting-a

cp_grid_2 <- expand.grid( .cp = seq(0.005, 0.02, 0.0005))

set.seed(1)
tree_cv_2 <- train(x = train_data[,-16],
                   y = train_data$high_revenue,
                   method = 'rpart', 
                   trControl = tr_ctrl,
                   tuneGrid = cp_grid_2, 
                   minsplit = 20)

tree_cv_2
plot(tree_cv_2)
best_cp_2 <- tree_cv_2$bestTune$cp 

# Kreiranje cetvrtog modela sa novom optimalnom vrednoscu parametra cp

set.seed(1)
tree4 <- rpart(high_revenue ~ ., data = train_data, 
               control = rpart.control(minsplit = 20, cp = best_cp_2))

rpart.plot(tree4, extra = 104)

tree4_pred <- predict(tree4, newdata = test_data, type = "class")
cm4_dt <- table(actual = test_data$high_revenue,
                predicted = tree4_pred)
cm4_dt

eval4_dt <- compute_eval_metrics(cm4_dt)
eval4_dt

data.frame(rbind(eval1_dt, eval4_dt), row.names = paste("Tree_", 1:2))

# Provera znacajnosti atributa

tree4$variable.importance













