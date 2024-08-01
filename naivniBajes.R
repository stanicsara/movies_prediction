############## DRUGI ALGORITAM - NAIVNI BAJES #################


# Ucitavanje biblioteka

library(bnlearn)
library(nortest)
library(e1071)
library(caret)
library(DMwR2)
#install.packages('ROSE')
library(ROSE)
#installed.packages("naivebayes")
library(naivebayes)
#install.packages('pROC')
library(pROC)
source('util.R')

# Ucitavanje trening i test seta 

test_nb <- readRDS("test.RDS")
train_nb <- readRDS("train.RDS")

# Provera normalnosti numerickih varijabli

num_vars <- c(1:8)

apply(train_nb[,num_vars], 2, ad.test)
apply(test_nb[,num_vars], 2, ad.test)
##nemaju normalnu raspodelu

# Diskretizacija varijabli na train setu

to_discretize <- c(1:8)

transformed_data_train <- discretize(train_nb[,to_discretize],
                                     method = "quantile",
                                     breaks = 5)
summary(transformed_data_train) #dobijamo neujednacene vrenosti za release_year, point, runtime, release_month, pa cemo promeniti broj intervala

# Kreiramo histograme za ove varijable kako bi lakse odredili broj intervala

ggplot(train_nb,
       aes(x = release_year)) +
  geom_histogram() + 
  theme_minimal()

ggplot(train_nb,
       aes(x = point)) +
  geom_histogram() + 
  theme_minimal()

ggplot(train_nb,
       aes(x = runtime)) +
  geom_histogram() + 
  theme_minimal()

ggplot(train_nb,
       aes(x = release_month)) +
  geom_histogram() + 
  theme_minimal()

transformed_data_train <- discretize(train_nb[,to_discretize],
                                     method = "quantile",
                                     breaks = c(3,3,5,5,5,3,5,3))
summary(transformed_data_train)

# Diskretizacija varijabli na test setu

transformed_data_test <- discretize(test_nb[,to_discretize],
                                    method = "quantile",
                                    breaks = 5)
summary(transformed_data_test) 

ggplot(test_nb,
       aes(x = release_year)) +
  geom_histogram() + 
  theme_minimal()

ggplot(test_nb,
       aes(x = point)) +
  geom_histogram() + 
  theme_minimal()

ggplot(test_nb,
       aes(x = runtime)) +
  geom_histogram() + 
  theme_minimal()

ggplot(test_nb,
       aes(x = release_month)) +
  geom_histogram() + 
  theme_minimal()

transformed_data_test <- discretize(test_nb[,to_discretize],
                                    method = "quantile",
                                    breaks = c(3,2,5,5,5,5,5,5))
summary(transformed_data_test)

# Transformacija train seta

to_add_col_train <- setdiff(colnames(train_nb), colnames(transformed_data_train))
to_add_col_train
train_nb <- cbind(transformed_data_train,
                  train_nb[,to_add_col_train])

# Transformacija test seta

to_add_col_test <- setdiff(colnames(test_nb), colnames(transformed_data_test))
test_nb <- cbind(transformed_data_test,
                 test_nb[,to_add_col_test])

# Kreiranje prvog modela sa predefinisanom vrednoscu praga verovatnoce i na originalnom skupu podataka

set.seed(1)
nb1 <- naiveBayes(high_revenue ~ ., data = train_nb)
print(nb1)

nb1_pred <- predict(nb1, newdata = test_nb, type = "class")
cm1_nb <- table(actual = test_nb$high_revenue,
                predicted = nb1_pred)
cm1_nb

eval1_nb <- compute_eval_metrics(cm1_nb)
eval1_nb

prop.table(table(train_nb$high_revenue))

# Balansiranje skupa podataka


ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")
#down-sampling

set.seed(1)
down_train <- train(x = train_nb[,-16],
                    y = train_nb$high_revenue,
                    method = "naive_bayes",
                    metric = "ROC",
                    trControl = ctrl)

#up-sampling

ctrl$sampling <- "up"
set.seed(1)
up_train <- train(x = train_nb[,-16],
                  y = train_nb$high_revenue,
                  method = "naive_bayes",
                  metric = "ROC",
                  trControl = ctrl)

#ROSE

ctrl$sampling <- "rose"
set.seed(1)
rose_train <- train(x = train_nb[,-16],
                    y = train_nb$high_revenue,
                    method = "naive_bayes",
                    metric = "ROC",
                    trControl = ctrl)

#originalni skup podataka

ctrl$sampling <- NULL
set.seed(1)
original_train <- train(x = train_nb[,-16],
                        y = train_nb$high_revenue,
                        method = "naive_bayes",
                        metric = "ROC",
                        trControl = ctrl)


inside_models <- list(original = original_train,
                      down = down_train,
                      up = up_train,
                      ROSE = rose_train)

inside_resampling <- resamples(inside_models)
summary(inside_resampling, metric = "ROC")

# ROC 
#              Min.    1st Qu.   Median     Mean     3rd Qu.    Max.    NA's
# original 0.8688005 0.8863971 0.8918772 0.8933858 0.9023702 0.9186448    0
# down     0.8673587 0.8866045 0.8917849 0.8932292 0.9019233 0.9179642    0
# up       0.8694694 0.8871324 0.8920069 0.8934282 0.9017933 0.9190254    0
# ROSE     0.8691811 0.8846785 0.8917964 0.8932275 0.9009188 0.9169493    0

##Biramo up-sampling metodu za balansiranje podataka

set.seed(1)
train_up <- upSample(x = train_nb[,-16], y = train_nb$high_revenue)
colnames(train_up)[16] <- "high_revenue"
str(train_up)
table(train_up$high_revenue)

# Kreiranje drugog modela nad balansiranim skupom podataka i predefinisanom vrednoscu praga verovatnoce

set.seed(1)
nb2 <- naiveBayes(high_revenue ~ ., data = train_up)
print(nb2)

nb2_pred <- predict(nb2, newdata = test_nb, type = "class")
cm2_nb <- table(actual = test_nb$high_revenue,
                predicted = nb2_pred)
cm2_nb

eval2_nb <- compute_eval_metrics(cm2_nb)
eval2_nb

# Raw predikcije

nb2_pred_prob <- predict(nb2, test_nb, type = 'raw')
head(nb2_pred_prob)

# Kreiranje ROC krive

nb2_roc <- roc(response = as.integer(test_nb$high_revenue),
               predictor = nb2_pred_prob[,2],
               levels = c(1,2))
nb2_roc$auc #0.76

# Odredjivanje optimalne vrednosti praga verovatnoce koja maksimizira sumu metrika specificity i sensitivity

plot.roc(nb2_roc,
         print.thres = 'best',
         print.thres.best.method = "youden") #0.415 (0.600 0.788)

# Pravljenje predikcija sa optimalnom vrednoscu praga verovatnoce

nb2_pred2 <- ifelse(test = nb2_pred_prob[,2] >= 0.415,
                    yes = "Yes",
                    no = "No")
nb2_pred2 <- as.factor(nb2_pred2)
cm3_nb <- table(actual = test_nb$high_revenue,
                predicted = nb2_pred2)
cm3_nb

eval3_nb <- compute_eval_metrics(cm3_nb)
eval3_nb

data.frame(rbind(eval1_nb, eval2_nb, eval3_nb), row.names = paste("NB_", 1:3))

