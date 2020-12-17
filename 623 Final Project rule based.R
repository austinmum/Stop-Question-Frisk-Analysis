sqf <- read.csv(file = "AllYearsFrisked.csv")
head(sqf)
library(dplyr)
glimpse(sqf)
sqf_clean <- mutate_if(sqf, is.character, as.factor)
glimpse(sqf_clean)
create_train_test <- function(data, size= 0.8, train = TRUE){
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1:total_row
  if(train == TRUE){
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}
data_train <- create_train_test(sqf_clean, 0.8, train = TRUE)
data_test <- create_train_test(sqf_clean, 0.8, train = FALSE)
dim(data_train)
dim(data_test)
library(rpart)
library(rpart.plot)
fit <- rpart(Race~., data = data_train, method = 'class')
rpart.plot(fit, box.palette = "grey", extra = 106)
predict_unseen <- predict(fit, data_test, type = "class")
table_mat <- table(data_test$Race, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat))/sum(table_mat)
print(paste("Accuracy Test", accuracy_Test))
accuracy_tune <- function(fit){
  predict_unseen <- predict(fit, data_test, type = "class")
  table_mat <- table(data_test$Race, predict_unseen)
  accuracy_Test <- sum(diag(table_mat))/sum(table_mat)
  print(paste("Accuracy Test", accuracy_Test))
  rpart.plot(fit, box.palette = "grey", extra = 106)
}

control <- rpart.control(cp = 0.00172)
tune_fit <- rpart(Race~., data = data_train, method = "class", control = control)
accuracy_tune(tune_fit)
accuracy <- function(truth, prediction) {
  tbl <- table(truth, prediction)
  sum(diag(tbl))/sum(tbl)
}
n_train <- floor(nrow(sqf_clean) * .66)
n_train
train_id <- sample(seq_len(nrow(sqf_clean)), n_train)
head(train_id)
train <- sqf_clean %>% slice(train_id)
test <- sqf_clean %>% slice(-train_id) %>% select(-Race)
test_type <- sqf_clean %>% slice(-train_id) %>% pull(Race)

tree <- train %>% rpart(Race ~., data = ., control = rpart.control(minsplit = 2))
accuracy(train$Race, predict(tree, train, type="class"))
predict(tree, train, type="class")
rulesFit <- sqf_clean %>% train(Race ~ .,
                                method = "PART",
                                data = .,
                                tuneLength = 5,
                                trControl = trainControl(method = "cv", indexOut = train),
                                na.action = na.pass)
rulesFit
