library(ggplot2)
library(caret)
library(readr)
library(doParallel)

# Register parallel cluster
cl <- makeCluster(2)
registerDoParallel(cl)

sods <- read_rds("data/pruned.rds")

# Set up training strategy
ctrl <- trainControl(method = "cv", number = 5)

modelMetrics <- data.frame()
# Train xgboost
xgb1 <- train(LogSalary ~ ., data = sods,
              method = "xgbTree", trControl = ctrl)
modelMetrics <- rbind(modelMetrics, c(type = "xgb",
with(xgb1, results[best(results, maximize = FALSE, metric = "RMSE"), c("RMSE", "RMSESD")])), stringsAsFactors = F)

# Train glmnet
glmnet1 <- train(LogSalary ~ ., data = sods,
                 method = "glmnet", trControl = ctrl)
modelMetrics <- rbind(modelMetrics, c(type = "glmnet",
                                      with(glmnet1, results[best(results, maximize = FALSE, metric = "RMSE"), c("RMSE", "RMSESD")])), stringsAsFactors = F)
plot(glmnet1)

# Train ctree2
ctree1 <- train(LogSalary ~ ., data = sods,
              method = "ctree2", trControl = ctrl)
modelMetrics <- rbind(modelMetrics, c(type = "ctree",
                                      with(ctree1, results[best(results, maximize = FALSE, metric = "RMSE"), c("RMSE", "RMSESD")])), stringsAsFactors = F)

# Train enet
enet1 <- train(LogSalary ~ ., data = sods,
                method = "enet", trControl = ctrl)
modelMetrics <- rbind(modelMetrics, c(type = "enet",
                                      with(enet1, results[best(results, maximize = FALSE, metric = "RMSE"), c("RMSE", "RMSESD")])), stringsAsFactors = F)


xgbLinear1 <- train(LogSalary ~ ., data = sods,
               method = "xgbLinear", trControl = ctrl)
modelMetrics <- rbind(modelMetrics, c(type = "xgbLinear",
                                      with(xgbLinear1, results[best(results, maximize = FALSE, metric = "RMSE"), c("RMSE", "RMSESD")])), stringsAsFactors = F)


pls1 <- train(LogSalary ~ ., data = sods,
              method = "pls", trControl = ctrl)
modelMetrics <- rbind(modelMetrics, c(type = "pls",
                                      with(pls1, results[best(results, maximize = FALSE, metric = "RMSE"), c("RMSE", "RMSESD")])), stringsAsFactors = F)

svmRadial1 <- train(LogSalary ~ ., data = sods,
              method = "svmRadial", trControl = ctrl)
modelMetrics <- rbind(modelMetrics, c(type = "svmRadial",
                                      with(svmRadial1, results[best(results, maximize = FALSE, metric = "RMSE"), c("RMSE", "RMSESD")])), stringsAsFactors = F)


# Plot model RMSE vs. SD
qplot(RMSESD, RMSE, data = modelMetrics) + geom_label(aes(label = type))

# Closer look at xgbTree vs xgbLinear
qplot(xgb1$results$RMSE, geom = "density") + 
  geom_density(data = data.frame(RMSE = xgbLinear1$results$RMSE), aes(x = RMSE), col = "red")

# Go for xgbLinear as it has lower variance
finalModel <- xgbLinear1
write_rds(finalModel, "data/finalModel_xgbLinear_caret.rds")

