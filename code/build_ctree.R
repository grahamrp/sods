
# quick model

x <- readr::read_rds("data/prepared.rds")

library(rpart)
library(rpart.plot)

rp1 <- rpart(ConvertedSalary ~ . - Country, data = x)
rpart.plot(rp1)



c5_1 <- C5.0(CSBin ~ ., 
             data = mutate(x, CSBin = cut(ConvertedSalary, 
                           breaks = 10, labels = paste0("bin", 1:10))))

library(caret)
library(doParallel)
cl <- makeCluster(3)
registerDoParallel(cl)

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
rpart_1 <- train(ConvertedSalary ~ ., data = x, method = "ctree", na.action = na.pass)
ctree_1 <- rpart_1$finalModel
readr::write_rds(ctree_1, "ctree.rds")

plot(rpart_1)
plot(rpart_1$finalModel)
