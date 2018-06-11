library(caret)
library(dplyr)

sods <- read_rds("data/prepared.rds")
sbfCtrl <- sbfControl(functions = lmSBF)
sbf(x = select(sods, -ConvertedSalary), y = sods$ConvertedSalary, sbfControl = sbfCtrl)


ctrl <- trainControl(method = "cv", number = 10)
glmnet_1 <- train(ConvertedSalary ~ ., data = x, method = "lm",
                  na.action = na.pass)



x2 <- x[sample(nrow(x), size = 8000), ]
x2 <- mutate_if(x2, is.character, as.factor)

ctrl <- trainControl(method = "cv", number = 5)
glmnet_1 <- train(ConvertedSalary ~ ., 
                  data = select(x2, -Respondent, -nearZeroVar(x2)) %>% 
                    filter(complete.cases(x2)),
                  method = "glmnet", trControl = ctrl, 
                  #preProc = c("knnImpute"),
                  na.action = na.pass)
glmnet_1

nrow(x[complete.cases(x), ])
length(nearZeroVar(x, freqCut = 90/5))
