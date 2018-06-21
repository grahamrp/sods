# Use caret rfe to find good features from the remaining set
library(caret)
library(dplyr)
library(readr)

sods <- read_rds("data/prepared.rds")

subsets <- c(3, 5, 7, 9)
set.seed(100)

ctrl <- rfeControl(functions = treebagFuncs,
                   method = "repeatedcv",
                   number = 5,
                   repeats = 5,
                   verbose = FALSE)

rfProfile <- rfe(x = select(sods, -LogSalary), y = sods$LogSalary,
                 sizes = subsets,
                 rfeControl = ctrl)

# rfProfile
# predictors(rfProfile)
# ggplot(rfProfile, metric = "RMSE")
# varImp(rfProfile)

# Use top 8 but get rid of vars that look like they are tracking the same thing:
selectedVars <- rfProfile$variables %>% 
  group_by(var) %>% 
  summarise(Overall = mean(Overall)) %>% 
  arrange(desc(Overall)) %>% 
  slice(1:8) %>% 
  filter(!var %in% c("CurrencySymbol", "YearsCoding")) %>% 
  pull(var)

sods <- select(sods, one_of(selectedVars, "LogSalary"))
write_rds(sods, "data/pruned.rds")
