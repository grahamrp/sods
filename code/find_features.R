library(dplyr)
library(readr)

sods <- read_rds("data/prepared.rds")

# Quick and dirty - chisquared test for all vars!
csp <- function(x) {
  chisq.test(table(sods2$ConvertedSalary, x))$p.value
}
sods2 <- sample_frac(sods, 0.1)
purrr::map(sods2, csp)

