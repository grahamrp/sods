library(dplyr)
library(readr)
library(stringr)
library(tidyr)

x <- read_csv("data/developer_survey_2018/survey_results_public.csv")

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# filter for just full-time devs with a salary
x <- x %>% 
  filter(Student == "No",
         Employment == "Employed full-time",
         LastNewJob != "I've never had a job",
         !is.na(ConvertedSalary),
         ConvertedSalary > 0)

# Remove superfluous cols
x <- x %>% 
  select(-one_of("Student", "Employment", "Currency", "RaceEthnicity")) %>% 
  select(-matches("Desire")) %>% 
  select(-Salary)

# Find compound cols and convert them into dummifed cols

sc_count <- unlist(lapply(x, FUN = function(x){
  sum(str_detect(x, ";"), na.rm = T)
}))

# Get multi vars where there are lots of multi responses
multi_vars <- names(sc_count[sc_count > 10000])

stack_multi <- function(data, columns) {
data %>%
    dplyr::select(Respondent, one_of(columns)) %>%
    tidyr::gather(column, answer, -Respondent) %>%
    dplyr::filter(!is.na(answer)) %>%
    tidyr::unnest(answer = stringr::str_split(answer, ";"))
}

expanded_multi <- stack_multi(x, multi_vars)

multi_varnames <- # Make lookup for valid varnames before spreading
  expanded_multi %>% 
  select(-Respondent) %>% 
  distinct() %>% 
  group_by(column) %>% 
  mutate(encoded_answer = paste0(column, row_number()))

# Convert multi answers into dummified grid
multi_grid <- expanded_multi %>% 
  ungroup() %>% 
  left_join(multi_varnames) %>%
  mutate(val = 1) %>% 
  select(-column, -answer) %>% 
  spread(key = encoded_answer, value = val, fill = 0)


# drop original multi-vars and add dummified versions
x <- x %>% 
  select(-one_of(multi_vars)) %>% 
  left_join(multi_grid)

# Drop near-zero variance cols
x <- x[, -caret::nearZeroVar(x)]

# Drop vars with lots of missing values
propMissing <- unlist(lapply(x, function(x) sum(is.na(x)) / length(x)))
x <- select(x, -one_of(names(propMissing[propMissing > 0.15])))

# Convert to factors, remove id
x <- mutate_if(x, is.character, as.factor)
x <- select(x, -Respondent)

# lump factors
library(forcats)
x <- x %>% mutate_at(.vars = vars(Country, CurrencySymbol, VersionControl),
          fct_lump, prop = 0.01)

# Impute missing values (just fill with mode!)
x <- as.data.frame(lapply(x, FUN = function(.x) { .x[is.na(.x)] <- Mode(.x); .x }))

# Convert salaray to log salary
x <- mutate(x, LogSalary = log(ConvertedSalary),
            ConvertedSalary = NULL)


write_rds(x, path = "data/prepared.rds")

# tidy up
rm(expanded_multi, multi_grid, multi_varnames, stack_multi, multi_vars, sc_count, x,
   propMissing, csp, Mode)
