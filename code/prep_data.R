library(dplyr)
library(readr)
library(stringr)
library(tidyr)

x <- read_csv("data/developer_survey_2018/survey_results_public.csv")

# filter for just full-time devs with a salary
x <- x %>% 
  filter(Student == "No",
         Employment == "Employed full-time",
         !is.na(ConvertedSalary))

# Remove superfluous cols
x <- x %>% 
  select(-one_of("Student", "Employment")) %>% 
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
  mutate(val = 1L) %>% 
  select(-column, -answer) %>% 
  spread(key = encoded_answer, value = val, fill = 0L)


# drop original multi-vars and add dummified versions
x <- x %>% 
  select(-one_of(multi_vars)) %>% 
  left_join(multi_grid)

# drop near-zero variance cols

x <- x[, -caret::nearZeroVar(x)]

# Convert to factors, remove id
x <- mutate_if(x, is.character, as.factor)
x <- select(x, -Respondent)

# lump factors
library(forcats)
x <- x %>% mutate_at(.vars = vars(Country, CurrencySymbol, VersionControl, AdBlockerReasons, RaceEthnicity),
          fct_lump, prop = 0.01)

write_rds(x, path = "data/prepared.rds")

# tidy up
rm(expanded_multi, multi_grid, multi_varnames, stack_multi, multi_vars, sc_count, x)
