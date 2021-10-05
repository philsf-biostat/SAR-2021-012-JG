# setup -------------------------------------------------------------------
# library(data.table)
library(tidyverse)
library(readxl)
# library(lubridate)
library(labelled)

# data loading ------------------------------------------------------------
set.seed(42)
data.raw <- tibble(id=gl(2, 10), group = gl(2, 10), outcome = rnorm(20))
# data.raw <- read_excel("dataset/file.xlsx") %>%
#   janitor::clean_names()


# data cleaning -----------------------------------------------------------

# data.raw <- data.raw %>%
#   filter() %>%
#   select()

# data wrangling ----------------------------------------------------------

# data.raw <- data.raw %>%
#   mutate(
#
#   )

# labels ------------------------------------------------------------------

data.raw <- data.raw %>%
  set_variable_labels(
    group = "Study group",
    outcome = "Study outcome",
  )

# analytical dataset ------------------------------------------------------

analytical <- data.raw %>%
  # select analytic variables
  select(
    id,
    group,
    outcome,
  )

# mockup of analytical dataset for SAP and public SAR
analytical_mockup <- tibble( id = c( "1", "2", "3", "...", as.character(nrow(analytical)) ) ) %>%
  left_join(analytical %>% head(0), by = "id") %>%
  mutate_all(as.character) %>%
  replace(is.na(.), "")
