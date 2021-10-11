# setup -------------------------------------------------------------------
# library(data.table)
library(tidyverse)
# library(readxl)
# library(lubridate)
library(labelled)

# data loading ------------------------------------------------------------
set.seed(42)
# data.raw <- tibble(id=gl(2, 10), group = gl(2, 10), outcome = rnorm(20))
data.raw <- read_csv("dataset/eleitos_2018.csv") %>%
  janitor::clean_names()


# data cleaning -----------------------------------------------------------

data.raw <- data.raw %>%
#   filter() %>%
  select(
    -eleito,
  )

# data wrangling ----------------------------------------------------------

data.raw <- data.raw %>%
  mutate(
  id = as.character(id),
  )

# labels ------------------------------------------------------------------

data.raw <- data.raw %>%
  set_variable_labels(
    # partido = "",
  )

# analytical dataset ------------------------------------------------------

analytical <- data.raw %>%
  # select analytic variables
  select(
    !starts_with("perc"),
    -nome,
    -igreja,
    -categoria,
    -filiados,
    # -primeira,
    -total_receita
  )

# mockup of analytical dataset for SAP and public SAR
analytical_mockup <- tibble( id = c( "1", "2", "3", "...", as.character(nrow(analytical)) ) ) %>%
  left_join(analytical %>% head(0), by = "id") %>%
  mutate_all(as.character) %>%
  replace(is.na(.), "")
