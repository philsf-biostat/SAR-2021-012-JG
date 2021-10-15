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
  total_receita = total_receita/1000000,
  num_votos = num_votos/100000,
  )

# labels ------------------------------------------------------------------

data.raw <- data.raw %>%
  set_variable_labels(
    # partido = "",
    total_receita = "Receita (milhão R$)",
    num_votos = "Votos (100k´)",
  )

# analytical dataset ------------------------------------------------------

analytical <- data.raw %>%
  # select analytic variables
  select(
    !starts_with(c("perc", "receita_")),
    -nome,
    -igreja,
    -categoria,
    -filiados,
    # -primeira,
  )

# mockup of analytical dataset for SAP and public SAR
analytical_mockup <- tibble( id = c( "1", "2", "3", "...", as.character(nrow(analytical)) ) ) %>%
  left_join(analytical %>% head(0), by = "id") %>%
  mutate_all(as.character) %>%
  replace(is.na(.), "")
