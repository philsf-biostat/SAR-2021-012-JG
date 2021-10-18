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
    primeira = factor(primeira, labels = c("Primeiro mandato", "Reeleito")),
    sexo = factor(sexo, labels = c("Masculino", "Feminino")),
    evangelico = factor(evangelico, labels = c("Outros", "Evangélico")),
    igreja = fct_rev(fct_infreq(igreja)),
    total_receita = total_receita/1000000,
    num_votos = num_votos/100000,
    capilaridade = capilaridade*10,
  )

# labels ------------------------------------------------------------------

data.raw <- data.raw %>%
  set_variable_labels(
    partido = "Partido",
    uf = "UF",
    capilaridade = "Capilaridade (10%)",
    primeira = "Releição vs primeiro mandato",
    sexo = "Sexo",
    evangelico = "Evangélico",
    num_votos = "Votos (100k)",
    decil_filiados = "Decil do núm. de filiados",
    decil_deputados = "Decil do núm. de deputados",
    total_receita = "Receita total (milhão R$)",
    posicao = "Índice de Power e Silveira-Rodrigues",
    igreja = "Nome da Igreja",
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
