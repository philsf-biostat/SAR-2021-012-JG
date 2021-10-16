# setup -------------------------------------------------------------------

# library(Hmisc) # describe
# library(skimr) # skim
# library(tableone)
# library(gmodels) # CrossTable
library(gtsummary)
library(gt)
# library(effectsize)

# setup gtsummary theme
theme_gtsummary_mean_sd() # mean/sd
theme_gtsummary_language(language = "pt") # traduzir

# exploratory -------------------------------------------------------------

# overall description
# analytical %>%
#   skimr::skim()

# minimum detectable effect size
# interpret_d(0.5)

analytical %>%
  group_by(evangelico) %>%
  summarise(cv = sd(num_votos, na.rm = TRUE)/mean(num_votos, na.rm = TRUE)*100)

analytical %>%
  group_by(evangelico) %>%
  transmute(total_receita*1000000) %>%
  skimr::skim()

analytical %>%
  filter(num_votos > 5)

# tables ------------------------------------------------------------------

tab_desc <- analytical %>%
  # select
  select(-id, -partido, -uf) %>%
  select(total_receita, everything()) %>%
  tbl_summary(
    by = evangelico,
  ) %>%
  # modify_caption(caption = "**Tabela 1** Características demográficas") %>%
  # modify_header(label ~ "**Características dos pacientes**") %>%
  bold_labels() %>%
  modify_table_styling(columns = "label", align = "c")

data.raw %>%
  select(evangelico, igreja) %>%
  tbl_summary(missing = "no")

analytical %>%
  filter(evangelico == "Evangélico") %>%
  mutate(partido = fct_infreq(partido)) %>%
  tbl_summary(include = partido)
