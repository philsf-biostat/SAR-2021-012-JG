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


# tables ------------------------------------------------------------------

tab_desc <- analytical %>%
  # select
  select(-id, ) %>%
  tbl_summary(
    # by = group
  ) %>%
  # modify_caption(caption = "**Tabela 1** Características demográficas") %>%
  # modify_header(label ~ "**Características dos pacientes**") %>%
  bold_labels() %>%
  modify_table_styling(columns = "label", align = "c")

