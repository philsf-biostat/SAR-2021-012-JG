# setup -------------------------------------------------------------------
# library(caret)

model.dat <- analytical %>%
  # cbind(predict(dummyVars(formula = ~ partido + uf, data = analytical), analytical)) %>%
  # select(-id, -partido, -uf) %>%
  select(-id) %>%
  drop_na()

# raw estimate ------------------------------------------------------------

m.min <- glm(
  formula = evangelico ~ total_receita - 1,
  family = binomial,
  data = model.dat)

# stepwise ----------------------------------------------------------------

k <- qchisq(.20, 1, lower.tail = FALSE) # use p < .20 for selection
m.sat <- glm(
  formula = evangelico ~ .,
  family = binomial,
  data = model.dat)
f.lower <- formula(m.min)
f.upper <- formula(m.sat)

# realizar o stepwise (modelo final salvo à parte)
# m.step <- step(m.min, scope = list(lower = f.lower, upper = f.upper), direction = "forward", k = k)

# adjusted ----------------------------------------------------------------

# final model - forward
m.final <- glm(
  formula = evangelico ~ total_receita + partido + sexo + capilaridade + num_votos - 1,
  family = binomial,
  data = model.dat)

# final model - dummy vars
# glm(formula = evangelico ~ total_receita + partidoPRB + posicao + 
#       sexo + partidoPHS + partidoPSC + num_votos + decil_filiados + 
#       capilaridade + ufRJ + partidoAVANTE + ufAC + partidoPSOL + 
#       ufGO + `partidoPC do B` + partidoPPS + ufSC, family = binomial, 
#     data = model.dat)

# diagnostics -------------------------------------------------------------

m.aic <- AIC(m.min, m.final)
m.final %>%
  summary()
# m.final %>%
#   tidy()
# m.final %>%
#   glance()

# table -------------------------------------------------------------------

tab_mod.min <- m.min %>%
  tbl_regression(exp = TRUE) %>%
  bold_labels()
tab_mod.final <- m.final %>%
  tbl_regression(exp = TRUE) %>%
  bold_labels()
tab_mod <- tbl_merge(list(
  tab_mod.min,
  tab_mod.final
), tab_spanner = c("Bruta (não ajustada)", "Ajustada"))
