# setup -------------------------------------------------------------------
# library(gt)
# library(gtsummary)
# library(moderndive)
# library(broom)
# library(broom.mixed)
library(caret)

# raw estimate ------------------------------------------------------------

model.dat <- analytical %>%
  # cbind(predict(dummyVars(formula = ~ partido + uf, data = analytical), analytical)) %>%
  # select(-id, -partido, -uf) %>%
  select(-id) %>%
  drop_na()

k <- qchisq(.20, 1, lower.tail = FALSE) # use p < .20 for selection
m.min <- glm(formula = evangelico ~ total_receita, family = binomial, 
             data = model.dat)
m.sat <- glm(formula = evangelico ~ ., family = binomial, data = model.dat)
f.lower <- formula(m.min)
f.upper <- formula(m.sat)
# m.step <- step(m.min, scope = list(lower = f.lower, upper = f.upper), direction = "forward", k = k)

# final model - forward
m.final <- glm(formula = evangelico ~ total_receita + partido + sexo + capilaridade + num_votos, family = binomial, data = model.dat)

# final model - dummy vars
# glm(formula = evangelico ~ total_receita + partidoPRB + posicao + 
#       sexo + partidoPHS + partidoPSC + num_votos + decil_filiados + 
#       capilaridade + ufRJ + partidoAVANTE + ufAC + partidoPSOL + 
#       ufGO + `partidoPC do B` + partidoPPS + ufSC, family = binomial, 
#     data = model.dat)


# adjusted ----------------------------------------------------------------

