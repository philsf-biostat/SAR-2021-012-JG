# setup -------------------------------------------------------------------
# library(ggplot2)
# library(survminer)

ff.col <- "steelblue" # good for single groups scale fill/color brewer
ff.pal <- "Paired"    # good for binary groups scale fill/color brewer

# Theme setting (less is more)
theme_set(
  theme_classic()
)
theme_update(
  legend.position = "top",
  legend.title = element_blank(),
)

gg <- ggplot(analytical) +
  scale_color_brewer(palette = ff.pal) +
  scale_fill_brewer(palette = ff.pal)

# plots -------------------------------------------------------------------

gg.receitas <- data.raw %>%
  rename(receita_total = total_receita) %>%
  select(contains("receita")) %>%
  pivot_longer(everything(), names_prefix = "receita_") %>%
  ggplot(aes(value)) +
  geom_histogram(binwidth = 100000, fill = ff.col) +
  facet_wrap(~name) +
  labs(x = "", y = "", subtitle = "Distribuição das receitas, por origem")

# data.raw %>%
#   filter(!is.na(igreja)) %>%
#   ggplot(aes(igreja)) +
#   geom_bar(fill = ff.col) +
#   coord_flip() +
#   labs(x = "", y = "")

# analytical %>%
#   select_if(is.factor) %>%
#   pivot_longer(c(primeira, sexo)) %>%
#   ggplot(aes(value, fill = evangelico)) +
#   geom_bar(position = "fill") +
#   coord_flip() +
#   facet_wrap(~name) +
#   scale_color_brewer(palette = ff.pal) +
#   scale_fill_brewer(palette = ff.pal)

# gg +
#   geom_density(aes(num_votos, fill = evangelico), alpha = .6) +
#   xlab(attr(analytical$num_votos, "label")) + ylab("")

# gg +
#   geom_hline(yintercept = mean(analytical$total_receita), lty = 4, size = .2) +
#   geom_point(aes(num_votos, total_receita, col = evangelico)) +
#   # geom_vline(xintercept = 4, col = "blue") +
#   geom_vline(xintercept = 5, col = "red") +
#   xlab(attr(analytical$num_votos, "label")) +
#   ylab(attr(analytical$total_receita, "label"))

gg.rec_total <- gg +
  geom_histogram(aes(total_receita), binwidth = .5, fill = ff.col) +
  xlab(attr(analytical$total_receita, "label")) +
  ylab("") +
  facet_wrap(~ evangelico)
