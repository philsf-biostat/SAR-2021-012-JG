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
  legend.position = "top"
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
