# setup -------------------------------------------------------------------
height <- 8
width <- 8
units <- "cm"

# publication ready tables ------------------------------------------------

# Don't need to version these files on git
# tab_mod %>%
#   as_gt() %>%
#   as_rtf() %>%
#   writeLines(con = "report/SAR-2021-012-JG-v01-T2.rtf")

# save plots --------------------------------------------------------------

ggsave(filename = "figures/receitas.png", plot = gg.receitas, height = 16, width = 16, units = units)
ggsave(filename = "figures/receita_total.png", plot = gg.rec_total, height = height, width = width, units = units)
