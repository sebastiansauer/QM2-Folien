tab24 <-
t %>%
  rename(ways = product) %>%
  mutate(p = blue1 / 4) %>%
  mutate(pl = ways / sum(ways)) %>%
  transmute(`Hyp` = str_c("[", d1, " ", d2, " ", d3, " ", d4, "]"),
            p                      = p,
            `AP` = ways,
            `Pl`         = pl) %>%

  # format for the table
  kable()

print(tab24)
