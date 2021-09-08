t <-
  t %>%
  rename(pc = nc) %>%
  mutate(fc = c(0, 3:0)) %>%
  mutate(nc = pc * fc)

# format the table
tab23 <-
  t %>%
  transmute(Hyp      = str_c("[", d1, " ", d2, " ", d3, " ", d4, "]"),
            `HA`   = pc,
            `HF` = fc,
            `HN`     = str_c(pc, " * ", fc, " = ", nc)) %>%
  kable()

print(tab23)
