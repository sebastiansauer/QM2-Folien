t <-
  t %>%
  mutate(nc = blue1 * product)

# format the table
tab22 <-
t %>%
  transmute(Hyp            = str_c("[", d1, " ", d2, " ", d3, " ", d4, "]"),
            `PB` = blue1,
            `HA`        = product,
            `HN`           = str_c(blue1, " * ", product, " = ", nc)) %>%
kbl()

print(tab22)
