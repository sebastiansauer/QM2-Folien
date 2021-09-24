n_blue  <- function(x) rowSums(x == "B")
n_white <- function(x) rowSums(x == "W")

# make the data
t <-
  tibble(d1 = rep(c("W", "B"), times = c(1, 4)),
         d2 = rep(c("W", "B"), times = c(2, 3)),
         d3 = rep(c("W", "B"), times = c(3, 2)),
         d4 = rep(c("W", "B"), times = c(4, 1))) %>%
  mutate(blue1 = n_blue(.),
         white = n_white(.),
         blue2 = n_blue(.)) %>%
  mutate(product = blue1 * white * blue2)

# format the table
tab21 <-
t %>%
  transmute(Hypothese = str_c("[", d1, " ", d2, " ", d3, " ", d4, "]"),
            `Häufigkeit BWB` = str_c(blue1, " * ", white, " * ", blue2, " = ", product)) %>%
  kable()
  # flextable() %>%
  # width(j = 1:2, width = c(1, 2)) %>%
  # align(align = "center", part = "all")

#tab21 <- autofit(tab21)
print(tab21)
