
n <- 100

set.seed(42)

d_sim <-
  tibble(
    x = rnorm(n, 0, 0.5),
    y = rnorm(n, 0, 0.5),
    group = "A"
  ) %>%
  bind_rows(
    tibble(
      x = rnorm(n, 1, 0.5),
      y = rnorm(n, 1, 0.5),
      group = "B")
  )


p_konf1 <-
  d_sim %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Ohne Kontrolle der Konfundierungsvariablen")

p_konf2 <-
  d_sim %>%
  ggplot(aes(x = x, y = y, color = group)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme(legend.position = c(0.8, 0.2))



