globe_qa_18 <-
  quap(
    alist(
      w ~ dbinom(9 * 2, p),
      p ~ dunif(0, 1)
    ), data = list(w = 6 * 2))

globe_qa_36 <-
  quap(
    alist(
      w ~ dbinom(9 * 4, p),
      p ~ dunif(0, 1)
    ), data = list(w = 6 * 4))


n_grid <- 100

plot243 <-
# wrangle
tibble(w = c(6, 12, 24),
       n = c(9, 18, 36),
       s = c(.16, .11, .08)) %>%
  expand(nesting(w, n, s),
         p_grid = seq(from = 0, to = 1, length.out = n_grid)) %>%
  mutate(prior = 1,
         m     = .67)  %>%
  mutate(likelihood = dbinom(w, size = n, prob = p_grid)) %>%
  mutate(unstd_grid_posterior = likelihood * prior,
         unstd_quad_posterior = dnorm(p_grid, m, s)) %>%
  group_by(w) %>%
  mutate(grid_posterior = unstd_grid_posterior / sum(unstd_grid_posterior),
         quad_posterior = unstd_quad_posterior / sum(unstd_quad_posterior),
         n              = str_c("n = ", n)) %>%
  mutate(n = factor(n, levels = c("n = 9", "n = 18", "n = 36"))) %>%

  # plot
  ggplot(aes(x = p_grid)) +
  geom_line(aes(y = grid_posterior)) +
  geom_line(aes(y = quad_posterior),
            color = "grey50") +
  labs(x = "p(W)",
       y = "Post") +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ n, scales = "free")


print(plot243)
