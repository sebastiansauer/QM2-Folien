
d <-
  tibble(p_grid = seq(from = 0, to = 1, length.out = 20),      # define grid
         prior  = 1) %>%                                       # define prior
  mutate(likelihood = dbinom(6, size = 9, prob = p_grid)) %>%  # compute likelihood at each value in grid
  mutate(unstd_posterior = likelihood * prior) %>%             # compute product of likelihood and prior
  mutate(posterior = unstd_posterior / sum(unstd_posterior))   # standardize the posterior, so it sums to 1



p1 <-
  d %>%
  ggplot(aes(x = p_grid, y = posterior)) +
  geom_point() +
  geom_line() +
  labs(subtitle = "20 Gitterwerte",
       x = "p(W)",
       y = "Post") +
  theme(panel.grid = element_blank())


p2 <-
  tibble(p_grid = seq(from = 0, to = 1, length.out = 10),
         prior  = 1) %>%
  mutate(likelihood = dbinom(6, size = 9, prob = p_grid)) %>%
  mutate(unstd_posterior = likelihood * prior) %>%
  mutate(posterior = unstd_posterior / sum(unstd_posterior)) %>%
  ggplot(aes(x = p_grid, y = posterior)) +
  geom_point() +
  geom_line() +
  labs(subtitle = "10 Gitterwerte",
       x = "p(W)",
       y = "Post") +
  theme(panel.grid = element_blank())



p3 <-
  tibble(p_grid = seq(from = 0, to = 1, length.out = 5),
         prior  = 1) %>%
  mutate(likelihood = dbinom(6, size = 9, prob = p_grid)) %>%
  mutate(unstd_posterior = likelihood * prior) %>%
  mutate(posterior = unstd_posterior / sum(unstd_posterior)) %>%

  ggplot(aes(x = p_grid, y = posterior)) +
  geom_point() +
  geom_line() +
  labs(subtitle = "5 Gitterwerte",
       x = "p(W)",
       y = "Post") +
  theme(panel.grid = element_blank())




plot242 <- p3 + p2  + p1

print(plot242)
