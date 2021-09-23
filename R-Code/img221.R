d <- tibble(toss = c("w", "l", "w", "w", "w", "l", "w", "l", "w"))


d <-
  d %>%
  mutate(n_trials  = 1:9,
         n_success = cumsum(toss == "w"))



sequence_length <- 50

plot221 <-
d %>%
  expand(nesting(n_trials, toss, n_success),
         p_water = seq(from = 0, to = 1, length.out = sequence_length)) %>%
  group_by(p_water) %>%
  # you can learn more about lagging here: https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/lag or here: https://dplyr.tidyverse.org/reference/lead-lag.html
  mutate(lagged_n_trials  = lag(n_trials,  k = 1),
         lagged_n_success = lag(n_success, k = 1)) %>%
  ungroup() %>%
  mutate(prior      = ifelse(n_trials == 1, .5,
                             dbinom(x    = lagged_n_success,
                                    size = lagged_n_trials,
                                    prob = p_water)),
         likelihood = dbinom(x    = n_success,
                             size = n_trials,
                             prob = p_water),
         strip      = str_c("n = ", n_trials)) %>%
  # the next three lines allow us to normalize the prior and the likelihood,
  # putting them both in a probability metric
  group_by(n_trials) %>%
  mutate(prior      = prior      / sum(prior),
         likelihood = likelihood / sum(likelihood)) %>%

  # plot!
  ggplot(aes(x = p_water)) +
  geom_line(aes(y = prior), linetype = 2) +
  geom_line(aes(y = likelihood)) +
  scale_x_continuous("Anteil Wasser", breaks = c(0, .5, 1)) +
  scale_y_continuous("Plausibilität", breaks = NULL) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~strip, scales = "free_y") +
  labs(caption= "Gestrichelte Linie: Priori-Verteilung (vor den Daten);\nDurchgezogene Linie: Posteriori-Verteilung (nach Daten)")

print(plot221)
