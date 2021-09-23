sequence_length <- 1e3

d <-
  tibble(probability = seq(from = 0, to = 1, length.out = sequence_length)) %>%
  expand(probability, row = c("flat", "stepped", "Laplace")) %>%
  arrange(row, probability) %>%
  mutate(prior = ifelse(row == "flat", 1,
                        ifelse(row == "stepped", rep(0:1, each = sequence_length / 2),
                               exp(-abs(probability - 0.5) / .25) / ( 2 * 0.25))),
         likelihood = dbinom(x = 6, size = 9, prob = probability)) %>%
  group_by(row) %>%
  mutate(posterior = prior * likelihood / sum(prior * likelihood)) %>%
  pivot_longer(prior:posterior)  %>%
  ungroup() %>%
  mutate(name = factor(name, levels = c("prior", "likelihood", "posterior")),
         row  = factor(row, levels = c("flat", "stepped", "Laplace")))



p1 <-
  d %>%
  filter(row == "flat") %>%
  ggplot(aes(x = probability, y = value)) +
  geom_line() +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ name, scales = "free_y")

p2 <-
  d %>%
  filter(row == "stepped") %>%
  ggplot(aes(x = probability, y = value)) +
  geom_line() +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  facet_wrap(~ name, scales = "free_y")

p3 <-
  d %>%
  filter(row == "Laplace") %>%
  ggplot(aes(x = probability, y = value)) +
  geom_line() +
  scale_x_continuous(NULL, breaks = c(0, .5, 1)) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  facet_wrap(~ name, scales = "free_y")

# combine

plot241 <- p1 / p2 / p3

print(plot241)
