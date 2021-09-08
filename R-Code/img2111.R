# source: https://bookdown.org/content/4857/small-worlds-and-large-worlds.html#the-garden-of-forking-data

d <-
  tibble(p1 = 0,
         p2 = rep(1:0, times = c(1, 3)),
         p3 = rep(1:0, times = c(2, 2)),
         p4 = rep(1:0, times = c(3, 1)),
         p5 = 1)

plot2111 <-
d %>%
  set_names(1:5) %>%
  mutate(x = 1:4) %>%
  pivot_longer(-x, names_to = "Hypothesen") %>%
  mutate(value = value %>% as.character()) %>%
  ggplot(aes(x = x, y = Hypothesen, fill = value)) +
  geom_point(shape = 21, size = 5) +
  scale_fill_manual(values = c("white", "navy")) +
  scale_x_discrete(NULL, breaks = NULL) +
  theme(legend.position = "none")

print(plot2111)
