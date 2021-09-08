# source: https://bookdown.org/content/4857/small-worlds-and-large-worlds.html#the-garden-of-forking-data

d <-
  tibble(position = c((1:4^1) / 4^0,
                      (1:4^2) / 4^1,
                      (1:4^3) / 4^2),
         draw     = rep(1:3, times = c(4^1, 4^2, 4^3)),
         fill     = rep(c("b", "w"), times = c(1, 3)) %>%
           rep(., times = c(4^0 + 4^1 + 4^2)))

plot2113 <-
  d %>%
  filter(draw == 1) %>%
  ggplot(aes(x = position, y = draw, fill = fill)) +
  geom_point(shape = 21, size = 3) +
  scale_fill_manual(values  = c("navy", "white")) +
  scale_y_continuous(breaks = 1:3) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  labs(y = "Zug",
       x = "Gezogene Murmel") +
  scale_x_discrete(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme_minimal()+
  guides(fill = FALSE)
print(plot2113)
