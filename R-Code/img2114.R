# source: https://bookdown.org/content/4857/small-worlds-and-large-worlds.html#the-garden-of-forking-data



d <-
  tibble(position = c((1:4^1) / 4^0,
                      (1:4^2) / 4^1,
                      (1:4^3) / 4^2),
         draw     = rep(1:3, times = c(4^1, 4^2, 4^3)),
         fill     = rep(c("b", "w"), times = c(1, 3)) %>%
           rep(., times = c(4^0 + 4^1 + 4^2)))





lines_1 <-
  tibble(x    = rep((1:4), each = 4),
         xend = ((1:4^2) / 4),
         y    = 1,
         yend = 2)

lines_2 <-
  tibble(x    = rep(((1:4^2) / 4), each = 4),
         xend = (1:4^3) / (4^2),
         y    = 2,
         yend = 3)


plot2114 <-
  d %>%
  filter(draw < 3) %>%
  ggplot(aes(x = position, y = draw)) +
  geom_segment(data = lines_1,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               size  = 1/3) +
  # geom_segment(data = lines_2,
  #              aes(x = x, xend = xend,
  #                  y = y, yend = yend),
  #              size  = 1/3) +
  geom_point(aes(fill = fill),
             shape = 21, size = 3) +
  scale_fill_manual(values  = c("navy", "white")) +
  scale_y_continuous(breaks = 1:3) +
  theme(legend.position  = "none",
        panel.grid.minor = element_blank()) +
  labs(y = "Zug",
       x = "Gezogene Murmel") +
  scale_x_discrete(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme_minimal()+
  guides(fill = FALSE)
print(plot2114)
