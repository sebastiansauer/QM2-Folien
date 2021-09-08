sample1 <-
  tibble(Farbe = c("B", "W", "B"),
         x = c(1, 2, 3))

plot2112 <-
sample1 %>%
  ggplot(aes(x = x, y = 0, fill = Farbe)) +
  geom_point(shape = 21, size = 5) +
  scale_fill_manual(values = c("navy", "white")) +
  scale_x_discrete(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme_minimal() +
  guides(fill = FALSE)
theme(legend.position = "none",
      axis.line.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank())

print(plot2112)
