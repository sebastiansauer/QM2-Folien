
myf <- function(x) -x+0.75

myf2 <- function(x) -x + 1.25


n <- 1e3


d2 <- tibble(
  x = runif(n),
  y = runif(n),
  status = case_when(
    y > myf(x) & y < myf2(x) ~ TRUE,
    TRUE ~ FALSE
  )
)


p_coll1 <-
  d2 %>%
  ggplot() +
  aes(x  = x,
      y = y) +
  geom_point() +
  # scale_color_manual(values = c("grey80", "black")) +
  theme_bw() +
  labs(x = "Looks",
       y = "Talent") +
  theme(legend.position = "bottom",
        axis.text = element_blank())
