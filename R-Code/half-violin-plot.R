library(tidyverse)
library(gghalves)

n <- 1e3
x <- sample(0:15, n, replace = TRUE)
y <- 1000 + 200*x + rnorm(n, 0, 300)
df <- data.frame(x, y)
lm_fit <- lm(y ~ x, data = df)

df2 <-
  df %>%
  filter(x %in% c(3,6,9,12))

df %>%
  ggplot() +
  aes(x = x, y = y) +
  geom_boxplot() +
  geom_point(alpha = .1) +
  geom_abline(slope = coef(lm_fit)[2], intercept = coef(lm_fit)[1]) +
  geom_half_violin(data = df2, aes(x = x, y = y, group = x))


