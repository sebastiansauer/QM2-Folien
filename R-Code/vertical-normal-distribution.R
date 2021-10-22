library(ggplot2)

# source: https://stackoverflow.com/questions/61589781/plot-vertical-density-of-normal-distribution-in-r-and-ggplot2

x <- runif(100, 0, 15)
y <- 1000 + 200*x + rnorm(100, 0, 300)
df <- data.frame(x, y)
lm_fit <- lm(y ~ x, data = df)

k <- 2.5
sigma <- sigma(lm_fit)

ab <- coef(lm_fit); a <- ab[1]; b <- ab[2]

x <- seq(-k*sigma, k*sigma, length.out = 50)
y <- dnorm(x, 0, sigma)/dnorm(0, 0, sigma) * 3

x0 <- 0
y0 <- a+b*x0
path1 <- data.frame(x = y + x0, y = x + y0)
segment1 <- data.frame(x = x0, y = y0 - k*sigma, xend = x0, yend = y0 + k*sigma)
x0 <- 5
y0 <- a+b*x0
path2 <- data.frame(x = y + x0, y = x + y0)
segment2 <- data.frame(x = x0, y = y0 - k*sigma, xend = x0, yend = y0 + k*sigma)
x0 <- 10
y0 <- a+b*x0
path3 <- data.frame(x = y + x0, y = x + y0)
segment3 <- data.frame(x = x0, y = y0 - k*sigma, xend = x0, yend = y0 + k*sigma)

p <- ggplot(df, mapping = aes(x=x, y=y)) + geom_point(color="blue") +
  geom_smooth(method='lm', se=FALSE, color="red") +
  geom_path(aes(x,y), data = path1, color = "green") +
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data = segment1) +
  geom_path(aes(x,y), data = path2, color = "green") +
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data = segment2) +
  geom_path(aes(x,y), data = path3, color = "green") +
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data = segment3)

p
