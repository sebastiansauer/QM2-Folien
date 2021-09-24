library(titanic)

data("titanic_train")


plottitanic1 <-
titanic_train %>%
  select(Pclass, Survived) %>%
  mutate(Survived = factor(Survived)) %>%
  ggplot(aes(x = Pclass)) +
  geom_bar(aes(fill = Survived), position = "fill") +
  theme(legend.position = "bottom")


plottitanic2 <-
titanic_train %>%
  select(Survived, Embarked) %>%
  filter(Embarked %in% c("C", "Q", "S")) %>%
  mutate(Survived = factor(Survived)) %>%
  ggplot(aes(x = Embarked)) +
  geom_bar(aes(fill = Survived), position = "fill") +
  theme(legend.position = "bottom")



library(gmp)

plottitanic3 <-
  titanic_train %>%
  select(Survived, Age) %>%
  mutate(Age_prime = isprime(Age),
         Age_prime = factor(Age_prime)) %>%
  mutate(Survived = factor(Survived)) %>%
  ggplot(aes(x = Age_prime)) +
  geom_bar(aes(fill = Survived), position = "fill") +
  theme(legend.position = "bottom") +
  scale_x_discrete(breaks = c(0, 2),
                     labels = c("Nicht Prim", "Prim"))

# plottitanic3
