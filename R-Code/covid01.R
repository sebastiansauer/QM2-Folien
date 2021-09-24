# source: https://ourworldindata.org/covid-vaccinations
# access date: 2021-09-24
# licence: https://ourworldindata.org/covid-vaccinations#licence



dfile <- "/Users/sebastiansaueruser/datasets/Covid/owid-covid-data.csv"

library(lubridate)

d <- read_csv(dfile)

d2<-
  d %>%
  filter(iso_code %in% c("DEU", "USA")) %>%
  mutate(date = as_date(date)) %>%
  rename(Land = iso_code) %>%
  select(date,
         Land,
         #total_deaths,
         #new_deaths,
         people_fully_vaccinated_per_hundred,
         total_deaths_per_million,
         #new_vaccinations,
         total_vaccinations) %>%
  filter(date == "2021-09-23") %>%
  group_by(Land)


# d2 %>%
#   ungroup() %>%
#   count(people_fully_vaccinated_per_hundred)

plot_covid1 <-
  d2 %>%
  ggplot(aes(x = Land,
             y = people_fully_vaccinated_per_hundred)) +
  geom_col() +
  labs(title = "Anteil komplett geimpfter Personen",
       subtitle = "2021-09-23")




plot_covid2 <-
  d2 %>%
  ggplot(aes(x = Land,
             y = total_deaths_per_million)) +
  geom_col()+
  labs(title = "Corona-Tote pro Million",
       subtitle = "2021-09-23")


