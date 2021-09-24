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
  select(date,
         iso_code,
         #total_deaths,
         #new_deaths,
         people_fully_vaccinated_per_hundred,
         total_deaths_per_million,
         #new_vaccinations,
         total_vaccinations) %>%
  filter(date == "2021-09-23") %>%
  group_by(iso_code)

plot_covid1 <-
  d2 %>%
  ggplot(aes(x = iso_code,
             y = people_fully_vaccinated_per_hundred)) +
  geom_col()




plot_covid2 <-
  d2 %>%
  ggplot(aes(x = iso_code,
             y = total_deaths_per_million)) +
  geom_col()


