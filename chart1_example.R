library(ggplot2)
library(tidyverse)

us_prison_pop <- read_csv("us-prison-pop.csv")

us_prison_pop$total_prison_pop[is.na(us_prison_pop$total_prison_pop)] <- 0

ca_prison_pop_6 <- us_prison_pop %>% 
  filter(state == "CA") %>% 
  filter(county_name %in% c("Los Angeles County", "San Diego County", 
                        "Orange County", "Riverside County", "San Bernardino County", 
                        "Santa Clara County"))

ggplot(ca_prison_pop_6, aes(x = year, y = total_prison_pop, color = county_name)) +
  geom_point() + 
  labs(title = "Prison Population Trends in California’s Six Most Populous Counties",
       subtitle = "Prison Population Trends from 1970 to 2018",
       x = "Year", y = "Total Prison Population")
