library(ggplot2)
library(tidyverse)

us_prison_pop <- read_csv("us-prison-pop.csv")

ca_prison_pop_6 <- us_prison_pop %>% 
  filter(state == "CA") %>% 
  filter(county_name %in% c("Los Angeles County", "San Diego County", 
                            "Orange County", "Riverside County", "San Bernardino County", 
                            "Santa Clara County")) %>% 
  na.omit(us_prison_pop)

ggplot(ca_prison_pop_6, aes(x = total_pop, y = total_prison_pop, color = county_name)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE, col = "grey25", aes(group = county_name))
  labs(title = "Relationship Between Total Population and Total Prison Population",
       subtitle = "Prison Population Trends from 1970 to 2018",
       x = "Total Population", y = "Total Prison Population")

  