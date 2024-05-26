library(ggplot2)
library(dplyr)
library(maps)
library(mapproj)

#install.packages("maps")
#install.packages("mapproj")

us_prison_pop <- read_csv("us-prison-pop.csv")

us_prison_pop$total_prison_pop[is.na(us_prison_pop$total_prison_pop)] <- 0

ca_prison_pop_6 <- us_prison_pop %>% 
  filter(state == "CA") %>% 
  mutate(ratio = total_prison_pop / total_pop) %>% 
  group_by(county_name) %>%
  summarize(mean_ratio = mean(ratio, na.rm = TRUE))

ca_counties <- map_data("county", region = "california")

ca_prison_pop_6 <- ca_prison_pop_6 %>%
  mutate(county_name = tolower(gsub(" County", "", county_name)))

ca_counties <- ca_counties %>%
  mutate(subregion = tolower(subregion))

ca_map_data <- left_join(ca_counties, ca_prison_pop_6, by = c("subregion" = "county_name"))
  
ggplot(data = ca_map_data) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = mean_ratio), color = "black") +
  scale_fill_continuous(low = "pink", high = "red") +
  coord_map() +
  labs(title = "Crime Rate in California state",
       subtitle = "from 1970 to 2018", fill = "Mean Ratio of Total Prison Population to Total Population")

