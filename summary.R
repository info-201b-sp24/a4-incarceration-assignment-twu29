library(dplyr)

us_prison_pop <- read_csv("us-prison-pop.csv")

summary(us_prison_pop)

us_prison_pop$total_prison_pop[is.na(us_prison_pop$total_prison_pop)] <- 0

ca_prison_pop <- us_prison_pop %>% 
  filter(state == "CA") 

# avg of ca_prison_pop 
mean_ca_prison_pop <- mean(ca_prison_pop$total_prison_pop)

# Which county has the highest number of total_prison_pop in CA?
max_ca_prison_pop <- ca_prison_pop %>% 
  filter(total_prison_pop == max(total_prison_pop, na.rm = TRUE)) %>% 
  pull(county_name)

num_highest_county <- ca_prison_pop %>% 
  filter(total_prison_pop == max(total_prison_pop, na.rm = TRUE)) %>% 
  pull(total_prison_pop)

# Which county in CA has the highest ratio of total_prison_pop to total_pop
county_highest_ratio <- ca_prison_pop %>% 
  mutate(ratio = total_prison_pop / total_pop) %>% 
  filter(ratio == max(ratio, na.rm = TRUE)) %>% 
  pull(county_name)

num_highest_ratio <- ca_prison_pop %>% 
  mutate(ratio = total_prison_pop / total_pop) %>% 
  filter(ratio == max(ratio, na.rm = TRUE)) %>% 
  pull(ratio)

# Which county in CA has the highest ratio of black_prison_pop to total_prison_pop
county_highest_black_ratio <- ca_prison_pop %>% 
  mutate(black_ratio = black_prison_pop / total_prison_pop) %>% 
  filter(black_ratio == max(black_ratio, na.rm = TRUE)) %>% 
  pull(county_name)

num_highest_black_ratio <- ca_prison_pop %>% 
  mutate(black_ratio = black_prison_pop / total_prison_pop) %>% 
  filter(black_ratio == max(black_ratio, na.rm = TRUE)) %>% 
  pull(black_ratio)

# Which county in CA has the highest ratio of latinx_prison_pop to total_prison_pop
county_highest_latinx_ratio <- ca_prison_pop %>% 
  mutate(latinx_ratio  = latinx_prison_pop / total_prison_pop) %>% 
  filter(latinx_ratio  == max(latinx_ratio, na.rm = TRUE)) %>% 
  pull(county_name)

num_highest_latinx_ratio <- ca_prison_pop %>% 
  mutate(latinx_ratio  = latinx_prison_pop / total_prison_pop) %>% 
  filter(latinx_ratio  == max(latinx_ratio, na.rm = TRUE)) %>% 
  pull(latinx_ratio )

# Which county in CA has the highest ratio of white_prison_pop to total_prison_pop
county_highest_white_ratio <- ca_prison_pop %>% 
  mutate(white_ratio = white_prison_pop / total_prison_pop) %>% 
  filter(white_ratio == max(white_ratio, na.rm = TRUE)) %>% 
  pull(county_name)

num_highest_white_ratio <- ca_prison_pop %>% 
  mutate(white_ratio = white_prison_pop / total_prison_pop) %>% 
  filter(white_ratio == max(white_ratio, na.rm = TRUE)) %>% 
  pull(white_ratio)

# Which county in CA has the highest ratio of other_race_prison_pop to total_prison_pop
county_highest_other_ratio <- ca_prison_pop %>% 
  mutate(other_ratio = other_race_prison_pop / total_prison_pop) %>% 
  filter(other_ratio == max(other_ratio, na.rm = TRUE)) %>% 
  pull(county_name)

num_highest_other_ratio <- ca_prison_pop %>% 
  mutate(other_ratio = other_race_prison_pop / total_prison_pop) %>% 
  filter(other_ratio == max(other_ratio, na.rm = TRUE)) %>% 
  pull(other_ratio)

