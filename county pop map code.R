library(tidyverse)
library(sf)
library(Hmisc)

mn_counties <- st_read("MNCounties_MNDOT.shp", quiet = TRUE) %>%
  rename(countyfp = FIPS_CODE)

pop.county <- read_csv("countypop1900_2016Kelly.csv") %>%
  mutate(countyfp = formatC(countyfp, width = 3, flag = "0")) %>%
  gather(key = "year", value = "population", pop2016:pop1900) %>%
  left_join(mn_counties[,c(4,7)], by = "countyfp") %>%
  mutate(year = str_sub(year, -4, -1),
         year = as.integer(year),
         Name = capitalize(Name)) %>%
  group_by(Name) %>%
  arrange(year) %>%
  mutate(pct.change = (population - lag(population)) / lag(population)) %>%
  ungroup() %>%
  mutate(bins = cut(population,
                    breaks = c(0, 9999, 19999, 29999, 39999, 49999, 2000000),
                    labels = c("1- 9,999", "10,000 - 19,999", "20,000 - 29,999", "30,000 - 39,999", "40,000 - 49,999", "50,000+")),
         bins = ifelse(is.na(bins), "Not yet incorporated", as.character(bins)))
