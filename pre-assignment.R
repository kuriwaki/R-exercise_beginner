library(tidyverse)
library(readxl)
library(ggrepel)
library(scales)


# Read-in dataset --------
weo <- read_excel("data/input/WEO-2018.xlsx")


# 3: Sorting by Values ------
## 3.1 
weo %>%
  arrange(rgdp2017) %>%
  select(country, rgdp2017)

## 3.2
weo %>%
  arrange(desc(rgdp2017)) %>%
  select(country, rgdp2017)


## 3.3
weo %>%
  arrange(continent, desc(rgdp2017)) %>%
  select(continent, country, rgdp2017)


## 3.4
weo %>%
  filter(continent == "Africa") %>%
  arrange(desc(rgdp2017)) %>%
  select(country, rgdp2017)


## 4: GDP per capita --------

weo_percap <- weo %>%
  mutate(gdp_percap_2017 = rgdp2017 / pop2017,
         gdp_percap_1992 = rgdp1992 / pop1992,
         growth_2017_1992 = gdp_percap_2017 - gdp_percap_1992)

## 5. Graphing --------

## 5.1
ggplot(weo_percap, aes(gdp_percap_1992, gdp_percap_2017)) +
  geom_point()


## 5.2
ggplot(weo_percap,
       aes(x = gdp_percap_1992, y = gdp_percap_2017, color = continent)) +
  geom_point()


## 5.3
ggplot(weo_percap,
       aes(x = gdp_percap_1992, y = gdp_percap_2017, shape = continent)) +
  geom_point(color = "navy")


## 5.4
ggplot(weo_percap, aes(x = gdp_percap_1992, 
                       y = gdp_percap_2017, 
                       color = continent)) +
  geom_point() +
  labs(x = "GDP per capita in 1992",
       y = "GDP per capita in 2007",
       color = "Continent")


## 6: Mean and Median --------
## 6.1
weo_percap %>%
  summarize(mean_gcap_2017 = mean(gdp_percap_2017),
            median_gcap_2017 = median(gdp_percap_2017))


## 6.2
weo_percap %>%
  filter(is.na(gdp_percap_1992)) %>%
  select(country)

weo_percap %>%
  filter(is.na(gdp_percap_2017)) %>%
  select(country)


## 6.3
weo_percap %>%
  summarize(mean_gcap_1992 = mean(gdp_percap_1992, na.rm = TRUE),
            median_gcap_1992 = median(gdp_percap_1992, na.rm = TRUE))


## 7: slice and filter --------
weo %>%
 arrange(desc(rgdp2017)) %>%
 select(country, rgdp2017) %>%
 slice(c(1:3, 189:191))


## Optional exercises --------

# only use large enough countiries
ds_large <- weo_percap %>%
  filter(pop1992 > 5 | pop2017 > 5)

# subset to show country names. Only use the top 5.
sub_show <- ds_large %>%
  mutate(abs_change = abs(gdp_percap_2017 - gdp_percap_1992)) %>%
  arrange(desc(abs_change)) %>%
  select(country, gdp_percap_2017, gdp_percap_1992) %>%
  slice(1:5)

# Creat the graph, but add a layer of geom_text_repel and input the subset there 
gg_scatter <- ggplot(ds_large, aes(gdp_percap_1992, gdp_percap_2017)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(aes(size = pop2017), alpha = 0.5) +
  coord_equal() +
  geom_text_repel(data = sub_show, aes(label = country)) +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = dollar) +
  guides(size = FALSE) +
  labs(x = "1992 GDP per Capita",
       y = "2017 GDP per Capita",
       caption = "Points sized by 2017 population and labels show top 5 countries
       with the most absolute change. Only countries with population at least 5 million\nin 1992 or 2017 shown. Dotted line indicates 45-degree line.")

## save the ggplot object to a pdf - 5-inch square
ggsave("gg_scatter.pdf", gg_scatter, width = 5, height = 5)

