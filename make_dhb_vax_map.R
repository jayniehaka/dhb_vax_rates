# dhb vax rates map

# libraries
library(tidyverse)
library(scales)
library(ggthemes)
library(DHBins)

# get data from csv
dhb_data <- read_csv("dhb_vax_rates.csv") %>%
  head(-2) %>%
  rename(dhb = 1,
         first_doses = `First doses %`,
         second_doses = `Second doses %`) %>%
  mutate(dhb = dhb_fixname(dhb),
         first_doses = parse_number(first_doses)/100,
         second_doses = parse_number(second_doses)/100) %>%
  select(dhb, first_doses, second_doses) %>%
  pivot_longer(!dhb, names_to = "doses", values_to = "pop_percent")

# make ggplot theme
my_theme <- theme_tufte(base_family = "Georgia") +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size=18),
        plot.title.position = "plot",
        legend.position = c(1.1, .15),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0, size = 11))

# map second doses only
second_doses_map <- dhb_data %>%
  filter(doses == "second_doses") %>%
  ggplot() +
  geom_dhb(aes(fill = pop_percent, map_id = dhb)) +
  scale_fill_distiller(palette = 13, direction = 1,
                       labels = percent_format(accuracy = 1),
                       limits = c(.60, .91)) +
  geom_label_dhb(short = FALSE, colour = "black", cex = 2.7) +
  labs(title = "Vaccination rates by DHB",
       subtitle = "Percentage of eligible population with two doses as at 22/10/2021",
       caption = "By @jayniehaka") +
  my_theme

# map first and second doses
both_doses_map <- dhb_data %>%
  mutate(doses = case_when(doses == "second_doses" ~ "Second dose",
                           doses == "first_doses" ~ "First dose")) %>%
  ggplot() +
  geom_dhb(aes(fill = pop_percent, map_id = dhb)) +
  facet_grid(cols = vars(doses)) +
  scale_fill_distiller(palette = 13, direction = 1,
                       labels = percent_format(accuracy = 1),
                       limits = c(.59, .95)) +
  geom_label_dhb(short = TRUE, colour = "black", cex = 3.5) +
  labs(title = "Vaccination rates by DHB",
       subtitle = "Percentage of eligible population as at 22/10/2021",
       caption = "By @jayniehaka") +
  my_theme

# save the maps
ggsave("maps/dhb_second_doses_map.jpg", plot = second_doses_map,
       width = 2000, height = 2500, units = "px")
ggsave("maps/dhb_both_doses_map.jpg", plot = both_doses_map,
       width = 2500, height = 2000, units = "px")