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
         first_doses_percent = `First doses %`,
         second_doses_percent = `Second doses %`) %>%
  mutate(dhb = dhb_fixname(dhb),
         first_doses_percent = parse_number(first_doses_percent)/100,
         second_doses_percent = parse_number(second_doses_percent)/100)

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

# draw the map
ggplot(dhb_data) +
  geom_dhb(aes(fill = second_doses_percent, map_id = dhb)) +
  scale_fill_distiller(palette = 13, direction = 1,
                       labels = percent_format(accuracy = 1),
                       limits = c(.59, .91)) +
  geom_label_dhb(short = FALSE, colour = "black", cex = 2.7) +
  labs(title = "Vaccination rates by DHB",
       subtitle = "Percentage of eligible population with two doses as at 22/10/2021",
       caption = "By @jayniehaka") +
  my_theme