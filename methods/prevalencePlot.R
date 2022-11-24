library(ggplot2)
library(dplyr)

str(age_dist <- bigreadr::fread2("../Prevalences/RESULTS_age_distribution.txt"))

age_dist %>%
  filter(birth_year == 1950) %>%
  ggplot(aes(age_dx, pct, color = sex)) +
  geom_line() +
  facet_wrap(~ dx)

str(cip <- bigreadr::fread2("../Prevalences/RESULTS_cip.txt"))

#disorders in the data: e.g. d33 = single and recurrent depression
cip %>%
  select(dx) %>%
  pull() %>%
  unique()

code = "D33"

cip %>%
  filter(birth_year == 1980) %>%
  ggplot(aes(age, cip, color = sex)) +
  geom_line() +
  facet_wrap(~ dx, scales = "free_y")

code = "D33"
# ADHD: adhd
# ASD: asd2 (the larger of the two groups)
# SCZ: D21
# DEP: D33

# grey grid lines
col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255)

title_string = "Depression"
CIP_Plot = cip %>%
  filter(dx == code) %>%
  filter(birth_year %% 5 == 0, birth_year <= 2005, birth_year >= 1950 ) %>%
  ggplot(aes(age, cip, color = sex)) +
  geom_line(size = 1.1) +
  theme_minimal(base_size = 22) +
  theme(plot.title = element_text(hjust = .5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = col_grid, size = .5), 
        legend.position = "none") +
  ggtitle(title_string) +
  scale_color_manual(labels = c("F", "M"), values = c("red", "blue")) +
  facet_wrap(~ birth_year, ncol = 3) +
  ylab("Cumulative Incidence Proportion")

ggsave(filename = "prevalencePlot_DEP.png",
       plot = CIP_Plot,
       device = "png",
       dpi = 300,
       width = 7,
       height = 8)
