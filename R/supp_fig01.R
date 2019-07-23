# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Supp Fig showing tidal height. Data from E. Rhem.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

tide <- tibble(
  hour = 0:23,
  height = c(0.8, 0.7, 0.7, 0.7, 0.8, 0.9, 1.0, 1.1, 1.1, 1.0, 0.9, 0.7, 0.5, 0.3, 0.2, 0.3, 0.3, 0.5, 0.7, 0.9, 1.0, 1.1, 1.1, 1.0)
)

p <- tide %>%
  filter(between(hour, 8, 21)) %>%
  ggplot(aes(x = hour, y = height)) +
  geom_line() +
  geom_point() +
  xlab("Hours of the day") +
  ylab("Tide height (m)")

ggsave("graphs/supp_fig01.pdf", width = 8, height = 6, units = "cm", device = cairo_pdf)
