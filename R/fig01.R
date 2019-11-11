# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Make a map of the sampling locations.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# ne_ocean <- rnaturalearth::ne_download(category = "physical", type = "ocean", returnclass = "sf", scale = "large")
ne_land <- rnaturalearth::ne_download(category = "physical", type = "land", returnclass = "sf", scale = "medium")

# plot --------------------------------------------------------------------

arrow <- tibble(
  x = -65,
  xend = -62.5,
  y = 73,
  yend = 68
)

station_coords <- tibble(lon = c(-63.78953333), lat = c(67.47973333))

p <- ggplot() +
  # geom_sf(data = ne_ocean, size = 0.25) +
  geom_sf(data = ne_land, size = 0.1) +
  coord_sf(xlim = c(-90, 0), ylim = c(55, 85)) +
  geom_point(
    data = station_coords,
    aes(x = lon, y = lat),
    inherit.aes = FALSE,
    color = "red",
    size = 1
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.5,
    height = unit(0.1, "cm")
  ) +
  theme(axis.title = element_blank()) +
  annotate(
    geom = "text",
    x = -76,
    y = 73.5,
    label = "Qikiqtarjuaq",
    vjust = -0.25,
    hjust = 0,
    size = 2
  ) +
  annotate(
    geom = "text",
    x = -53,
    y = 78,
    label = "Greenland",
    vjust = 0,
    hjust = 0,
    size = 3
  ) +
  theme(
    panel.grid.major = element_line(
      color = "#999999",
      linetype = "dashed",
      size = 0.1
    ),
    panel.background = element_rect(fill = "aliceblue")
  ) +
  geom_curve(
    data = arrow,
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    curvature = -0.3,
    size = 0.2,
    arrow = arrow(length = unit(0.05, "inch"))
  )

ggsave("graphs/fig01.pdf", width = 8, height = 8, units = "cm", device = cairo_pdf)

# annotate(geom = "text", x = 1, y = 78, label = "Greenland Sea", vjust = 0, hjust = 0, size = 4, family = "IBM Plex Sans") +