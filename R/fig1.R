library(PlotSvalbard)

rm(list = ls())

coords <- sf::st_as_sf(tibble(lon = c(-50, -35), lat = c(50, 90)), coords = c("lon", "lat"), crs = 4326) %>%
  sf::st_transform(crs = "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

station_coords <- sf::st_as_sf(tibble(lon = c(-63.78953333), lat = c(67.47973333)), coords = c("lon", "lat"), crs = 4326) %>%
  sf::st_transform(crs = "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

p <- basemap("panarctic", limits = sf::st_bbox(coords)[c(1, 3, 2, 4)], bathymetry = TRUE, bathy.detailed = TRUE, land.size = 0.01, base_size = 10) +
  geom_point(aes(x = -2222852, y = -1094283), color = "red", size = 0.25) +
  theme(legend.text = element_text(size = 6)) +
  theme(legend.title = element_text(size = 8)) +
  theme(legend.key.size = unit(0.25, "cm"))
  
ggsave("graphs/fig1.pdf", width = 8, height = 8, units = "cm", device = cairo_pdf)
system("pdfcrop graphs/fig1.pdf graphs/fig1.pdf")

detach("package:PlotSvalbard", unload = TRUE)
