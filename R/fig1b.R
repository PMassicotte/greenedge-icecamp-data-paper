# lim <- tibble(xlim = c(5, 26), ylim = c(76, 82)) %>%
#   st_as_sf(coords = c("xlim", "ylim"), crs = 4326) %>%
#   st_transform(crs = proj)

ex <- tibble(lon = c(-64, -58), lat = c(66.637, 68.6799)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = st_crs(bathy))

bathy <- raster::brick("data/raw/bathy.tiff") %>%
  raster::crop(c(st_bbox(ex)[1], st_bbox(ex)[3], st_bbox(ex)[2], st_bbox(ex)[4])) %>% 
  raster::focal(w=matrix(1, 5, 5), mean)

station_coords <- tibble(lon = c(-63.78953333), lat = c(67.47973333)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = st_crs(bathy))

RStoolbox::ggRGB(bathy, r = 1, g = 2, b = 3, ggLayer = F) + 
  # coord_sf(crs = 3411, expand = c(0, 0)) +
  xlab(NULL) +
  ylab(NULL) +
  geom_sf(data = station_coords, size = 5, color = "red") +
  coord_sf(crs = 3411, expand = c(0, 0))


library(PlotSvalbard)

basemap("panarctic", bathymetry = TRUE, limits.lon = c(-64, -60.63), limits.lat = c(66.637, 68.6799))
