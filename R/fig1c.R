library(raster)

r <- raster("/home/pmassicotte/Downloads/IBCAO_V3_500m_SM_tif/IBCAO_V3_500m_SM.tif")

ex <- tibble(lon = c(-64.97, -61.63), lat = c(67.35, 68)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = st_crs(r))

r2 <- raster::crop(r, c(st_bbox(ex)[1], st_bbox(ex)[3], st_bbox(ex)[2], st_bbox(ex)[4]))

plot(r2)

station_coords <- tibble(lon = c(-63.78953333), lat = c(67.47973333)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = st_crs(r2))

points(-2246155, -1105755)


res <- r2 %>% 
  rasterToPoints() %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  mutate(ibcao_v3_500m_sm = ifelse(ibcao_v3_500m_sm > 0, 0, ibcao_v3_500m_sm)) %>% 
  MBA::mba.surf(no.X = 1000, no.Y = 1000)

filled.contour(res$xyz.est)
  
library(MBA)

  
  mutate(depth = cut(ibcao_v3_500m_sm, seq(-2000, 2000, length.out = 9), include.lowest = TRUE)) %>% 
  ggplot(aes(x = x, y = y, fill = depth)) +
  geom_raster() +
  coord_equal() +
  scale_fill_brewer(direction = -1)

  df <- tibble(
 x = 1:2,
   y = list(c(a = 1, b = 2), c(a = 10, b = 11, c = 12))
 )
  