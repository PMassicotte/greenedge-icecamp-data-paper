library(MBA)

rm(list = ls())


df <- read_csv("/mnt/nfs/scratch/mariepieramyot/backup/primary_prod/greenedge_primary_prod.csv") %>% 
  filter(str_detect(mission, "ice_camp")) %>% 
  filter(sample_type == "water" & sample_source == "niskin") %>% 
  drop_na(pp_mg_m3_24h)

df <- df %>%
  group_by(mission) %>%
  nest() %>%
  mutate(interpolated_pp_mg_m3_24h = map(data, function(df) {
    
    res <- df %>%
      dplyr::select(date, depth_m, pp_mg_m3_24h) %>%
      mutate(date = as.numeric(date, origin = "1970-01-01", tz = "UTC")) %>%
      mba.surf(500, 500, extend = TRUE)
    
    res2 <- expand.grid(date = res$xyz.est$x, depth_m = res$xyz.est$y) %>%
      mutate(interpolated_pp_mg_m3_24h = as.vector(res$xyz.est$z))
    
    return(res2)
    
  }))

df <- df %>%
  unnest(interpolated_pp_mg_m3_24h) %>%
  drop_na(interpolated_pp_mg_m3_24h) %>%
  # mutate(no3_um_l = ifelse(no3_um_l < 0, 0, no3_um_l)) %>%
  mutate(bin = cut(
    interpolated_pp_mg_m3_24h,
    breaks = seq(min(interpolated_pp_mg_m3_24h), max(interpolated_pp_mg_m3_24h) + 1, by = 0.2),
    include.lowest = TRUE,
    right = TRUE
  )) %>%
  mutate(date = as.Date(date, origin = "1970-01-01", tz = "UTC"))

mylabels <- c(
  "ice_camp_2015" = "Ice camp 2015",
  "ice_camp_2016" = "Ice camp 2016"
)

df %>%
  distinct(mission, date, depth_m, .keep_all = T) %>% 
  ggplot(aes(x = date, y = interpolated_pp_mg_m3_24h, fill = bin)) +
  geom_raster() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_fill_manual(values = colorRampPalette(viridis::viridis(22, direction = -1))(22)) +
  scale_x_date(expand = c(0, 0), date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(~mission, scales = "free", ncol = 1, labeller = labeller(mission = mylabels)) +
  theme(legend.text = element_text(size = 6)) +
  theme(legend.title = element_text(size = 6)) +
  theme(legend.key.size = unit(0.25, "cm")) +
  labs(fill = "Salinity (PSU)") +
  xlab("Date") +
  ylab("Depth (m)") 
