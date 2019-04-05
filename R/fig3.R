# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Temporal evolution of the salinity in the water column.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- data.table::fread("/mnt/nfs/scratch/mariepieramyot/backup/ctd/greenedge_ctd.csv") %>%
  filter(str_detect(mission, "ice_camp")) %>%
  drop_na(psal_psu) %>%
  mutate(date = as.Date(date)) %>%
  group_by(mission, pres_decibars, date) %>%
  summarise(psal_psu = mean(psal_psu), n = n()) %>%
  filter(pres_decibars <= 5)


df <- df %>%
  group_by(mission) %>%
  nest() %>%
  mutate(interpolated_psal = map(data, function(df) {
    res <- df %>%
      dplyr::select(date, pres_decibars, psal_psu) %>%
      mutate(date = as.numeric(date, origin = "1970-01-01", tz = "UTC")) %>%
      mba.surf(500, 500, extend = TRUE)
    
    res2 <- expand.grid(date = res$xyz.est$x, pres_decibars = res$xyz.est$y) %>%
      mutate(interpolated_psal = as.vector(res$xyz.est$z))
    
    return(res2)
    
  }))

df <- df %>%
  unnest(interpolated_psal) %>%
  drop_na(interpolated_psal) %>%
  # mutate(no3_um_l = ifelse(no3_um_l < 0, 0, no3_um_l)) %>%
  mutate(bin = cut(
    interpolated_psal,
    breaks = seq(min(interpolated_psal), max(interpolated_psal) + 1, by = 2),
    include.lowest = TRUE,
    right = TRUE
  )) %>%
  mutate(date = as.Date(date, origin = "1970-01-01", tz = "UTC"))

mylabels <- c(
  "ice_camp_2015" = "Ice camp 2015",
  "ice_camp_2016" = "Ice camp 2016"
)

p <- df %>%
  ggplot(aes(x = lubridate::yday(date), y = pres_decibars, fill = bin)) +
  geom_raster() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_fill_manual(values = colorRampPalette(viridis::viridis(15, direction = -1))(15)) +
  scale_x_continuous(expand = c(0, 0)) +
  facet_wrap(~mission, ncol = 1, labeller = labeller(mission = mylabels)) +
  theme(legend.text = element_text(size = 6)) +
  theme(legend.title = element_text(size = 6)) +
  theme(legend.key.size = unit(0.25, "cm")) +
  labs(fill = "Salinity (PSU)") +
  xlab("Day of the year") +
  ylab("Depth (m)") 

ggsave("graphs/fig3.pdf", width = 8, height = 10, units = "cm", device = cairo_pdf)

