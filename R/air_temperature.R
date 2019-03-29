df <- data.table::fread("/mnt/nfs/scratch/mariepieramyot/backup/meteo/greenedge_meteo_tower.csv") %>% 
  filter(str_detect(mission, "ice_camp"))

df <- df %>% 
  mutate(hour = lubridate::hour(date)) %>% 
  mutate(yday = lubridate::yday(date)) %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(yday, year) %>% 
  summarise(mean_temperature = mean(air_temp_celsius, na.rm = TRUE), n = n())

df %>% 
  ggplot(aes(x = yday, y = mean_temperature, color = factor(year))) +
  geom_line() +
  geom_point()

df %>% 
  ggplot(aes(x = mean_temperature, fill = factor(year), color = factor(year))) +
  geom_density(alpha = 0.5)
