rm(list = ls())

df <- read_csv("/mnt/nfs/scratch/mariepieramyot/backup/ice_conc_history/greenedge_ice_conc_history.csv") %>% 
  filter(str_detect(mission, "ice_camp")) %>% 
  mutate(yday = lubridate::yday(date))

df %>% 
  ggplot(aes(x = yday, y = sea_ice_conc_percent / 100, color = mission)) +
  geom_line() +
  geom_point()