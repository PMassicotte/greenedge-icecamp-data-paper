rm(list = ls())

df <- data.table::fread("/mnt/nfs/scratch/mariepieramyot/backup/absorption/greenedge_absorption_cdom.csv") %>% 
  as_tibble() %>% 
  dplyr::filter(str_detect(mission, "ice_camp")) %>% 
  dplyr::filter(sample_type == "water") %>% 
  filter(sample_source == "niskin") %>% 
  # dplyr::filter(!str_detect(comments, "salinity")) %>% 
  dplyr::filter(between(wavelength, 254, 600)) %>% 
  filter(depth_m == 2)

res <- df %>% 
  group_by(date, mission, snow_thickness, depth_m) %>% 
  nest() %>% 
  mutate(n = map_int(data, nrow)) %>% 
  arrange(desc(n)) %>% 
  slice(2) %>% 
  unnest()


df %>% 
  ggplot(aes(x = wavelength, y = absorption_cdom, group = interaction(date), color = mission)) +
  geom_line(size = 0.1) 
