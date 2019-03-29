rm(list = ls())

df <- read_csv("/mnt/nfs/scratch/mariepieramyot/backup/ifcb/greenedge_ifcb.csv") %>% 
  filter(str_detect(mission, "ice_camp")) %>% 
  filter(sample_type == "water" & sample_source == "niskin") %>% 
  mutate(yday = lubridate::yday(date)) %>% 
  drop_na(carbon_estimate_mg_c_m3)

df <- df %>% 
  filter(depth_m == 1.5) %>% 
  group_by(mission, yday) %>% 
  mutate(proportion_carbon_estimate_mg_c_m3 = carbon_estimate_mg_c_m3 / sum(carbon_estimate_mg_c_m3)) %>% 
  ungroup()
  
  
df %>% 
  ggplot(aes(x = yday, y = proportion_carbon_estimate_mg_c_m3, fill = taxonomy, group = taxonomy)) +
  geom_area() +
  facet_wrap(~mission)
