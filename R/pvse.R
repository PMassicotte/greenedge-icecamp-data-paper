
rm(list = ls())

df <- read_csv("/mnt/nfs/scratch/mariepieramyot/backup/pvse/greenedge_pvse.csv") %>% 
  filter(str_detect(mission, "ice_camp")) %>% 
  filter(sample_type == "water" & sample_source == "niskin" & depth_m != "interface") %>% 
  mutate(depth_m = parse_number(depth_m))

df %>% 
  count(mission, depth_m)

df %>% 
  filter(depth_m == 1.5) %>% 
  ggplot(aes(x = lubridate::yday(date), y = ps, color = mission)) +
  geom_path()
