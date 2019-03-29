rm(list = ls())

df <- read_csv("/mnt/nfs/scratch/mariepieramyot/backup/cyto/greenedge_cyto.csv", guess_max = 1e6) %>% 
  filter(sample_type == "water" & sample_source == "niskin") %>% 
  filter(cytoplankton == "bacteria" & cyto_type == "HBact") %>% 
  mutate(yday = lubridate::yday(date_ok))

df %>% 
  ggplot(aes(x = factor(depth_m), y = nb_cell_ml)) +
  geom_boxplot() +
  facet_wrap(~mission)

df %>% 
  filter(depth_m == 1.5) %>% 
  drop_na(nb_cell_ml) %>% 
  ggplot(aes(x = yday, y = nb_cell_ml, color = mission)) +
  geom_line()
