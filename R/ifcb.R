rm(list = ls())

df <- read_csv("/mnt/nfs/scratch/mariepieramyot/backup/ifcb/greenedge_ifcb.csv") %>% 
  filter(str_detect(mission, "ice_camp")) %>% 
  filter(sample_type == "water" & sample_source == "niskin") %>% 
  mutate(yday = lubridate::yday(date)) %>% 
  drop_na(carbon_estimate_mg_c_m3)

# df <- df %>% 
#   filter(depth_m == 1.5) %>% 
#   group_by(mission, yday) %>% 
#   mutate(proportion_carbon_estimate_mg_c_m3 = carbon_estimate_mg_c_m3 / sum(carbon_estimate_mg_c_m3)) %>% 
#   ungroup()


# Try 1 -------------------------------------------------------------------

res <- df %>% 
  group_by(mission, taxonomy) %>% 
  summarise(sum_estimate_mg_c_m3 = sum(carbon_estimate_mg_c_m3)) %>% 
  group_by(mission) %>% 
  mutate(percent_contribution = sum_estimate_mg_c_m3 / sum(sum_estimate_mg_c_m3)) %>% 
  top_n(9, percent_contribution) %>% 
  inner_join(df)

res %>% 
  filter(carbon_estimate_mg_c_m3 > 0) %>% 
  ggplot(aes(x = carbon_estimate_mg_c_m3)) +
  geom_density(aes(fill = mission, color = mission), alpha = 0.5) +
  scale_x_log10() +
  annotation_logticks(sides = "b") +
  facet_wrap(~taxonomy, ncol = 3) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2")

# Try 2 -------------------------------------------------------------------

res <- df %>% 
  filter(as.Date(date) %in% as.Date(c("2016-06-17", "2016-06-29"))) %>% 
  group_by(date) %>% 
  mutate(percent_contribution = carbon_estimate_mg_c_m3 / sum(carbon_estimate_mg_c_m3)) %>%
  ungroup()

res %>% 
  ggplot(aes(x = taxonomy, y = percent_contribution)) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~date)
