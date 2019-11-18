df <- data.table::fread("/mnt/nfs/scratch/mariepieramyot/backup/ctd/greenedge_ctd.csv") %>%
  filter(str_detect(mission, "ice_camp")) %>% 
  select(contains("sal"))

df %>% 
  drop_na() %>% 
  ggplot(aes(x = psal_psu, y = asal_g_kg)) +
  geom_point() +
  xlab("Salinity (PSU)") +
  ylab(bquote("Absolute salinity"~(k~kg^{-1}))) +
  geom_abline(color = "red") +
  labs(
    title = "Comparison of PSU and absolute salinities",
    subtitle = "The red line indicates the 1:1 line"
  )

ggsave("graphs/comparison_psu_absolute_salinities.png", type = "cairo", dpi = 300)
