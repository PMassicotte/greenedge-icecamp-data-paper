# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Temporal evolution of PAR under the ice (1.3 meters).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

cops <- read_csv("/mnt/nfs/scratch/mariepieramyot/backup/cops/cops.csv", guess_max = 1e6) %>% 
  filter(str_detect(mission, "ice_camp"))

# "../cops/data/raw/cops/ice-camp/2015/RES.EXCEL/GE2015.ICMP_ICEP_150403_CAST_002/"

df <- cops %>% 
  filter(between(depth_m, 10, 50)) %>% 
  group_by(mission, date, depth_m) %>% 
  summarise(averaged_par_d_fit_daily_ein_m_2_day_1 = mean(averaged_par_d_fit_daily_ein_m_2_day_1, na.rm = TRUE))

df %>% 
  ggplot(aes(x = averaged_par_d_fit_daily_ein_m_2_day_1, y = depth_m, group = date)) +
  geom_line() +
  scale_y_reverse() +
  facet_wrap(~mission, scales = "free")

par <- df %>% 
  group_by(mission, date) %>% 
  nest() %>% 
  mutate(par_model_water = purrr::map(
  data,
  ~ minpack.lm::nlsLM(
    averaged_par_d_fit_daily_ein_m_2_day_1 ~ a0 * exp(-kd * depth_m),
    start = list(a0 = 1, kd = 0.05),
    data = .
  )
)) %>%
  mutate(par_1_3m_ein_m_2_day_1 = map_dbl(par_model_water, predict, newdata = c(depth_m = 1.3))) %>% 
  mutate(kd = map_dbl(par_model_water, function(x) {
    
    coef(x)[2]
    
  }))

par %>% 
  mutate(yday = lubridate::yday(date)) %>% 
  ggplot(aes(x = yday, y = par_1_3m_ein_m_2_day_1, color = mission)) +
  geom_line(size = 0.25) +
  geom_point(show.legend = FALSE, size = 0.5) +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(as.Date("2015-01-01"), as.Date("2015-12-31"), by = "1 month") %>% lubridate::yday(), 
                     limits = c(90, 200), 
                     labels = function(x) {as.Date(paste0("2015-", x), "%Y-%j") %>% format("%b")}) +
  annotation_logticks(sides = "l") +
  xlab(NULL) +
  ylab(bquote(PAR~(mol~m^{-2}~d^{-1}))) +
  geom_hline(aes(yintercept = 0.415), lty = 2, color = "gray50", size = 0.5) +
  annotate("text", x = 195, y = 0.415, label = "0.415", vjust = -0.5, size = 3, color = "gray50") +
  theme(legend.position = c(1, 0), legend.justification = c(1.01, -0.01)) +
  theme(legend.title = element_blank()) +
  scale_color_brewer(palette = "Set2", breaks = c("ice_camp_2015", "ice_camp_2016"), labels = c("2015", "2016"))

ggsave("graphs/fig4.pdf", width = 8, height = 6, units = "cm", device = cairo_pdf)
